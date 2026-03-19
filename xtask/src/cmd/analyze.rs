use crate::model::{
    discover_runs, fmt_comma, project_results_dir, Error, Result, TokenUsage,
    PRICE_CACHE_READ, PRICE_CACHE_WRITE, PRICE_INPUT, PRICE_OUTPUT,
};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

// ---------------------------------------------------------------------------
// Per-request deduped metrics
// ---------------------------------------------------------------------------

struct RequestMetrics {
    request_key: String,
    session_label: String,
    timestamp: Option<String>,
    stop_reason: Option<String>,
    usage_rows: u32,
    usage: TokenUsage,
    tool_names: HashMap<String, u32>,
    tool_ids: HashSet<String>,
    text_chars: usize,
    thinking_chars: usize,
    text_items: usize,
    thinking_items: usize,
    content_items: usize,
}

impl RequestMetrics {
    fn new(request_key: String, session_label: String) -> Self {
        Self {
            request_key,
            session_label,
            timestamp: None,
            stop_reason: None,
            usage_rows: 0,
            usage: TokenUsage::default(),
            tool_names: HashMap::new(),
            tool_ids: HashSet::new(),
            text_chars: 0,
            thinking_chars: 0,
            text_items: 0,
            thinking_items: 0,
            content_items: 0,
        }
    }

    fn tool_count(&self) -> usize {
        self.tool_names.values().map(|v| *v as usize).sum()
    }

    fn observe(&mut self, entry: &serde_json::Value, usage: &TokenUsage) {
        self.usage_rows += 1;

        if self.timestamp.is_none() {
            self.timestamp = entry.get("timestamp").and_then(|t| t.as_str()).map(|s| s.to_string());
        }

        let message = entry.get("message").and_then(|m| m.as_object());
        let stop_reason = message.and_then(|m| m.get("stop_reason")).and_then(|s| s.as_str());
        let content = message
            .and_then(|m| m.get("content"))
            .and_then(|c| c.as_array());

        let mut text_chars: usize = 0;
        let mut thinking_chars: usize = 0;
        let mut text_items: usize = 0;
        let mut thinking_items: usize = 0;
        let mut content_items: usize = 0;

        if let Some(content) = content {
            for (idx, item) in content.iter().enumerate() {
                let item = match item.as_object() {
                    Some(o) => o,
                    None => continue,
                };
                let content_type = item.get("type").and_then(|t| t.as_str()).unwrap_or("unknown");
                content_items += 1;

                match content_type {
                    "tool_use" => {
                        let tool_name = item.get("name").and_then(|n| n.as_str()).unwrap_or("?");
                        let tool_id = item
                            .get("id")
                            .and_then(|i| i.as_str())
                            .map(|s| s.to_string())
                            .unwrap_or_else(|| format!("{tool_name}:{idx}:{}", self.usage_rows));
                        if self.tool_ids.insert(tool_id) {
                            *self.tool_names.entry(tool_name.to_string()).or_insert(0) += 1;
                        }
                    }
                    "text" => {
                        text_items += 1;
                        text_chars += item.get("text").and_then(|t| t.as_str()).map(|s| s.len()).unwrap_or(0);
                    }
                    "thinking" => {
                        thinking_items += 1;
                        thinking_chars += item.get("thinking").and_then(|t| t.as_str()).map(|s| s.len()).unwrap_or(0);
                    }
                    _ => {}
                }
            }
        }

        // Keep max across streaming rows
        self.text_chars = self.text_chars.max(text_chars);
        self.thinking_chars = self.thinking_chars.max(thinking_chars);
        self.text_items = self.text_items.max(text_items);
        self.thinking_items = self.thinking_items.max(thinking_items);
        self.content_items = self.content_items.max(content_items);

        // Snapshot replacement: keep the usage with highest output_tokens
        let replace = if self.usage_rows == 1 {
            true
        } else if usage.output_tokens > self.usage.output_tokens {
            true
        } else if usage.output_tokens == self.usage.output_tokens {
            (self.stop_reason.is_none() && stop_reason.is_some())
                || content_items > self.content_items
        } else {
            false
        };

        if replace {
            self.usage = usage.clone();
            self.stop_reason = stop_reason.map(|s| s.to_string());
        }
    }
}

// ---------------------------------------------------------------------------
// Run summary
// ---------------------------------------------------------------------------

struct RunSummary {
    name: String,
    session_count: usize,
    usage_rows: u32,
    requests: Vec<RequestMetrics>,
    totals: TokenUsage,
}

impl RunSummary {
    fn billed_requests(&self) -> usize {
        self.requests.len()
    }
}

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

pub fn run(runs: Vec<PathBuf>, all: bool) -> Result<()> {
    let run_dirs = resolve_runs(runs, all)?;
    if run_dirs.is_empty() {
        return Err(Error::NoRuns);
    }

    let mut summaries: Vec<RunSummary> = Vec::new();
    for run_dir in &run_dirs {
        let summary = build_run_summary(run_dir)?;
        print_run_summary(&summary);
        summaries.push(summary);
    }

    if summaries.len() == 2 {
        print_comparison(&summaries[0], &summaries[1]);
    }

    Ok(())
}

fn resolve_runs(runs: Vec<PathBuf>, all: bool) -> Result<Vec<PathBuf>> {
    if all {
        let results_dir = project_results_dir();
        let discovered = discover_runs(&results_dir)?;
        Ok(discovered.into_iter().map(|(path, _)| path).collect())
    } else if runs.is_empty() {
        Err(Error::NoRuns)
    } else {
        for r in &runs {
            if !r.is_dir() {
                return Err(Error::RunNotFound { path: r.clone() });
            }
        }
        Ok(runs)
    }
}

// ---------------------------------------------------------------------------
// Session file discovery
// ---------------------------------------------------------------------------

fn session_files(results_dir: &Path) -> Vec<(String, PathBuf)> {
    let top = results_dir.join("session.jsonl");
    if top.is_file() {
        let label = results_dir
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_default();
        return vec![(label, top)];
    }

    let mut files = Vec::new();
    if let Ok(entries) = fs::read_dir(results_dir) {
        let mut level_dirs: Vec<PathBuf> = entries
            .flatten()
            .filter(|e| e.file_name().to_string_lossy().starts_with('L') && e.path().is_dir())
            .map(|e| e.path())
            .collect();
        level_dirs.sort();

        for ldir in level_dirs {
            let session = ldir.join("session.jsonl");
            if session.is_file() {
                let label = ldir
                    .file_name()
                    .map(|n| n.to_string_lossy().into_owned())
                    .unwrap_or_default();
                files.push((label, session));
            }
        }
    }
    files
}

// ---------------------------------------------------------------------------
// Build run summary with request-level dedup
// ---------------------------------------------------------------------------

fn build_run_summary(results_dir: &Path) -> Result<RunSummary> {
    let name = results_dir
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default();

    let sessions = session_files(results_dir);
    if sessions.is_empty() {
        return Err(Error::RunNotFound {
            path: results_dir.to_path_buf(),
        });
    }

    let mut all_requests: Vec<RequestMetrics> = Vec::new();
    let mut total_usage_rows: u32 = 0;

    for (session_label, session_path) in &sessions {
        let (requests, usage_rows) = parse_session(session_path, session_label)?;
        total_usage_rows += usage_rows;
        all_requests.extend(requests);
    }

    // Sort by timestamp
    all_requests.sort_by(|a, b| a.timestamp.cmp(&b.timestamp));

    // Sum deduped totals
    let mut totals = TokenUsage::default();
    for req in &all_requests {
        totals.add(&req.usage);
    }

    Ok(RunSummary {
        name,
        session_count: sessions.len(),
        usage_rows: total_usage_rows,
        requests: all_requests,
        totals,
    })
}

fn parse_session(
    jsonl_path: &Path,
    session_label: &str,
) -> Result<(Vec<RequestMetrics>, u32)> {
    let content = fs::read_to_string(jsonl_path).map_err(|e| Error::io(jsonl_path, e))?;
    let mut per_request: HashMap<String, RequestMetrics> = HashMap::new();
    let mut usage_rows: u32 = 0;

    for line in content.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let entry: serde_json::Value = match serde_json::from_str(line) {
            Ok(v) => v,
            Err(_) => continue,
        };

        let (request_key, usage) = match extract_usage(&entry) {
            Some(pair) => pair,
            None => continue,
        };

        usage_rows += 1;
        let metrics = per_request
            .entry(request_key.clone())
            .or_insert_with(|| RequestMetrics::new(request_key, session_label.to_string()));
        metrics.observe(&entry, &usage);
    }

    let mut requests: Vec<RequestMetrics> = per_request.into_values().collect();
    requests.sort_by(|a, b| a.timestamp.cmp(&b.timestamp));
    Ok((requests, usage_rows))
}

fn extract_usage(entry: &serde_json::Value) -> Option<(String, TokenUsage)> {
    // Find usage: try top-level, then message.usage
    let usage_obj = entry
        .get("usage")
        .and_then(|u| u.as_object())
        .or_else(|| {
            entry
                .get("message")
                .and_then(|m| m.as_object())
                .and_then(|m| m.get("usage"))
                .and_then(|u| u.as_object())
        })?;

    // Find request key: requestId || message.id || uuid
    let request_key = entry
        .get("requestId")
        .and_then(|r| r.as_str())
        .or_else(|| {
            entry
                .get("message")
                .and_then(|m| m.as_object())
                .and_then(|m| m.get("id"))
                .and_then(|i| i.as_str())
        })
        .or_else(|| entry.get("uuid").and_then(|u| u.as_str()))?;

    let usage = TokenUsage {
        input_tokens: usage_obj.get("input_tokens").and_then(|v| v.as_u64()).unwrap_or(0),
        output_tokens: usage_obj.get("output_tokens").and_then(|v| v.as_u64()).unwrap_or(0),
        cache_creation_input_tokens: usage_obj.get("cache_creation_input_tokens").and_then(|v| v.as_u64()).unwrap_or(0),
        cache_read_input_tokens: usage_obj.get("cache_read_input_tokens").and_then(|v| v.as_u64()).unwrap_or(0),
    };

    Some((request_key.to_string(), usage))
}

// ---------------------------------------------------------------------------
// Output: per-run summary
// ---------------------------------------------------------------------------

fn print_run_summary(summary: &RunSummary) {
    let requests = &summary.requests;
    let n = summary.billed_requests();
    let cost_parts = cost_breakdown(&summary.totals);
    let total_cost = cost_parts.iter().map(|(_, v)| v).sum::<f64>();

    let sep = "=".repeat(96);
    println!("\n{sep}");
    println!("  {}", summary.name);
    println!("{sep}");

    if n > 0 {
        println!(
            "Sessions={}  Rows={}  Requests={}  Rows/Request={:.2}",
            summary.session_count,
            fmt_comma(summary.usage_rows as u64),
            fmt_comma(n as u64),
            summary.usage_rows as f64 / n as f64,
        );
    } else {
        println!(
            "Sessions={}  Rows={}  Requests=0",
            summary.session_count,
            fmt_comma(summary.usage_rows as u64),
        );
        return;
    }

    println!(
        "Cost=${:.2}  Input=${:.2}  Output=${:.2}  CacheWrite=${:.2}  CacheRead=${:.2}",
        total_cost, cost_parts[0].1, cost_parts[1].1, cost_parts[2].1, cost_parts[3].1,
    );

    // Stop reasons
    let mut stop_counts: HashMap<String, usize> = HashMap::new();
    for req in requests {
        let reason = req.stop_reason.as_deref().unwrap_or("none");
        *stop_counts.entry(reason.to_string()).or_insert(0) += 1;
    }
    let mut stop_sorted: Vec<_> = stop_counts.into_iter().collect();
    stop_sorted.sort_by(|a, b| b.1.cmp(&a.1));
    let stop_str: Vec<String> = stop_sorted.iter().map(|(k, v)| format!("{k}={v}")).collect();
    println!("Stop reasons: {}", stop_str.join(", "));

    // Tool mix
    let mut tool_counts: HashMap<String, u32> = HashMap::new();
    for req in requests {
        for (name, count) in &req.tool_names {
            *tool_counts.entry(name.clone()).or_insert(0) += count;
        }
    }
    let mut tool_sorted: Vec<_> = tool_counts.into_iter().collect();
    tool_sorted.sort_by(|a, b| b.1.cmp(&a.1));
    let tool_str: Vec<String> = tool_sorted.iter().map(|(k, v)| format!("{k}={v}")).collect();
    println!("Tool mix: {}", if tool_str.is_empty() { "none".to_string() } else { tool_str.join(", ") });

    // Turn shape
    let no_tool = requests.iter().filter(|r| r.tool_count() == 0).count();
    let single_tool = requests.iter().filter(|r| r.tool_count() == 1).count();
    let multi_tool = requests.iter().filter(|r| r.tool_count() > 1).count();
    let avg_tools = requests.iter().map(|r| r.tool_count()).sum::<usize>() as f64 / n as f64;
    println!(
        "Turn shape: no-tool={no_tool}, single-tool={single_tool}, multi-tool={multi_tool}, avg_tools/request={avg_tools:.2}"
    );

    // Output buckets
    let small = requests.iter().filter(|r| r.usage.output_tokens <= 300).count();
    let medium = requests.iter().filter(|r| (301..=2000).contains(&r.usage.output_tokens)).count();
    let large = requests.iter().filter(|r| r.usage.output_tokens > 2000).count();
    println!(
        "Output buckets: <=300={small} ({}), 301-2000={medium} ({}), >2000={large} ({})",
        pct(small, n), pct(medium, n), pct(large, n),
    );

    // Text/thinking stats
    let text_reqs = requests.iter().filter(|r| r.text_items > 0).count();
    let text_chars: usize = requests.iter().map(|r| r.text_chars).sum();
    let thinking_reqs = requests.iter().filter(|r| r.thinking_items > 0).count();
    let thinking_chars: usize = requests.iter().map(|r| r.thinking_chars).sum();
    println!(
        "Text requests={text_reqs} ({}), text chars={text_chars}",
        pct(text_reqs, n),
    );
    println!(
        "Thinking requests={thinking_reqs} ({}), thinking chars={thinking_chars}",
        pct(thinking_reqs, n),
    );

    // Avg cache read
    let avg_cache = summary.totals.cache_read_input_tokens as f64 / n as f64;
    println!("Avg cache read/request={avg_cache:.1}");

    // Anti-pattern detection
    println!("Likely drivers:");
    let drivers = likely_drivers(summary);
    for issue in &drivers {
        println!("  - {issue}");
    }

    // Top requests
    print_top_requests("Top cache-read requests:", requests, |r| r.usage.cache_read_input_tokens);
    print_top_requests("Top output requests:", requests, |r| r.usage.output_tokens);
}

fn print_top_requests(title: &str, requests: &[RequestMetrics], key: fn(&RequestMetrics) -> u64) {
    let mut ranked: Vec<&RequestMetrics> = requests.iter().filter(|r| key(r) > 0).collect();
    ranked.sort_by(|a, b| key(b).cmp(&key(a)));
    ranked.truncate(3);

    if ranked.is_empty() {
        return;
    }

    println!("{title}");
    for req in &ranked {
        println!(
            "  - {}:{} {}={} output={} stop={} tools={}",
            req.session_label,
            req.request_key,
            title.split_whitespace().nth(1).unwrap_or("?"),
            fmt_comma(key(req)),
            fmt_comma(req.usage.output_tokens),
            req.stop_reason.as_deref().unwrap_or("none"),
            req.tool_count(),
        );
    }
}

// ---------------------------------------------------------------------------
// Anti-pattern detection
// ---------------------------------------------------------------------------

fn likely_drivers(summary: &RunSummary) -> Vec<String> {
    let requests = &summary.requests;
    let n = summary.billed_requests();
    if n == 0 {
        return vec!["No billed requests found.".to_string()];
    }

    let cost_parts = cost_breakdown(&summary.totals);
    let total_cost: f64 = cost_parts.iter().map(|(_, v)| v).sum();
    let cache_read_cost = cost_parts[3].1;

    let single_tool = requests.iter().filter(|r| r.tool_count() == 1).count();
    let small_output = requests.iter().filter(|r| r.usage.output_tokens <= 300).count();
    let text_reqs = requests.iter().filter(|r| r.text_items > 0).count();
    let text_chars: usize = requests.iter().map(|r| r.text_chars).sum();
    let max_token_reqs: Vec<&RequestMetrics> = requests
        .iter()
        .filter(|r| r.stop_reason.as_deref() == Some("max_tokens"))
        .collect();

    let mut issues = Vec::new();

    if n >= 80 {
        issues.push(format!(
            "{n} billed requests; the run is heavily fragmented across turns."
        ));
    }
    if total_cost > 0.0 && cache_read_cost >= total_cost * 0.5 {
        issues.push(format!(
            "Cache reads are {} of total cost.",
            pct_of(cache_read_cost, total_cost),
        ));
    }
    if n >= 25 && single_tool >= n * 8 / 10 {
        issues.push(format!(
            "{} of requests use exactly one tool; related reads and shell calls are not being batched.",
            pct(single_tool, n),
        ));
    }
    if n >= 25 && small_output >= n / 2 {
        issues.push(format!(
            "{} of requests produce <=300 output tokens, which looks like micro-turn tool loops.",
            pct(small_output, n),
        ));
    }
    if text_chars >= 50_000 && text_reqs >= n * 4 / 10 {
        issues.push(format!(
            "Narration is heavy: {text_chars} text chars across {text_reqs} requests."
        ));
    }
    if !max_token_reqs.is_empty() {
        let largest = max_token_reqs
            .iter()
            .max_by_key(|r| r.usage.output_tokens)
            .expect("non-empty");
        issues.push(format!(
            "{} requests hit max_tokens; the largest emitted {} output tokens.",
            max_token_reqs.len(),
            fmt_comma(largest.usage.output_tokens),
        ));
    }

    if issues.is_empty() {
        issues.push("No obvious anti-pattern crossed the analyzer thresholds.".to_string());
    }

    issues
}

// ---------------------------------------------------------------------------
// Comparison (two runs)
// ---------------------------------------------------------------------------

fn print_comparison(left: &RunSummary, right: &RunSummary) {
    let left_parts = cost_breakdown(&left.totals);
    let right_parts = cost_breakdown(&right.totals);
    let left_cost: f64 = left_parts.iter().map(|(_, v)| v).sum();
    let right_cost: f64 = right_parts.iter().map(|(_, v)| v).sum();

    let ln = left.billed_requests().max(1) as f64;
    let rn = right.billed_requests().max(1) as f64;

    let rows: Vec<(&str, f64, f64, &str)> = vec![
        ("Requests", left.billed_requests() as f64, right.billed_requests() as f64, "count"),
        ("Total cost", left_cost, right_cost, "money"),
        ("Cache read tokens", left.totals.cache_read_input_tokens as f64, right.totals.cache_read_input_tokens as f64, "count"),
        (
            "Cache read cost share",
            if left_cost > 0.0 { left_parts[3].1 / left_cost } else { 0.0 },
            if right_cost > 0.0 { right_parts[3].1 / right_cost } else { 0.0 },
            "ratio",
        ),
        ("Avg cache read/request", left.totals.cache_read_input_tokens as f64 / ln, right.totals.cache_read_input_tokens as f64 / rn, "count"),
        (
            "Avg tools/request",
            left.requests.iter().map(|r| r.tool_count()).sum::<usize>() as f64 / ln,
            right.requests.iter().map(|r| r.tool_count()).sum::<usize>() as f64 / rn,
            "float",
        ),
        (
            "Single-tool ratio",
            left.requests.iter().filter(|r| r.tool_count() == 1).count() as f64 / ln,
            right.requests.iter().filter(|r| r.tool_count() == 1).count() as f64 / rn,
            "ratio",
        ),
        (
            "Small-output ratio",
            left.requests.iter().filter(|r| r.usage.output_tokens <= 300).count() as f64 / ln,
            right.requests.iter().filter(|r| r.usage.output_tokens <= 300).count() as f64 / rn,
            "ratio",
        ),
        (
            "Text chars",
            left.requests.iter().map(|r| r.text_chars).sum::<usize>() as f64,
            right.requests.iter().map(|r| r.text_chars).sum::<usize>() as f64,
            "count",
        ),
        (
            "Max-tokens requests",
            left.requests.iter().filter(|r| r.stop_reason.as_deref() == Some("max_tokens")).count() as f64,
            right.requests.iter().filter(|r| r.stop_reason.as_deref() == Some("max_tokens")).count() as f64,
            "count",
        ),
    ];

    let sep = "=".repeat(96);
    println!("\n{sep}");
    println!("  Comparison: {} vs {}", right.name, left.name);
    println!("{sep}");
    println!("{:<24} {:>18} {:>18} {:>18}", "Metric", left.name, right.name, "Diff");
    println!("{}", "-".repeat(96));

    for (label, lv, rv, kind) in &rows {
        let lv = *lv;
        let rv = *rv;
        let diff = rv - lv;
        match *kind {
            "money" => {
                let pct_diff = if lv > 0.0 { format!(" ({:+.1}%)", diff / lv * 100.0) } else { String::new() };
                println!("{:<24} {:>17}$ {:>17}$ {:>17}",
                    label, format!("{lv:.2}"), format!("{rv:.2}"), format!("{diff:+.2}{pct_diff}"));
            }
            "ratio" => {
                let pp = (rv - lv) * 100.0;
                println!("{:<24} {:>17}% {:>17}% {:>17}",
                    label, format!("{:.1}", lv * 100.0), format!("{:.1}", rv * 100.0), format!("{pp:+.1}pp"));
            }
            "float" => {
                let pct_diff = if lv > 0.0 { format!(" ({:+.1}%)", diff / lv * 100.0) } else { String::new() };
                println!("{:<24} {:>18.2} {:>18.2} {:>17}",
                    label, lv, rv, format!("{diff:+.2}{pct_diff}"));
            }
            _ => {
                let pct_diff = if lv > 0.0 { format!(" ({:+.1}%)", diff / lv * 100.0) } else { String::new() };
                println!("{:<24} {:>18} {:>18} {:>17}",
                    label, fmt_comma(lv as u64), fmt_comma(rv as u64), format!("{:+}{pct_diff}", diff as i64));
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn cost_breakdown(t: &TokenUsage) -> Vec<(&'static str, f64)> {
    vec![
        ("input", t.input_tokens as f64 * PRICE_INPUT),
        ("output", t.output_tokens as f64 * PRICE_OUTPUT),
        ("cache_write", t.cache_creation_input_tokens as f64 * PRICE_CACHE_WRITE),
        ("cache_read", t.cache_read_input_tokens as f64 * PRICE_CACHE_READ),
    ]
}

fn pct(num: usize, denom: usize) -> String {
    if denom == 0 {
        "0.0%".to_string()
    } else {
        format!("{:.1}%", num as f64 / denom as f64 * 100.0)
    }
}

fn pct_of(num: f64, denom: f64) -> String {
    if denom == 0.0 {
        "0.0%".to_string()
    } else {
        format!("{:.1}%", num / denom * 100.0)
    }
}
