use crate::codex;
use crate::model::{
    discover_runs, fmt_comma, project_results_dir, Error, MetaJson, Result, TokenUsage,
    PRICE_CACHE_READ, PRICE_CACHE_WRITE, PRICE_INPUT, PRICE_OUTPUT,
};
use crate::session::{self, SessionFormat};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

// ---------------------------------------------------------------------------
// Provider / usage types
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Provider {
    Claude,
    Codex,
}

impl Provider {
    fn from_agent(agent: &str) -> Option<Self> {
        match agent {
            "claude" => Some(Self::Claude),
            "codex" => Some(Self::Codex),
            _ => None,
        }
    }

    fn from_format(format: SessionFormat) -> Self {
        match format {
            SessionFormat::Claude => Self::Claude,
            SessionFormat::Codex => Self::Codex,
        }
    }

    fn avg_cache_bucket_label(self) -> &'static str {
        match self {
            Self::Claude => "Avg cache read/request",
            Self::Codex => "Avg cached input/request",
        }
    }

    fn top_cache_bucket_title(self) -> &'static str {
        match self {
            Self::Claude => "Top cache-read requests:",
            Self::Codex => "Top cached-input requests:",
        }
    }

    fn cache_bucket_metric_label(self) -> &'static str {
        match self {
            Self::Claude => "cache_read",
            Self::Codex => "cached_input",
        }
    }

    fn cache_bucket_tokens_label(self) -> &'static str {
        match self {
            Self::Claude => "Cache read tokens",
            Self::Codex => "Cached input tokens",
        }
    }

    fn cache_bucket_cost_share_label(self) -> &'static str {
        match self {
            Self::Claude => "Cache read cost share",
            Self::Codex => "Cached input cost share",
        }
    }

    fn cache_bucket_phrase(self) -> &'static str {
        match self {
            Self::Claude => "Cache reads",
            Self::Codex => "Cached input",
        }
    }
}

#[derive(Default, Clone, Debug)]
struct UsageTotals {
    input_tokens: u64,
    output_tokens: u64,
    cache_write_tokens: u64,
    cache_read_tokens: u64,
    cached_input_tokens: u64,
}

impl UsageTotals {
    fn from_claude(usage: &TokenUsage) -> Self {
        Self {
            input_tokens: usage.input_tokens,
            output_tokens: usage.output_tokens,
            cache_write_tokens: usage.cache_creation_input_tokens,
            cache_read_tokens: usage.cache_read_input_tokens,
            cached_input_tokens: 0,
        }
    }

    fn from_codex(usage: &codex::Usage) -> Self {
        Self {
            input_tokens: usage.input_tokens,
            output_tokens: usage.output_tokens,
            cache_write_tokens: 0,
            cache_read_tokens: 0,
            cached_input_tokens: usage.cached_input_tokens,
        }
    }

    fn add(&mut self, other: &Self) {
        self.input_tokens += other.input_tokens;
        self.output_tokens += other.output_tokens;
        self.cache_write_tokens += other.cache_write_tokens;
        self.cache_read_tokens += other.cache_read_tokens;
        self.cached_input_tokens += other.cached_input_tokens;
    }

    fn output_tokens(&self) -> u64 {
        self.output_tokens
    }

    fn cache_bucket_tokens(&self, provider: Provider) -> u64 {
        match provider {
            Provider::Claude => self.cache_read_tokens,
            Provider::Codex => self.cached_input_tokens,
        }
    }
}

#[derive(Default, Clone, Copy, Debug)]
struct CostTotals {
    input: f64,
    output: f64,
    cache_write: f64,
    cache_read: f64,
    cached_input: f64,
}

impl CostTotals {
    fn add(&mut self, other: &Self) {
        self.input += other.input;
        self.output += other.output;
        self.cache_write += other.cache_write;
        self.cache_read += other.cache_read;
        self.cached_input += other.cached_input;
    }

    fn total(&self) -> f64 {
        self.input + self.output + self.cache_write + self.cache_read + self.cached_input
    }

    fn cache_bucket_cost(&self, provider: Provider) -> f64 {
        match provider {
            Provider::Claude => self.cache_read,
            Provider::Codex => self.cached_input,
        }
    }
}

fn claude_cost_breakdown(usage: &TokenUsage) -> CostTotals {
    CostTotals {
        input: usage.input_tokens as f64 * PRICE_INPUT,
        output: usage.output_tokens as f64 * PRICE_OUTPUT,
        cache_write: usage.cache_creation_input_tokens as f64 * PRICE_CACHE_WRITE,
        cache_read: usage.cache_read_input_tokens as f64 * PRICE_CACHE_READ,
        cached_input: 0.0,
    }
}

fn codex_cost_breakdown(usage: &codex::Usage, model: Option<&str>) -> Option<CostTotals> {
    let pricing = model.and_then(codex::pricing)?;
    let breakdown = pricing.cost_breakdown(usage);
    Some(CostTotals {
        input: breakdown.input,
        output: breakdown.output,
        cache_write: 0.0,
        cache_read: 0.0,
        cached_input: breakdown.cached_input,
    })
}

// ---------------------------------------------------------------------------
// Content observation (Claude request aggregation)
// ---------------------------------------------------------------------------

fn observe_content(
    content: Option<&Vec<serde_json::Value>>,
    tool_ids: &mut HashSet<String>,
    tool_names: &mut HashMap<String, u32>,
    usage_rows: u32,
) -> ContentStats {
    let mut stats = ContentStats::default();
    let Some(content) = content else { return stats };
    for (idx, item) in content.iter().enumerate() {
        let Some(item) = item.as_object() else {
            continue;
        };
        stats.content_items += 1;
        tally_content_item(item, idx, &mut stats, tool_ids, tool_names, usage_rows);
    }
    stats
}

fn tally_content_item(
    item: &serde_json::Map<String, serde_json::Value>,
    idx: usize,
    stats: &mut ContentStats,
    tool_ids: &mut HashSet<String>,
    tool_names: &mut HashMap<String, u32>,
    usage_rows: u32,
) {
    let content_type = item
        .get("type")
        .and_then(|t| t.as_str())
        .unwrap_or("unknown");
    match content_type {
        "tool_use" => {
            let tool_name = item.get("name").and_then(|n| n.as_str()).unwrap_or("?");
            let tool_id = item
                .get("id")
                .and_then(|i| i.as_str())
                .map(|s| s.to_string())
                .unwrap_or_else(|| format!("{tool_name}:{idx}:{usage_rows}"));
            if tool_ids.insert(tool_id) {
                *tool_names.entry(tool_name.to_string()).or_insert(0) += 1;
            }
        }
        "text" => {
            stats.text_items += 1;
            stats.text_chars += item
                .get("text")
                .and_then(|t| t.as_str())
                .map(|s| s.len())
                .unwrap_or(0);
        }
        "thinking" => {
            stats.thinking_items += 1;
            stats.thinking_chars += item
                .get("thinking")
                .and_then(|t| t.as_str())
                .map(|s| s.len())
                .unwrap_or(0);
        }
        "tool_result" => {
            stats.tool_result_items += 1;
        }
        _ => {}
    }
}

#[derive(Default)]
struct ContentStats {
    text_chars: usize,
    thinking_chars: usize,
    text_items: usize,
    thinking_items: usize,
    tool_result_items: usize,
    content_items: usize,
}

// ---------------------------------------------------------------------------
// Per-request deduped metrics
// ---------------------------------------------------------------------------

struct RequestMetrics {
    request_key: String,
    session_label: String,
    timestamp: Option<String>,
    stop_reason: Option<String>,
    usage_rows: u32,
    usage: UsageTotals,
    costs: CostTotals,
    pricing_known: bool,
    tool_names: HashMap<String, u32>,
    tool_ids: HashSet<String>,
    text_chars: usize,
    thinking_chars: usize,
    text_items: usize,
    thinking_items: usize,
    tool_result_items: usize,
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
            usage: UsageTotals::default(),
            costs: CostTotals::default(),
            pricing_known: false,
            tool_names: HashMap::new(),
            tool_ids: HashSet::new(),
            text_chars: 0,
            thinking_chars: 0,
            text_items: 0,
            thinking_items: 0,
            tool_result_items: 0,
            content_items: 0,
        }
    }

    fn tool_count(&self) -> usize {
        self.tool_names.values().map(|v| *v as usize).sum()
    }

    fn output_tokens(&self) -> u64 {
        self.usage.output_tokens()
    }

    fn cache_bucket_tokens(&self, provider: Provider) -> u64 {
        self.usage.cache_bucket_tokens(provider)
    }

    fn observe_claude(&mut self, entry: &serde_json::Value, usage: &TokenUsage) {
        self.usage_rows += 1;

        if self.timestamp.is_none() {
            self.timestamp = entry
                .get("timestamp")
                .and_then(|t| t.as_str())
                .map(|s| s.to_string());
        }

        let message = entry.get("message").and_then(|m| m.as_object());
        let stop_reason = message
            .and_then(|m| m.get("stop_reason"))
            .and_then(|s| s.as_str());
        let content = message
            .and_then(|m| m.get("content"))
            .and_then(|c| c.as_array());

        let stats = observe_content(
            content,
            &mut self.tool_ids,
            &mut self.tool_names,
            self.usage_rows,
        );

        self.text_chars = self.text_chars.max(stats.text_chars);
        self.thinking_chars = self.thinking_chars.max(stats.thinking_chars);
        self.text_items = self.text_items.max(stats.text_items);
        self.thinking_items = self.thinking_items.max(stats.thinking_items);
        self.tool_result_items = self.tool_result_items.max(stats.tool_result_items);
        self.content_items = self.content_items.max(stats.content_items);

        let replace = if self.usage_rows == 1 || usage.output_tokens > self.output_tokens() {
            true
        } else if usage.output_tokens == self.output_tokens() {
            (self.stop_reason.is_none() && stop_reason.is_some())
                || stats.content_items > self.content_items
        } else {
            false
        };

        if replace {
            self.usage = UsageTotals::from_claude(usage);
            self.costs = claude_cost_breakdown(usage);
            self.pricing_known = true;
            self.stop_reason = stop_reason.map(|s| s.to_string());
        }
    }
}

// ---------------------------------------------------------------------------
// Run summary
// ---------------------------------------------------------------------------

struct RunSummary {
    provider: Provider,
    name: String,
    session_count: usize,
    usage_rows: u32,
    requests: Vec<RequestMetrics>,
    totals: UsageTotals,
    costs: CostTotals,
    pricing_complete: bool,
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
        runs.into_iter()
            .map(|run| session::resolve_run(&run))
            .collect()
    }
}

// ---------------------------------------------------------------------------
// Build run summary with provider-aware request aggregation
// ---------------------------------------------------------------------------

fn build_run_summary(results_dir: &Path) -> Result<RunSummary> {
    let name = results_dir
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default();

    let sessions = session::session_files(results_dir);
    if sessions.is_empty() {
        return Err(Error::RunNotFound {
            path: results_dir.to_path_buf(),
        });
    }

    let provider = detect_provider(results_dir, &sessions);
    let mut all_requests: Vec<RequestMetrics> = Vec::new();
    let mut totals = UsageTotals::default();
    let mut costs = CostTotals::default();
    let mut total_usage_rows: u32 = 0;
    let mut pricing_complete = true;

    for (session_label, session_path) in &sessions {
        let format = session::detect_session_format(session_path).unwrap_or(match provider {
            Provider::Claude => SessionFormat::Claude,
            Provider::Codex => SessionFormat::Codex,
        });
        let session_dir = session_dir(results_dir, session_label);

        let (requests, usage_rows) = match format {
            SessionFormat::Claude => parse_claude_session(session_path, session_label)?,
            SessionFormat::Codex => parse_codex_session(session_path, session_label, &session_dir)?,
        };

        total_usage_rows += usage_rows;
        for request in requests {
            totals.add(&request.usage);
            costs.add(&request.costs);
            pricing_complete &= request.pricing_known;
            all_requests.push(request);
        }
    }

    all_requests.sort_by(|a, b| a.timestamp.cmp(&b.timestamp));

    Ok(RunSummary {
        provider,
        name,
        session_count: sessions.len(),
        usage_rows: total_usage_rows,
        requests: all_requests,
        totals,
        costs,
        pricing_complete,
    })
}

fn detect_provider(results_dir: &Path, sessions: &[(String, PathBuf)]) -> Provider {
    if let Some(provider) = read_meta_agent(results_dir)
        .as_deref()
        .and_then(Provider::from_agent)
    {
        return provider;
    }

    sessions
        .first()
        .and_then(|(_, path)| session::detect_session_format(path))
        .map(Provider::from_format)
        .unwrap_or(Provider::Claude)
}

fn read_meta_agent(results_dir: &Path) -> Option<String> {
    let meta_path = results_dir.join("meta.json");
    let content = fs::read_to_string(meta_path).ok()?;
    let meta: MetaJson = serde_json::from_str(&content).ok()?;
    meta.agent
}

fn session_dir(results_dir: &Path, session_label: &str) -> PathBuf {
    if session_label == "full" {
        results_dir.to_path_buf()
    } else {
        results_dir.join(session_label)
    }
}

// ---------------------------------------------------------------------------
// Claude request parsing
// ---------------------------------------------------------------------------

fn parse_claude_session(
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

        let Some((request_key, usage)) = extract_claude_usage(&entry) else {
            continue;
        };

        usage_rows += 1;
        let metrics = per_request
            .entry(request_key.clone())
            .or_insert_with(|| RequestMetrics::new(request_key, session_label.to_string()));
        metrics.observe_claude(&entry, &usage);
    }

    let mut requests: Vec<RequestMetrics> = per_request.into_values().collect();
    requests.sort_by(|a, b| a.timestamp.cmp(&b.timestamp));
    Ok((requests, usage_rows))
}

fn extract_claude_usage(entry: &serde_json::Value) -> Option<(String, TokenUsage)> {
    let usage_obj = entry.get("usage").and_then(|u| u.as_object()).or_else(|| {
        entry
            .get("message")
            .and_then(|m| m.as_object())
            .and_then(|m| m.get("usage"))
            .and_then(|u| u.as_object())
    })?;

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
        input_tokens: usage_obj
            .get("input_tokens")
            .and_then(|v| v.as_u64())
            .unwrap_or(0),
        output_tokens: usage_obj
            .get("output_tokens")
            .and_then(|v| v.as_u64())
            .unwrap_or(0),
        cache_creation_input_tokens: usage_obj
            .get("cache_creation_input_tokens")
            .and_then(|v| v.as_u64())
            .unwrap_or(0),
        cache_read_input_tokens: usage_obj
            .get("cache_read_input_tokens")
            .and_then(|v| v.as_u64())
            .unwrap_or(0),
    };

    Some((request_key.to_string(), usage))
}

// ---------------------------------------------------------------------------
// Codex request-equivalent parsing
// ---------------------------------------------------------------------------

#[derive(Default)]
struct PendingCodexActivity {
    timestamp: Option<String>,
    tool_names: HashMap<String, u32>,
    tool_ids: HashSet<String>,
    text_chars: usize,
    thinking_chars: usize,
    text_items: usize,
    thinking_items: usize,
    tool_result_items: usize,
    content_items: usize,
}

impl PendingCodexActivity {
    fn note_timestamp(&mut self, timestamp: Option<&str>) {
        if self.timestamp.is_none() {
            self.timestamp = timestamp.map(|ts| ts.to_string());
        }
    }

    fn note_text(&mut self, timestamp: Option<&str>, text: &str) {
        if text.is_empty() {
            return;
        }
        self.note_timestamp(timestamp);
        self.text_items += 1;
        self.text_chars += text.len();
        self.content_items += 1;
    }

    fn note_thinking(&mut self, timestamp: Option<&str>, text: &str) {
        if text.is_empty() {
            return;
        }
        self.note_timestamp(timestamp);
        self.thinking_items += 1;
        self.thinking_chars += text.len();
        self.content_items += 1;
    }

    fn note_tool_use(&mut self, timestamp: Option<&str>, name: &str, tool_id: Option<String>) {
        self.note_timestamp(timestamp);
        let tool_id = tool_id.unwrap_or_else(|| format!("{name}:{}", self.tool_ids.len()));
        if self.tool_ids.insert(tool_id) {
            *self.tool_names.entry(name.to_string()).or_insert(0) += 1;
        }
        self.content_items += 1;
    }

    fn note_tool_result(&mut self, timestamp: Option<&str>, content: &str) {
        if content.is_empty() {
            return;
        }
        self.note_timestamp(timestamp);
        self.tool_result_items += 1;
        self.content_items += 1;
    }

    fn into_request(
        self,
        request_key: String,
        session_label: String,
        timestamp: Option<String>,
        usage: codex::Usage,
        model: Option<&str>,
    ) -> RequestMetrics {
        let cost = codex_cost_breakdown(&usage, model);
        let stop_reason = if self.tool_names.is_empty() {
            Some("end_turn".to_string())
        } else {
            Some("tool_use".to_string())
        };

        RequestMetrics {
            request_key,
            session_label,
            timestamp: timestamp.or(self.timestamp),
            stop_reason,
            usage_rows: 1,
            usage: UsageTotals::from_codex(&usage),
            costs: cost.unwrap_or_default(),
            pricing_known: cost.is_some(),
            tool_names: self.tool_names,
            tool_ids: self.tool_ids,
            text_chars: self.text_chars,
            thinking_chars: self.thinking_chars,
            text_items: self.text_items,
            thinking_items: self.thinking_items,
            tool_result_items: self.tool_result_items,
            content_items: self.content_items,
        }
    }
}

fn parse_codex_session(
    jsonl_path: &Path,
    session_label: &str,
    source_dir: &Path,
) -> Result<(Vec<RequestMetrics>, u32)> {
    let content = fs::read_to_string(jsonl_path).map_err(|e| Error::io(jsonl_path, e))?;
    let mut state = CodexParseState::new(source_dir);

    for line in content.lines() {
        let Some(entry) = parse_jsonl_entry(line) else {
            continue;
        };
        state.observe_entry(&entry, session_label);
    }

    Ok((state.requests, state.usage_rows))
}

fn observe_codex_response_item(entry: &serde_json::Value, pending: &mut PendingCodexActivity) {
    let timestamp = entry.get("timestamp").and_then(|value| value.as_str());
    let Some(payload) = entry.get("payload") else {
        return;
    };

    match payload.get("type").and_then(|value| value.as_str()) {
        Some("message") => observe_codex_message(payload, timestamp, pending),
        Some("reasoning") => {
            let text = codex_reasoning_text(payload);
            pending.note_thinking(timestamp, &text);
        }
        Some("function_call") | Some("custom_tool_call") => {
            observe_codex_tool_use(payload, timestamp, pending)
        }
        Some("function_call_output") | Some("custom_tool_call_output") => {
            observe_codex_tool_result(payload, timestamp, pending)
        }
        _ => {}
    }
}

fn codex_reasoning_text(payload: &serde_json::Value) -> String {
    let mut pieces = Vec::new();
    if let Some(summary) = payload.get("summary") {
        collect_visible_text(summary, &mut pieces);
    }
    if let Some(content) = payload.get("content") {
        collect_visible_text(content, &mut pieces);
    }
    pieces
        .into_iter()
        .filter(|piece| !piece.is_empty())
        .collect::<Vec<_>>()
        .join("\n")
}

fn collect_visible_text(value: &serde_json::Value, out: &mut Vec<String>) {
    match value {
        serde_json::Value::String(text) => out.push(text.clone()),
        serde_json::Value::Array(items) => {
            for item in items {
                collect_visible_text(item, out);
            }
        }
        serde_json::Value::Object(map) => {
            for next in visible_text_fields(map) {
                collect_visible_text(next, out);
            }
        }
        _ => {}
    }
}

fn parse_tool_result_content(value: Option<&serde_json::Value>) -> String {
    match value {
        Some(serde_json::Value::String(text)) => parse_tool_result_string(text),
        Some(serde_json::Value::Array(items)) => items
            .iter()
            .filter_map(|item| item.get("text").and_then(|value| value.as_str()))
            .collect::<Vec<_>>()
            .join("\n"),
        Some(serde_json::Value::Object(map)) => first_non_empty_nested(map, &["output", "content"])
            .unwrap_or_else(|| {
                map.get("text")
                    .and_then(|value| value.as_str())
                    .unwrap_or("")
                    .to_string()
            }),
        Some(other) => other.to_string(),
        None => String::new(),
    }
}

fn parse_jsonl_entry(line: &str) -> Option<serde_json::Value> {
    let line = line.trim();
    if line.is_empty() {
        return None;
    }
    serde_json::from_str(line).ok()
}

struct CodexParseState {
    requests: Vec<RequestMetrics>,
    usage_rows: u32,
    request_idx: usize,
    prior_totals: Option<codex::Usage>,
    pending: PendingCodexActivity,
    model: Option<String>,
}

impl CodexParseState {
    fn new(source_dir: &Path) -> Self {
        Self {
            requests: Vec::new(),
            usage_rows: 0,
            request_idx: 0,
            prior_totals: None,
            pending: PendingCodexActivity::default(),
            model: codex::parse_output_info(&source_dir.join("agent-output.txt")).model,
        }
    }

    fn observe_entry(&mut self, entry: &serde_json::Value, session_label: &str) {
        match entry.get("type").and_then(|value| value.as_str()) {
            Some("turn_context") => self.observe_turn_context(entry),
            Some("response_item") => observe_codex_response_item(entry, &mut self.pending),
            Some("event_msg") => self.observe_event_message(entry, session_label),
            _ => {}
        }
    }

    fn observe_turn_context(&mut self, entry: &serde_json::Value) {
        if self.model.is_some() {
            return;
        }

        self.model = entry
            .get("payload")
            .and_then(|payload| payload.get("model"))
            .and_then(|value| value.as_str())
            .map(|value| value.to_string());
    }

    fn observe_event_message(&mut self, entry: &serde_json::Value, session_label: &str) {
        let Some(snapshot) = codex_token_snapshot(entry) else {
            return;
        };
        self.usage_rows += 1;
        if self.prior_totals.as_ref() == Some(&snapshot.totals) {
            return;
        }

        self.request_idx += 1;
        let delta = self
            .prior_totals
            .as_ref()
            .map(|prior| usage_delta(&snapshot.totals, prior))
            .unwrap_or_else(|| snapshot.totals.clone());
        let request = std::mem::take(&mut self.pending).into_request(
            format!("snapshot-{:03}", self.request_idx),
            session_label.to_string(),
            snapshot.timestamp,
            delta,
            self.model.as_deref(),
        );

        self.requests.push(request);
        self.prior_totals = Some(snapshot.totals);
    }
}

struct CodexSnapshot {
    timestamp: Option<String>,
    totals: codex::Usage,
}

fn codex_token_snapshot(entry: &serde_json::Value) -> Option<CodexSnapshot> {
    let payload = entry.get("payload")?;
    if payload.get("type").and_then(|value| value.as_str()) != Some("token_count") {
        return None;
    }

    Some(CodexSnapshot {
        timestamp: entry
            .get("timestamp")
            .and_then(|value| value.as_str())
            .map(|value| value.to_string()),
        totals: extract_codex_totals(payload)?,
    })
}

fn observe_codex_message(
    payload: &serde_json::Value,
    timestamp: Option<&str>,
    pending: &mut PendingCodexActivity,
) {
    if payload.get("role").and_then(|value| value.as_str()) != Some("assistant") {
        return;
    }

    let Some(items) = payload.get("content").and_then(|value| value.as_array()) else {
        return;
    };
    for text in items.iter().filter_map(codex_message_text) {
        pending.note_text(timestamp, text);
    }
}

fn codex_message_text(item: &serde_json::Value) -> Option<&str> {
    match item.get("type").and_then(|value| value.as_str()) {
        Some("output_text") | Some("text") => item.get("text").and_then(|value| value.as_str()),
        _ => None,
    }
}

fn observe_codex_tool_use(
    payload: &serde_json::Value,
    timestamp: Option<&str>,
    pending: &mut PendingCodexActivity,
) {
    let name = payload
        .get("name")
        .and_then(|value| value.as_str())
        .unwrap_or("unknown");
    let tool_id = payload
        .get("call_id")
        .or_else(|| payload.get("id"))
        .and_then(|value| value.as_str())
        .map(|value| value.to_string());
    pending.note_tool_use(timestamp, name, tool_id);
}

fn observe_codex_tool_result(
    payload: &serde_json::Value,
    timestamp: Option<&str>,
    pending: &mut PendingCodexActivity,
) {
    let content = parse_tool_result_content(payload.get("output"));
    pending.note_tool_result(timestamp, &content);
}

fn visible_text_fields(
    map: &serde_json::Map<String, serde_json::Value>,
) -> impl Iterator<Item = &serde_json::Value> {
    ["text", "content", "summary_text"]
        .into_iter()
        .filter_map(|key| map.get(key))
}

fn parse_tool_result_string(text: &str) -> String {
    serde_json::from_str::<serde_json::Value>(text)
        .ok()
        .map(|parsed| parse_tool_result_content(Some(&parsed)))
        .filter(|nested| !nested.is_empty())
        .unwrap_or_else(|| text.to_string())
}

fn first_non_empty_nested(
    map: &serde_json::Map<String, serde_json::Value>,
    keys: &[&str],
) -> Option<String> {
    keys.iter()
        .filter_map(|key| map.get(*key))
        .map(|value| parse_tool_result_content(Some(value)))
        .find(|nested| !nested.is_empty())
}

fn extract_codex_totals(payload: &serde_json::Value) -> Option<codex::Usage> {
    let totals = payload
        .get("info")
        .and_then(|value| value.get("total_token_usage"))
        .and_then(|value| value.as_object())?;

    let get = |key| {
        totals
            .get(key)
            .and_then(|value| value.as_u64())
            .unwrap_or(0)
    };
    Some(codex::Usage {
        input_tokens: get("input_tokens"),
        cached_input_tokens: get("cached_input_tokens"),
        output_tokens: get("output_tokens"),
    })
}

fn usage_delta(current: &codex::Usage, prior: &codex::Usage) -> codex::Usage {
    codex::Usage {
        input_tokens: current.input_tokens.saturating_sub(prior.input_tokens),
        cached_input_tokens: current
            .cached_input_tokens
            .saturating_sub(prior.cached_input_tokens),
        output_tokens: current.output_tokens.saturating_sub(prior.output_tokens),
    }
}

// ---------------------------------------------------------------------------
// Output: per-run summary
// ---------------------------------------------------------------------------

fn print_run_summary(summary: &RunSummary) {
    let n = summary.billed_requests();
    print_summary_header(summary, n);
    if n == 0 {
        return;
    }

    print_cost_summary(summary);
    print_stop_reasons_and_tools(&summary.requests);
    print_turn_shape_and_buckets(&summary.requests, n);
    print_average_cache_bucket(summary, n);
    print_driver_summary(summary);
    print_top_request_sections(summary);
}

fn print_summary_header(summary: &RunSummary, request_count: usize) {
    let sep = "=".repeat(96);
    println!("\n{sep}");
    println!("  {}", summary.name);
    println!("{sep}");

    if request_count > 0 {
        println!(
            "Sessions={}  Rows={}  Requests={}  Rows/Request={:.2}",
            summary.session_count,
            fmt_comma(summary.usage_rows as u64),
            fmt_comma(request_count as u64),
            summary.usage_rows as f64 / request_count as f64,
        );
        return;
    }

    println!(
        "Sessions={}  Rows={}  Requests=0",
        summary.session_count,
        fmt_comma(summary.usage_rows as u64),
    );
}

fn print_cost_summary(summary: &RunSummary) {
    let total_cost = summary.costs.total();
    match summary.provider {
        Provider::Claude => println!(
            "Cost=${:.2}  Input=${:.2}  Output=${:.2}  CacheWrite=${:.2}  CacheRead=${:.2}",
            total_cost,
            summary.costs.input,
            summary.costs.output,
            summary.costs.cache_write,
            summary.costs.cache_read,
        ),
        Provider::Codex => println!(
            "Cost=${:.2}  Input=${:.2}  Output=${:.2}  CachedInput=${:.2}",
            total_cost, summary.costs.input, summary.costs.output, summary.costs.cached_input,
        ),
    }

    if summary.provider == Provider::Codex && !summary.pricing_complete {
        println!(
            "Cost note: at least one Codex request had unknown pricing; totals may be incomplete."
        );
    }
}

fn print_average_cache_bucket(summary: &RunSummary, request_count: usize) {
    let avg_cache_bucket =
        summary.totals.cache_bucket_tokens(summary.provider) as f64 / request_count as f64;
    println!(
        "{}={avg_cache_bucket:.1}",
        summary.provider.avg_cache_bucket_label(),
    );
}

fn print_driver_summary(summary: &RunSummary) {
    println!("Likely drivers:");
    for issue in likely_drivers(summary) {
        println!("  - {issue}");
    }
}

fn print_top_request_sections(summary: &RunSummary) {
    print_top_requests(
        summary.provider.top_cache_bucket_title(),
        summary.provider.cache_bucket_metric_label(),
        &summary.requests,
        |request| request.cache_bucket_tokens(summary.provider),
    );
    print_top_requests(
        "Top output requests:",
        "output",
        &summary.requests,
        |request| request.output_tokens(),
    );
}

fn print_stop_reasons_and_tools(requests: &[RequestMetrics]) {
    let mut stop_counts: HashMap<String, usize> = HashMap::new();
    for request in requests {
        let reason = request.stop_reason.as_deref().unwrap_or("none");
        *stop_counts.entry(reason.to_string()).or_insert(0) += 1;
    }
    let mut stop_sorted: Vec<_> = stop_counts.into_iter().collect();
    stop_sorted.sort_by(|a, b| b.1.cmp(&a.1));
    let stop_str: Vec<String> = stop_sorted
        .iter()
        .map(|(key, value)| format!("{key}={value}"))
        .collect();
    println!("Stop reasons: {}", stop_str.join(", "));

    let mut tool_counts: HashMap<String, u32> = HashMap::new();
    for request in requests {
        for (name, count) in &request.tool_names {
            *tool_counts.entry(name.clone()).or_insert(0) += count;
        }
    }
    let mut tool_sorted: Vec<_> = tool_counts.into_iter().collect();
    tool_sorted.sort_by(|a, b| b.1.cmp(&a.1));
    let tool_str: Vec<String> = tool_sorted
        .iter()
        .map(|(key, value)| format!("{key}={value}"))
        .collect();
    println!(
        "Tool mix: {}",
        if tool_str.is_empty() {
            "none".to_string()
        } else {
            tool_str.join(", ")
        }
    );
}

fn print_turn_shape_and_buckets(requests: &[RequestMetrics], n: usize) {
    let no_tool = requests
        .iter()
        .filter(|request| request.tool_count() == 0)
        .count();
    let single_tool = requests
        .iter()
        .filter(|request| request.tool_count() == 1)
        .count();
    let multi_tool = requests
        .iter()
        .filter(|request| request.tool_count() > 1)
        .count();
    let avg_tools = requests
        .iter()
        .map(|request| request.tool_count())
        .sum::<usize>() as f64
        / n as f64;
    println!(
        "Turn shape: no-tool={no_tool}, single-tool={single_tool}, multi-tool={multi_tool}, avg_tools/request={avg_tools:.2}"
    );

    let small = requests
        .iter()
        .filter(|request| request.output_tokens() <= 300)
        .count();
    let medium = requests
        .iter()
        .filter(|request| (301..=2000).contains(&request.output_tokens()))
        .count();
    let large = requests
        .iter()
        .filter(|request| request.output_tokens() > 2000)
        .count();
    println!(
        "Output buckets: <=300={small} ({}), 301-2000={medium} ({}), >2000={large} ({})",
        pct(small, n),
        pct(medium, n),
        pct(large, n),
    );

    let text_reqs = requests
        .iter()
        .filter(|request| request.text_items > 0)
        .count();
    let text_chars: usize = requests.iter().map(|request| request.text_chars).sum();
    let thinking_reqs = requests
        .iter()
        .filter(|request| request.thinking_items > 0)
        .count();
    let thinking_chars: usize = requests.iter().map(|request| request.thinking_chars).sum();
    println!(
        "Text requests={text_reqs} ({}), text chars={text_chars}",
        pct(text_reqs, n),
    );
    println!(
        "Thinking requests={thinking_reqs} ({}), thinking chars={thinking_chars}",
        pct(thinking_reqs, n),
    );
}

fn print_top_requests(
    title: &str,
    metric_label: &str,
    requests: &[RequestMetrics],
    key: impl Fn(&RequestMetrics) -> u64,
) {
    let mut ranked: Vec<&RequestMetrics> =
        requests.iter().filter(|request| key(request) > 0).collect();
    ranked.sort_by_key(|request| std::cmp::Reverse(key(request)));
    ranked.truncate(3);

    if ranked.is_empty() {
        return;
    }

    println!("{title}");
    for request in ranked {
        println!(
            "  - {}:{} {}={} output={} stop={} tools={}",
            request.session_label,
            request.request_key,
            metric_label,
            fmt_comma(key(request)),
            fmt_comma(request.output_tokens()),
            request.stop_reason.as_deref().unwrap_or("none"),
            request.tool_count(),
        );
    }
}

// ---------------------------------------------------------------------------
// Anti-pattern detection
// ---------------------------------------------------------------------------

fn likely_drivers(summary: &RunSummary) -> Vec<String> {
    let n = summary.billed_requests();
    if n == 0 {
        return vec!["No billed requests found.".to_string()];
    }

    let metrics = DriverMetrics::from_summary(summary);
    let mut issues = Vec::new();
    extend_issue(&mut issues, fragmented_run_issue(n));
    extend_issue(&mut issues, cache_cost_issue(summary.provider, &metrics));
    extend_issue(&mut issues, single_tool_issue(n, metrics.single_tool));
    extend_issue(&mut issues, small_output_issue(n, metrics.small_output));
    extend_issue(&mut issues, narration_issue(&metrics, n));
    extend_issue(&mut issues, max_tokens_issue(&metrics));

    if issues.is_empty() {
        issues.push("No obvious anti-pattern crossed the analyzer thresholds.".to_string());
    }

    issues
}

// ---------------------------------------------------------------------------
// Comparison (two runs)
// ---------------------------------------------------------------------------

fn print_comparison(left: &RunSummary, right: &RunSummary) {
    let rows = build_comparison_rows(left, right);

    let sep = "=".repeat(96);
    println!("\n{sep}");
    println!("  Comparison: {} vs {}", right.name, left.name);
    println!("{sep}");
    println!(
        "{:<24} {:>18} {:>18} {:>18}",
        "Metric", left.name, right.name, "Diff"
    );
    println!("{}", "-".repeat(96));

    for (label, lv, rv, kind) in &rows {
        print_comparison_row(label, *lv, *rv, kind);
    }
}

fn fmt_pct_diff(diff: f64, base: f64) -> String {
    if base > 0.0 {
        format!(" ({:+.1}%)", diff / base * 100.0)
    } else {
        String::new()
    }
}

fn print_comparison_row(label: &str, lv: f64, rv: f64, kind: &str) {
    let diff = rv - lv;
    match kind {
        "money" => {
            let pct_diff = fmt_pct_diff(diff, lv);
            println!(
                "{:<24} {:>17}$ {:>17}$ {:>17}",
                label,
                format!("{lv:.2}"),
                format!("{rv:.2}"),
                format!("{diff:+.2}{pct_diff}")
            );
        }
        "ratio" => {
            let pp = (rv - lv) * 100.0;
            println!(
                "{:<24} {:>17}% {:>17}% {:>17}",
                label,
                format!("{:.1}", lv * 100.0),
                format!("{:.1}", rv * 100.0),
                format!("{pp:+.1}pp")
            );
        }
        "float" => {
            let pct_diff = fmt_pct_diff(diff, lv);
            println!(
                "{:<24} {:>18.2} {:>18.2} {:>17}",
                label,
                lv,
                rv,
                format!("{diff:+.2}{pct_diff}")
            );
        }
        _ => {
            let pct_diff = fmt_pct_diff(diff, lv);
            println!(
                "{:<24} {:>18} {:>18} {:>17}",
                label,
                fmt_comma(lv as u64),
                fmt_comma(rv as u64),
                format!("{:+}{pct_diff}", diff as i64)
            );
        }
    }
}

fn build_comparison_rows(
    left: &RunSummary,
    right: &RunSummary,
) -> Vec<(String, f64, f64, &'static str)> {
    let context = ComparisonContext::new(left, right);
    vec![
        comparison_row(
            "Requests",
            left.billed_requests() as f64,
            right.billed_requests() as f64,
            "count",
        ),
        comparison_row("Total cost", context.left_cost, context.right_cost, "money"),
        comparison_row(
            context.cache_bucket_label,
            cache_bucket_tokens(left),
            cache_bucket_tokens(right),
            "count",
        ),
        comparison_row(
            context.cache_bucket_share_label,
            cache_bucket_cost_share(left),
            cache_bucket_cost_share(right),
            "ratio",
        ),
        comparison_row(
            context.avg_cache_bucket_label,
            avg_cache_bucket_per_request(left),
            avg_cache_bucket_per_request(right),
            "count",
        ),
        comparison_row(
            "Avg tools/request",
            avg_tools_per_request(left),
            avg_tools_per_request(right),
            "float",
        ),
        comparison_row(
            "Single-tool ratio",
            single_tool_ratio(left),
            single_tool_ratio(right),
            "ratio",
        ),
        comparison_row(
            "Small-output ratio",
            small_output_ratio(left),
            small_output_ratio(right),
            "ratio",
        ),
        comparison_row(
            "Text chars",
            total_text_chars(left),
            total_text_chars(right),
            "count",
        ),
        comparison_row(
            "Max-tokens requests",
            max_tokens_request_count(left),
            max_tokens_request_count(right),
            "count",
        ),
    ]
}

fn extend_issue(issues: &mut Vec<String>, issue: Option<String>) {
    if let Some(issue) = issue {
        issues.push(issue);
    }
}

struct DriverMetrics {
    total_cost: f64,
    cache_bucket_cost: f64,
    single_tool: usize,
    small_output: usize,
    text_reqs: usize,
    text_chars: usize,
    max_tokens_count: usize,
    max_tokens_largest_output: u64,
}

impl DriverMetrics {
    fn from_summary(summary: &RunSummary) -> Self {
        let max_tokens_requests: Vec<_> = summary
            .requests
            .iter()
            .filter(|request| request.stop_reason.as_deref() == Some("max_tokens"))
            .collect();

        Self {
            total_cost: summary.costs.total(),
            cache_bucket_cost: summary.costs.cache_bucket_cost(summary.provider),
            single_tool: summary
                .requests
                .iter()
                .filter(|request| request.tool_count() == 1)
                .count(),
            small_output: summary
                .requests
                .iter()
                .filter(|request| request.output_tokens() <= 300)
                .count(),
            text_reqs: summary
                .requests
                .iter()
                .filter(|request| request.text_items > 0)
                .count(),
            text_chars: summary
                .requests
                .iter()
                .map(|request| request.text_chars)
                .sum(),
            max_tokens_count: max_tokens_requests.len(),
            max_tokens_largest_output: max_tokens_requests
                .iter()
                .map(|request| request.output_tokens())
                .max()
                .unwrap_or(0),
        }
    }
}

fn fragmented_run_issue(request_count: usize) -> Option<String> {
    (request_count >= 80).then(|| {
        format!("{request_count} billed requests; the run is heavily fragmented across turns.")
    })
}

fn cache_cost_issue(provider: Provider, metrics: &DriverMetrics) -> Option<String> {
    (metrics.total_cost > 0.0 && metrics.cache_bucket_cost >= metrics.total_cost * 0.5).then(|| {
        format!(
            "{} are {} of total cost.",
            provider.cache_bucket_phrase(),
            pct_of(metrics.cache_bucket_cost, metrics.total_cost),
        )
    })
}

fn single_tool_issue(request_count: usize, single_tool: usize) -> Option<String> {
    (request_count >= 25 && single_tool >= request_count * 8 / 10).then(|| {
        format!(
            "{} of requests use exactly one tool; related reads and shell calls are not being batched.",
            pct(single_tool, request_count),
        )
    })
}

fn small_output_issue(request_count: usize, small_output: usize) -> Option<String> {
    (request_count >= 25 && small_output >= request_count / 2).then(|| {
        format!(
            "{} of requests produce <=300 output tokens, which looks like micro-turn tool loops.",
            pct(small_output, request_count),
        )
    })
}

fn narration_issue(metrics: &DriverMetrics, request_count: usize) -> Option<String> {
    (metrics.text_chars >= 50_000 && metrics.text_reqs >= request_count * 4 / 10).then(|| {
        format!(
            "Narration is heavy: {} text chars across {} requests.",
            metrics.text_chars, metrics.text_reqs,
        )
    })
}

fn max_tokens_issue(metrics: &DriverMetrics) -> Option<String> {
    (metrics.max_tokens_count > 0).then(|| {
        format!(
            "{} requests hit max_tokens; the largest emitted {} output tokens.",
            metrics.max_tokens_count,
            fmt_comma(metrics.max_tokens_largest_output),
        )
    })
}

struct ComparisonContext {
    left_cost: f64,
    right_cost: f64,
    cache_bucket_label: String,
    cache_bucket_share_label: String,
    avg_cache_bucket_label: String,
}

impl ComparisonContext {
    fn new(left: &RunSummary, right: &RunSummary) -> Self {
        let same_provider = left.provider == right.provider;
        Self {
            left_cost: left.costs.total(),
            right_cost: right.costs.total(),
            cache_bucket_label: if same_provider {
                left.provider.cache_bucket_tokens_label().to_string()
            } else {
                "Cached bucket tokens".to_string()
            },
            cache_bucket_share_label: if same_provider {
                left.provider.cache_bucket_cost_share_label().to_string()
            } else {
                "Cached bucket cost share".to_string()
            },
            avg_cache_bucket_label: if same_provider {
                left.provider.avg_cache_bucket_label().to_string()
            } else {
                "Avg cached bucket/request".to_string()
            },
        }
    }
}

fn comparison_row(
    label: impl Into<String>,
    left: f64,
    right: f64,
    kind: &'static str,
) -> (String, f64, f64, &'static str) {
    (label.into(), left, right, kind)
}

fn cache_bucket_tokens(summary: &RunSummary) -> f64 {
    summary.totals.cache_bucket_tokens(summary.provider) as f64
}

fn cache_bucket_cost_share(summary: &RunSummary) -> f64 {
    let total_cost = summary.costs.total();
    if total_cost > 0.0 {
        summary.costs.cache_bucket_cost(summary.provider) / total_cost
    } else {
        0.0
    }
}

fn avg_cache_bucket_per_request(summary: &RunSummary) -> f64 {
    cache_bucket_tokens(summary) / summary.billed_requests().max(1) as f64
}

fn avg_tools_per_request(summary: &RunSummary) -> f64 {
    summary
        .requests
        .iter()
        .map(|request| request.tool_count())
        .sum::<usize>() as f64
        / summary.billed_requests().max(1) as f64
}

fn single_tool_ratio(summary: &RunSummary) -> f64 {
    request_ratio(summary, |request| request.tool_count() == 1)
}

fn small_output_ratio(summary: &RunSummary) -> f64 {
    request_ratio(summary, |request| request.output_tokens() <= 300)
}

fn total_text_chars(summary: &RunSummary) -> f64 {
    summary
        .requests
        .iter()
        .map(|request| request.text_chars)
        .sum::<usize>() as f64
}

fn max_tokens_request_count(summary: &RunSummary) -> f64 {
    summary
        .requests
        .iter()
        .filter(|request| request.stop_reason.as_deref() == Some("max_tokens"))
        .count() as f64
}

fn request_ratio(summary: &RunSummary, predicate: impl Fn(&RequestMetrics) -> bool) -> f64 {
    summary
        .requests
        .iter()
        .filter(|request| predicate(request))
        .count() as f64
        / summary.billed_requests().max(1) as f64
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

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

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn unique_temp_dir(prefix: &str) -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("xtask-analyze-{prefix}-{nanos}"));
        fs::create_dir_all(&dir).expect("create temp dir");
        dir
    }

    #[test]
    fn codex_requests_dedupe_duplicate_snapshots_and_compute_deltas() {
        let dir = unique_temp_dir("codex");
        fs::write(
            dir.join("agent-output.txt"),
            "model: gpt-5.4\nsession id: 019d0fb0-4d8b-7b01-b301-af3c83368c65\n",
        )
        .expect("write output");

        let session_path = dir.join("session.jsonl");
        let content = r#"{"timestamp":"2026-03-21T09:00:00Z","type":"response_item","payload":{"type":"message","role":"assistant","content":[{"type":"output_text","text":"Starting."}]}}
{"timestamp":"2026-03-21T09:00:01Z","type":"response_item","payload":{"type":"function_call","name":"exec_command","call_id":"call_1","arguments":"{\"cmd\":\"cargo xtask test 01\"}"}}
{"timestamp":"2026-03-21T09:00:02Z","type":"event_msg","payload":{"type":"token_count","info":{"total_token_usage":{"input_tokens":100,"cached_input_tokens":20,"output_tokens":10}}}}
{"timestamp":"2026-03-21T09:00:03Z","type":"event_msg","payload":{"type":"token_count","info":{"total_token_usage":{"input_tokens":100,"cached_input_tokens":20,"output_tokens":10}}}}
{"timestamp":"2026-03-21T09:00:04Z","type":"response_item","payload":{"type":"function_call_output","call_id":"call_1","output":"PASSED"}}
{"timestamp":"2026-03-21T09:00:05Z","type":"response_item","payload":{"type":"message","role":"assistant","content":[{"type":"output_text","text":"Done."}]}}
{"timestamp":"2026-03-21T09:00:06Z","type":"event_msg","payload":{"type":"token_count","info":{"total_token_usage":{"input_tokens":140,"cached_input_tokens":30,"output_tokens":25}}}}"#;
        fs::write(&session_path, content).expect("write session");

        let (requests, usage_rows) =
            parse_codex_session(&session_path, "full", &dir).expect("parse codex session");

        assert_eq!(usage_rows, 3);
        assert_eq!(requests.len(), 2);

        assert_eq!(requests[0].request_key, "snapshot-001");
        assert_eq!(requests[0].stop_reason.as_deref(), Some("tool_use"));
        assert_eq!(requests[0].usage.input_tokens, 100);
        assert_eq!(requests[0].usage.cached_input_tokens, 20);
        assert_eq!(requests[0].usage.output_tokens, 10);
        assert_eq!(requests[0].tool_count(), 1);
        assert_eq!(requests[0].text_items, 1);

        assert_eq!(requests[1].request_key, "snapshot-002");
        assert_eq!(requests[1].stop_reason.as_deref(), Some("end_turn"));
        assert_eq!(requests[1].usage.input_tokens, 40);
        assert_eq!(requests[1].usage.cached_input_tokens, 10);
        assert_eq!(requests[1].usage.output_tokens, 15);
        assert_eq!(requests[1].tool_result_items, 1);
        assert_eq!(requests[1].text_items, 1);

        fs::remove_dir_all(&dir).expect("cleanup");
    }
}
