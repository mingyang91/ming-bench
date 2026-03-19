use crate::model::{
    discover_runs, fmt_comma, project_results_dir, Error, MetaJson, Result, TokenUsage,
    PRICE_CACHE_READ, PRICE_CACHE_WRITE, PRICE_INPUT, PRICE_OUTPUT,
};
use std::fs;
use std::path::{Path, PathBuf};

pub fn run(runs: Vec<PathBuf>, all: bool) -> Result<()> {
    let run_dirs = resolve_runs(runs, all)?;

    if run_dirs.is_empty() {
        return Err(Error::NoRuns);
    }

    let mut all_totals: Vec<(String, TokenUsage)> = Vec::new();

    for run_dir in &run_dirs {
        let label = run_dir
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| run_dir.display().to_string());
        let total = process_run(run_dir, &label)?;
        all_totals.push((label, total));
    }

    // Comparison table if multiple runs
    if all_totals.len() >= 2 {
        print_comparison(&all_totals);
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
        // Verify each exists
        for r in &runs {
            if !r.is_dir() {
                return Err(Error::RunNotFound { path: r.clone() });
            }
        }
        Ok(runs)
    }
}

fn process_run(run_dir: &Path, label: &str) -> Result<TokenUsage> {
    let agent = read_agent(run_dir);
    let is_codex = agent.as_deref() == Some("codex");

    // Determine session sources: L* subdirs, or root dir for full-mode runs
    let mut levels = find_level_dirs(run_dir);
    levels.sort();
    let is_full_mode = levels.is_empty();

    let separator = "=".repeat(70);
    println!("\n{separator}");
    println!("  {label}");
    println!("{separator}");

    if is_codex {
        // Codex: only total tokens available (no input/output/cache breakdown)
        println!(
            "{:<8} {:>12} {}",
            "Level", "Total Tokens", "Status"
        );
        println!(
            "{} {} {}",
            "-".repeat(8),
            "-".repeat(12),
            "-".repeat(10)
        );

        let mut grand_total_tokens: u64 = 0;
        let sources = codex_sources(run_dir, is_full_mode);

        for (level_name, dir) in &sources {
            let status = read_status(dir);
            match parse_codex_output(dir) {
                Some(total) => {
                    grand_total_tokens += total;
                    println!(
                        "{:<8} {:>12} {}",
                        level_name,
                        fmt_comma(total),
                        status,
                    );
                }
                None => {
                    println!("{:<8} {:>12} {}", level_name, "—", status);
                }
            }
        }

        println!("{} {}", "-".repeat(8), "-".repeat(12));
        println!("{:<8} {:>12}", "TOTAL", fmt_comma(grand_total_tokens));
        println!();
        println!("  Total tokens: {}", fmt_comma(grand_total_tokens));
        println!("  Agent: codex (GPT) — cost breakdown not available");

        // Return empty TokenUsage — codex pricing differs from Claude
        return Ok(TokenUsage::default());
    }

    // Claude: full breakdown available
    println!(
        "{:<8} {:>10} {:>10} {:>12} {:>12} {}",
        "Level", "Input", "Output", "Cache Write", "Cache Read", "Status"
    );
    println!(
        "{} {} {} {} {} {}",
        "-".repeat(8),
        "-".repeat(10),
        "-".repeat(10),
        "-".repeat(12),
        "-".repeat(12),
        "-".repeat(10)
    );

    let mut grand_total = TokenUsage::default();

    if is_full_mode {
        // Full-mode: session.jsonl at run root
        let session_file = run_dir.join("session.jsonl");
        match parse_session(&session_file) {
            Some(tokens) => {
                grand_total.add(&tokens);
                println!(
                    "{:<8} {:>10} {:>10} {:>12} {:>12}",
                    "full",
                    fmt_comma(tokens.input_tokens),
                    fmt_comma(tokens.output_tokens),
                    fmt_comma(tokens.cache_creation_input_tokens),
                    fmt_comma(tokens.cache_read_input_tokens),
                );
            }
            None => {
                println!("{:<8} {:>10} {:>10} {:>12} {:>12}", "full", "—", "—", "—", "—");
            }
        }
    } else {
        for level_dir in &levels {
            let level_name = level_dir
                .file_name()
                .map(|n| n.to_string_lossy().into_owned())
                .unwrap_or_default();

            let session_file = level_dir.join("session.jsonl");
            let status = read_status(level_dir);

            match parse_session(&session_file) {
                Some(tokens) => {
                    grand_total.add(&tokens);
                    println!(
                        "{:<8} {:>10} {:>10} {:>12} {:>12} {}",
                        level_name,
                        fmt_comma(tokens.input_tokens),
                        fmt_comma(tokens.output_tokens),
                        fmt_comma(tokens.cache_creation_input_tokens),
                        fmt_comma(tokens.cache_read_input_tokens),
                        status,
                    );
                }
                None => {
                    println!(
                        "{:<8} {:>10} {:>10} {:>12} {:>12} {}",
                        level_name, "—", "—", "—", "—", status,
                    );
                }
            }
        }
    }

    // Totals
    println!(
        "{} {} {} {} {}",
        "-".repeat(8),
        "-".repeat(10),
        "-".repeat(10),
        "-".repeat(12),
        "-".repeat(12)
    );
    println!(
        "{:<8} {:>10} {:>10} {:>12} {:>12}",
        "TOTAL",
        fmt_comma(grand_total.input_tokens),
        fmt_comma(grand_total.output_tokens),
        fmt_comma(grand_total.cache_creation_input_tokens),
        fmt_comma(grand_total.cache_read_input_tokens),
    );

    // Cost breakdown
    let cost_input = grand_total.input_tokens as f64 * PRICE_INPUT;
    let cost_output = grand_total.output_tokens as f64 * PRICE_OUTPUT;
    let cost_cache_w = grand_total.cache_creation_input_tokens as f64 * PRICE_CACHE_WRITE;
    let cost_cache_r = grand_total.cache_read_input_tokens as f64 * PRICE_CACHE_READ;
    let total_cost = cost_input + cost_output + cost_cache_w + cost_cache_r;

    println!();
    println!("  Total tokens: {}", fmt_comma(grand_total.total()));
    println!("  Estimated cost: ${total_cost:.2}");
    println!(
        "    Input: ${cost_input:.2} | Output: ${cost_output:.2} | Cache Write: ${cost_cache_w:.2} | Cache Read: ${cost_cache_r:.2}"
    );

    Ok(grand_total)
}

fn print_comparison(all_totals: &[(String, TokenUsage)]) {
    // Extract short labels: "main_claude-main-full_20260319T..." → "claude-main-full"
    let short_labels: Vec<String> = all_totals
        .iter()
        .map(|(label, _)| extract_run_name(label))
        .collect();

    let col_width = short_labels.iter().map(|s| s.len()).max().unwrap_or(15).max(15);
    let total_width = 25 + all_totals.len() * (col_width + 1);
    let separator = "=".repeat(total_width);

    println!("\n{separator}");
    println!("  COMPARISON");
    println!("{separator}");

    // Header
    print!("{:<25}", "Metric");
    for label in &short_labels {
        print!(" {:>width$}", label, width = col_width);
    }
    println!();

    print!("{}", "-".repeat(25));
    for _ in all_totals {
        print!(" {}", "-".repeat(col_width));
    }
    println!();

    let fields: &[(&str, fn(&TokenUsage) -> u64)] = &[
        ("Input Tokens", |t| t.input_tokens),
        ("Output Tokens", |t| t.output_tokens),
        ("Cache Write", |t| t.cache_creation_input_tokens),
        ("Cache Read", |t| t.cache_read_input_tokens),
        ("Total", |t| t.total()),
    ];

    for (name, extract) in fields {
        print!("{:<25}", name);
        for (_, tokens) in all_totals {
            print!(" {:>width$}", fmt_comma(extract(tokens)), width = col_width);
        }
        println!();
    }

    // Cost row (only meaningful for Claude runs)
    print!("{:<25}", "Est. Cost (Claude)");
    for (_, tokens) in all_totals {
        let cost = tokens.cost();
        if cost > 0.0 {
            print!(" {:>width$}", format!("${cost:.2}"), width = col_width);
        } else {
            print!(" {:>width$}", "—", width = col_width);
        }
    }
    println!();
}

/// Extract the run name from a directory name like "main_claude-main-full_20260319T025317"
fn extract_run_name(dir_name: &str) -> String {
    // Pattern: {base}_{name}_{timestamp} — extract {name}
    let parts: Vec<&str> = dir_name.splitn(3, '_').collect();
    if parts.len() >= 2 {
        // Remove timestamp suffix if present
        let name_and_ts = &dir_name[parts[0].len() + 1..];
        if let Some(idx) = name_and_ts.rfind('_') {
            let candidate = &name_and_ts[..idx];
            // Verify the suffix looks like a timestamp (digits + T)
            let suffix = &name_and_ts[idx + 1..];
            if suffix.len() >= 8 && suffix.chars().take(8).all(|c| c.is_ascii_digit() || c == 'T') {
                return candidate.to_string();
            }
        }
        name_and_ts.to_string()
    } else {
        dir_name.to_string()
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn find_level_dirs(run_dir: &Path) -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    let entries = match fs::read_dir(run_dir) {
        Ok(e) => e,
        Err(_) => return dirs,
    };
    for entry in entries.flatten() {
        let name = entry.file_name();
        let name_str = name.to_string_lossy();
        if name_str.starts_with('L') && entry.path().is_dir() {
            dirs.push(entry.path());
        }
    }
    dirs
}

fn read_status(level_dir: &Path) -> String {
    let status_file = level_dir.join("status.txt");
    fs::read_to_string(status_file)
        .map(|s| s.trim().to_string())
        .unwrap_or_else(|_| "running...".to_string())
}

fn read_agent(run_dir: &Path) -> Option<String> {
    let meta_path = run_dir.join("meta.json");
    let content = fs::read_to_string(meta_path).ok()?;
    let meta: MetaJson = serde_json::from_str(&content).ok()?;
    meta.agent
}

/// For codex runs, return (label, dir) pairs to scan for agent-output.txt.
fn codex_sources(run_dir: &Path, is_full_mode: bool) -> Vec<(String, PathBuf)> {
    if is_full_mode {
        vec![("full".to_string(), run_dir.to_path_buf())]
    } else {
        let mut levels = find_level_dirs(run_dir);
        levels.sort();
        levels
            .into_iter()
            .map(|d| {
                let name = d
                    .file_name()
                    .map(|n| n.to_string_lossy().into_owned())
                    .unwrap_or_default();
                (name, d)
            })
            .collect()
    }
}

/// Parse codex agent-output.txt for total tokens.
/// Format: "tokens used\n141,772\n" near end of file (with ANSI codes).
fn parse_codex_output(dir: &Path) -> Option<u64> {
    let path = dir.join("agent-output.txt");
    let content = fs::read_to_string(&path).ok()?;

    // Strip ANSI escape sequences
    let stripped: String = strip_ansi(&content);

    // Look for "tokens used" followed by a number on the next line
    let lines: Vec<&str> = stripped.lines().collect();
    for i in 0..lines.len().saturating_sub(1) {
        if lines[i].trim() == "tokens used" {
            let num_str: String = lines[i + 1].trim().chars().filter(|c| c.is_ascii_digit()).collect();
            if !num_str.is_empty() {
                return num_str.parse().ok();
            }
        }
    }
    None
}

/// Strip ANSI escape sequences from a string.
fn strip_ansi(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '\x1b' {
            // Skip until we hit a letter (end of ANSI sequence)
            if chars.peek() == Some(&'[') {
                chars.next();
                while let Some(&next) = chars.peek() {
                    chars.next();
                    if next.is_ascii_alphabetic() {
                        break;
                    }
                }
            }
        } else if c == '\r' {
            // Skip carriage returns
        } else {
            result.push(c);
        }
    }
    result
}

fn parse_session(jsonl_path: &Path) -> Option<TokenUsage> {
    let content = fs::read_to_string(jsonl_path).ok()?;
    let mut totals = TokenUsage::default();
    let mut found = false;

    for line in content.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let obj: serde_json::Value = match serde_json::from_str(line) {
            Ok(v) => v,
            Err(_) => continue,
        };

        let usage = obj
            .get("message")
            .and_then(|m| m.as_object())
            .and_then(|m| m.get("usage"))
            .and_then(|u| u.as_object());

        if let Some(usage) = usage {
            found = true;
            totals.input_tokens += usage
                .get("input_tokens")
                .and_then(|v| v.as_u64())
                .unwrap_or(0);
            totals.output_tokens += usage
                .get("output_tokens")
                .and_then(|v| v.as_u64())
                .unwrap_or(0);
            totals.cache_creation_input_tokens += usage
                .get("cache_creation_input_tokens")
                .and_then(|v| v.as_u64())
                .unwrap_or(0);
            totals.cache_read_input_tokens += usage
                .get("cache_read_input_tokens")
                .and_then(|v| v.as_u64())
                .unwrap_or(0);
        }
    }

    if found {
        Some(totals)
    } else {
        None
    }
}
