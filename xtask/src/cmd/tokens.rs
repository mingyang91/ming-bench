use crate::model::{
    discover_runs, fmt_comma, project_results_dir, Error, MetaJson, Result, TokenUsage,
    PRICE_CACHE_READ, PRICE_CACHE_WRITE, PRICE_INPUT, PRICE_OUTPUT,
};
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Clone, Debug)]
struct RunReport {
    agent: String,
    model: Option<String>,
    usage: RunUsage,
    total_tokens: u64,
    cost: Option<f64>,
}

impl RunReport {
    fn is_codex(&self) -> bool {
        matches!(self.usage, RunUsage::Codex { .. })
    }

    fn claude_usage(&self) -> Option<&TokenUsage> {
        match &self.usage {
            RunUsage::Claude { usage: Some(usage) } => Some(usage),
            _ => None,
        }
    }

    fn codex_usage(&self) -> Option<&CodexUsage> {
        match &self.usage {
            RunUsage::Codex { usage: Some(usage) } => Some(usage),
            _ => None,
        }
    }

    fn cost_display(&self) -> String {
        self.cost
            .map(|cost| format!("${cost:.2}"))
            .unwrap_or_else(|| "—".to_string())
    }

    fn total_tokens_display(&self) -> String {
        if self.total_tokens > 0 {
            fmt_comma(self.total_tokens)
        } else {
            "—".to_string()
        }
    }

    fn model_display(&self) -> String {
        self.model.clone().unwrap_or_else(|| "—".to_string())
    }
}

#[derive(Clone, Debug)]
enum RunUsage {
    Claude { usage: Option<TokenUsage> },
    Codex { usage: Option<CodexUsage> },
}

#[derive(Default, Clone, Debug)]
struct CodexUsage {
    input_tokens: u64,
    cached_input_tokens: u64,
    output_tokens: u64,
}

impl CodexUsage {
    fn total_tokens(&self) -> u64 {
        self.input_tokens + self.output_tokens
    }

    fn uncached_input_tokens(&self) -> u64 {
        self.input_tokens.saturating_sub(self.cached_input_tokens)
    }

    fn add(&mut self, other: &CodexUsage) {
        self.input_tokens += other.input_tokens;
        self.cached_input_tokens += other.cached_input_tokens;
        self.output_tokens += other.output_tokens;
    }
}

#[derive(Default, Clone, Copy, Debug)]
struct CodexCostBreakdown {
    input: f64,
    cached_input: f64,
    output: f64,
}

impl CodexCostBreakdown {
    fn total(&self) -> f64 {
        self.input + self.cached_input + self.output
    }

    fn add(&mut self, other: &CodexCostBreakdown) {
        self.input += other.input;
        self.cached_input += other.cached_input;
        self.output += other.output;
    }
}

#[derive(Clone, Copy, Debug)]
struct CodexPricing {
    input_per_token: f64,
    cached_input_per_token: f64,
    output_per_token: f64,
}

impl CodexPricing {
    fn cost_breakdown(&self, usage: &CodexUsage) -> CodexCostBreakdown {
        CodexCostBreakdown {
            input: usage.uncached_input_tokens() as f64 * self.input_per_token,
            cached_input: usage.cached_input_tokens as f64 * self.cached_input_per_token,
            output: usage.output_tokens as f64 * self.output_per_token,
        }
    }
}

#[derive(Default, Clone, Debug)]
struct CodexOutputInfo {
    model: Option<String>,
    session_id: Option<String>,
    total_tokens: Option<u64>,
}

#[derive(Default, Clone, Debug)]
struct CodexSessionData {
    model: Option<String>,
    usage: CodexUsage,
}

pub fn run(runs: Vec<PathBuf>, all: bool) -> Result<()> {
    let run_dirs = resolve_runs(runs, all)?;

    if run_dirs.is_empty() {
        return Err(Error::NoRuns);
    }

    let mut all_reports: Vec<(String, RunReport)> = Vec::new();

    for run_dir in &run_dirs {
        let label = run_dir
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| run_dir.display().to_string());
        let report = process_run(run_dir, &label)?;
        all_reports.push((label, report));
    }

    if all_reports.len() >= 2 {
        print_comparison(&all_reports);
    }

    Ok(())
}

fn resolve_runs(runs: Vec<PathBuf>, all: bool) -> Result<Vec<PathBuf>> {
    if all {
        let results_dir = project_results_dir();
        let discovered = discover_runs(&results_dir)?;
        return Ok(discovered.into_iter().map(|(path, _)| path).collect());
    }
    if runs.is_empty() {
        return Err(Error::NoRuns);
    }
    for run in &runs {
        if !run.is_dir() {
            return Err(Error::RunNotFound { path: run.clone() });
        }
    }
    Ok(runs)
}

fn process_run(run_dir: &Path, label: &str) -> Result<RunReport> {
    let agent = read_agent(run_dir).unwrap_or_else(|| "claude".to_string());

    let mut levels = find_level_dirs(run_dir);
    levels.sort();
    let is_full_mode = levels.is_empty();

    print_run_header(label);

    if agent == "codex" {
        return process_codex_run(run_dir, &agent, is_full_mode);
    }

    print_claude_table_header();
    let (grand_total, any_usage) = print_claude_level_rows(run_dir, &levels, is_full_mode);
    print_claude_table_footer(&grand_total);
    print_claude_cost_summary(&grand_total, any_usage);

    let total_tokens = grand_total.total();
    let cost = any_usage.then_some(grand_total.cost());
    let usage = any_usage.then_some(grand_total);

    Ok(RunReport {
        agent,
        model: None,
        usage: RunUsage::Claude { usage },
        total_tokens,
        cost,
    })
}

fn print_run_header(label: &str) {
    let separator = "=".repeat(70);
    println!("\n{separator}");
    println!("  {label}");
    println!("{separator}");
}

fn print_claude_table_header() {
    println!(
        "{:<8} {:>10} {:>10} {:>12} {:>12} Status",
        "Level", "Input", "Output", "Cache Write", "Cache Read"
    );
    println!(
        "{} {} {} {} {} {}",
        "-".repeat(8), "-".repeat(10), "-".repeat(10),
        "-".repeat(12), "-".repeat(12), "-".repeat(10)
    );
}

fn print_claude_level_rows(
    run_dir: &Path, levels: &[PathBuf], is_full_mode: bool,
) -> (TokenUsage, bool) {
    let mut grand_total = TokenUsage::default();
    let mut any_usage = false;

    if is_full_mode {
        let session_file = run_dir.join("session.jsonl");
        print_claude_row("full", &session_file, None, &mut grand_total, &mut any_usage);
    } else {
        for level_dir in levels {
            let level_name = level_dir
                .file_name()
                .map(|n| n.to_string_lossy().into_owned())
                .unwrap_or_default();
            let session_file = level_dir.join("session.jsonl");
            let status = read_status(level_dir);
            print_claude_row(&level_name, &session_file, Some(&status), &mut grand_total, &mut any_usage);
        }
    }

    (grand_total, any_usage)
}

fn print_claude_row(
    label: &str, session_file: &Path, status: Option<&str>,
    grand_total: &mut TokenUsage, any_usage: &mut bool,
) {
    let suffix = status.map(|s| format!(" {s}")).unwrap_or_default();
    match parse_session(session_file) {
        Some(tokens) => {
            *any_usage = true;
            grand_total.add(&tokens);
            println!(
                "{:<8} {:>10} {:>10} {:>12} {:>12}{suffix}",
                label,
                fmt_comma(tokens.input_tokens),
                fmt_comma(tokens.output_tokens),
                fmt_comma(tokens.cache_creation_input_tokens),
                fmt_comma(tokens.cache_read_input_tokens),
            );
        }
        None => {
            println!("{:<8} {:>10} {:>10} {:>12} {:>12}{suffix}", label, "—", "—", "—", "—");
        }
    }
}

fn print_claude_table_footer(grand_total: &TokenUsage) {
    println!(
        "{} {} {} {} {}",
        "-".repeat(8), "-".repeat(10), "-".repeat(10),
        "-".repeat(12), "-".repeat(12)
    );
    println!(
        "{:<8} {:>10} {:>10} {:>12} {:>12}",
        "TOTAL",
        fmt_comma(grand_total.input_tokens),
        fmt_comma(grand_total.output_tokens),
        fmt_comma(grand_total.cache_creation_input_tokens),
        fmt_comma(grand_total.cache_read_input_tokens),
    );
}

fn print_claude_cost_summary(grand_total: &TokenUsage, any_usage: bool) {
    println!();
    if !any_usage {
        println!("  Total tokens: unavailable");
        println!("  Estimated cost: unavailable");
        return;
    }
    let cost_input = grand_total.input_tokens as f64 * PRICE_INPUT;
    let cost_output = grand_total.output_tokens as f64 * PRICE_OUTPUT;
    let cost_cache_w = grand_total.cache_creation_input_tokens as f64 * PRICE_CACHE_WRITE;
    let cost_cache_r = grand_total.cache_read_input_tokens as f64 * PRICE_CACHE_READ;
    let total_cost = grand_total.cost();
    println!("  Total tokens: {}", fmt_comma(grand_total.total()));
    println!("  Estimated cost: ${total_cost:.2}");
    println!(
        "    Input: ${cost_input:.2} | Output: ${cost_output:.2} | Cache Write: ${cost_cache_w:.2} | Cache Read: ${cost_cache_r:.2}"
    );
}

struct CodexRunState {
    grand_usage: CodexUsage,
    grand_total_tokens: u64,
    grand_costs: CodexCostBreakdown,
    any_usage: bool,
    complete_usage: bool,
    complete_cost: bool,
    run_model: Option<String>,
    mixed_model: bool,
}

fn process_codex_run(run_dir: &Path, agent: &str, is_full_mode: bool) -> Result<RunReport> {
    print_codex_table_header();

    let mut state = CodexRunState {
        grand_usage: CodexUsage::default(),
        grand_total_tokens: 0,
        grand_costs: CodexCostBreakdown::default(),
        any_usage: false,
        complete_usage: true,
        complete_cost: true,
        run_model: None,
        mixed_model: false,
    };

    for (level_name, dir) in codex_sources(run_dir, is_full_mode) {
        process_codex_level(&level_name, &dir, &mut state);
    }

    let model = if state.mixed_model { Some("multiple".to_string()) } else { state.run_model.clone() };
    print_codex_table_footer(&state, &model);

    Ok(RunReport {
        agent: agent.to_string(),
        model,
        usage: RunUsage::Codex {
            usage: (state.any_usage && state.complete_usage).then_some(state.grand_usage),
        },
        total_tokens: state.grand_total_tokens,
        cost: (state.complete_cost && state.any_usage).then_some(state.grand_costs.total()),
    })
}

fn print_codex_table_header() {
    println!(
        "{:<8} {:>10} {:>12} {:>10} {:>12} {:>10} Status",
        "Level", "Input", "Cached In", "Output", "Total", "Cost"
    );
    println!(
        "{} {} {} {} {} {} {}",
        "-".repeat(8), "-".repeat(10), "-".repeat(12),
        "-".repeat(10), "-".repeat(12), "-".repeat(10), "-".repeat(10)
    );
}

fn process_codex_level(level_name: &str, dir: &Path, state: &mut CodexRunState) {
    let status = read_status(dir);
    let output_info = parse_codex_output_info(dir);
    let session_data = load_codex_session(dir, output_info.session_id.as_deref());

    let level_model = session_data
        .as_ref()
        .and_then(|data| data.model.clone())
        .or(output_info.model.clone());
    merge_model_label(&mut state.run_model, &mut state.mixed_model, level_model.as_deref());

    match session_data {
        Some(data) => print_codex_level_with_data(level_name, &data, &level_model, &status, state),
        None => print_codex_level_fallback(level_name, &output_info, &status, state),
    }
}

fn print_codex_level_with_data(
    level_name: &str, data: &CodexSessionData, level_model: &Option<String>,
    status: &str, state: &mut CodexRunState,
) {
    state.any_usage = true;
    state.grand_usage.add(&data.usage);
    let level_total_tokens = data.usage.total_tokens();
    state.grand_total_tokens += level_total_tokens;

    let level_cost = level_model
        .as_deref()
        .and_then(codex_pricing)
        .map(|pricing| {
            let breakdown = pricing.cost_breakdown(&data.usage);
            state.grand_costs.add(&breakdown);
            breakdown.total()
        });

    if level_cost.is_none() {
        state.complete_cost = false;
    }

    println!(
        "{:<8} {:>10} {:>12} {:>10} {:>12} {:>10} {status}",
        level_name,
        fmt_comma(data.usage.input_tokens),
        fmt_comma(data.usage.cached_input_tokens),
        fmt_comma(data.usage.output_tokens),
        fmt_comma(level_total_tokens),
        level_cost.map(|cost| format!("${cost:.2}")).unwrap_or_else(|| "—".to_string()),
    );
}

fn print_codex_level_fallback(
    level_name: &str, output_info: &CodexOutputInfo, status: &str, state: &mut CodexRunState,
) {
    state.complete_usage = false;
    state.complete_cost = false;
    let total_str = match output_info.total_tokens {
        Some(total_tokens) => {
            state.grand_total_tokens += total_tokens;
            fmt_comma(total_tokens)
        }
        None => "—".to_string(),
    };
    println!(
        "{:<8} {:>10} {:>12} {:>10} {:>12} {:>10} {status}",
        level_name, "—", "—", "—", total_str, "—",
    );
}

fn print_codex_table_footer(state: &CodexRunState, model: &Option<String>) {
    let dash = |ok: bool, val: u64| {
        if ok { fmt_comma(val) } else { "—".to_string() }
    };
    let complete = state.any_usage && state.complete_usage;

    println!(
        "{} {} {} {} {} {}",
        "-".repeat(8), "-".repeat(10), "-".repeat(12),
        "-".repeat(10), "-".repeat(12), "-".repeat(10)
    );
    println!(
        "{:<8} {:>10} {:>12} {:>10} {:>12} {:>10}",
        "TOTAL",
        dash(complete, state.grand_usage.input_tokens),
        dash(complete, state.grand_usage.cached_input_tokens),
        dash(complete, state.grand_usage.output_tokens),
        dash(state.grand_total_tokens > 0, state.grand_total_tokens),
        if state.complete_cost && state.any_usage {
            format!("${:.2}", state.grand_costs.total())
        } else {
            "—".to_string()
        },
    );

    println!();
    if state.grand_total_tokens > 0 {
        println!("  Total tokens: {}", fmt_comma(state.grand_total_tokens));
    } else {
        println!("  Total tokens: unavailable");
    }
    if let Some(model_name) = model.as_deref() {
        println!("  Model: {model_name}");
    }
    if state.complete_cost && state.any_usage {
        println!("  Estimated cost: ${:.2}", state.grand_costs.total());
        println!(
            "    Uncached input: ${:.2} | Cached input: ${:.2} | Output: ${:.2}",
            state.grand_costs.input, state.grand_costs.cached_input, state.grand_costs.output,
        );
    } else {
        println!("  Estimated cost: unavailable");
    }
}

fn print_comparison(all_reports: &[(String, RunReport)]) {
    if all_reports
        .iter()
        .all(|(_, report)| !report.is_codex() && report.claude_usage().is_some())
    {
        print_claude_comparison(all_reports);
    } else if all_reports.iter().all(|(_, report)| report.is_codex()) {
        print_codex_comparison(all_reports);
    } else {
        print_mixed_comparison(all_reports);
    }
}

fn print_claude_comparison(all_reports: &[(String, RunReport)]) {
    let short_labels = short_labels(all_reports);
    let col = |f: fn(&TokenUsage) -> String| -> Vec<String> {
        all_reports.iter()
            .map(|(_, r)| f(r.claude_usage().expect("checked")))
            .collect()
    };
    let rows = vec![
        ("Input Tokens", col(|u| fmt_comma(u.input_tokens))),
        ("Output Tokens", col(|u| fmt_comma(u.output_tokens))),
        ("Cache Write", col(|u| fmt_comma(u.cache_creation_input_tokens))),
        ("Cache Read", col(|u| fmt_comma(u.cache_read_input_tokens))),
        ("Total Tokens", col(|u| fmt_comma(u.total()))),
        ("Est. Cost", all_reports.iter().map(|(_, r)| r.cost_display()).collect()),
    ];
    print_string_comparison(&short_labels, &rows);
}

fn print_codex_comparison(all_reports: &[(String, RunReport)]) {
    let short_labels = short_labels(all_reports);
    let codex_col = |f: fn(&CodexUsage) -> String| -> Vec<String> {
        all_reports.iter()
            .map(|(_, r)| r.codex_usage().map(f).unwrap_or_else(|| "—".to_string()))
            .collect()
    };
    let rows = vec![
        ("Model", all_reports.iter().map(|(_, r)| r.model_display()).collect()),
        ("Input Tokens", codex_col(|u| fmt_comma(u.input_tokens))),
        ("Cached Input", codex_col(|u| fmt_comma(u.cached_input_tokens))),
        ("Output Tokens", codex_col(|u| fmt_comma(u.output_tokens))),
        ("Total Tokens", all_reports.iter().map(|(_, r)| r.total_tokens_display()).collect()),
        ("Est. Cost", all_reports.iter().map(|(_, r)| r.cost_display()).collect()),
    ];
    print_string_comparison(&short_labels, &rows);
}

fn print_mixed_comparison(all_reports: &[(String, RunReport)]) {
    let short_labels = short_labels(all_reports);
    let rows = vec![
        (
            "Agent",
            all_reports
                .iter()
                .map(|(_, report)| report.agent.clone())
                .collect(),
        ),
        (
            "Model",
            all_reports
                .iter()
                .map(|(_, report)| report.model_display())
                .collect(),
        ),
        (
            "Total Tokens",
            all_reports
                .iter()
                .map(|(_, report)| report.total_tokens_display())
                .collect(),
        ),
        (
            "Est. Cost",
            all_reports
                .iter()
                .map(|(_, report)| report.cost_display())
                .collect(),
        ),
    ];
    print_string_comparison(&short_labels, &rows);
}

fn print_string_comparison(short_labels: &[String], rows: &[(&str, Vec<String>)]) {
    let col_width = short_labels
        .iter()
        .map(|label| label.len())
        .chain(
            rows.iter()
                .flat_map(|(_, values)| values.iter().map(|value| value.len())),
        )
        .max()
        .unwrap_or(15)
        .max(15);

    let total_width = 25 + short_labels.len() * (col_width + 1);
    let separator = "=".repeat(total_width);

    println!("\n{separator}");
    println!("  COMPARISON");
    println!("{separator}");

    print!("{:<25}", "Metric");
    for label in short_labels {
        print!(" {label:>col_width$}");
    }
    println!();

    print!("{}", "-".repeat(25));
    for _ in short_labels {
        print!(" {}", "-".repeat(col_width));
    }
    println!();

    for (name, values) in rows {
        print!("{name:<25}");
        for value in values {
            print!(" {value:>col_width$}");
        }
        println!();
    }
}

fn short_labels(all_reports: &[(String, RunReport)]) -> Vec<String> {
    all_reports
        .iter()
        .map(|(label, _)| extract_run_name(label))
        .collect()
}

fn extract_run_name(dir_name: &str) -> String {
    let parts: Vec<&str> = dir_name.splitn(3, '_').collect();
    if parts.len() < 2 {
        return dir_name.to_string();
    }
    let name_and_ts = &dir_name[parts[0].len() + 1..];
    let Some(idx) = name_and_ts.rfind('_') else {
        return name_and_ts.to_string();
    };
    let candidate = &name_and_ts[..idx];
    let suffix = &name_and_ts[idx + 1..];
    let looks_like_ts = suffix.len() >= 8
        && suffix.chars().take(8).all(|c| c.is_ascii_digit() || c == 'T');
    if looks_like_ts {
        candidate.to_string()
    } else {
        name_and_ts.to_string()
    }
}

fn find_level_dirs(run_dir: &Path) -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    let Ok(entries) = fs::read_dir(run_dir) else {
        return dirs;
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

fn read_status(path: &Path) -> String {
    let status_file = path.join("status.txt");
    if let Ok(content) = fs::read_to_string(status_file) {
        return content.trim().to_string();
    }

    if path.join("bench.log").is_file() || read_meta(path).and_then(|meta| meta.end_time).is_some()
    {
        return "done".to_string();
    }

    "running...".to_string()
}

fn read_agent(run_dir: &Path) -> Option<String> {
    read_meta(run_dir)?.agent
}

fn read_meta(run_dir: &Path) -> Option<MetaJson> {
    let meta_path = run_dir.join("meta.json");
    let content = fs::read_to_string(meta_path).ok()?;
    serde_json::from_str(&content).ok()
}

fn codex_sources(run_dir: &Path, is_full_mode: bool) -> Vec<(String, PathBuf)> {
    if is_full_mode {
        vec![("full".to_string(), run_dir.to_path_buf())]
    } else {
        let mut levels = find_level_dirs(run_dir);
        levels.sort();
        levels
            .into_iter()
            .map(|dir| {
                let name = dir
                    .file_name()
                    .map(|n| n.to_string_lossy().into_owned())
                    .unwrap_or_default();
                (name, dir)
            })
            .collect()
    }
}

fn parse_codex_output_info(dir: &Path) -> CodexOutputInfo {
    let path = dir.join("agent-output.txt");
    let Ok(content) = fs::read_to_string(path) else {
        return CodexOutputInfo::default();
    };

    let stripped = strip_ansi(&content);
    let lines: Vec<&str> = stripped.lines().map(str::trim).collect();

    let mut info = CodexOutputInfo::default();

    for (idx, line) in lines.iter().enumerate() {
        if let Some(model) = line.strip_prefix("model:") {
            info.model = Some(model.trim().to_string());
        }
        if let Some(session_id) = line.strip_prefix("session id:") {
            info.session_id = Some(session_id.trim().to_string());
        }
        if *line == "tokens used" {
            info.total_tokens = parse_following_number(&lines, idx + 1);
        }
    }

    info
}

fn parse_following_number(lines: &[&str], start_idx: usize) -> Option<u64> {
    for line in lines.iter().skip(start_idx) {
        let digits: String = line.chars().filter(|c| c.is_ascii_digit()).collect();
        if !digits.is_empty() {
            return digits.parse().ok();
        }
        if !line.is_empty() {
            break;
        }
    }
    None
}

fn load_codex_session(dir: &Path, session_id: Option<&str>) -> Option<CodexSessionData> {
    let captured = dir.join("session.jsonl");
    if captured.is_file() {
        return parse_codex_rollout(&captured);
    }

    let session_id = session_id?;
    let rollout = find_codex_rollout(session_id)?;
    parse_codex_rollout(&rollout)
}

fn find_codex_rollout(session_id: &str) -> Option<PathBuf> {
    let home = std::env::var("HOME").ok()?;
    let sessions_root = PathBuf::from(home).join(".codex").join("sessions");
    if !sessions_root.is_dir() {
        return None;
    }

    if let Some(day_dir) = codex_day_dir(&sessions_root, session_id) {
        if let Some(found) = find_rollout_in_dir(&day_dir, session_id) {
            return Some(found);
        }
    }

    find_rollout_recursive(&sessions_root, session_id, 3)
}

fn codex_day_dir(sessions_root: &Path, session_id: &str) -> Option<PathBuf> {
    let unix_ms = uuid_v7_unix_ms(session_id)?;
    let (year, month, day, _, _, _) = utc_from_epoch((unix_ms / 1000) as i64)?;
    Some(
        sessions_root
            .join(format!("{year:04}"))
            .join(format!("{month:02}"))
            .join(format!("{day:02}")),
    )
}

fn find_rollout_in_dir(dir: &Path, session_id: &str) -> Option<PathBuf> {
    let suffix = format!("{session_id}.jsonl");
    let entries = fs::read_dir(dir).ok()?;
    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_file() {
            continue;
        }
        let Some(name) = path.file_name().and_then(|name| name.to_str()) else {
            continue;
        };
        if name.starts_with("rollout-") && name.ends_with(&suffix) {
            return Some(path);
        }
    }
    None
}

fn find_rollout_recursive(dir: &Path, session_id: &str, remaining_depth: usize) -> Option<PathBuf> {
    if let Some(found) = find_rollout_in_dir(dir, session_id) {
        return Some(found);
    }
    if remaining_depth == 0 {
        return None;
    }

    let entries = fs::read_dir(dir).ok()?;
    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }
        if let Some(found) = find_rollout_recursive(&path, session_id, remaining_depth - 1) {
            return Some(found);
        }
    }
    None
}

fn uuid_v7_unix_ms(session_id: &str) -> Option<u64> {
    let compact: String = session_id.chars().filter(|c| *c != '-').collect();
    if compact.len() != 32 {
        return None;
    }
    if compact.chars().nth(12)? != '7' {
        return None;
    }
    u64::from_str_radix(&compact[..12], 16).ok()
}

fn utc_from_epoch(epoch: i64) -> Option<(i64, i64, i64, i64, i64, i64)> {
    if epoch < 0 {
        return None;
    }

    let days = epoch / 86_400;
    let secs_in_day = epoch % 86_400;

    let mut year = 1970i64;
    let mut remaining_days = days;
    loop {
        let year_days = if is_leap_year(year) { 366 } else { 365 };
        if remaining_days < year_days {
            break;
        }
        remaining_days -= year_days;
        year += 1;
    }

    let month_days = [
        31,
        28 + if is_leap_year(year) { 1 } else { 0 },
        31,
        30,
        31,
        30,
        31,
        31,
        30,
        31,
        30,
        31,
    ];

    let mut month = 1i64;
    for month_len in month_days {
        if remaining_days < month_len {
            break;
        }
        remaining_days -= month_len;
        month += 1;
    }

    let day = remaining_days + 1;
    let hour = secs_in_day / 3_600;
    let minute = (secs_in_day % 3_600) / 60;
    let second = secs_in_day % 60;

    Some((year, month, day, hour, minute, second))
}

fn is_leap_year(year: i64) -> bool {
    year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
}

fn parse_codex_rollout(path: &Path) -> Option<CodexSessionData> {
    let content = fs::read_to_string(path).ok()?;
    let mut usage = CodexUsage::default();
    let mut model = None;
    let mut found_usage = false;

    for line in content.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        let obj: serde_json::Value = match serde_json::from_str(line) {
            Ok(value) => value,
            Err(_) => continue,
        };

        if model.is_none()
            && obj.get("type").and_then(|value| value.as_str()) == Some("turn_context")
        {
            model = obj
                .get("payload")
                .and_then(|value| value.get("model"))
                .and_then(|value| value.as_str())
                .map(|value| value.to_string());
        }

        if obj.get("type").and_then(|value| value.as_str()) != Some("event_msg") {
            continue;
        }

        let Some(payload) = obj.get("payload") else { continue };
        if payload.get("type").and_then(|value| value.as_str()) != Some("token_count") {
            continue;
        }

        let Some(totals) = payload
            .get("info")
            .and_then(|value| value.get("total_token_usage"))
            .and_then(|value| value.as_object())
        else {
            continue;
        };

        update_codex_usage_from_totals(&mut usage, totals);
        found_usage = true;
    }

    if found_usage {
        Some(CodexSessionData { model, usage })
    } else {
        None
    }
}

fn update_codex_usage_from_totals(usage: &mut CodexUsage, totals: &serde_json::Map<String, serde_json::Value>) {
    let get = |key| totals.get(key).and_then(|v| v.as_u64()).unwrap_or(0);
    usage.input_tokens = usage.input_tokens.max(get("input_tokens"));
    usage.cached_input_tokens = usage.cached_input_tokens.max(get("cached_input_tokens"));
    usage.output_tokens = usage.output_tokens.max(get("output_tokens"));
}

const GPT_54: CodexPricing = CodexPricing {
    input_per_token: 2.50 / 1_000_000.0,
    cached_input_per_token: 0.25 / 1_000_000.0,
    output_per_token: 15.00 / 1_000_000.0,
};
const GPT_54_MINI: CodexPricing = CodexPricing {
    input_per_token: 0.75 / 1_000_000.0,
    cached_input_per_token: 0.075 / 1_000_000.0,
    output_per_token: 4.50 / 1_000_000.0,
};
const GPT_54_NANO: CodexPricing = CodexPricing {
    input_per_token: 0.20 / 1_000_000.0,
    cached_input_per_token: 0.02 / 1_000_000.0,
    output_per_token: 1.25 / 1_000_000.0,
};
const GPT_53_CODEX: CodexPricing = CodexPricing {
    input_per_token: 1.75 / 1_000_000.0,
    cached_input_per_token: 0.175 / 1_000_000.0,
    output_per_token: 14.00 / 1_000_000.0,
};
const GPT_52: CodexPricing = CodexPricing {
    input_per_token: 1.75 / 1_000_000.0,
    cached_input_per_token: 0.175 / 1_000_000.0,
    output_per_token: 14.00 / 1_000_000.0,
};
const GPT_51: CodexPricing = CodexPricing {
    input_per_token: 1.25 / 1_000_000.0,
    cached_input_per_token: 0.125 / 1_000_000.0,
    output_per_token: 10.00 / 1_000_000.0,
};
const GPT_5_MINI: CodexPricing = CodexPricing {
    input_per_token: 0.25 / 1_000_000.0,
    cached_input_per_token: 0.025 / 1_000_000.0,
    output_per_token: 2.00 / 1_000_000.0,
};
const CODEX_MINI_LATEST: CodexPricing = CodexPricing {
    input_per_token: 1.50 / 1_000_000.0,
    cached_input_per_token: 0.375 / 1_000_000.0,
    output_per_token: 6.00 / 1_000_000.0,
};

fn codex_pricing(model: &str) -> Option<CodexPricing> {
    let model = model.trim().to_ascii_lowercase();
    resolve_codex_pricing(&model)
}

fn resolve_codex_pricing(model: &str) -> Option<CodexPricing> {
    if model == "codex-mini-latest" || model.starts_with("codex-mini-latest-") {
        return Some(CODEX_MINI_LATEST);
    }
    if model == "gpt-5.4" || model.starts_with("gpt-5.4-") {
        return Some(resolve_gpt54_variant(model));
    }
    if model == "gpt-5.3-codex" || model.starts_with("gpt-5.3-codex-") {
        return Some(GPT_53_CODEX);
    }
    if matches_any(model, &["gpt-5.2", "gpt-5.2-codex"], &["gpt-5.2-", "gpt-5.2-codex-"]) {
        return Some(GPT_52);
    }
    if model == "gpt-5-codex-mini" || model.starts_with("gpt-5-codex-mini-") {
        return None;
    }
    if matches_any(model, &["gpt-5.1-codex-mini", "gpt-5-mini"], &["gpt-5.1-codex-mini-", "gpt-5-mini-"]) {
        return Some(GPT_5_MINI);
    }
    if matches_any(model,
        &["gpt-5.1-codex-max", "gpt-5.1-codex", "gpt-5-codex", "gpt-5.1", "gpt-5"],
        &["gpt-5.1-codex-max-", "gpt-5.1-codex-", "gpt-5-codex-", "gpt-5.1-", "gpt-5-"],
    ) {
        return Some(GPT_51);
    }
    None
}

fn resolve_gpt54_variant(model: &str) -> CodexPricing {
    if model.starts_with("gpt-5.4-mini") {
        GPT_54_MINI
    } else if model.starts_with("gpt-5.4-nano") {
        GPT_54_NANO
    } else {
        GPT_54
    }
}

fn matches_any(model: &str, exact: &[&str], prefixes: &[&str]) -> bool {
    exact.contains(&model) || prefixes.iter().any(|p| model.starts_with(p))
}

fn merge_model_label(current: &mut Option<String>, mixed: &mut bool, candidate: Option<&str>) {
    let Some(candidate) = candidate.filter(|value| !value.is_empty()) else {
        return;
    };

    match current {
        Some(existing) if existing != candidate => *mixed = true,
        Some(_) => {}
        None => *current = Some(candidate.to_string()),
    }
}

fn strip_ansi(input: &str) -> String {
    let mut result = String::with_capacity(input.len());
    let mut chars = input.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '\x1b' {
            skip_ansi_escape(&mut chars);
        } else if ch != '\r' {
            result.push(ch);
        }
    }
    result
}

fn skip_ansi_escape(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) {
    if chars.peek() != Some(&'[') {
        return;
    }
    chars.next();
    while let Some(&next) = chars.peek() {
        chars.next();
        if next.is_ascii_alphabetic() {
            break;
        }
    }
}

pub(crate) fn parse_session(jsonl_path: &Path) -> Option<TokenUsage> {
    let content = fs::read_to_string(jsonl_path).ok()?;
    let mut totals = TokenUsage::default();
    let mut found = false;

    for line in content.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        let obj: serde_json::Value = match serde_json::from_str(line) {
            Ok(value) => value,
            Err(_) => continue,
        };

        let usage = obj
            .get("message")
            .and_then(|message| message.as_object())
            .and_then(|message| message.get("usage"))
            .and_then(|usage| usage.as_object());

        if let Some(usage) = usage {
            found = true;
            totals.input_tokens += usage
                .get("input_tokens")
                .and_then(|value| value.as_u64())
                .unwrap_or(0);
            totals.output_tokens += usage
                .get("output_tokens")
                .and_then(|value| value.as_u64())
                .unwrap_or(0);
            totals.cache_creation_input_tokens += usage
                .get("cache_creation_input_tokens")
                .and_then(|value| value.as_u64())
                .unwrap_or(0);
            totals.cache_read_input_tokens += usage
                .get("cache_read_input_tokens")
                .and_then(|value| value.as_u64())
                .unwrap_or(0);
        }
    }

    if found {
        Some(totals)
    } else {
        None
    }
}
