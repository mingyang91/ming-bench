use crate::model::{self, fmt_comma, fmt_duration, Color, MetaJson, Result};
use crate::session::{self, ContentBlock, EventKind, SessionEvent};
use std::collections::HashMap;
use std::path::PathBuf;

pub fn run(run_arg: PathBuf) -> Result<()> {
    let run_dir = session::resolve_run(&run_arg)?;
    let run_name = run_dir
        .file_name()
        .map(|n| n.to_string_lossy().to_string())
        .unwrap_or_default();

    // Read meta.json
    let meta: MetaJson = {
        let meta_path = run_dir.join("meta.json");
        let content =
            std::fs::read_to_string(&meta_path).map_err(|e| model::Error::io(&meta_path, e))?;
        serde_json::from_str(&content).unwrap_or_default()
    };

    let strategy = meta
        .strategy
        .as_deref()
        .or_else(|| infer_strategy(&run_name))
        .unwrap_or("unknown");
    let mode = meta.mode.as_deref().unwrap_or("unknown");

    // Parse all session files
    let session_files = session::session_files(&run_dir);
    if session_files.is_empty() {
        eprintln!("No session files found in {}", run_dir.display());
        return Ok(());
    }

    let mut all_events: Vec<SessionEvent> = Vec::new();
    for (_, path) in &session_files {
        let events = session::parse_session(path)?;
        all_events.extend(events);
    }

    let stats = gather_stats(&all_events);

    print_report(&run_name, strategy, mode, &session_files, &all_events, &stats, &meta);

    Ok(())
}

struct Stats {
    thinking_count: u64,
    thinking_chars: u64,
    text_count: u64,
    text_chars: u64,
    tool_use_count: u64,
    tool_result_count: u64,
    user_count: u64,
    tool_names: HashMap<String, u64>,
    bash_cmds: HashMap<String, u64>,
    test_results: (u64, u64),
    levels_attempted: Vec<String>,
}

fn gather_stats(events: &[SessionEvent]) -> Stats {
    let mut s = Stats {
        thinking_count: 0, thinking_chars: 0,
        text_count: 0, text_chars: 0,
        tool_use_count: 0, tool_result_count: 0, user_count: 0,
        tool_names: HashMap::new(), bash_cmds: HashMap::new(),
        test_results: (0, 0), levels_attempted: Vec::new(),
    };

    for event in events {
        match &event.kind {
            EventKind::User { .. } => s.user_count += 1,
            EventKind::Assistant { blocks } => tally_blocks(blocks, &mut s),
            _ => {}
        }
    }

    s.levels_attempted.sort();
    s
}

fn tally_blocks(blocks: &[ContentBlock], s: &mut Stats) {
    for block in blocks {
        tally_single_block(block, s);
    }
}

fn tally_single_block(block: &ContentBlock, s: &mut Stats) {
    match block {
        ContentBlock::Thinking(t) => {
            s.thinking_count += 1;
            s.thinking_chars += t.len() as u64;
        }
        ContentBlock::Text(t) => {
            s.text_count += 1;
            s.text_chars += t.len() as u64;
        }
        ContentBlock::ToolUse { name, input_json } => {
            s.tool_use_count += 1;
            *s.tool_names.entry(name.clone()).or_default() += 1;
            if name == "Bash" {
                tally_bash(input_json, s);
            }
        }
        ContentBlock::ToolResult { content } => {
            s.tool_result_count += 1;
            tally_test_result(content, s);
        }
    }
}

fn tally_test_result(content: &str, s: &mut Stats) {
    if content.contains("test result: ok") || content.contains("PASSED") {
        s.test_results.0 += 1;
    } else if content.contains("FAILED") || content.contains("test result: FAILED") {
        s.test_results.1 += 1;
    }
}

fn tally_bash(input_json: &str, s: &mut Stats) {
    let Some(cmd) = extract_bash_command(input_json) else { return };
    let key = classify_bash_command(&cmd);
    *s.bash_cmds.entry(key).or_default() += 1;

    let effective_cmd = strip_cd_prefix(&cmd);
    if !effective_cmd.contains("cargo xtask test") {
        return;
    }
    if let Some(level) = extract_test_level(&cmd) {
        if !s.levels_attempted.contains(&level) {
            s.levels_attempted.push(level);
        }
    }
}

fn print_report(
    run_name: &str,
    strategy: &str,
    mode: &str,
    session_files: &[(String, std::path::PathBuf)],
    all_events: &[SessionEvent],
    s: &Stats,
    meta: &MetaJson,
) {
    println!(
        "{}Run:{} {run_name} (strategy: {strategy}, mode: {mode})",
        Color::BOLD, Color::RESET
    );
    println!("Session: {} file(s), {} events", session_files.len(), all_events.len());
    println!();

    println!("{}CONTENT BLOCKS:{}", Color::BOLD, Color::RESET);
    println!("  thinking:     {:>4} blocks, {:>8} chars", s.thinking_count, fmt_comma(s.thinking_chars));
    println!("  text:         {:>4} blocks, {:>8} chars", s.text_count, fmt_comma(s.text_chars));
    println!("  tool_use:     {:>4} blocks", s.tool_use_count);
    println!("  tool_result:  {:>4} blocks", s.tool_result_count);
    println!("  user:         {:>4} messages", s.user_count);
    println!();

    print_tool_usage(&s.tool_names);
    print_bash_cmds(&s.bash_cmds, s.test_results);
    print_timeline(meta, &s.levels_attempted);
}

fn print_tool_usage(tool_names: &HashMap<String, u64>) {
    println!("{}TOOL USAGE:{}", Color::BOLD, Color::RESET);
    let mut sorted: Vec<_> = tool_names.iter().collect();
    sorted.sort_by(|a, b| b.1.cmp(a.1));
    for (name, count) in &sorted {
        println!("  {name:<14} {count}");
    }
    println!();
}

fn print_bash_cmds(bash_cmds: &HashMap<String, u64>, test_results: (u64, u64)) {
    if bash_cmds.is_empty() {
        return;
    }
    println!("{}BASH COMMANDS:{}", Color::BOLD, Color::RESET);
    let mut sorted: Vec<_> = bash_cmds.iter().collect();
    sorted.sort_by(|a, b| b.1.cmp(a.1));
    for (cmd, count) in &sorted {
        println!("  {cmd:<30} {count}");
    }
    if test_results.0 > 0 || test_results.1 > 0 {
        println!("  test outcomes: {} pass, {} fail", test_results.0, test_results.1);
    }
    println!();
}

fn print_timeline(meta: &MetaJson, levels_attempted: &[String]) {
    println!("{}TIMELINE:{}", Color::BOLD, Color::RESET);
    if let Some(secs) = model::elapsed_secs(meta) {
        println!("  Duration: {}", fmt_duration(secs));
    }
    if let (Some(first), Some(last)) = (levels_attempted.first(), levels_attempted.last()) {
        println!(
            "  Levels attempted: {first}-{last} ({}/{})",
            levels_attempted.len(),
            crate::model::LEVELS.len()
        );
    }
}

fn extract_bash_command(input_json: &str) -> Option<String> {
    let v: serde_json::Value = serde_json::from_str(input_json).ok()?;
    v.get("command").and_then(|c| c.as_str()).map(String::from)
}

fn classify_bash_command(cmd: &str) -> String {
    // Skip leading "cd ... &&" prefix
    let effective = strip_cd_prefix(cmd);
    let trimmed = effective.trim();
    if trimmed.starts_with("cargo xtask test") {
        "cargo xtask test".to_string()
    } else if trimmed.starts_with("cargo xtask") {
        "cargo xtask (other)".to_string()
    } else if trimmed.starts_with("cargo build") {
        "cargo build".to_string()
    } else if trimmed.starts_with("cargo clippy") {
        "cargo clippy".to_string()
    } else if trimmed.starts_with("cargo test") {
        "cargo test (DIRECT!)".to_string()
    } else if trimmed.starts_with("cargo") {
        "cargo (other)".to_string()
    } else {
        let first_word = trimmed.split_whitespace().next().unwrap_or(trimmed);
        first_word.to_string()
    }
}

/// Strip "cd /some/path && " prefix from bash commands.
fn strip_cd_prefix(cmd: &str) -> &str {
    let trimmed = cmd.trim();
    if trimmed.starts_with("cd ") {
        if let Some(pos) = trimmed.find("&&") {
            return trimmed[pos + 2..].trim();
        }
    }
    trimmed
}

fn extract_test_level(cmd: &str) -> Option<String> {
    let parts: Vec<&str> = cmd.split_whitespace().collect();
    // "cargo xtask test 01" -> "L01"
    let pos = parts.iter().position(|&p| p == "test")?;
    let level = parts.get(pos + 1)?;
    let is_level = level.len() == 2 && level.chars().all(|c| c.is_ascii_digit());
    is_level.then(|| format!("L{level}"))
}

fn infer_strategy(run_name: &str) -> Option<&str> {
    if run_name.starts_with("quality-gate_") || run_name.starts_with("strategy_") {
        Some("quality-gate")
    } else if run_name.starts_with("default_") || run_name.starts_with("main_") {
        Some("default")
    } else {
        None
    }
}
