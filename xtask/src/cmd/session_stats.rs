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

    // Count content blocks
    let mut thinking_count = 0u64;
    let mut thinking_chars = 0u64;
    let mut text_count = 0u64;
    let mut text_chars = 0u64;
    let mut tool_use_count = 0u64;
    let mut tool_result_count = 0u64;
    let mut user_count = 0u64;
    let mut tool_names: HashMap<String, u64> = HashMap::new();
    let mut bash_cmds: HashMap<String, u64> = HashMap::new();
    let mut test_results: (u64, u64) = (0, 0); // (pass, fail)
    let mut levels_attempted: Vec<String> = Vec::new();

    for event in &all_events {
        match &event.kind {
            EventKind::User { .. } => user_count += 1,
            EventKind::Assistant { blocks } => {
                for block in blocks {
                    match block {
                        ContentBlock::Thinking(t) => {
                            thinking_count += 1;
                            thinking_chars += t.len() as u64;
                        }
                        ContentBlock::Text(t) => {
                            text_count += 1;
                            text_chars += t.len() as u64;
                        }
                        ContentBlock::ToolUse { name, input_json } => {
                            tool_use_count += 1;
                            *tool_names.entry(name.clone()).or_default() += 1;

                            if name == "Bash" {
                                if let Some(cmd) = extract_bash_command(input_json) {
                                    let key = classify_bash_command(&cmd);
                                    *bash_cmds.entry(key.clone()).or_default() += 1;

                                    // Track test results
                                    let effective_cmd = strip_cd_prefix(&cmd);
                                    if effective_cmd.contains("cargo xtask test") {
                                        // Extract level
                                        if let Some(level) = extract_test_level(&cmd) {
                                            if !levels_attempted.contains(&level) {
                                                levels_attempted.push(level);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        ContentBlock::ToolResult { content } => {
                            tool_result_count += 1;
                            // Check test pass/fail in tool results
                            if content.contains("test result: ok") || content.contains("PASSED") {
                                test_results.0 += 1;
                            } else if content.contains("FAILED")
                                || content.contains("test result: FAILED")
                            {
                                test_results.1 += 1;
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    // Duration
    let duration = model::elapsed_secs(&meta);

    // Print report
    println!(
        "{}Run:{} {} (strategy: {}, mode: {})",
        Color::BOLD,
        Color::RESET,
        run_name,
        strategy,
        mode
    );
    println!(
        "Session: {} file(s), {} events",
        session_files.len(),
        all_events.len()
    );
    println!();

    println!("{}CONTENT BLOCKS:{}", Color::BOLD, Color::RESET);
    println!(
        "  thinking:     {:>4} blocks, {:>8} chars",
        thinking_count,
        fmt_comma(thinking_chars)
    );
    println!(
        "  text:         {:>4} blocks, {:>8} chars",
        text_count,
        fmt_comma(text_chars)
    );
    println!("  tool_use:     {:>4} blocks", tool_use_count);
    println!("  tool_result:  {:>4} blocks", tool_result_count);
    println!("  user:         {:>4} messages", user_count);
    println!();

    println!("{}TOOL USAGE:{}", Color::BOLD, Color::RESET);
    let mut sorted_tools: Vec<_> = tool_names.iter().collect();
    sorted_tools.sort_by(|a, b| b.1.cmp(a.1));
    for (name, count) in &sorted_tools {
        println!("  {:<14} {}", name, count);
    }
    println!();

    if !bash_cmds.is_empty() {
        println!("{}BASH COMMANDS:{}", Color::BOLD, Color::RESET);
        let mut sorted_bash: Vec<_> = bash_cmds.iter().collect();
        sorted_bash.sort_by(|a, b| b.1.cmp(a.1));
        for (cmd, count) in &sorted_bash {
            println!("  {:<30} {}", cmd, count);
        }
        if test_results.0 > 0 || test_results.1 > 0 {
            println!(
                "  test outcomes: {} pass, {} fail",
                test_results.0, test_results.1
            );
        }
        println!();
    }

    println!("{}TIMELINE:{}", Color::BOLD, Color::RESET);
    if let Some(secs) = duration {
        println!("  Duration: {}", fmt_duration(secs));
    }
    if !levels_attempted.is_empty() {
        levels_attempted.sort();
        let first = levels_attempted.first().unwrap();
        let last = levels_attempted.last().unwrap();
        println!(
            "  Levels attempted: {}-{} ({}/16)",
            first,
            last,
            levels_attempted.len()
        );
    }

    Ok(())
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
    if let Some(pos) = parts.iter().position(|&p| p == "test") {
        if let Some(level) = parts.get(pos + 1) {
            if level.len() == 2 && level.chars().all(|c| c.is_ascii_digit()) {
                return Some(format!("L{level}"));
            }
        }
    }
    None
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
