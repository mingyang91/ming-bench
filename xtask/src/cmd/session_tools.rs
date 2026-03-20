use crate::model::{Color, Result};
use crate::session::{self, ContentBlock, EventKind};
use std::collections::HashMap;
use std::path::PathBuf;

pub fn run(run_arg: PathBuf, summary: bool, level: Option<String>) -> Result<()> {
    let run_dir = session::resolve_run(&run_arg)?;
    let session_files = session::session_files(&run_dir);

    if session_files.is_empty() {
        eprintln!("No session files found in {}", run_dir.display());
        return Ok(());
    }

    // Filter by level if specified
    let files: Vec<_> = if let Some(ref lvl) = level {
        let target = if lvl.starts_with('L') {
            lvl.clone()
        } else {
            format!("L{lvl}")
        };
        session_files
            .into_iter()
            .filter(|(label, _)| label == &target)
            .collect()
    } else {
        session_files
    };

    // Collect all tool calls
    let mut tool_calls: Vec<ToolCall> = Vec::new();
    let mut tool_counts: HashMap<String, u64> = HashMap::new();
    let mut bash_subcmds: HashMap<String, u64> = HashMap::new();
    let mut tool_first_last: HashMap<String, (String, String)> = HashMap::new();

    let all_events = parse_all_events(&files)?;
    for event in &all_events {
        let EventKind::Assistant { blocks } = &event.kind else { continue };
        let ts = short_time(event.timestamp.as_deref());
        collect_tool_calls(
            blocks, event.index, &ts,
            &mut tool_calls, &mut tool_counts, &mut tool_first_last, &mut bash_subcmds,
        );
    }

    if summary {
        print_summary(&tool_counts, &tool_first_last, &bash_subcmds);
    } else {
        print_timeline(&tool_calls);

        // Print totals
        let mut sorted: Vec<_> = tool_counts.iter().collect();
        sorted.sort_by(|a, b| b.1.cmp(a.1));
        let total: u64 = sorted.iter().map(|(_, c)| *c).sum();
        let parts: Vec<String> = sorted.iter().map(|(n, c)| format!("{n}: {c}")).collect();
        println!(
            "\n{}TOTAL: {} tool calls ({}){}",
            Color::BOLD,
            total,
            parts.join(", "),
            Color::RESET
        );
    }

    Ok(())
}

fn parse_all_events(
    files: &[(String, std::path::PathBuf)],
) -> Result<Vec<session::SessionEvent>> {
    let mut all = Vec::new();
    for (_, path) in files {
        all.extend(session::parse_session(path)?);
    }
    Ok(all)
}

fn collect_tool_calls(
    blocks: &[ContentBlock],
    event_idx: usize,
    ts: &str,
    tool_calls: &mut Vec<ToolCall>,
    tool_counts: &mut HashMap<String, u64>,
    tool_first_last: &mut HashMap<String, (String, String)>,
    bash_subcmds: &mut HashMap<String, u64>,
) {
    for block in blocks {
        let ContentBlock::ToolUse { name, input_json } = block else { continue };
        let input_summary = summarize_input(name, input_json);
        tool_calls.push(ToolCall {
            index: event_idx,
            time: ts.to_string(),
            tool: name.clone(),
            input: input_summary,
        });

        *tool_counts.entry(name.clone()).or_default() += 1;

        tool_first_last
            .entry(name.clone())
            .and_modify(|(_, last)| *last = ts.to_string())
            .or_insert((ts.to_string(), ts.to_string()));

        if name == "Bash" {
            let cmd = extract_bash_cmd(input_json);
            let key = classify_bash(&cmd);
            *bash_subcmds.entry(key).or_default() += 1;
        }
    }
}

struct ToolCall {
    index: usize,
    time: String,
    tool: String,
    input: String,
}

fn print_timeline(calls: &[ToolCall]) {
    println!(
        "{}{:<5} {:<10} {:<10} INPUT{}",
        Color::BOLD, "#", "TIME", "TOOL", Color::RESET
    );
    for call in calls {
        println!(
            "{:<5} {:<10} {:<10} {}",
            call.index, call.time, call.tool, call.input
        );
    }
}

fn print_summary(
    tool_counts: &HashMap<String, u64>,
    first_last: &HashMap<String, (String, String)>,
    bash_subcmds: &HashMap<String, u64>,
) {
    println!(
        "{}{:<14} {:>5}   {:<10} {:<10}{}",
        Color::BOLD, "TOOL", "COUNT", "FIRST", "LAST", Color::RESET
    );

    let mut sorted: Vec<_> = tool_counts.iter().collect();
    sorted.sort_by(|a, b| b.1.cmp(a.1));

    for (name, count) in &sorted {
        let (first, last) = first_last
            .get(*name)
            .cloned()
            .unwrap_or(("?".into(), "?".into()));
        println!("{name:<14} {count:>5}   {first:<10} {last:<10}");
    }

    if !bash_subcmds.is_empty() {
        println!();
        println!("{}BASH BREAKDOWN:{}",Color::BOLD, Color::RESET);
        let mut sorted_bash: Vec<_> = bash_subcmds.iter().collect();
        sorted_bash.sort_by(|a, b| b.1.cmp(a.1));
        for (cmd, count) in sorted_bash {
            println!("  {cmd:<30} {count:>3}");
        }
    }
}

fn short_time(ts: Option<&str>) -> String {
    ts.and_then(|s| s.split('T').nth(1))
        .map(|t| t.split('.').next().unwrap_or(t))
        .unwrap_or("??:??:??")
        .to_string()
}

fn summarize_input(tool_name: &str, input_json: &str) -> String {
    let v: serde_json::Value = match serde_json::from_str(input_json) {
        Ok(v) => v,
        Err(_) => return truncate(input_json, 60),
    };

    match tool_name {
        "Read" => v
            .get("file_path")
            .and_then(|p| p.as_str())
            .map(shorten_path)
            .unwrap_or_default(),
        "Write" => {
            let path = v
                .get("file_path")
                .and_then(|p| p.as_str())
                .map(shorten_path)
                .unwrap_or_default();
            let size = v
                .get("content")
                .and_then(|c| c.as_str())
                .map(|s| s.len())
                .unwrap_or(0);
            format!("{path} ({size} bytes)")
        }
        "Edit" => {
            let path = v
                .get("file_path")
                .and_then(|p| p.as_str())
                .map(shorten_path)
                .unwrap_or_default();
            path
        }
        "Bash" => v
            .get("command")
            .and_then(|c| c.as_str())
            .map(|s| truncate(s, 60))
            .unwrap_or_default(),
        "Grep" => {
            let pattern = v.get("pattern").and_then(|p| p.as_str()).unwrap_or("?");
            let path = v.get("path").and_then(|p| p.as_str()).unwrap_or(".");
            format!("/{pattern}/ in {}", shorten_path(path))
        }
        "Glob" => v
            .get("pattern")
            .and_then(|p| p.as_str())
            .unwrap_or("?")
            .to_string(),
        _ => truncate(input_json, 60),
    }
}

fn shorten_path(path: &str) -> String {
    // Shorten workspace paths
    if let Some(pos) = path.find("/bench/") {
        return format!("bench/{}", &path[pos + 7..]);
    }
    if let Some(pos) = path.find("/workspace/") {
        return path[pos + 11..].to_string();
    }
    path.to_string()
}

fn truncate(s: &str, max: usize) -> String {
    if s.len() > max {
        format!("{}…", &s[..max])
    } else {
        s.to_string()
    }
}

fn extract_bash_cmd(input_json: &str) -> String {
    serde_json::from_str::<serde_json::Value>(input_json)
        .ok()
        .and_then(|v| v.get("command").and_then(|c| c.as_str()).map(String::from))
        .unwrap_or_default()
}

fn classify_bash(cmd: &str) -> String {
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
        let first = trimmed.split_whitespace().next().unwrap_or(trimmed);
        first.to_string()
    }
}

fn strip_cd_prefix(cmd: &str) -> &str {
    let trimmed = cmd.trim();
    if trimmed.starts_with("cd ") {
        if let Some(pos) = trimmed.find("&&") {
            return trimmed[pos + 2..].trim();
        }
    }
    trimmed
}
