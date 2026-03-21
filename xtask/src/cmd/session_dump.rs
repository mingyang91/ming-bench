use crate::model::{Color, Result};
use crate::session::{self, ContentBlock, EventKind};
use std::path::PathBuf;

pub fn run(
    run_arg: PathBuf,
    thinking: bool,
    text: bool,
    tools: bool,
    user: bool,
    level: Option<String>,
) -> Result<()> {
    let run_dir = session::resolve_run(&run_arg)?;
    let session_files = session::session_files(&run_dir);

    if session_files.is_empty() {
        eprintln!("No session files found in {}", run_dir.display());
        return Ok(());
    }

    let files = filter_by_level(session_files, level.as_deref());

    // If no filter flags, show everything
    let show_all = !thinking && !text && !tools && !user;
    let filter = DumpFilter {
        thinking,
        text,
        tools,
        user,
        show_all,
    };

    for (label, path) in &files {
        if files.len() > 1 {
            println!("\n{}═══ {label} ═══{}", Color::BOLD, Color::RESET);
        }

        let events = session::parse_session(path)?;
        for event in &events {
            print_event(event, &filter);
        }
    }

    Ok(())
}

struct DumpFilter {
    thinking: bool,
    text: bool,
    tools: bool,
    user: bool,
    show_all: bool,
}

fn print_event(event: &session::SessionEvent, f: &DumpFilter) {
    let ts = short_timestamp(event.timestamp.as_deref());

    match &event.kind {
        EventKind::User { text: t } if f.show_all || f.user => {
            println!(
                "\n{}[{:03} {ts}] USER ({} chars){}",
                Color::CYAN,
                event.index,
                t.len(),
                Color::RESET
            );
            print_indented(t, 200);
        }
        EventKind::Assistant { blocks } => {
            print_assistant_blocks(blocks, event.index, &ts, f);
        }
        _ => {}
    }
}

fn print_assistant_blocks(blocks: &[ContentBlock], idx: usize, ts: &str, f: &DumpFilter) {
    for block in blocks {
        match block {
            ContentBlock::Thinking(t) if f.show_all || f.thinking => {
                println!(
                    "\n{}[{idx:03} {ts}] THINKING ({} chars){}",
                    Color::DIM,
                    t.len(),
                    Color::RESET
                );
                print_indented(t, 500);
            }
            ContentBlock::Text(t) if f.show_all || f.text => {
                println!(
                    "\n{}[{idx:03} {ts}] TEXT ({} chars){}",
                    Color::GREEN,
                    t.len(),
                    Color::RESET
                );
                print_indented(t, 300);
            }
            ContentBlock::ToolUse { name, input_json } if f.show_all || f.tools => {
                println!(
                    "\n{}[{idx:03} {ts}] TOOL: {name}{}",
                    Color::YELLOW,
                    Color::RESET
                );
                print_indented(input_json, 200);
            }
            ContentBlock::ToolResult { content } if f.show_all || f.tools => {
                println!(
                    "{}[{idx:03} {ts}] TOOL_RESULT ({} chars){}",
                    Color::DIM,
                    content.len(),
                    Color::RESET
                );
                print_indented(content, 150);
            }
            _ => {}
        }
    }
}

fn filter_by_level(
    session_files: Vec<(String, PathBuf)>,
    level: Option<&str>,
) -> Vec<(String, PathBuf)> {
    let Some(lvl) = level else {
        return session_files;
    };
    let target = if lvl.starts_with('L') {
        lvl.to_string()
    } else {
        format!("L{lvl}")
    };
    session_files
        .into_iter()
        .filter(|(label, _)| label == &target)
        .collect()
}

fn short_timestamp(ts: Option<&str>) -> String {
    // "2026-03-19T22:34:37.439Z" → "22:34:37"
    ts.and_then(|s| s.split('T').nth(1))
        .map(|t| t.split('.').next().unwrap_or(t))
        .unwrap_or("??:??:??")
        .to_string()
}

fn print_indented(text: &str, max_chars: usize) {
    let truncated = if text.len() > max_chars {
        format!("{}…", &text[..max_chars])
    } else {
        text.to_string()
    };
    for line in truncated.lines().take(10) {
        println!("  {line}");
    }
    let total_lines = text.lines().count();
    if total_lines > 10 {
        println!("  ... ({} more lines)", total_lines - 10);
    }
}
