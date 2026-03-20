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

    // If no filter flags, show everything
    let show_all = !thinking && !text && !tools && !user;

    for (label, path) in &files {
        if files.len() > 1 {
            println!(
                "\n{}═══ {} ═══{}",
                Color::BOLD,
                label,
                Color::RESET
            );
        }

        let events = session::parse_session(path)?;
        for event in &events {
            let ts = short_timestamp(event.timestamp.as_deref());

            match &event.kind {
                EventKind::User { text: t } if show_all || user => {
                    println!(
                        "\n{}[{:03} {}] USER ({} chars){}",
                        Color::CYAN,
                        event.index,
                        ts,
                        t.len(),
                        Color::RESET
                    );
                    print_indented(t, 200);
                }
                EventKind::Assistant { blocks } => {
                    for block in blocks {
                        match block {
                            ContentBlock::Thinking(t) if show_all || thinking => {
                                println!(
                                    "\n{}[{:03} {}] THINKING ({} chars){}",
                                    Color::DIM,
                                    event.index,
                                    ts,
                                    t.len(),
                                    Color::RESET
                                );
                                print_indented(t, 500);
                            }
                            ContentBlock::Text(t) if show_all || text => {
                                println!(
                                    "\n{}[{:03} {}] TEXT ({} chars){}",
                                    Color::GREEN,
                                    event.index,
                                    ts,
                                    t.len(),
                                    Color::RESET
                                );
                                print_indented(t, 300);
                            }
                            ContentBlock::ToolUse { name, input_json } if show_all || tools => {
                                println!(
                                    "\n{}[{:03} {}] TOOL: {}{}",
                                    Color::YELLOW,
                                    event.index,
                                    ts,
                                    name,
                                    Color::RESET
                                );
                                print_indented(input_json, 200);
                            }
                            ContentBlock::ToolResult { content } if show_all || tools => {
                                println!(
                                    "{}[{:03} {}] TOOL_RESULT ({} chars){}",
                                    Color::DIM,
                                    event.index,
                                    ts,
                                    content.len(),
                                    Color::RESET
                                );
                                print_indented(content, 150);
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }
    }

    Ok(())
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
