use crate::model::{Color, Result};
use crate::session::{self, ContentBlock, EventKind};
use std::path::PathBuf;

pub fn run(
    run_arg: PathBuf,
    keywords: Vec<String>,
    context: Option<usize>,
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

    // Parse all events
    let mut all_events = Vec::new();
    for (_, path) in &files {
        let events = session::parse_session(path)?;
        all_events.extend(events);
    }

    let total_events = all_events.len();
    let ctx_chars = context.unwrap_or(80);

    for keyword in &keywords {
        let kw_lower = keyword.to_lowercase();
        let mut thinking_hits: Vec<usize> = Vec::new();
        let mut text_hits: Vec<usize> = Vec::new();
        let mut tool_use_hits: Vec<usize> = Vec::new();
        let mut tool_result_hits: Vec<usize> = Vec::new();
        let mut user_hits: Vec<usize> = Vec::new();
        let mut snippets: Vec<(usize, String, String)> = Vec::new(); // (event_idx, type, snippet)

        for event in &all_events {
            match &event.kind {
                EventKind::User { text } => {
                    if text.to_lowercase().contains(&kw_lower) {
                        user_hits.push(event.index);
                        if snippets.len() < 3 {
                            snippets.push((
                                event.index,
                                "user".to_string(),
                                extract_context(text, &kw_lower, ctx_chars),
                            ));
                        }
                    }
                }
                EventKind::Assistant { blocks } => {
                    for block in blocks {
                        match block {
                            ContentBlock::Thinking(t) => {
                                if t.to_lowercase().contains(&kw_lower) {
                                    thinking_hits.push(event.index);
                                    if snippets.len() < 3 {
                                        snippets.push((
                                            event.index,
                                            "thinking".to_string(),
                                            extract_context(t, &kw_lower, ctx_chars),
                                        ));
                                    }
                                }
                            }
                            ContentBlock::Text(t) => {
                                if t.to_lowercase().contains(&kw_lower) {
                                    text_hits.push(event.index);
                                    if snippets.len() < 3 {
                                        snippets.push((
                                            event.index,
                                            "text".to_string(),
                                            extract_context(t, &kw_lower, ctx_chars),
                                        ));
                                    }
                                }
                            }
                            ContentBlock::ToolUse { input_json, .. } => {
                                if input_json.to_lowercase().contains(&kw_lower) {
                                    tool_use_hits.push(event.index);
                                }
                            }
                            ContentBlock::ToolResult { content } => {
                                if content.to_lowercase().contains(&kw_lower) {
                                    tool_result_hits.push(event.index);
                                }
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        let total_hits =
            thinking_hits.len() + text_hits.len() + tool_use_hits.len() + user_hits.len();

        println!(
            "\n{}KEYWORD: {} ({} hits){}",
            Color::BOLD,
            keyword,
            total_hits,
            Color::RESET
        );

        if total_hits == 0 && tool_result_hits.is_empty() {
            println!("  (no matches)");
            continue;
        }

        print_hit_line("thinking", &thinking_hits);
        print_hit_line("text", &text_hits);
        print_hit_line("tool_use", &tool_use_hits);
        print_hit_line("tool_result", &tool_result_hits);
        print_hit_line("user", &user_hits);

        // Span analysis
        let all_indices: Vec<usize> = {
            let mut v = Vec::new();
            v.extend(&thinking_hits);
            v.extend(&text_hits);
            v.extend(&tool_use_hits);
            v.extend(&user_hits);
            v.sort();
            v.dedup();
            v
        };

        if let (Some(&first), Some(&last)) = (all_indices.first(), all_indices.last()) {
            let span_pct = if total_events > 0 {
                ((last - first) as f64 / total_events as f64 * 100.0) as u32
            } else {
                0
            };
            println!();
            println!("  First: event {:>3}    Last: event {:>3}    Span: {}% of session",
                first, last, span_pct);
        }

        // Show context snippets
        if !snippets.is_empty() {
            println!();
            for (idx, kind, snippet) in &snippets {
                println!(
                    "  {}[event {} / {}]:{} {}",
                    Color::DIM, idx, kind, Color::RESET, snippet
                );
            }
        }
    }

    println!();
    Ok(())
}

fn print_hit_line(label: &str, hits: &[usize]) {
    if hits.is_empty() {
        println!("  {:<14} 0", format!("{label}:"));
    } else {
        let events_str: Vec<String> = hits.iter().map(|i| i.to_string()).collect();
        let display = if events_str.len() > 10 {
            format!(
                "{}, ... (+{})",
                events_str[..10].join(", "),
                events_str.len() - 10
            )
        } else {
            events_str.join(", ")
        };
        println!(
            "  {:<14} {} hits  [events: {}]",
            format!("{label}:"),
            hits.len(),
            display
        );
    }
}

fn extract_context(text: &str, keyword_lower: &str, ctx_chars: usize) -> String {
    let text_lower = text.to_lowercase();
    if let Some(pos) = text_lower.find(keyword_lower) {
        let start = pos.saturating_sub(ctx_chars / 2);
        let end = (pos + keyword_lower.len() + ctx_chars / 2).min(text.len());

        // Adjust to char boundaries
        let start = text.floor_char_boundary(start);
        let end = text.ceil_char_boundary(end);

        let snippet = &text[start..end];
        let snippet = snippet.replace('\n', " ");
        if start > 0 || end < text.len() {
            format!("...{}...", snippet.trim())
        } else {
            snippet.trim().to_string()
        }
    } else {
        String::new()
    }
}
