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

    let files = filter_by_level(session_files, level.as_deref());

    // Parse all events
    let mut all_events = Vec::new();
    for (_, path) in &files {
        let events = session::parse_session(path)?;
        all_events.extend(events);
    }

    let ctx_chars = context.unwrap_or(80);

    for keyword in &keywords {
        search_keyword(keyword, &all_events, ctx_chars);
    }

    println!();
    Ok(())
}

fn filter_by_level(
    session_files: Vec<(String, std::path::PathBuf)>,
    level: Option<&str>,
) -> Vec<(String, std::path::PathBuf)> {
    let Some(lvl) = level else { return session_files };
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

struct KeywordHits {
    thinking: Vec<usize>,
    text: Vec<usize>,
    tool_use: Vec<usize>,
    tool_result: Vec<usize>,
    user: Vec<usize>,
    snippets: Vec<(usize, String, String)>,
}

fn search_keyword(keyword: &str, events: &[session::SessionEvent], ctx_chars: usize) {
    let kw_lower = keyword.to_lowercase();
    let mut hits = KeywordHits {
        thinking: Vec::new(),
        text: Vec::new(),
        tool_use: Vec::new(),
        tool_result: Vec::new(),
        user: Vec::new(),
        snippets: Vec::new(),
    };

    for event in events {
        collect_event_hits(event, &kw_lower, ctx_chars, &mut hits);
    }

    let total_hits = hits.thinking.len() + hits.text.len() + hits.tool_use.len() + hits.user.len();

    println!(
        "\n{}KEYWORD: {keyword} ({total_hits} hits){}",
        Color::BOLD, Color::RESET
    );

    if total_hits == 0 && hits.tool_result.is_empty() {
        println!("  (no matches)");
        return;
    }

    print_hit_line("thinking", &hits.thinking);
    print_hit_line("text", &hits.text);
    print_hit_line("tool_use", &hits.tool_use);
    print_hit_line("tool_result", &hits.tool_result);
    print_hit_line("user", &hits.user);

    print_span_and_snippets(&hits, events.len());
}

fn collect_event_hits(
    event: &session::SessionEvent,
    kw_lower: &str,
    ctx_chars: usize,
    hits: &mut KeywordHits,
) {
    match &event.kind {
        EventKind::User { text } => {
            if !text.to_lowercase().contains(kw_lower) {
                return;
            }
            hits.user.push(event.index);
            if hits.snippets.len() < 3 {
                hits.snippets.push((event.index, "user".into(), extract_context(text, kw_lower, ctx_chars)));
            }
        }
        EventKind::Assistant { blocks } => {
            collect_block_hits(blocks, event.index, kw_lower, ctx_chars, hits);
        }
        _ => {}
    }
}

fn collect_block_hits(
    blocks: &[ContentBlock],
    idx: usize,
    kw_lower: &str,
    ctx_chars: usize,
    hits: &mut KeywordHits,
) {
    for block in blocks {
        collect_single_block(block, idx, kw_lower, ctx_chars, hits);
    }
}

fn collect_single_block(
    block: &ContentBlock,
    idx: usize,
    kw_lower: &str,
    ctx_chars: usize,
    hits: &mut KeywordHits,
) {
    match block {
        ContentBlock::Thinking(t) if t.to_lowercase().contains(kw_lower) => {
            hits.thinking.push(idx);
            maybe_add_snippet(hits, idx, "thinking", t, kw_lower, ctx_chars);
        }
        ContentBlock::Text(t) if t.to_lowercase().contains(kw_lower) => {
            hits.text.push(idx);
            maybe_add_snippet(hits, idx, "text", t, kw_lower, ctx_chars);
        }
        ContentBlock::ToolUse { input_json, .. } if input_json.to_lowercase().contains(kw_lower) => {
            hits.tool_use.push(idx);
        }
        ContentBlock::ToolResult { content } if content.to_lowercase().contains(kw_lower) => {
            hits.tool_result.push(idx);
        }
        _ => {}
    }
}

fn maybe_add_snippet(
    hits: &mut KeywordHits,
    idx: usize,
    kind: &str,
    text: &str,
    kw_lower: &str,
    ctx_chars: usize,
) {
    if hits.snippets.len() < 3 {
        hits.snippets.push((idx, kind.into(), extract_context(text, kw_lower, ctx_chars)));
    }
}

fn print_span_and_snippets(hits: &KeywordHits, total_events: usize) {
    let mut all_indices: Vec<usize> = Vec::new();
    all_indices.extend(&hits.thinking);
    all_indices.extend(&hits.text);
    all_indices.extend(&hits.tool_use);
    all_indices.extend(&hits.user);
    all_indices.sort();
    all_indices.dedup();

    if let (Some(&first), Some(&last)) = (all_indices.first(), all_indices.last()) {
        let span_pct = if total_events > 0 {
            ((last - first) as f64 / total_events as f64 * 100.0) as u32
        } else {
            0
        };
        println!();
        println!("  First: event {first:>3}    Last: event {last:>3}    Span: {span_pct}% of session");
    }

    if !hits.snippets.is_empty() {
        println!();
        for (idx, kind, snippet) in &hits.snippets {
            println!(
                "  {}[event {idx} / {kind}]:{} {snippet}",
                Color::DIM, Color::RESET
            );
        }
    }
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
