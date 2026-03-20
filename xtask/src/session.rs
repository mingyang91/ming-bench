use crate::model::{Error, Result};
use std::fs;
use std::path::{Path, PathBuf};

// ---------------------------------------------------------------------------
// Content blocks extracted from assistant messages
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub enum ContentBlock {
    Thinking(String),
    Text(String),
    ToolUse { name: String, input_json: String },
    ToolResult { content: String },
}

// ---------------------------------------------------------------------------
// Session events
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct SessionEvent {
    pub index: usize,
    pub timestamp: Option<String>,
    pub kind: EventKind,
}

#[derive(Debug, Clone)]
pub enum EventKind {
    User { text: String },
    Assistant { blocks: Vec<ContentBlock> },
    QueueOp,
    LastPrompt,
}

// ---------------------------------------------------------------------------
// Parsing
// ---------------------------------------------------------------------------

/// Parse a session.jsonl file into a sequence of typed events.
pub fn parse_session(path: &Path) -> Result<Vec<SessionEvent>> {
    let content = fs::read_to_string(path).map_err(|e| Error::io(path, e))?;
    let mut events = Vec::new();
    let mut index = 0;

    for line in content.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let obj: serde_json::Value = match serde_json::from_str(line) {
            Ok(v) => v,
            Err(_) => continue,
        };

        let timestamp = obj
            .get("timestamp")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string());

        let kind = match obj.get("type").and_then(|v| v.as_str()) {
            Some("user") => {
                let text = obj
                    .pointer("/message/content")
                    .and_then(|v| v.as_str())
                    .unwrap_or("")
                    .to_string();
                EventKind::User { text }
            }
            Some("assistant") => {
                let blocks = parse_content_blocks(&obj);
                EventKind::Assistant { blocks }
            }
            Some("last-prompt") => EventKind::LastPrompt,
            Some("queue-operation") => EventKind::QueueOp,
            _ => continue,
        };

        events.push(SessionEvent {
            index,
            timestamp,
            kind,
        });
        index += 1;
    }

    Ok(events)
}

fn parse_content_blocks(obj: &serde_json::Value) -> Vec<ContentBlock> {
    let mut blocks = Vec::new();
    let Some(serde_json::Value::Array(content)) = obj.pointer("/message/content") else {
        return blocks;
    };

    for item in content {
        let Some(block_type) = item.get("type").and_then(|v| v.as_str()) else {
            continue;
        };

        if let Some(block) = parse_single_block(block_type, item) {
            blocks.push(block);
        }
    }

    blocks
}

fn parse_single_block(block_type: &str, item: &serde_json::Value) -> Option<ContentBlock> {
    match block_type {
        "thinking" => {
            let text = item.get("thinking").and_then(|v| v.as_str()).unwrap_or("");
            if text.is_empty() { return None; }
            Some(ContentBlock::Thinking(text.to_string()))
        }
        "text" => {
            let text = item.get("text").and_then(|v| v.as_str()).unwrap_or("");
            if text.is_empty() { return None; }
            Some(ContentBlock::Text(text.to_string()))
        }
        "tool_use" => {
            let name = item.get("name").and_then(|v| v.as_str()).unwrap_or("unknown").to_string();
            let input_json = item.get("input").map(|v| v.to_string()).unwrap_or_default();
            Some(ContentBlock::ToolUse { name, input_json })
        }
        "tool_result" => {
            let content_str = parse_tool_result_content(item);
            if content_str.is_empty() { return None; }
            Some(ContentBlock::ToolResult { content: content_str })
        }
        _ => None,
    }
}

fn parse_tool_result_content(item: &serde_json::Value) -> String {
    match item.get("content") {
        Some(serde_json::Value::String(s)) => s.clone(),
        Some(serde_json::Value::Array(arr)) => {
            // tool_result content can be an array of {type: "text", text: "..."}
            arr.iter()
                .filter_map(|v| v.get("text").and_then(|t| t.as_str()))
                .collect::<Vec<_>>()
                .join("\n")
        }
        _ => String::new(),
    }
}

// ---------------------------------------------------------------------------
// Session file discovery
// ---------------------------------------------------------------------------

/// Find session.jsonl files for a run directory.
/// Returns (label, path) pairs.
/// Handles both flat mode (session.jsonl) and levels mode (L01/session.jsonl, ...).
pub fn session_files(run_dir: &Path) -> Vec<(String, PathBuf)> {
    let mut files = Vec::new();

    // Check for top-level session.jsonl (full mode)
    let top = run_dir.join("session.jsonl");
    if top.is_file() {
        files.push(("full".to_string(), top));
    }

    // Check for level directories (L01..L19)
    for level in crate::model::LEVELS {
        let level_dir = run_dir.join(format!("L{level}"));
        let session = level_dir.join("session.jsonl");
        if session.is_file() {
            files.push((format!("L{level}"), session));
        }
    }

    files
}

/// Resolve a run argument to a results directory path.
/// Accepts:
///   - Full path: results/default_cl-def-full_20260319T223223
///   - Run name: default_cl-def-full_20260319T223223
///   - Partial name: cl-def-full (matches first containing directory)
pub fn resolve_run(run_arg: &Path) -> Result<PathBuf> {
    // If the path exists directly, use it
    if run_arg.is_dir() {
        return Ok(run_arg.to_path_buf());
    }

    // Try under results/
    let results_dir = crate::model::project_results_dir();
    let under_results = results_dir.join(run_arg);
    if under_results.is_dir() {
        return Ok(under_results);
    }

    // Try partial match
    let run_str = run_arg.to_string_lossy();
    if let Ok(entries) = fs::read_dir(&results_dir) {
        let mut matches: Vec<PathBuf> = entries
            .filter_map(|e| e.ok())
            .filter(|e| {
                e.file_name()
                    .to_string_lossy()
                    .contains(run_str.as_ref())
            })
            .map(|e| e.path())
            .filter(|p| p.is_dir() && p.join("meta.json").is_file())
            .collect();
        matches.sort();
        if let Some(m) = matches.last() {
            return Ok(m.clone());
        }
    }

    Err(Error::RunNotFound {
        path: run_arg.to_path_buf(),
    })
}
