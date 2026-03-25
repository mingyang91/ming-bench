use crate::codex;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SessionFormat {
    Claude,
    Codex,
    /// Codex agent-output.txt (thread.started / item.completed / turn.completed)
    CodexAgentOutput,
}

// ---------------------------------------------------------------------------
// Parsing
// ---------------------------------------------------------------------------

/// Parse a supported session transcript into a sequence of typed events.
pub fn parse_session(path: &Path) -> Result<Vec<SessionEvent>> {
    let content = fs::read_to_string(path).map_err(|e| Error::io(path, e))?;
    let Some(format) = detect_session_format_content(&content) else {
        return Ok(Vec::new());
    };
    Ok(parse_session_content(&content, format))
}

pub fn detect_session_format(path: &Path) -> Option<SessionFormat> {
    let content = fs::read_to_string(path).ok()?;
    detect_session_format_content(&content)
}

pub fn shell_command(name: &str, input_json: &str) -> Option<String> {
    let value: serde_json::Value = serde_json::from_str(input_json).ok()?;
    match name {
        "Bash" => value
            .get("command")
            .and_then(|field| field.as_str())
            .map(String::from),
        "exec_command" => value
            .get("cmd")
            .and_then(|field| field.as_str())
            .map(String::from),
        _ => None,
    }
}

fn detect_session_format_content(content: &str) -> Option<SessionFormat> {
    for line in content.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let obj: serde_json::Value = match serde_json::from_str(line) {
            Ok(value) => value,
            Err(_) => continue,
        };
        let Some(kind) = obj.get("type").and_then(|value| value.as_str()) else {
            continue;
        };
        return match kind {
            "user" | "assistant" | "last-prompt" | "queue-operation" => Some(SessionFormat::Claude),
            "session_meta" | "event_msg" | "response_item" | "turn_context" => {
                Some(SessionFormat::Codex)
            }
            "thread.started" | "turn.started" | "turn.completed" | "item.started"
            | "item.completed" => Some(SessionFormat::CodexAgentOutput),
            _ => None,
        };
    }
    None
}

fn parse_session_content(content: &str, format: SessionFormat) -> Vec<SessionEvent> {
    match format {
        SessionFormat::Claude => parse_claude_session(content),
        SessionFormat::Codex => parse_codex_session(content),
        SessionFormat::CodexAgentOutput => parse_codex_agent_output(content),
    }
}

fn parse_claude_session(content: &str) -> Vec<SessionEvent> {
    let mut events = Vec::new();
    let mut index = 0;

    for line in content.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let obj: serde_json::Value = match serde_json::from_str(line) {
            Ok(value) => value,
            Err(_) => continue,
        };

        let timestamp = obj
            .get("timestamp")
            .and_then(|value| value.as_str())
            .map(|value| value.to_string());

        let kind = match obj.get("type").and_then(|value| value.as_str()) {
            Some("user") => {
                let text = obj
                    .pointer("/message/content")
                    .and_then(|value| value.as_str())
                    .unwrap_or("")
                    .to_string();
                EventKind::User { text }
            }
            Some("assistant") => EventKind::Assistant {
                blocks: parse_claude_content_blocks(&obj),
            },
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

    events
}

fn parse_codex_session(content: &str) -> Vec<SessionEvent> {
    let mut events = Vec::new();
    let mut index = 0;

    for line in content.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let obj: serde_json::Value = match serde_json::from_str(line) {
            Ok(value) => value,
            Err(_) => continue,
        };

        let timestamp = obj
            .get("timestamp")
            .and_then(|value| value.as_str())
            .map(|value| value.to_string());

        let kind = match obj.get("type").and_then(|value| value.as_str()) {
            Some("event_msg") => parse_codex_event_message(&obj),
            Some("response_item") => parse_codex_response_item(&obj),
            _ => None,
        };

        let Some(kind) = kind else { continue };
        events.push(SessionEvent {
            index,
            timestamp,
            kind,
        });
        index += 1;
    }

    events
}

fn parse_codex_agent_output(content: &str) -> Vec<SessionEvent> {
    let mut events = Vec::new();
    let mut index = 0;

    for line in content.lines() {
        let line = line.trim();
        if line.is_empty() || !line.starts_with('{') {
            continue;
        }
        let obj: serde_json::Value = match serde_json::from_str(line) {
            Ok(value) => value,
            Err(_) => continue,
        };

        let Some(event_type) = obj.get("type").and_then(|v| v.as_str()) else {
            continue;
        };

        let kind = match event_type {
            "item.completed" => parse_codex_agent_output_item(&obj),
            // Emit agent_message items as User events too, so turn counting works.
            // Each agent_message represents a "turn" of agent reasoning.
            _ => None,
        };

        let Some(kind) = kind else { continue };
        events.push(SessionEvent {
            index,
            timestamp: None,
            kind,
        });
        index += 1;
    }

    events
}

/// Extract output_tokens from a Codex agent-output.txt turn.completed event.
pub fn codex_agent_output_tokens(path: &Path) -> u64 {
    let Ok(content) = fs::read_to_string(path) else {
        return 0;
    };
    for line in content.lines().rev() {
        let line = line.trim();
        if !line.starts_with('{') {
            continue;
        }
        let Ok(obj) = serde_json::from_str::<serde_json::Value>(line) else {
            continue;
        };
        if obj.get("type").and_then(|v| v.as_str()) == Some("turn.completed") {
            return obj
                .pointer("/usage/output_tokens")
                .and_then(|v| v.as_u64())
                .unwrap_or(0);
        }
    }
    0
}

/// Extract total usage from a Codex agent-output.txt turn.completed event.
pub fn codex_agent_output_usage(path: &Path) -> Option<(u64, u64, u64)> {
    let content = fs::read_to_string(path).ok()?;
    for line in content.lines().rev() {
        let line = line.trim();
        if !line.starts_with('{') {
            continue;
        }
        let obj: serde_json::Value = serde_json::from_str(line).ok()?;
        if obj.get("type").and_then(|v| v.as_str()) == Some("turn.completed") {
            let usage = obj.get("usage")?;
            let input = usage.get("input_tokens").and_then(|v| v.as_u64()).unwrap_or(0);
            let cached = usage.get("cached_input_tokens").and_then(|v| v.as_u64()).unwrap_or(0);
            let output = usage.get("output_tokens").and_then(|v| v.as_u64()).unwrap_or(0);
            return Some((input, cached, output));
        }
    }
    None
}

fn parse_codex_agent_output_item(obj: &serde_json::Value) -> Option<EventKind> {
    let item = obj.get("item")?;
    let item_type = item.get("type").and_then(|v| v.as_str())?;

    match item_type {
        "agent_message" => {
            let text = item
                .get("text")
                .and_then(|v| v.as_str())
                .unwrap_or("")
                .to_string();
            if text.is_empty() {
                return None;
            }
            Some(EventKind::Assistant {
                blocks: vec![ContentBlock::Text(text)],
            })
        }
        "command_execution" => {
            let command = item
                .get("command")
                .and_then(|v| v.as_str())
                .unwrap_or("")
                .to_string();
            let output = item
                .get("aggregated_output")
                .and_then(|v| v.as_str())
                .unwrap_or("")
                .to_string();
            // Only emit completed commands (with exit_code)
            if item.get("exit_code").and_then(|v| v.as_i64()).is_none() {
                return None;
            }
            let mut blocks = vec![ContentBlock::ToolUse {
                name: "Bash".to_string(),
                input_json: serde_json::json!({"command": command}).to_string(),
            }];
            if !output.is_empty() {
                blocks.push(ContentBlock::ToolResult { content: output });
            }
            Some(EventKind::Assistant { blocks })
        }
        _ => None,
    }
}

fn parse_claude_content_blocks(obj: &serde_json::Value) -> Vec<ContentBlock> {
    let mut blocks = Vec::new();
    let Some(serde_json::Value::Array(content)) = obj.pointer("/message/content") else {
        return blocks;
    };

    for item in content {
        let Some(block_type) = item.get("type").and_then(|value| value.as_str()) else {
            continue;
        };

        if let Some(block) = parse_claude_content_block(block_type, item) {
            blocks.push(block);
        }
    }

    blocks
}

fn parse_claude_content_block(block_type: &str, item: &serde_json::Value) -> Option<ContentBlock> {
    match block_type {
        "thinking" => {
            let text = item
                .get("thinking")
                .and_then(|value| value.as_str())
                .unwrap_or("");
            if text.is_empty() {
                return None;
            }
            Some(ContentBlock::Thinking(text.to_string()))
        }
        "text" => {
            let text = item
                .get("text")
                .and_then(|value| value.as_str())
                .unwrap_or("");
            if text.is_empty() {
                return None;
            }
            Some(ContentBlock::Text(text.to_string()))
        }
        "tool_use" => {
            let name = item
                .get("name")
                .and_then(|value| value.as_str())
                .unwrap_or("unknown")
                .to_string();
            let input_json = item
                .get("input")
                .map(json_value_to_string)
                .unwrap_or_default();
            Some(ContentBlock::ToolUse { name, input_json })
        }
        "tool_result" => {
            let content = parse_tool_result_content(item.get("content"));
            if content.is_empty() {
                return None;
            }
            Some(ContentBlock::ToolResult { content })
        }
        _ => None,
    }
}

fn parse_codex_event_message(obj: &serde_json::Value) -> Option<EventKind> {
    let payload = obj.get("payload")?;
    match payload.get("type").and_then(|value| value.as_str()) {
        Some("user_message") | Some("agent_message") => {
            let text = payload
                .get("message")
                .and_then(|value| value.as_str())
                .unwrap_or("")
                .to_string();
            Some(EventKind::User { text })
        }
        _ => None,
    }
}

fn parse_codex_response_item(obj: &serde_json::Value) -> Option<EventKind> {
    let payload = obj.get("payload")?;
    let blocks = match payload.get("type").and_then(|value| value.as_str()) {
        Some("message") => parse_codex_message_blocks(payload),
        Some("reasoning") => parse_codex_reasoning_blocks(payload),
        Some("function_call") => {
            let name = payload
                .get("name")
                .and_then(|value| value.as_str())
                .unwrap_or("unknown")
                .to_string();
            let input_json = payload
                .get("arguments")
                .map(json_value_to_string)
                .unwrap_or_default();
            vec![ContentBlock::ToolUse { name, input_json }]
        }
        Some("custom_tool_call") => {
            let name = payload
                .get("name")
                .and_then(|value| value.as_str())
                .unwrap_or("unknown")
                .to_string();
            let input_json = payload
                .get("input")
                .map(json_value_to_string)
                .unwrap_or_default();
            vec![ContentBlock::ToolUse { name, input_json }]
        }
        Some("function_call_output") | Some("custom_tool_call_output") => {
            let content = parse_tool_result_content(payload.get("output"));
            if content.is_empty() {
                Vec::new()
            } else {
                vec![ContentBlock::ToolResult { content }]
            }
        }
        _ => Vec::new(),
    };

    (!blocks.is_empty()).then_some(EventKind::Assistant { blocks })
}

fn parse_codex_message_blocks(payload: &serde_json::Value) -> Vec<ContentBlock> {
    if payload.get("role").and_then(|value| value.as_str()) != Some("assistant") {
        return Vec::new();
    }
    let Some(content) = payload.get("content").and_then(|value| value.as_array()) else {
        return Vec::new();
    };

    content
        .iter()
        .filter_map(
            |item| match item.get("type").and_then(|value| value.as_str()) {
                Some("output_text") | Some("text") => item
                    .get("text")
                    .and_then(|value| value.as_str())
                    .filter(|value| !value.is_empty())
                    .map(|value| ContentBlock::Text(value.to_string())),
                _ => None,
            },
        )
        .collect()
}

fn parse_codex_reasoning_blocks(payload: &serde_json::Value) -> Vec<ContentBlock> {
    let mut pieces = Vec::new();

    if let Some(summary) = payload.get("summary") {
        collect_visible_text(summary, &mut pieces);
    }
    if let Some(content) = payload.get("content") {
        collect_visible_text(content, &mut pieces);
    }

    let text = pieces
        .into_iter()
        .filter(|piece| !piece.is_empty())
        .collect::<Vec<_>>()
        .join("\n");

    if text.is_empty() {
        Vec::new()
    } else {
        vec![ContentBlock::Thinking(text)]
    }
}

fn collect_visible_text(value: &serde_json::Value, out: &mut Vec<String>) {
    match value {
        serde_json::Value::String(text) => out.push(text.clone()),
        serde_json::Value::Array(items) => {
            for item in items {
                collect_visible_text(item, out);
            }
        }
        serde_json::Value::Object(map) => {
            for next in visible_text_fields(map) {
                collect_visible_text(next, out);
            }
        }
        _ => {}
    }
}

fn parse_tool_result_content(value: Option<&serde_json::Value>) -> String {
    match value {
        Some(serde_json::Value::String(text)) => parse_tool_result_string(text),
        Some(serde_json::Value::Array(items)) => items
            .iter()
            .filter_map(|item| item.get("text").and_then(|value| value.as_str()))
            .collect::<Vec<_>>()
            .join("\n"),
        Some(serde_json::Value::Object(map)) => first_non_empty_nested(map, &["output", "content"])
            .unwrap_or_else(|| {
                map.get("text")
                    .and_then(|value| value.as_str())
                    .unwrap_or("")
                    .to_string()
            }),
        Some(other) => other.to_string(),
        None => String::new(),
    }
}

fn visible_text_fields(
    map: &serde_json::Map<String, serde_json::Value>,
) -> impl Iterator<Item = &serde_json::Value> {
    ["text", "content", "summary_text"]
        .into_iter()
        .filter_map(|key| map.get(key))
}

fn parse_tool_result_string(text: &str) -> String {
    serde_json::from_str::<serde_json::Value>(text)
        .ok()
        .map(|parsed| parse_tool_result_content(Some(&parsed)))
        .filter(|nested| !nested.is_empty())
        .unwrap_or_else(|| text.to_string())
}

fn first_non_empty_nested(
    map: &serde_json::Map<String, serde_json::Value>,
    keys: &[&str],
) -> Option<String> {
    keys.iter()
        .filter_map(|key| map.get(*key))
        .map(|value| parse_tool_result_content(Some(value)))
        .find(|nested| !nested.is_empty())
}

fn json_value_to_string(value: &serde_json::Value) -> String {
    value
        .as_str()
        .map(str::to_string)
        .unwrap_or_else(|| value.to_string())
}

// ---------------------------------------------------------------------------
// Session file discovery
// ---------------------------------------------------------------------------

/// Find supported session transcript files for a run directory.
/// Returns (label, path) pairs. Handles both full mode and levels mode.
pub fn session_files(run_dir: &Path) -> Vec<(String, PathBuf)> {
    let mut files = Vec::new();

    if let Some(top) = supported_session_path(run_dir) {
        files.push(("full".to_string(), top));
    }

    for level in crate::model::LEVELS {
        let level_dir = run_dir.join(format!("L{level}"));
        if let Some(session) = supported_session_path(&level_dir) {
            files.push((format!("L{level}"), session));
        }
    }

    files
}

fn supported_session_path(dir: &Path) -> Option<PathBuf> {
    if !dir.is_dir() {
        return None;
    }

    for name in ["session.jsonl", "session.log"] {
        let candidate = dir.join(name);
        if candidate.is_file() && detect_session_format(&candidate).is_some() {
            return Some(candidate);
        }
    }

    let output_file = dir.join("agent-output.txt");
    if output_file.is_file() {
        // Prefer rollout file (has token_count events) over agent-output.txt
        if let Some(rollout) = codex::resolve_rollout_from_output_file(&output_file) {
            return Some(rollout);
        }
        // Fall back to parsing agent-output.txt directly
        if detect_session_format(&output_file).is_some() {
            return Some(output_file);
        }
    }

    None
}

/// Resolve a run argument to a results directory path.
/// Accepts:
///   - Full path: results/default_cl-def-full_20260319T223223
///   - Run name: default_cl-def-full_20260319T223223
///   - Partial name: cl-def-full (matches first containing directory)
pub fn resolve_run(run_arg: &Path) -> Result<PathBuf> {
    if run_arg.is_dir() {
        return Ok(run_arg.to_path_buf());
    }

    let results_dir = crate::model::project_results_dir();
    let under_results = results_dir.join(run_arg);
    if under_results.is_dir() {
        return Ok(under_results);
    }

    let run_str = run_arg.to_string_lossy();
    if let Ok(entries) = fs::read_dir(&results_dir) {
        let mut matches: Vec<PathBuf> = entries
            .filter_map(|entry| entry.ok())
            .filter(|entry| {
                entry
                    .file_name()
                    .to_string_lossy()
                    .contains(run_str.as_ref())
            })
            .map(|entry| entry.path())
            .filter(|path| path.is_dir() && path.join("meta.json").is_file())
            .collect();
        matches.sort();
        if let Some(found) = matches.last() {
            return Ok(found.clone());
        }
    }

    Err(Error::RunNotFound {
        path: run_arg.to_path_buf(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::{Mutex, OnceLock};
    use std::time::{SystemTime, UNIX_EPOCH};

    fn test_lock() -> &'static Mutex<()> {
        static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
        LOCK.get_or_init(|| Mutex::new(()))
    }

    fn unique_temp_dir(prefix: &str) -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("xtask-{prefix}-{nanos}"));
        fs::create_dir_all(&dir).expect("create temp dir");
        dir
    }

    const CLAUDE_SAMPLE: &str = r#"{"type":"user","timestamp":"2026-03-21T09:00:00Z","message":{"content":"hello"}}
{"type":"assistant","timestamp":"2026-03-21T09:00:01Z","message":{"content":[{"type":"text","text":"world"}]}}"#;

    const CODEX_SAMPLE: &str = r#"{"timestamp":"2026-03-21T09:00:00Z","type":"event_msg","payload":{"type":"user_message","message":"hello","images":[],"local_images":[],"text_elements":[]}}
{"timestamp":"2026-03-21T09:00:01Z","type":"response_item","payload":{"type":"message","role":"assistant","content":[{"type":"output_text","text":"world"}]}}
{"timestamp":"2026-03-21T09:00:02Z","type":"response_item","payload":{"type":"reasoning","summary":["think"],"content":null,"encrypted_content":"secret"}}
{"timestamp":"2026-03-21T09:00:03Z","type":"response_item","payload":{"type":"function_call","name":"exec_command","arguments":"{\"cmd\":\"cargo xtask test 01\"}","call_id":"call_1"}}
{"timestamp":"2026-03-21T09:00:04Z","type":"response_item","payload":{"type":"function_call_output","call_id":"call_1","output":"PASSED"}}"#;

    #[test]
    fn detects_supported_formats() {
        assert_eq!(
            detect_session_format_content(CLAUDE_SAMPLE),
            Some(SessionFormat::Claude)
        );
        assert_eq!(
            detect_session_format_content(CODEX_SAMPLE),
            Some(SessionFormat::Codex)
        );
    }

    #[test]
    fn parses_codex_events_into_shared_shape() {
        let events = parse_session_content(CODEX_SAMPLE, SessionFormat::Codex);
        assert_eq!(events.len(), 5);
        assert!(matches!(
            &events[0].kind,
            EventKind::User { text } if text == "hello"
        ));
        assert!(matches!(
            &events[1].kind,
            EventKind::Assistant { blocks }
                if matches!(&blocks[0], ContentBlock::Text(text) if text == "world")
        ));
        assert!(matches!(
            &events[2].kind,
            EventKind::Assistant { blocks }
                if matches!(&blocks[0], ContentBlock::Thinking(text) if text == "think")
        ));
        assert!(matches!(
            &events[3].kind,
            EventKind::Assistant { blocks }
                if matches!(&blocks[0], ContentBlock::ToolUse { name, input_json }
                    if name == "exec_command" && input_json.contains("cargo xtask test 01"))
        ));
        assert!(matches!(
            &events[4].kind,
            EventKind::Assistant { blocks }
                if matches!(&blocks[0], ContentBlock::ToolResult { content } if content == "PASSED")
        ));
    }

    #[test]
    fn session_files_prefers_local_transcript() {
        let run_dir = unique_temp_dir("local-session");
        let level_dir = run_dir.join("L01");
        fs::create_dir_all(&level_dir).expect("create level dir");
        let local = level_dir.join("session.jsonl");
        fs::write(&local, CODEX_SAMPLE).expect("write local session");

        let files = session_files(&run_dir);
        assert_eq!(files.len(), 1);
        assert_eq!(files[0].0, "L01");
        assert_eq!(files[0].1, local);

        fs::remove_dir_all(&run_dir).expect("cleanup temp dir");
    }

    #[test]
    fn session_files_falls_back_to_rollout_from_agent_output() {
        let _guard = test_lock().lock().expect("lock");
        let old_home = std::env::var("HOME").ok();

        let temp_home = unique_temp_dir("codex-home");
        std::env::set_var("HOME", &temp_home);

        let rollout_dir = temp_home
            .join(".codex")
            .join("sessions")
            .join("2026")
            .join("03")
            .join("21");
        fs::create_dir_all(&rollout_dir).expect("create rollout dir");
        let session_id = "019d0fb0-4d8b-7b01-b301-af3c83368c65";
        let rollout = rollout_dir.join(format!("rollout-2026-03-21T09-18-25-{session_id}.jsonl"));
        fs::write(&rollout, CODEX_SAMPLE).expect("write rollout");

        let run_dir = unique_temp_dir("fallback-session");
        let level_dir = run_dir.join("L01");
        fs::create_dir_all(&level_dir).expect("create level dir");
        fs::write(
            level_dir.join("agent-output.txt"),
            format!("model: gpt-5.4\nsession id: {session_id}\n"),
        )
        .expect("write agent output");

        let files = session_files(&run_dir);
        assert_eq!(files.len(), 1);
        assert_eq!(files[0].0, "L01");
        assert_eq!(files[0].1, rollout);

        fs::remove_dir_all(&run_dir).expect("cleanup run dir");
        fs::remove_dir_all(&temp_home).expect("cleanup temp home");
        if let Some(old_home) = old_home {
            std::env::set_var("HOME", old_home);
        } else {
            std::env::remove_var("HOME");
        }
    }

    #[test]
    fn extracts_exec_command_shell_input() {
        let command = shell_command("exec_command", r#"{"cmd":"cargo xtask test 01"}"#);
        assert_eq!(command.as_deref(), Some("cargo xtask test 01"));
    }
}
