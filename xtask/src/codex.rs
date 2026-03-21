use std::fs;
use std::path::{Path, PathBuf};

#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct Usage {
    pub input_tokens: u64,
    pub cached_input_tokens: u64,
    pub output_tokens: u64,
}

impl Usage {
    pub fn total_tokens(&self) -> u64 {
        self.input_tokens + self.output_tokens
    }

    pub fn uncached_input_tokens(&self) -> u64 {
        self.input_tokens.saturating_sub(self.cached_input_tokens)
    }

    pub fn add(&mut self, other: &Usage) {
        self.input_tokens += other.input_tokens;
        self.cached_input_tokens += other.cached_input_tokens;
        self.output_tokens += other.output_tokens;
    }
}

#[derive(Default, Clone, Copy, Debug, PartialEq)]
pub struct CostBreakdown {
    pub input: f64,
    pub cached_input: f64,
    pub output: f64,
}

impl CostBreakdown {
    pub fn total(&self) -> f64 {
        self.input + self.cached_input + self.output
    }

    pub fn add(&mut self, other: &CostBreakdown) {
        self.input += other.input;
        self.cached_input += other.cached_input;
        self.output += other.output;
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Pricing {
    input_per_token: f64,
    cached_input_per_token: f64,
    output_per_token: f64,
}

impl Pricing {
    pub fn cost_breakdown(&self, usage: &Usage) -> CostBreakdown {
        CostBreakdown {
            input: usage.uncached_input_tokens() as f64 * self.input_per_token,
            cached_input: usage.cached_input_tokens as f64 * self.cached_input_per_token,
            output: usage.output_tokens as f64 * self.output_per_token,
        }
    }
}

#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct OutputInfo {
    pub model: Option<String>,
    pub session_id: Option<String>,
    pub total_tokens: Option<u64>,
}

#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct SessionData {
    pub model: Option<String>,
    pub usage: Usage,
}

pub fn parse_output_info(output_path: &Path) -> OutputInfo {
    let Ok(content) = fs::read_to_string(output_path) else {
        return OutputInfo::default();
    };
    parse_output_info_content(&content)
}

pub fn parse_output_info_content(content: &str) -> OutputInfo {
    let stripped = strip_ansi(content);
    let lines: Vec<&str> = stripped.lines().map(str::trim).collect();

    let mut info = OutputInfo::default();
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

pub fn find_rollout(session_id: &str) -> Option<PathBuf> {
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

pub fn resolve_rollout_from_output_file(output_path: &Path) -> Option<PathBuf> {
    let info = parse_output_info(output_path);
    let session_id = info.session_id?;
    find_rollout(&session_id)
}

pub fn load_session(dir: &Path, session_id: Option<&str>) -> Option<SessionData> {
    for local_name in ["session.jsonl", "session.log"] {
        let local = dir.join(local_name);
        if !local.is_file() {
            continue;
        }
        if let Some(data) = parse_rollout(&local) {
            return Some(data);
        }
    }

    let session_id = session_id?;
    let rollout = find_rollout(session_id)?;
    parse_rollout(&rollout)
}

pub fn parse_rollout(path: &Path) -> Option<SessionData> {
    let content = fs::read_to_string(path).ok()?;
    let mut usage = Usage::default();
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

        let Some(payload) = obj.get("payload") else {
            continue;
        };
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

        update_usage_from_totals(&mut usage, totals);
        found_usage = true;
    }

    found_usage.then_some(SessionData { model, usage })
}

pub fn pricing(model: &str) -> Option<Pricing> {
    let model = model.trim().to_ascii_lowercase();
    resolve_pricing(&model)
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

fn update_usage_from_totals(
    usage: &mut Usage,
    totals: &serde_json::Map<String, serde_json::Value>,
) {
    let get = |key| totals.get(key).and_then(|v| v.as_u64()).unwrap_or(0);
    usage.input_tokens = usage.input_tokens.max(get("input_tokens"));
    usage.cached_input_tokens = usage.cached_input_tokens.max(get("cached_input_tokens"));
    usage.output_tokens = usage.output_tokens.max(get("output_tokens"));
}

const GPT_54: Pricing = Pricing {
    input_per_token: 2.50 / 1_000_000.0,
    cached_input_per_token: 0.25 / 1_000_000.0,
    output_per_token: 15.00 / 1_000_000.0,
};
const GPT_54_MINI: Pricing = Pricing {
    input_per_token: 0.75 / 1_000_000.0,
    cached_input_per_token: 0.075 / 1_000_000.0,
    output_per_token: 4.50 / 1_000_000.0,
};
const GPT_54_NANO: Pricing = Pricing {
    input_per_token: 0.20 / 1_000_000.0,
    cached_input_per_token: 0.02 / 1_000_000.0,
    output_per_token: 1.25 / 1_000_000.0,
};
const GPT_53_CODEX: Pricing = Pricing {
    input_per_token: 1.75 / 1_000_000.0,
    cached_input_per_token: 0.175 / 1_000_000.0,
    output_per_token: 14.00 / 1_000_000.0,
};
const GPT_52: Pricing = Pricing {
    input_per_token: 1.75 / 1_000_000.0,
    cached_input_per_token: 0.175 / 1_000_000.0,
    output_per_token: 14.00 / 1_000_000.0,
};
const GPT_51: Pricing = Pricing {
    input_per_token: 1.25 / 1_000_000.0,
    cached_input_per_token: 0.125 / 1_000_000.0,
    output_per_token: 10.00 / 1_000_000.0,
};
const GPT_5_MINI: Pricing = Pricing {
    input_per_token: 0.25 / 1_000_000.0,
    cached_input_per_token: 0.025 / 1_000_000.0,
    output_per_token: 2.00 / 1_000_000.0,
};
const CODEX_MINI_LATEST: Pricing = Pricing {
    input_per_token: 1.50 / 1_000_000.0,
    cached_input_per_token: 0.375 / 1_000_000.0,
    output_per_token: 6.00 / 1_000_000.0,
};

fn resolve_pricing(model: &str) -> Option<Pricing> {
    if model == "codex-mini-latest" || model.starts_with("codex-mini-latest-") {
        return Some(CODEX_MINI_LATEST);
    }
    if model == "gpt-5.4" || model.starts_with("gpt-5.4-") {
        return Some(resolve_gpt54_variant(model));
    }
    if model == "gpt-5.3-codex" || model.starts_with("gpt-5.3-codex-") {
        return Some(GPT_53_CODEX);
    }
    if matches_any(
        model,
        &["gpt-5.2", "gpt-5.2-codex"],
        &["gpt-5.2-", "gpt-5.2-codex-"],
    ) {
        return Some(GPT_52);
    }
    if model == "gpt-5-codex-mini" || model.starts_with("gpt-5-codex-mini-") {
        return None;
    }
    if matches_any(
        model,
        &["gpt-5.1-codex-mini", "gpt-5-mini"],
        &["gpt-5.1-codex-mini-", "gpt-5-mini-"],
    ) {
        return Some(GPT_5_MINI);
    }
    if matches_any(
        model,
        &[
            "gpt-5.1-codex-max",
            "gpt-5.1-codex",
            "gpt-5-codex",
            "gpt-5.1",
            "gpt-5",
        ],
        &[
            "gpt-5.1-codex-max-",
            "gpt-5.1-codex-",
            "gpt-5-codex-",
            "gpt-5.1-",
            "gpt-5-",
        ],
    ) {
        return Some(GPT_51);
    }
    None
}

fn resolve_gpt54_variant(model: &str) -> Pricing {
    if model.starts_with("gpt-5.4-mini") {
        GPT_54_MINI
    } else if model.starts_with("gpt-5.4-nano") {
        GPT_54_NANO
    } else {
        GPT_54
    }
}

fn matches_any(model: &str, exact: &[&str], prefixes: &[&str]) -> bool {
    exact.contains(&model) || prefixes.iter().any(|prefix| model.starts_with(prefix))
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_output_info_extracts_model_and_session_id() {
        let content = "\
\x1b[1mmodel:\x1b[0m gpt-5.4\n\
\x1b[1msession id:\x1b[0m 019d0fb0-4d8b-7b01-b301-af3c83368c65\n\
tokens used\n\
  2,020,538\n";
        let info = parse_output_info_content(content);
        assert_eq!(info.model.as_deref(), Some("gpt-5.4"));
        assert_eq!(
            info.session_id.as_deref(),
            Some("019d0fb0-4d8b-7b01-b301-af3c83368c65")
        );
        assert_eq!(info.total_tokens, Some(2_020_538));
    }
}
