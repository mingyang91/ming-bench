use serde::Deserialize;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

// ---------------------------------------------------------------------------
// MetaJson — deserialized from results/*/meta.json
// ---------------------------------------------------------------------------

#[derive(Deserialize, Default, Debug)]
#[allow(dead_code)]
pub struct MetaJson {
    pub name: Option<String>,
    pub base: Option<String>,
    pub agent: Option<String>,
    pub mode: Option<String>,
    pub score: Option<serde_json::Value>,
    pub start_time: Option<String>,
    pub end_time: Option<String>,
    pub session_id: Option<String>,
    pub timestamp: Option<String>,
    pub exit_code: Option<i32>,
}

// ---------------------------------------------------------------------------
// TokenUsage — accumulated from session.jsonl
// ---------------------------------------------------------------------------

#[derive(Default, Debug, Clone)]
pub struct TokenUsage {
    pub input_tokens: u64,
    pub output_tokens: u64,
    pub cache_creation_input_tokens: u64,
    pub cache_read_input_tokens: u64,
}

impl TokenUsage {
    pub fn total(&self) -> u64 {
        self.input_tokens
            + self.output_tokens
            + self.cache_creation_input_tokens
            + self.cache_read_input_tokens
    }

    pub fn cost(&self) -> f64 {
        self.input_tokens as f64 * PRICE_INPUT
            + self.output_tokens as f64 * PRICE_OUTPUT
            + self.cache_creation_input_tokens as f64 * PRICE_CACHE_WRITE
            + self.cache_read_input_tokens as f64 * PRICE_CACHE_READ
    }

    pub fn add(&mut self, other: &TokenUsage) {
        self.input_tokens += other.input_tokens;
        self.output_tokens += other.output_tokens;
        self.cache_creation_input_tokens += other.cache_creation_input_tokens;
        self.cache_read_input_tokens += other.cache_read_input_tokens;
    }
}

// ---------------------------------------------------------------------------
// Opus pricing (per token)
// ---------------------------------------------------------------------------

pub const PRICE_INPUT: f64 = 15.0 / 1_000_000.0;
pub const PRICE_OUTPUT: f64 = 75.0 / 1_000_000.0;
pub const PRICE_CACHE_WRITE: f64 = 18.75 / 1_000_000.0;
pub const PRICE_CACHE_READ: f64 = 1.50 / 1_000_000.0;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("I/O error on {path}: {source}")]
    Io {
        path: PathBuf,
        source: std::io::Error,
    },

    #[error("no results directory found at {path}")]
    NoResultsDir { path: PathBuf },

    #[error("no runs found matching criteria")]
    NoRuns,

    #[error("run directory not found: {path}")]
    RunNotFound { path: PathBuf },
}

impl Error {
    pub fn io(path: impl Into<PathBuf>, source: std::io::Error) -> Self {
        Self::Io {
            path: path.into(),
            source,
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

// ---------------------------------------------------------------------------
// Formatters
// ---------------------------------------------------------------------------

pub fn fmt_duration(secs: u64) -> String {
    if secs >= 3600 {
        format!("{}h{:02}m", secs / 3600, (secs % 3600) / 60)
    } else if secs >= 60 {
        format!("{}m{:02}s", secs / 60, secs % 60)
    } else {
        format!("{}s", secs)
    }
}

pub fn fmt_size(bytes: u64) -> String {
    if bytes >= 1_048_576 {
        format!("{:.1}MB", bytes as f64 / 1_048_576.0)
    } else if bytes >= 1024 {
        format!("{}KB", bytes / 1024)
    } else {
        format!("{}B", bytes)
    }
}

#[allow(dead_code)]
pub fn fmt_tokens(n: u64) -> String {
    if n >= 1_000_000 {
        format!("{:.1}M", n as f64 / 1_000_000.0)
    } else if n >= 1_000 {
        format!("{}K", n / 1_000)
    } else {
        format!("{}", n)
    }
}

/// Comma-separate a number: 1234567 → "1,234,567"
pub fn fmt_comma(n: u64) -> String {
    let s = n.to_string();
    let mut result = String::with_capacity(s.len() + s.len() / 3);
    for (i, ch) in s.chars().enumerate() {
        if i > 0 && (s.len() - i) % 3 == 0 {
            result.push(',');
        }
        result.push(ch);
    }
    result
}

// ---------------------------------------------------------------------------
// ISO-8601 timestamp → unix epoch (seconds)
//
// Handles: "2026-03-18T16:40:39+00:00" or "2026-03-18T16:40:39Z"
// All timestamps in this project are UTC.
// ---------------------------------------------------------------------------

pub fn parse_iso_epoch(s: &str) -> Option<i64> {
    // Strip timezone suffix
    let dt = s.split('+').next()?.trim_end_matches('Z');
    let (date, time) = dt.split_once('T')?;
    let parts: Vec<&str> = date.split('-').collect();
    if parts.len() != 3 {
        return None;
    }
    let year: i64 = parts[0].parse().ok()?;
    let month: i64 = parts[1].parse().ok()?;
    let day: i64 = parts[2].parse().ok()?;

    let time_parts: Vec<&str> = time.split(':').collect();
    if time_parts.len() != 3 {
        return None;
    }
    let hour: i64 = time_parts[0].parse().ok()?;
    let min: i64 = time_parts[1].parse().ok()?;
    let sec: i64 = time_parts[2].parse().ok()?;

    // Days from epoch using a simplified calculation
    // (accurate for dates 2000-2099, which is all we need)
    let mut days: i64 = 0;
    for y in 1970..year {
        days += if is_leap(y) { 366 } else { 365 };
    }
    let month_days = [31, 28 + if is_leap(year) { 1 } else { 0 }, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    for m in 0..(month - 1) as usize {
        days += month_days[m] as i64;
    }
    days += day - 1;

    Some(days * 86400 + hour * 3600 + min * 60 + sec)
}

fn is_leap(y: i64) -> bool {
    y % 4 == 0 && (y % 100 != 0 || y % 400 == 0)
}

pub fn now_epoch() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system clock before unix epoch")
        .as_secs() as i64
}

// ---------------------------------------------------------------------------
// Run discovery
// ---------------------------------------------------------------------------

pub fn project_results_dir() -> PathBuf {
    // Try RESULTS_DIR env, else cwd/results
    if let Ok(dir) = std::env::var("RESULTS_DIR") {
        return PathBuf::from(dir);
    }
    std::env::current_dir()
        .expect("cannot read current directory")
        .join("results")
}

pub fn discover_runs(results_dir: &Path) -> Result<Vec<(PathBuf, MetaJson)>> {
    if !results_dir.is_dir() {
        return Err(Error::NoResultsDir {
            path: results_dir.to_path_buf(),
        });
    }

    let mut runs = Vec::new();
    let entries = fs::read_dir(results_dir).map_err(|e| Error::io(results_dir, e))?;

    for entry in entries {
        let entry = match entry {
            Ok(e) => e,
            Err(_) => continue,
        };
        let meta_path = entry.path().join("meta.json");
        if !meta_path.is_file() {
            continue;
        }
        let content = match fs::read_to_string(&meta_path) {
            Ok(c) => c,
            Err(_) => continue,
        };
        let meta: MetaJson = match serde_json::from_str(&content) {
            Ok(m) => m,
            Err(_) => continue,
        };
        runs.push((entry.path(), meta));
    }

    runs.sort_by(|a, b| {
        let name_a = a.0.file_name().map(|n| n.to_string_lossy().into_owned());
        let name_b = b.0.file_name().map(|n| n.to_string_lossy().into_owned());
        name_a.cmp(&name_b)
    });

    Ok(runs)
}

// ---------------------------------------------------------------------------
// ANSI color helpers
// ---------------------------------------------------------------------------

pub struct Color;

impl Color {
    pub const RED: &str = "\x1b[0;31m";
    pub const YELLOW: &str = "\x1b[0;33m";
    pub const GREEN: &str = "\x1b[0;32m";
    pub const CYAN: &str = "\x1b[0;36m";
    pub const DIM: &str = "\x1b[2m";
    pub const BOLD: &str = "\x1b[1m";
    pub const RESET: &str = "\x1b[0m";
}

/// Wrap text in ANSI color codes.
pub fn colored(text: &str, color: &str) -> String {
    format!("{}{}{}", color, text, Color::RESET)
}

// ---------------------------------------------------------------------------
// Display helpers
// ---------------------------------------------------------------------------

/// Score display value from MetaJson
pub fn score_display(meta: &MetaJson) -> String {
    match &meta.score {
        Some(serde_json::Value::String(s)) => s.clone(),
        Some(serde_json::Value::Number(n)) => n.to_string(),
        _ => "?".to_string(),
    }
}

/// Duration in seconds between start_time and end_time, or start_time and now
pub fn elapsed_secs(meta: &MetaJson) -> Option<u64> {
    let start = parse_iso_epoch(meta.start_time.as_deref()?)?;
    let end = meta
        .end_time
        .as_deref()
        .and_then(parse_iso_epoch)
        .unwrap_or_else(now_epoch);
    Some((end - start).max(0) as u64)
}
