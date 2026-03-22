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
    pub strategy: Option<String>,
    pub lang: Option<String>,
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
// Language support
// ---------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub enum Lang {
    Rust,
    Java,
    Go,
    TypeScript,
    Scala,
}

impl Lang {
    pub fn from_str(s: &str) -> std::result::Result<Self, String> {
        match s.to_lowercase().as_str() {
            "rust" | "rs" => Ok(Self::Rust),
            "java" => Ok(Self::Java),
            "go" | "golang" => Ok(Self::Go),
            "typescript" | "ts" => Ok(Self::TypeScript),
            "scala" => Ok(Self::Scala),
            _ => Err(format!(
                "unknown language: {s} (expected: rust, java, go, ts, scala)"
            )),
        }
    }

    pub fn dir_name(&self) -> &str {
        match self {
            Self::Rust => "rust",
            Self::Java => "java",
            Self::Go => "go",
            Self::TypeScript => "ts",
            Self::Scala => "scala",
        }
    }

    pub fn display_name(&self) -> &str {
        match self {
            Self::Rust => "Rust",
            Self::Java => "Java",
            Self::Go => "Go",
            Self::TypeScript => "TypeScript",
            Self::Scala => "Scala",
        }
    }

    pub fn container_image(&self) -> &str {
        match self {
            Self::Rust | Self::Go => "ming",
            Self::Java | Self::Scala => "ming-jvm",
            Self::TypeScript => "ming-node",
        }
    }
}

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

    #[error("command `{cmd}` failed with exit code {exit_code}")]
    CommandFailed { cmd: String, exit_code: i32 },

    #[error("{name} not found in PATH — ensure ~/.local/bin and ~/.cargo/bin are in PATH")]
    BinaryNotFound { name: String },

    #[error("worktree directory already exists: {path}")]
    WorktreeDirExists { path: PathBuf },

    #[error("git branch already exists: {branch} (remove with `git branch -D {branch}` and `git worktree prune`)")]
    BranchExists { branch: String },

    #[error("lock conflict: {path} held by PID {pid}")]
    LockConflict { path: PathBuf, pid: u32 },

    #[error("test binary not found after compilation")]
    TestBinaryNotFound,
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
        format!("{secs}s")
    }
}

pub fn fmt_size(bytes: u64) -> String {
    if bytes >= 1_048_576 {
        format!("{:.1}MB", bytes as f64 / 1_048_576.0)
    } else if bytes >= 1024 {
        format!("{}KB", bytes / 1024)
    } else {
        format!("{bytes}B")
    }
}

#[allow(dead_code)]
pub fn fmt_tokens(n: u64) -> String {
    if n >= 1_000_000 {
        format!("{:.1}M", n as f64 / 1_000_000.0)
    } else if n >= 1_000 {
        format!("{}K", n / 1_000)
    } else {
        format!("{n}")
    }
}

/// Comma-separate a number: 1234567 → "1,234,567"
pub fn fmt_comma(n: u64) -> String {
    let s = n.to_string();
    let mut result = String::with_capacity(s.len() + s.len() / 3);
    for (i, ch) in s.chars().enumerate() {
        if i > 0 && (s.len() - i).is_multiple_of(3) {
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
    // Strip fractional seconds (e.g., "04.423" → "04")
    let sec_str = time_parts[2].split('.').next().unwrap_or(time_parts[2]);
    let sec: i64 = sec_str.parse().ok()?;

    // Days from epoch using a simplified calculation
    // (accurate for dates 2000-2099, which is all we need)
    let mut days: i64 = 0;
    for y in 1970..year {
        days += if is_leap(y) { 366 } else { 365 };
    }
    let month_days = [
        31,
        28 + if is_leap(year) { 1 } else { 0 },
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
    for md in month_days.iter().take((month - 1) as usize) {
        days += *md as i64;
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
        let Ok(entry) = entry else { continue };
        let meta_path = entry.path().join("meta.json");
        if !meta_path.is_file() {
            continue;
        }
        let Ok(content) = fs::read_to_string(&meta_path) else {
            continue;
        };
        let Ok(meta) = serde_json::from_str::<MetaJson>(&content) else {
            continue;
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

// ---------------------------------------------------------------------------
// TestEntry — deserialized from bench/tests.json
// ---------------------------------------------------------------------------

#[derive(Deserialize, Debug, Clone)]
pub struct TestEntry {
    pub name: String,
    pub level: u32,
    pub fixture: String,
    pub kind: String,
    #[serde(default)]
    pub expected: Option<String>,
    #[serde(default)]
    pub expected_output: Option<String>,
    #[serde(default)]
    pub deprecated_after: Option<u32>,
}

pub fn load_tests_json() -> Result<Vec<TestEntry>> {
    let path = project_dir().join("bench/tests.json");
    let json = fs::read_to_string(&path).map_err(|e| Error::CommandFailed {
        cmd: format!("read {}", path.display()),
        exit_code: e.raw_os_error().unwrap_or(1),
    })?;
    serde_json::from_str(&json).map_err(|e| Error::CommandFailed {
        cmd: format!("parse tests.json: {e}"),
        exit_code: 1,
    })
}

pub fn fixtures_dir() -> PathBuf {
    project_dir().join("bench/fixtures")
}

// ---------------------------------------------------------------------------
// Level constants
// ---------------------------------------------------------------------------

pub const LEVELS: [&str; 27] = [
    "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16",
    "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27",
];

// ---------------------------------------------------------------------------
// Turn limits
// ---------------------------------------------------------------------------

/// Default turn limit for a given level number.
///
/// Tiers based on difficulty (v2 reordered):
///   L01-L03 (foundation)                       → 45 turns
///   L04-L06 (error quality, strings, mutable)  → 30 turns
///   L07-L09 (TCO, set!, variadic)              → 45 turns
///   L10-L12 (call/cc, macros, integration)     → 90 turns
///   L13     (builtin grab bag)                 → 45 turns
///   L14     (string immutability req-change)   → 45 turns
///   L15     (equality, letrec, case, vectors)  → 60 turns
///   L16-L18 (dynamic-wind, guard, values)      → 60 turns
///   L19-L20 (exact arith, records)             → 75 turns
///   L21-L23 (pair mutation, syntax-case, integration) → 90 turns
///   L24-L27 (tech-debt: case-lambda, do, let-values, parameterize) → 60 turns
pub fn turns_for_level(level_num: u32, max_turns: Option<u32>) -> u32 {
    if let Some(t) = max_turns {
        return t;
    }
    match level_num {
        1..=3 => 45,
        4..=6 => 30,   // error quality, strings, mutable strings
        7..=9 => 45,   // TCO, set!, variadic
        10..=12 => 90, // call/cc, macros, integration
        13..=14 => 45, // builtins, string immutability (req change)
        15 => 60,      // equality, letrec, case, vectors
        16..=18 => 60, // dynamic-wind, guard, values
        19..=20 => 75, // exact arith, records
        21..=23 => 90, // pair mutation, syntax-case, final integration
        24..=27 => 60, // tech-debt: case-lambda, do, let-values, parameterize
        _ => 75,
    }
}

// ---------------------------------------------------------------------------
// Project directory helpers
// ---------------------------------------------------------------------------

/// Locate project root. Walks up from cwd looking for Cargo.toml with [workspace].
pub fn project_dir() -> PathBuf {
    if let Ok(dir) = std::env::var("PROJECT_DIR") {
        return PathBuf::from(dir);
    }
    let mut dir = std::env::current_dir().expect("cannot read current directory");
    loop {
        let cargo = dir.join("Cargo.toml");
        let is_workspace = cargo.is_file()
            && fs::read_to_string(&cargo)
                .map(|c| c.contains("[workspace]"))
                .unwrap_or(false);
        if is_workspace {
            return dir;
        }
        if !dir.pop() {
            // Fall back to cwd
            return std::env::current_dir().expect("cannot read current directory");
        }
    }
}

// ---------------------------------------------------------------------------
// Command execution helpers
// ---------------------------------------------------------------------------

/// Run a command, inheriting stdout/stderr. Returns exit code.
pub fn run_cmd(cmd: &str, args: &[&str], cwd: &Path) -> Result<i32> {
    let status = std::process::Command::new(cmd)
        .args(args)
        .current_dir(cwd)
        .status()
        .map_err(|e| Error::Io {
            path: PathBuf::from(cmd),
            source: e,
        })?;
    Ok(status.code().unwrap_or(1))
}

/// Run a command and capture stdout. Returns (exit_code, stdout).
pub fn run_cmd_capture(cmd: &str, args: &[&str], cwd: &Path) -> Result<(i32, String)> {
    let output = std::process::Command::new(cmd)
        .args(args)
        .current_dir(cwd)
        .output()
        .map_err(|e| Error::Io {
            path: PathBuf::from(cmd),
            source: e,
        })?;
    let code = output.status.code().unwrap_or(1);
    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    Ok((code, stdout))
}

/// Run a command, capture combined stdout+stderr. Returns (exit_code, output).
pub fn run_cmd_capture_all(cmd: &str, args: &[&str], cwd: &Path) -> Result<(i32, String)> {
    let output = std::process::Command::new(cmd)
        .args(args)
        .current_dir(cwd)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .output()
        .map_err(|e| Error::Io {
            path: PathBuf::from(cmd),
            source: e,
        })?;
    let code = output.status.code().unwrap_or(1);
    let mut combined = String::from_utf8_lossy(&output.stdout).into_owned();
    combined.push_str(&String::from_utf8_lossy(&output.stderr));
    Ok((code, combined))
}

/// Write a serde_json::Value to a file as pretty JSON.
pub fn write_meta(path: &Path, meta: &serde_json::Value) -> Result<()> {
    let content = serde_json::to_string_pretty(meta).expect("json serialization failed");
    fs::write(path, content).map_err(|e| Error::io(path, e))
}

/// Generate an ISO-8601 timestamp string for "now" in UTC.
pub fn iso_now() -> String {
    // Format from epoch — all our timestamps are UTC
    let epoch = now_epoch();
    let secs_in_day = epoch % 86400;
    let days = epoch / 86400;

    // Reverse the epoch→date calculation
    let mut year = 1970i64;
    let mut remaining_days = days;
    loop {
        let year_days = if is_leap(year) { 366 } else { 365 };
        if remaining_days < year_days {
            break;
        }
        remaining_days -= year_days;
        year += 1;
    }
    let month_days = [
        31,
        28 + if is_leap(year) { 1 } else { 0 },
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
    let mut month = 1;
    for md in &month_days {
        if remaining_days < *md {
            break;
        }
        remaining_days -= md;
        month += 1;
    }
    let day = remaining_days + 1;

    format!(
        "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}+00:00",
        year,
        month,
        day,
        secs_in_day / 3600,
        (secs_in_day % 3600) / 60,
        secs_in_day % 60,
    )
}

/// Generate a timestamp string like "20260318T135418"
pub fn compact_timestamp() -> String {
    let iso = iso_now();
    // "2026-03-18T13:54:18+00:00" → "20260318T135418"
    iso.replace('-', "")
        .split('+')
        .next()
        .expect("iso timestamp has no +")
        .replace(':', "")
}

/// Check if a command exists on PATH.
pub fn command_exists(cmd: &str) -> bool {
    std::process::Command::new("which")
        .arg(cmd)
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

/// Generate a UUID v4 (simple random-based, no external crate).
pub fn uuid_v4() -> String {
    use std::io::Read;
    let mut bytes = [0u8; 16];
    if let Ok(mut f) = fs::File::open("/dev/urandom") {
        let _ = f.read_exact(&mut bytes);
    }
    // Set version (4) and variant (10xx)
    bytes[6] = (bytes[6] & 0x0f) | 0x40;
    bytes[8] = (bytes[8] & 0x3f) | 0x80;
    format!(
        "{:02x}{:02x}{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}",
        bytes[0], bytes[1], bytes[2], bytes[3],
        bytes[4], bytes[5],
        bytes[6], bytes[7],
        bytes[8], bytes[9],
        bytes[10], bytes[11], bytes[12], bytes[13], bytes[14], bytes[15],
    )
}
