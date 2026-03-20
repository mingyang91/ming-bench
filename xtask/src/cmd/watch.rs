use crate::model::{
    colored, discover_runs, elapsed_secs, fmt_duration, fmt_size, now_epoch, parse_iso_epoch,
    project_results_dir, score_display, Color, Error, MetaJson, Result,
};
use std::fs;
use std::path::{Path, PathBuf};
use std::thread;
use std::time::Duration;

const REFRESH_SECS: u64 = 15;
const STALE_THRESHOLD: i64 = 6 * 3600; // 6 hours

#[derive(Debug, PartialEq)]
enum Status {
    Running,
    Done,
    Dead,
}

impl Status {
    fn label(&self) -> &'static str {
        match self {
            Status::Running => "RUNNING",
            Status::Done => "DONE",
            Status::Dead => "DEAD",
        }
    }

    fn color(&self) -> &'static str {
        match self {
            Status::Running => Color::CYAN,
            Status::Done => Color::GREEN,
            Status::Dead => Color::RED,
        }
    }
}

pub fn run(once: bool, ts: Option<String>, all: bool) -> Result<()> {
    if once {
        render(ts.as_deref(), all)?;
    } else {
        loop {
            // Clear screen
            print!("\x1b[2J\x1b[H");
            let now = chrono_hms();
            println!(
                "{}Agent Monitor{}  {}  (refresh {}s, Ctrl-C to quit)\n",
                Color::BOLD,
                Color::RESET,
                now,
                REFRESH_SECS,
            );
            render(ts.as_deref(), all)?;
            thread::sleep(Duration::from_secs(REFRESH_SECS));
        }
    }
    Ok(())
}

fn chrono_hms() -> String {
    // Quick wall-clock HH:MM:SS from epoch
    let epoch = now_epoch();
    let secs_today = epoch % 86400;
    format!(
        "{:02}:{:02}:{:02}",
        secs_today / 3600,
        (secs_today % 3600) / 60,
        secs_today % 60
    )
}

fn render(ts_filter: Option<&str>, show_all: bool) -> Result<()> {
    let results_dir = project_results_dir();
    let runs = match discover_runs(&results_dir) {
        Ok(r) => r,
        Err(Error::NoResultsDir { .. }) => {
            println!("  No results directory found.");
            return Ok(());
        }
        Err(e) => return Err(e),
    };

    if runs.is_empty() {
        println!("  No runs found.");
        return Ok(());
    }

    print_header();
    let now = now_epoch();

    for (run_dir, meta) in &runs {
        render_run(run_dir, meta, ts_filter, show_all, now);
    }

    Ok(())
}

fn print_header() {
    println!(
        "{}{:<28}  {:<9}  {:<8}  {:<10}  {:<8}  {:<8}  DETAIL{}",
        Color::BOLD, "NAME", "STATUS", "LEVEL", "IDLE", "ELAPSED", "OUTPUT", Color::RESET,
    );
    println!("{}", "─".repeat(100));
}

fn render_run(
    run_dir: &Path, meta: &MetaJson, ts_filter: Option<&str>, show_all: bool, now: i64,
) {
    let dirname = run_dir
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default();

    if let Some(ts) = ts_filter {
        if !dirname.contains(ts) {
            return;
        }
    }

    let name = meta.name.as_deref().unwrap_or(&dirname);
    let status = detect_status(run_dir, meta);

    if should_skip_run(&status, show_all, ts_filter.is_some(), meta, now) {
        return;
    }

    let mode = meta.mode.as_deref().unwrap_or("?");
    let agent = meta.agent.as_deref().unwrap_or("?");
    let level_str = if mode == "levels" { level_progress(run_dir) } else { "---".to_string() };
    let (idle_secs, idle_str, detail) = compute_idle_and_detail(run_dir, meta, &status, name, agent, mode, now);
    let elapsed_str = elapsed_secs(meta).map(fmt_duration).unwrap_or_else(|| "---".to_string());
    let out_str = find_latest_output(run_dir, mode)
        .and_then(|f| fs::metadata(&f).ok())
        .map(|m| fmt_size(m.len()))
        .unwrap_or_else(|| "---".to_string());
    let idle_color = idle_color(&status, idle_secs);

    println!(
        "{:<28}  {}  {:<8}  {}  {:<8}  {:<8}  {}{}{}",
        name,
        colored(&format!("{:<9}", status.label()), status.color()),
        level_str,
        colored(&format!("{idle_str:<10}"), idle_color),
        elapsed_str,
        out_str,
        Color::DIM,
        detail,
        Color::RESET,
    );
}

fn should_skip_run(
    status: &Status, show_all: bool, has_ts: bool, meta: &MetaJson, now: i64,
) -> bool {
    if *status != Status::Done || show_all || has_ts {
        return false;
    }
    meta.start_time.as_deref()
        .and_then(parse_iso_epoch)
        .is_some_and(|start| now - start > STALE_THRESHOLD)
}

fn compute_idle_and_detail(
    run_dir: &Path, meta: &MetaJson, status: &Status,
    name: &str, agent: &str, mode: &str, now: i64,
) -> (u64, String, String) {
    let output_file = find_latest_output(run_dir, mode);
    let (mut idle_secs, mut idle_str) = match &output_file {
        Some(f) => {
            let secs = (now - file_mtime(f)).max(0) as u64;
            (secs, fmt_duration(secs))
        }
        None => (0, "---".to_string()),
    };

    let mut detail = String::new();
    if agent == "claude" {
        let (d, live_idle) = claude_detail(run_dir, meta, status, name, now);
        detail = d;
        if let Some(li) = live_idle {
            idle_secs = li;
            idle_str = fmt_duration(li);
        }
    }
    if *status == Status::Done {
        let score = score_display(meta);
        if score != "?" && score != "null" {
            detail = format!("score: {score}");
        }
    }

    (idle_secs, idle_str, detail)
}

fn idle_color(status: &Status, idle_secs: u64) -> &'static str {
    if *status != Status::Running {
        return Color::RESET;
    }
    if idle_secs > 900 {
        Color::RED
    } else if idle_secs > 300 {
        Color::YELLOW
    } else {
        Color::RESET
    }
}

// ---------------------------------------------------------------------------
// Status detection
// ---------------------------------------------------------------------------

fn detect_status(run_dir: &Path, meta: &MetaJson) -> Status {
    if meta.end_time.is_some() {
        return Status::Done;
    }

    let lockfile = run_dir.join(".run.lock");
    if !lockfile.is_file() {
        return Status::Done;
    }

    if lock_pid_alive(&lockfile) {
        Status::Running
    } else {
        Status::Dead
    }
}

fn lock_pid_alive(lockfile: &Path) -> bool {
    let Ok(content) = fs::read_to_string(lockfile) else { return false };
    let Ok(pid) = content.trim().parse::<u32>() else { return false };
    Path::new(&format!("/proc/{pid}")).exists()
}

// ---------------------------------------------------------------------------
// Level progress
// ---------------------------------------------------------------------------

fn level_progress(run_dir: &Path) -> String {
    let mut passed = 0u32;
    let mut last_level = String::new();
    let mut total = 0u32;

    let Ok(entries) = fs::read_dir(run_dir) else {
        return "---".to_string();
    };

    let mut level_dirs: Vec<PathBuf> = entries
        .flatten()
        .filter(|e| e.file_name().to_string_lossy().starts_with('L') && e.path().is_dir())
        .map(|e| e.path())
        .collect();
    level_dirs.sort();

    for ldir in &level_dirs {
        total += 1;
        let lname = ldir
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_default();

        let status_file = ldir.join("status.txt");
        if !status_file.is_file() {
            last_level = format!("{lname}…");
            continue;
        }
        let has_passed = fs::read_to_string(&status_file)
            .map(|c| c.to_uppercase().contains("PASSED"))
            .unwrap_or(false);
        if has_passed {
            passed += 1;
        }
        last_level = lname;
    }

    if total > 0 {
        format!("{last_level} ({passed}/{})", crate::model::LEVELS.len())
    } else {
        "---".to_string()
    }
}

// ---------------------------------------------------------------------------
// Idle time helpers
// ---------------------------------------------------------------------------

fn find_latest_output(run_dir: &Path, mode: &str) -> Option<PathBuf> {
    let mut candidates = vec![run_dir.join("agent-output.txt")];

    if mode == "levels" {
        collect_level_outputs(run_dir, &mut candidates);
    }

    candidates
        .into_iter()
        .filter(|p| p.is_file())
        .max_by_key(|p| file_mtime(p))
}

fn collect_level_outputs(run_dir: &Path, candidates: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(run_dir) else { return };
    for entry in entries.flatten() {
        if entry.file_name().to_string_lossy().starts_with('L') && entry.path().is_dir() {
            candidates.push(entry.path().join("agent-output.txt"));
        }
    }
}

fn file_mtime(path: &Path) -> i64 {
    fs::metadata(path)
        .and_then(|m| m.modified())
        .map(|t| {
            t.duration_since(std::time::UNIX_EPOCH)
                .expect("mtime before epoch")
                .as_secs() as i64
        })
        .unwrap_or(0)
}

// ---------------------------------------------------------------------------
// Claude detail: last tool_use + cumulative output tokens
// ---------------------------------------------------------------------------

fn claude_detail(
    run_dir: &Path,
    meta: &MetaJson,
    status: &Status,
    name: &str,
    now: i64,
) -> (String, Option<u64>) {
    let mut jsonl_path = run_dir.join("session.jsonl");
    let mut live_idle = None;

    if *status == Status::Running && !jsonl_path.is_file() {
        if let Some(live) = find_live_session(name, meta) {
            live_idle = live_idle_from_mtime(&live, now);
            jsonl_path = live;
        }
    }

    let content = match fs::read_to_string(&jsonl_path) {
        Ok(c) if !c.is_empty() => c,
        _ => return (String::new(), live_idle),
    };

    let (last_tool, total_out) = scan_session_detail(&content);

    let detail = format_detail(&last_tool, total_out);
    (detail, live_idle)
}

fn live_idle_from_mtime(path: &Path, now: i64) -> Option<u64> {
    let mt = file_mtime(path);
    if mt > 0 { Some((now - mt).max(0) as u64) } else { None }
}

fn scan_session_detail(content: &str) -> (String, u64) {
    let mut last_tool = String::new();
    let mut total_out: u64 = 0;

    for line in content.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let Ok(obj) = serde_json::from_str::<serde_json::Value>(line) else { continue };
        if obj.get("type").and_then(|t| t.as_str()) != Some("assistant") {
            continue;
        }
        let Some(msg) = obj.get("message").and_then(|m| m.as_object()) else { continue };

        total_out += msg
            .get("usage")
            .and_then(|u| u.get("output_tokens"))
            .and_then(|v| v.as_u64())
            .unwrap_or(0);

        if let Some(blocks) = msg.get("content").and_then(|c| c.as_array()) {
            last_tool = extract_last_tool_name(blocks, &last_tool);
        }
    }

    (last_tool, total_out)
}

fn extract_last_tool_name(blocks: &[serde_json::Value], current: &str) -> String {
    let mut last = current.to_string();
    for block in blocks {
        let is_tool = block.get("type").and_then(|t| t.as_str()) == Some("tool_use");
        if !is_tool {
            continue;
        }
        if let Some(name) = block.get("name").and_then(|n| n.as_str()) {
            last = name.to_string();
        }
    }
    last
}

fn format_detail(last_tool: &str, total_out: u64) -> String {
    if last_tool.is_empty() {
        return String::new();
    }
    let mut detail = format!("last: {last_tool}");
    if total_out > 0 {
        detail = format!("{detail}  tok: {total_out}");
    }
    detail
}

/// Try to locate a live Claude session.jsonl for a running agent.
///
/// Strategy 1: Derive Claude's project directory from the workspace path.
/// Strategy 2: Search by session_id in ~/.claude/projects/
fn find_live_session(name: &str, meta: &MetaJson) -> Option<PathBuf> {
    let home = std::env::var("HOME").ok()?;

    // Strategy 1: Derive project dir from workspace path
    if let Some(found) = find_live_from_workspace(name, &home) {
        return Some(found);
    }

    // Strategy 2: Search by session_id
    let sid = meta.session_id.as_deref()?;
    let projects_dir = PathBuf::from(&home).join(".claude/projects");
    find_jsonl_by_session_id(&projects_dir, sid)
}

fn find_live_from_workspace(name: &str, home: &str) -> Option<PathBuf> {
    let ws = project_workspace_dir(name)?;
    let real = fs::canonicalize(&ws).ok()?;
    let proj_dir_name = derive_claude_project_name(&real);
    let proj_dir = PathBuf::from(home).join(".claude/projects").join(&proj_dir_name);
    if !proj_dir.is_dir() {
        return None;
    }
    newest_jsonl(&proj_dir)
}

fn project_workspace_dir(name: &str) -> Option<PathBuf> {
    // The workspace is at <project_root>/../workspace/<name>
    // project_root is cwd (or where results/ lives)
    let cwd = std::env::current_dir().ok()?;
    let ws = cwd.parent()?.join("workspace").join(name);
    if ws.is_dir() {
        Some(ws)
    } else {
        None
    }
}

/// Derive Claude's project directory name from a real path.
/// Claude uses: strip leading /, replace / and . with -, prepend -
fn derive_claude_project_name(path: &Path) -> String {
    let s = path.to_string_lossy();
    let stripped = s.strip_prefix('/').unwrap_or(&s);
    let replaced = stripped.replace(['/', '.'], "-");
    format!("-{replaced}")
}

fn newest_jsonl(dir: &Path) -> Option<PathBuf> {
    let entries = fs::read_dir(dir).ok()?;
    let mut best: Option<(PathBuf, i64)> = None;

    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("jsonl") {
            continue;
        }
        let mt = file_mtime(&path);
        if best.as_ref().is_none_or(|(_, prev)| mt > *prev) {
            best = Some((path, mt));
        }
    }

    best.map(|(p, _)| p)
}

fn find_jsonl_by_session_id(projects_dir: &Path, session_id: &str) -> Option<PathBuf> {
    let target = format!("{session_id}.jsonl");
    let entries = fs::read_dir(projects_dir).ok()?;

    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }
        let candidate = path.join(&target);
        if candidate.is_file() {
            return Some(candidate);
        }
    }

    None
}
