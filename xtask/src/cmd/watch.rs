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

    // Header
    println!(
        "{}{:<28}  {:<9}  {:<8}  {:<10}  {:<8}  {:<8}  DETAIL{}",
        Color::BOLD,
        "NAME",
        "STATUS",
        "LEVEL",
        "IDLE",
        "ELAPSED",
        "OUTPUT",
        Color::RESET,
    );
    println!("{}", "─".repeat(100));

    let now = now_epoch();

    for (run_dir, meta) in &runs {
        let dirname = run_dir
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_default();

        // Apply timestamp filter
        if let Some(ts) = ts_filter {
            if !dirname.contains(ts) {
                continue;
            }
        }

        let name = meta.name.as_deref().unwrap_or(&dirname);
        let agent = meta.agent.as_deref().unwrap_or("?");
        let mode = meta.mode.as_deref().unwrap_or("?");

        // --- Status ---
        let status = detect_status(run_dir, meta);

        // Skip old DONE runs unless --all or --ts
        if status == Status::Done && !show_all && ts_filter.is_none() {
            if let Some(start) = meta.start_time.as_deref().and_then(parse_iso_epoch) {
                if now - start > STALE_THRESHOLD {
                    continue;
                }
            }
        }

        // --- Level progress ---
        let level_str = if mode == "levels" {
            level_progress(run_dir)
        } else {
            "---".to_string()
        };

        // --- Idle time ---
        let output_file = find_latest_output(run_dir, mode);
        let (mut idle_secs, mut idle_str) = match &output_file {
            Some(f) => {
                let mtime = file_mtime(f);
                let secs = (now - mtime).max(0) as u64;
                (secs, fmt_duration(secs))
            }
            None => (0, "---".to_string()),
        };

        // --- Elapsed ---
        let elapsed_str = elapsed_secs(meta)
            .map(fmt_duration)
            .unwrap_or_else(|| "---".to_string());

        // --- Output size ---
        let out_str = output_file
            .as_ref()
            .and_then(|f| fs::metadata(f).ok())
            .map(|m| fmt_size(m.len()))
            .unwrap_or_else(|| "---".to_string());

        // --- Detail ---
        let mut detail = String::new();
        if agent == "claude" {
            let (d, live_idle) = claude_detail(run_dir, meta, &status, name, now);
            detail = d;
            // Update idle from live session if found
            if let Some(li) = live_idle {
                idle_secs = li;
                idle_str = fmt_duration(li);
            }
        }
        if status == Status::Done {
            let score = score_display(meta);
            if score != "?" && score != "null" {
                detail = format!("score: {score}");
            }
        }

        // --- Idle color ---
        let idle_color = if status == Status::Running {
            if idle_secs > 900 {
                Color::RED
            } else if idle_secs > 300 {
                Color::YELLOW
            } else {
                Color::RESET
            }
        } else {
            Color::RESET
        };

        // --- Print row ---
        println!(
            "{:<28}  {}  {:<8}  {}  {:<8}  {:<8}  {}{}{}",
            name,
            colored(&format!("{:<9}", status.label()), status.color()),
            level_str,
            colored(&format!("{:<10}", idle_str), idle_color),
            elapsed_str,
            out_str,
            Color::DIM,
            detail,
            Color::RESET,
        );
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Status detection
// ---------------------------------------------------------------------------

fn detect_status(run_dir: &Path, meta: &MetaJson) -> Status {
    if meta.end_time.is_some() {
        return Status::Done;
    }

    let lockfile = run_dir.join(".run.lock");
    if lockfile.is_file() {
        if let Ok(content) = fs::read_to_string(&lockfile) {
            if let Ok(pid) = content.trim().parse::<u32>() {
                // Check /proc/{pid} exists
                if Path::new(&format!("/proc/{pid}")).exists() {
                    return Status::Running;
                }
            }
        }
        return Status::Dead;
    }

    Status::Done
}

// ---------------------------------------------------------------------------
// Level progress
// ---------------------------------------------------------------------------

fn level_progress(run_dir: &Path) -> String {
    let mut passed = 0u32;
    let mut last_level = String::new();
    let mut total = 0u32;

    let entries = match fs::read_dir(run_dir) {
        Ok(e) => e,
        Err(_) => return "---".to_string(),
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
        if status_file.is_file() {
            if let Ok(content) = fs::read_to_string(&status_file) {
                if content.to_uppercase().contains("PASSED") {
                    passed += 1;
                }
            }
            last_level = lname;
        } else {
            // Level started but no status = currently running
            last_level = format!("{lname}…");
        }
    }

    if total > 0 {
        format!("{last_level} ({passed}/16)")
    } else {
        "---".to_string()
    }
}

// ---------------------------------------------------------------------------
// Idle time helpers
// ---------------------------------------------------------------------------

fn find_latest_output(run_dir: &Path, mode: &str) -> Option<PathBuf> {
    let mut best: Option<(PathBuf, i64)> = None;

    let mut check = |path: PathBuf| {
        if path.is_file() {
            let mt = file_mtime(&path);
            if best.as_ref().is_none_or(|(_, prev)| mt > *prev) {
                best = Some((path, mt));
            }
        }
    };

    if mode == "levels" {
        // Check all L*/agent-output.txt
        if let Ok(entries) = fs::read_dir(run_dir) {
            for entry in entries.flatten() {
                if entry.file_name().to_string_lossy().starts_with('L') && entry.path().is_dir() {
                    check(entry.path().join("agent-output.txt"));
                }
            }
        }
    }

    // Also check top-level
    check(run_dir.join("agent-output.txt"));

    best.map(|(p, _)| p)
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

    // For running agents, try to find live JSONL
    if *status == Status::Running && !jsonl_path.is_file() {
        if let Some(live) = find_live_session(name, meta) {
            // Update idle from live session mtime
            let mt = file_mtime(&live);
            if mt > 0 {
                live_idle = Some((now - mt).max(0) as u64);
            }
            jsonl_path = live;
        }
    }

    if !jsonl_path.is_file() {
        return (String::new(), live_idle);
    }

    let content = match fs::read_to_string(&jsonl_path) {
        Ok(c) if !c.is_empty() => c,
        _ => return (String::new(), live_idle),
    };

    let mut last_tool = String::new();
    let mut total_out: u64 = 0;

    for line in content.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let obj: serde_json::Value = match serde_json::from_str(line) {
            Ok(v) => v,
            Err(_) => continue,
        };

        if obj.get("type").and_then(|t| t.as_str()) != Some("assistant") {
            continue;
        }

        let msg = match obj.get("message").and_then(|m| m.as_object()) {
            Some(m) => m,
            None => continue,
        };

        if let Some(usage) = msg.get("usage").and_then(|u| u.as_object()) {
            total_out += usage
                .get("output_tokens")
                .and_then(|v| v.as_u64())
                .unwrap_or(0);
        }

        if let Some(content) = msg.get("content").and_then(|c| c.as_array()) {
            for block in content {
                if block.get("type").and_then(|t| t.as_str()) == Some("tool_use") {
                    if let Some(name) = block.get("name").and_then(|n| n.as_str()) {
                        last_tool = name.to_string();
                    }
                }
            }
        }
    }

    let mut detail = String::new();
    if !last_tool.is_empty() {
        detail = format!("last: {last_tool}");
        if total_out > 0 {
            detail = format!("{detail}  tok: {total_out}");
        }
    }

    (detail, live_idle)
}

/// Try to locate a live Claude session.jsonl for a running agent.
///
/// Strategy 1: Derive Claude's project directory from the workspace path.
/// Strategy 2: Search by session_id in ~/.claude/projects/
fn find_live_session(name: &str, meta: &MetaJson) -> Option<PathBuf> {
    let home = std::env::var("HOME").ok()?;

    // Strategy 1: Derive project dir from workspace path
    // Workspace is typically at ../<project_root>/../workspace/<name>
    let workspace_dir = project_workspace_dir(name);
    if let Some(ws) = workspace_dir {
        if let Ok(real) = fs::canonicalize(&ws) {
            let proj_dir_name = derive_claude_project_name(&real);
            let proj_dir = PathBuf::from(&home)
                .join(".claude/projects")
                .join(&proj_dir_name);
            if proj_dir.is_dir() {
                if let Some(live) = newest_jsonl(&proj_dir) {
                    return Some(live);
                }
            }
        }
    }

    // Strategy 2: Search by session_id
    if let Some(sid) = &meta.session_id {
        let projects_dir = PathBuf::from(&home).join(".claude/projects");
        if let Some(found) = find_jsonl_by_session_id(&projects_dir, sid) {
            return Some(found);
        }
    }

    None
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
        if path.extension().and_then(|e| e.to_str()) == Some("jsonl") {
            let mt = file_mtime(&path);
            if best.as_ref().is_none_or(|(_, prev)| mt > *prev) {
                best = Some((path, mt));
            }
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
