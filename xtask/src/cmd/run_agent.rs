use crate::model::{
    compact_timestamp, iso_now, project_dir, run_cmd, run_cmd_capture, uuid_v4, write_meta, Error,
    Result, LEVELS,
};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
use std::time::Instant;

static INTERRUPTED: AtomicBool = AtomicBool::new(false);
static CHILD_PID: AtomicU32 = AtomicU32::new(0);

pub struct RunAgentArgs {
    pub base: String,
    pub strategy: String,
    pub name: String,
    pub prompt: Option<String>,
    pub model: Option<String>,
    pub agent: String,
    pub mode: String,
    pub max_turns: Option<u32>,
    pub skip_bench: bool,
    pub resume: bool,
    pub from_level: Option<String>,
    pub clean: bool,
}

const DEFAULT_PROMPT: &str = "Implement the Scheme interpreter by following CLAUDE.md exactly.
Work through levels 1 through 16 in order.
After implementing each level, run cargo xtask test NN to verify.
Fix failures before proceeding. Do not skip levels.";

pub fn run(args: RunAgentArgs) -> Result<()> {
    // Install ctrl-c handler
    install_signal_handlers();

    let proj = project_dir();
    let session_uuid = uuid_v4();
    let timestamp = compact_timestamp();
    let start_time = iso_now();
    let prompt = args.prompt.as_deref().unwrap_or(DEFAULT_PROMPT);

    // Default non-Claude agents to levels mode
    let mode = if args.agent != "claude" && args.mode == "full" {
        println!(
            "NOTE: Defaulting to --mode levels for {} (override with explicit --mode full)",
            args.agent
        );
        "levels".to_string()
    } else {
        args.mode.clone()
    };

    // --- Worktree path ---
    let worktree_dir = proj
        .parent()
        .expect("project has no parent dir")
        .join("workspace")
        .join(&args.name);
    // Agent sees bench/ as its working directory
    let agent_workdir = worktree_dir.join("bench");

    // --- Results dir ---
    let results_dir = if args.resume {
        find_resume_dir(&proj, &args.strategy, &args.name)?
    } else {
        setup_fresh_run(&proj, &args, &worktree_dir)?
    };

    // --- Write initial meta.json ---
    if !args.resume {
        let meta = serde_json::json!({
            "base": args.base,
            "strategy": args.strategy,
            "name": args.name,
            "session_id": session_uuid,
            "agent": args.agent,
            "mode": mode,
            "model": args.model.as_deref().unwrap_or("default"),
            "prompt": prompt,
            "start_time": start_time,
            "timestamp": timestamp,
        });
        write_meta(&results_dir.join("meta.json"), &meta)?;
    }

    println!("=== Agent Run: {} ===", args.name);
    println!("Base:       {}", args.base);
    println!("Strategy:   {}", args.strategy);
    println!("Agent:      {}", args.agent);
    println!("Mode:       {mode}");
    println!("Model:      {}", args.model.as_deref().unwrap_or("default"));
    println!("Session:    {session_uuid}");
    println!("Results:    {}", results_dir.display());
    println!("Worktree:   {}", worktree_dir.display());
    println!("Resume:     {}", args.resume);
    println!();

    // --- Create worktree (fresh run only) ---
    if !args.resume {
        println!("Creating worktree from '{}'...", args.base);
        let exit = run_cmd(
            "git",
            &[
                "worktree",
                "add",
                "-b",
                &args.name,
                worktree_dir.to_str().expect("worktree path not utf8"),
                &args.base,
                "--quiet",
            ],
            &proj,
        )?;
        if exit != 0 {
            return Err(Error::CommandFailed {
                cmd: "git worktree add".to_string(),
                exit_code: exit,
            });
        }
        println!("Worktree created.");

        // --- Symlink strategy file as CLAUDE.md ---
        let strategy_src = format!("strategies/{}.md", args.strategy);
        let strategy_path = agent_workdir.join(&strategy_src);
        if !strategy_path.is_file() {
            return Err(Error::CommandFailed {
                cmd: format!(
                    "strategy file not found: {} (available: ls bench/strategies/)",
                    strategy_path.display()
                ),
                exit_code: 1,
            });
        }
        let claude_md = agent_workdir.join("CLAUDE.md");
        std::os::unix::fs::symlink(&strategy_src, &claude_md)
            .map_err(|e| Error::io(&claude_md, e))?;
        let agents_md = agent_workdir.join("AGENTS.md");
        std::os::unix::fs::symlink("CLAUDE.md", &agents_md)
            .map_err(|e| Error::io(&agents_md, e))?;
        println!("Strategy:   {} → CLAUDE.md", strategy_src);
    }

    // --- Warm dependency cache ---
    println!("Pre-building dependencies in worktree (release mode)...");
    let prebuild = run_cmd("cargo", &["build", "--release"], &worktree_dir);
    match prebuild {
        Ok(0) => println!("Pre-build complete."),
        _ => println!("WARNING: Pre-build failed. Agent may hit cold cache issues."),
    }

    // --- Lockfile ---
    let lockfile = results_dir.join(".run.lock");
    acquire_lock(&lockfile)?;

    // Setup cleanup on exit
    let _cleanup_ctx = CleanupContext {
        worktree_dir: worktree_dir.clone(),
        name: args.name.clone(),
        lockfile: lockfile.clone(),
    };

    // --- Execute ---
    let mut agent_exit = 0i32;
    let mut level_times: Vec<(String, i64, String)> = Vec::new(); // (level, duration, status)

    if mode == "levels" {
        println!("=== Level-by-level mode ===");

        let start_level: u32 = args
            .from_level
            .as_ref()
            .and_then(|s| s.parse().ok())
            .unwrap_or(1);

        for level in &LEVELS {
            if INTERRUPTED.load(Ordering::Relaxed) {
                println!("Interrupted — stopping.");
                break;
            }

            let level_num: u32 = level.parse().expect("level constant not a number");

            // Skip levels before --from-level
            if level_num < start_level {
                println!("Skipping L{level} (before --from-level {start_level})");
                continue;
            }

            let level_dir = results_dir.join(format!("L{level}"));

            // Resume: skip already-passed levels
            if args.resume {
                let status_file = level_dir.join("status.txt");
                if status_file.is_file() {
                    if let Ok(content) = fs::read_to_string(&status_file) {
                        if content.contains("PASSED") {
                            println!("Skipping L{level} — already PASSED (resume mode)");
                            continue;
                        }
                    }
                    // Level not passed — clean and retry
                    println!("Retrying L{level} — previous attempt was not PASSED");
                    let _ = fs::remove_dir_all(&level_dir);
                }
            }

            fs::create_dir_all(&level_dir).map_err(|e| Error::io(&level_dir, e))?;

            let level_uuid = uuid_v4();
            let level_turns = turns_for_level(level_num, args.max_turns);

            println!();
            println!("--- Level {level} (max {level_turns} turns) ---");

            // Build prompt
            let level_prompt = build_level_prompt(level, &worktree_dir, &results_dir);

            // Launch agent
            let level_start = Instant::now();
            agent_exit = launch_agent(
                &args.agent,
                &agent_workdir,
                &level_prompt,
                &level_uuid,
                &level_dir.join("agent-output.txt"),
                Some(level_turns),
                args.model.as_deref(),
            );
            let level_duration = level_start.elapsed().as_secs() as i64;

            // Capture session
            capture_session(&args.agent, &level_uuid, &level_dir);

            // Test this level
            let test_exit = run_cmd("cargo", &["xtask", "test", level], &worktree_dir).unwrap_or(1);

            let status_label = if test_exit == 0 { "PASSED" } else { "FAILED" };
            level_times.push((
                format!("L{level}"),
                level_duration,
                status_label.to_string(),
            ));

            let status_msg = format!("Level {level} {status_label} ({level_duration}s)");
            println!("{status_msg}");
            let _ = fs::write(level_dir.join("status.txt"), &status_msg);

            // Git checkpoint
            println!("Committing checkpoint for L{level}...");
            let commit_msg = format!("checkpoint: L{level} {status_label} ({level_duration}s)");
            let _ = run_cmd("git", &["add", "-A"], &worktree_dir);
            let _ = run_cmd(
                "git",
                &["commit", "-m", &commit_msg, "--allow-empty"],
                &worktree_dir,
            );

            if test_exit != 0 {
                println!("Level {level} FAILED — stopping");
                break;
            }
        }
    } else if mode == "full" {
        println!("=== Full run mode ===");

        agent_exit = launch_agent(
            &args.agent,
            &agent_workdir,
            prompt,
            &session_uuid,
            &results_dir.join("agent-output.txt"),
            args.max_turns,
            args.model.as_deref(),
        );

        println!();
        println!("Agent exited with code: {agent_exit}");

        // Git checkpoint
        let _ = run_cmd("git", &["add", "-A"], &worktree_dir);
        let _ = run_cmd(
            "git",
            &["commit", "-m", "agent: full run complete", "--allow-empty"],
            &worktree_dir,
        );

        // Capture session
        capture_session(&args.agent, &session_uuid, &results_dir);
    } else {
        eprintln!("ERROR: Unknown mode '{mode}'. Use 'full' or 'levels'.");
        return Err(Error::CommandFailed {
            cmd: "run-agent".to_string(),
            exit_code: 1,
        });
    }

    // --- Scoring ---
    let score = if !args.skip_bench {
        println!();
        println!("=== Scoring ===");
        let bench_log = results_dir.join("bench.log");
        let bench_exit = run_cmd(
            "cargo",
            &["xtask", "bench", &args.name, "--run-id", "bench"],
            &proj,
        )
        .unwrap_or(1);

        // Try to extract score from bench output
        if bench_log.is_file() {
            extract_score_from_log(&bench_log)
        } else {
            if bench_exit == 0 {
                "completed".to_string()
            } else {
                "unknown".to_string()
            }
        }
    } else {
        "skipped".to_string()
    };

    // --- Update meta.json ---
    let end_time = iso_now();
    let mut level_times_json = serde_json::Map::new();
    for (level, duration, status) in &level_times {
        level_times_json.insert(
            level.clone(),
            serde_json::json!({"duration_s": duration, "status": status}),
        );
    }

    let meta = serde_json::json!({
        "base": args.base,
        "name": args.name,
        "session_id": session_uuid,
        "agent": args.agent,
        "mode": mode,
        "model": args.model.as_deref().unwrap_or("default"),
        "prompt": prompt,
        "start_time": start_time,
        "timestamp": timestamp,
        "end_time": end_time,
        "exit_code": agent_exit,
        "score": score,
        "level_times": level_times_json,
    });
    write_meta(&results_dir.join("meta.json"), &meta)?;

    // --- Push branch ---
    println!("Pushing branch to origin...");
    let push_exit = run_cmd("git", &["push", "-u", "origin", &args.name], &worktree_dir);
    match push_exit {
        Ok(0) => println!("Branch pushed: origin/{}", args.name),
        _ => println!("WARNING: Push to origin failed (no remote or auth issue)"),
    }

    // Release lock
    let _ = fs::remove_file(&lockfile);

    println!();
    println!("=== Run complete ===");
    println!("Results: {}", results_dir.display());
    println!("Score:   {score}");

    Ok(())
}

// ---------------------------------------------------------------------------
// Setup helpers
// ---------------------------------------------------------------------------

fn find_resume_dir(proj: &Path, strategy: &str, name: &str) -> Result<PathBuf> {
    let results_dir = proj.join("results");
    let prefix = format!("{strategy}_{name}_");

    let mut matching: Vec<PathBuf> = Vec::new();
    if let Ok(entries) = fs::read_dir(&results_dir) {
        for entry in entries.flatten() {
            let fname = entry.file_name().to_string_lossy().into_owned();
            if fname.starts_with(&prefix) && entry.path().is_dir() {
                matching.push(entry.path());
            }
        }
    }
    matching.sort();
    matching.last().cloned().ok_or(Error::RunNotFound {
        path: results_dir.join(format!("{prefix}*")),
    })
}

fn setup_fresh_run(proj: &Path, args: &RunAgentArgs, worktree_dir: &Path) -> Result<PathBuf> {
    if args.clean {
        // Remove stale worktree directory
        if worktree_dir.is_dir() {
            println!("--clean: removing worktree dir {}", worktree_dir.display());
            let _ = run_cmd("git", &["worktree", "remove", "--force", &worktree_dir.to_string_lossy()], proj);
            if worktree_dir.is_dir() {
                fs::remove_dir_all(worktree_dir).map_err(|e| Error::io(worktree_dir, e))?;
            }
        }
        // Prune stale worktree refs and delete branch
        let _ = run_cmd("git", &["worktree", "prune"], proj);
        let _ = run_cmd("git", &["branch", "-D", &args.name], proj);
    }

    if worktree_dir.is_dir() {
        return Err(Error::WorktreeDirExists {
            path: worktree_dir.to_path_buf(),
        });
    }

    // Prune stale worktree refs before checking branch
    let _ = run_cmd("git", &["worktree", "prune"], proj);

    // Check branch doesn't already exist
    let (_, branches) = run_cmd_capture("git", &["branch", "--list", &args.name], proj)?;
    if !branches.trim().is_empty() {
        return Err(Error::BranchExists {
            branch: args.name.clone(),
        });
    }

    let timestamp = compact_timestamp();
    let results_dir = proj
        .join("results")
        .join(format!("{}_{}_{}", args.strategy, args.name, timestamp));
    fs::create_dir_all(&results_dir).map_err(|e| Error::io(&results_dir, e))?;
    Ok(results_dir)
}

// ---------------------------------------------------------------------------
// Lock management
// ---------------------------------------------------------------------------

fn acquire_lock(lockfile: &Path) -> Result<()> {
    if lockfile.is_file() {
        if let Ok(content) = fs::read_to_string(lockfile) {
            if let Ok(pid) = content.trim().parse::<u32>() {
                if Path::new(&format!("/proc/{pid}")).exists() {
                    return Err(Error::LockConflict {
                        path: lockfile.to_path_buf(),
                        pid,
                    });
                }
            }
        }
        // Stale lock — remove it
        println!("WARNING: Stale lockfile found. Removing.");
        let _ = fs::remove_file(lockfile);
    }
    fs::write(lockfile, format!("{}", std::process::id())).map_err(|e| Error::io(lockfile, e))
}

// ---------------------------------------------------------------------------
// Agent launchers
// ---------------------------------------------------------------------------

fn launch_agent(
    agent: &str,
    workdir: &Path,
    prompt: &str,
    session_id: &str,
    output_file: &Path,
    max_turns: Option<u32>,
    model: Option<&str>,
) -> i32 {
    let result = match agent {
        "claude" => launch_claude(workdir, prompt, session_id, output_file, max_turns, model),
        "codex" => launch_codex(workdir, prompt, output_file),
        "opencode" => launch_opencode(workdir, prompt, output_file),
        _ => {
            eprintln!("ERROR: Unknown agent '{agent}'");
            return 1;
        }
    };

    match result {
        Ok(code) => code,
        Err(e) => {
            eprintln!("Agent launch error: {e}");
            1
        }
    }
}

fn launch_claude(
    workdir: &Path,
    prompt: &str,
    session_id: &str,
    output_file: &Path,
    max_turns: Option<u32>,
    model: Option<&str>,
) -> Result<i32> {
    let mut cmd_args: Vec<String> = vec![
        "-p".to_string(),
        "--session-id".to_string(),
        session_id.to_string(),
        "--dangerously-skip-permissions".to_string(),
    ];
    if let Some(m) = model {
        cmd_args.push("--model".to_string());
        cmd_args.push(m.to_string());
    }
    if let Some(t) = max_turns {
        cmd_args.push("--max-turns".to_string());
        cmd_args.push(t.to_string());
    }
    cmd_args.push(prompt.to_string());

    let args_ref: Vec<&str> = cmd_args.iter().map(|s| s.as_str()).collect();
    run_agent_with_tee("claude", &args_ref, workdir, output_file)
}

fn launch_codex(workdir: &Path, prompt: &str, output_file: &Path) -> Result<i32> {
    let script_cmd = format!(
        "cd '{}' && codex exec --full-auto '{}'",
        workdir.display(),
        prompt.replace('\'', "'\\''"),
    );
    let out_str = output_file.to_str().expect("output file path not utf8");
    run_cmd("script", &["-qec", &script_cmd, out_str], workdir)
}

fn launch_opencode(workdir: &Path, prompt: &str, output_file: &Path) -> Result<i32> {
    let script_cmd = format!(
        "cd '{}' && echo '{}' | opencode",
        workdir.display(),
        prompt.replace('\'', "'\\''"),
    );
    let out_str = output_file.to_str().expect("output file path not utf8");
    run_cmd("script", &["-qec", &script_cmd, out_str], workdir)
}

fn run_agent_with_tee(cmd: &str, args: &[&str], workdir: &Path, output_file: &Path) -> Result<i32> {
    // Pipe through tee to capture output while showing it
    let mut child = Command::new(cmd)
        .args(args)
        .current_dir(workdir)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| {
            if e.kind() == std::io::ErrorKind::NotFound {
                Error::BinaryNotFound { name: cmd.to_string() }
            } else {
                Error::Io { path: PathBuf::from(cmd), source: e }
            }
        })?;

    CHILD_PID.store(child.id(), Ordering::Relaxed);

    // Use tee to write to both stdout and file
    let stdout = child.stdout.take();
    let stderr = child.stderr.take();

    let out_path = output_file.to_path_buf();
    let tee_thread = std::thread::spawn(move || {
        use std::io::{BufRead, BufReader, Write};
        let mut file = fs::File::create(&out_path).expect("cannot create output file");
        if let Some(stdout) = stdout {
            let reader = BufReader::new(stdout);
            for line in reader.lines() {
                let Ok(line) = line else { break };
                println!("{line}");
                let _ = writeln!(file, "{line}");
            }
        }
    });

    let err_thread = std::thread::spawn(move || {
        use std::io::{BufRead, BufReader};
        if let Some(stderr) = stderr {
            let reader = BufReader::new(stderr);
            for line in reader.lines() {
                let Ok(line) = line else { break };
                eprintln!("{line}");
            }
        }
    });

    let status = child.wait().map_err(|e| Error::Io {
        path: PathBuf::from(cmd),
        source: e,
    })?;

    CHILD_PID.store(0, Ordering::Relaxed);
    let _ = tee_thread.join();
    let _ = err_thread.join();

    Ok(status.code().unwrap_or(1))
}

// ---------------------------------------------------------------------------
// Level helpers
// ---------------------------------------------------------------------------

fn turns_for_level(level_num: u32, max_turns: Option<u32>) -> u32 {
    if let Some(t) = max_turns {
        return t;
    }
    if level_num <= 9 {
        60
    } else if level_num <= 13 {
        100
    } else {
        160
    }
}

fn build_level_prompt(level: &str, worktree_dir: &Path, results_dir: &Path) -> String {
    if level == "01" {
        return format!(
            "Implement the Scheme interpreter. Your task: make level {level} tests pass.\n\
             Read CLAUDE.md for full instructions.\n\
             Run cargo xtask test {level} to verify. Do not work on other levels."
        );
    }

    // Generate context summary
    let mut summary = String::new();
    summary.push_str("Files under src/scheme/:\n");

    // List .rs files with line counts
    if let Ok(entries) = fs::read_dir(worktree_dir.join("bench/src/scheme")) {
        let mut files: Vec<(String, usize)> = entries
            .flatten()
            .filter_map(|e| {
                let name = e.file_name().to_string_lossy().into_owned();
                if name.ends_with(".rs") {
                    let content = fs::read_to_string(e.path()).ok()?;
                    Some((name, content.lines().count()))
                } else {
                    None
                }
            })
            .collect();
        files.sort_by_key(|(_, lines)| *lines);
        for (name, lines) in &files {
            summary.push_str(&format!("  {lines:>5} {name}\n"));
        }
    }

    summary.push_str("\nLevels already passing:\n");
    let level_num: u32 = level.parse().unwrap_or(1);
    for prev_num in 1..level_num {
        let prev = format!("{prev_num:02}");
        let status_file = results_dir.join(format!("L{prev}/status.txt"));
        if let Ok(content) = fs::read_to_string(&status_file) {
            if content.contains("PASSED") {
                summary.push_str(&format!("  L{prev}: PASSED\n"));
            }
        }
    }

    format!(
        "Context from previous levels:\n\
         {summary}\n\
         Your task: make level {level} tests pass.\n\
         Read CLAUDE.md for full instructions.\n\
         Run cargo xtask test {level} to verify. Do not work on other levels."
    )
}

// ---------------------------------------------------------------------------
// Session capture
// ---------------------------------------------------------------------------

fn capture_session(agent: &str, session_id: &str, dest: &Path) {
    match agent {
        "claude" => {
            let home = std::env::var("HOME").unwrap_or_default();
            let projects_dir = PathBuf::from(&home).join(".claude/projects");
            if let Some(found) = find_session_jsonl(&projects_dir, session_id) {
                let target = dest.join("session.jsonl");
                match fs::copy(&found, &target) {
                    Ok(_) => println!("Session captured: {}", target.display()),
                    Err(e) => eprintln!("WARNING: Failed to copy session: {e}"),
                }
            } else {
                eprintln!("WARNING: Session JSONL not found for {session_id}");
            }
        }
        "codex" => {
            let home = std::env::var("HOME").unwrap_or_default();
            let codex_dir = PathBuf::from(&home).join(".codex");
            if codex_dir.is_dir() {
                if let Some(latest) = newest_file(&codex_dir, "log") {
                    let target = dest.join("session.log");
                    let _ = fs::copy(&latest, &target);
                    println!("Codex session captured: {}", target.display());
                }
            }
        }
        "opencode" => {
            let home = std::env::var("HOME").unwrap_or_default();
            let oc_dir = PathBuf::from(&home).join(".opencode");
            if oc_dir.is_dir() {
                if let Some(latest) = newest_file(&oc_dir, "json") {
                    let target = dest.join("session.json");
                    let _ = fs::copy(&latest, &target);
                    println!("OpenCode session captured: {}", target.display());
                }
            }
        }
        _ => {}
    }
}

fn find_session_jsonl(projects_dir: &Path, session_id: &str) -> Option<PathBuf> {
    let target = format!("{session_id}.jsonl");
    let entries = fs::read_dir(projects_dir).ok()?;
    for entry in entries.flatten() {
        if !entry.path().is_dir() {
            continue;
        }
        let candidate = entry.path().join(&target);
        if candidate.is_file() {
            return Some(candidate);
        }
    }
    None
}

fn newest_file(dir: &Path, extension: &str) -> Option<PathBuf> {
    let entries = fs::read_dir(dir).ok()?;
    let mut best: Option<(PathBuf, std::time::SystemTime)> = None;
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) == Some(extension) {
            if let Ok(meta) = path.metadata() {
                if let Ok(modified) = meta.modified() {
                    if best.as_ref().is_none_or(|(_, prev)| modified > *prev) {
                        best = Some((path, modified));
                    }
                }
            }
        }
    }
    best.map(|(p, _)| p)
}

fn extract_score_from_log(log_path: &Path) -> String {
    if let Ok(content) = fs::read_to_string(log_path) {
        for line in content.lines() {
            if let Some(pos) = line.find("Score: ") {
                let rest = &line[pos + 7..];
                let score: String = rest
                    .chars()
                    .take_while(|c| c.is_ascii_digit() || *c == '/')
                    .collect();
                if !score.is_empty() {
                    return score;
                }
            }
        }
    }
    "unknown".to_string()
}

// ---------------------------------------------------------------------------
// Signal handling (minimal, no external crate)
// ---------------------------------------------------------------------------

// POSIX signal constants
const SIGINT: i32 = 2;
const SIGTERM: i32 = 15;

extern "C" fn signal_handler(_sig: i32) {
    INTERRUPTED.store(true, Ordering::Relaxed);
    let pid = CHILD_PID.load(Ordering::Relaxed);
    if pid > 0 {
        // Send SIGTERM to child
        unsafe {
            // libc::kill equivalent
            extern "C" {
                fn kill(pid: i32, sig: i32) -> i32;
            }
            kill(pid as i32, SIGTERM);
        }
    }
}

fn install_signal_handlers() {
    unsafe {
        extern "C" {
            fn signal(sig: i32, handler: extern "C" fn(i32)) -> usize;
        }
        signal(SIGINT, signal_handler);
        signal(SIGTERM, signal_handler);
    }
}

/// Cleanup context — not RAII because we need manual control over the commit step.
struct CleanupContext {
    worktree_dir: PathBuf,
    name: String,
    lockfile: PathBuf,
}

impl Drop for CleanupContext {
    fn drop(&mut self) {
        if INTERRUPTED.load(Ordering::Relaxed) {
            eprintln!("\n=== Cleaning up ===");

            // Commit any uncommitted work
            if self.worktree_dir.is_dir() {
                eprintln!("Committing agent work before exit...");
                let _ = run_cmd("git", &["add", "-A"], &self.worktree_dir);
                let _ = run_cmd(
                    "git",
                    &[
                        "commit",
                        "-m",
                        "checkpoint: interrupted/cleanup",
                        "--allow-empty",
                    ],
                    &self.worktree_dir,
                );
                let _ = run_cmd(
                    "git",
                    &["push", "-u", "origin", &self.name],
                    &self.worktree_dir,
                );
            }

            // Release lock
            let _ = fs::remove_file(&self.lockfile);
        }
    }
}
