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
Work through levels 1 through 20 in order.
After implementing each level, run cargo xtask test NN to verify.
Fix failures before proceeding. Do not skip levels.";

pub fn run(args: RunAgentArgs) -> Result<()> {
    install_signal_handlers();

    let proj = project_dir();
    let session_uuid = uuid_v4();
    let timestamp = compact_timestamp();
    let start_time = iso_now();
    let prompt = args.prompt.as_deref().unwrap_or(DEFAULT_PROMPT);
    let mode = args.mode.clone();

    let worktree_dir = proj
        .parent()
        .expect("project has no parent dir")
        .join("workspace")
        .join(&args.name);
    let agent_workdir = worktree_dir.join("bench");

    let results_dir = if args.resume {
        find_resume_dir(&proj, &args.strategy, &args.name)?
    } else {
        setup_fresh_run(&proj, &args, &worktree_dir)?
    };

    if !args.resume {
        write_initial_meta(&args, &results_dir, &session_uuid, &mode, prompt, &start_time, &timestamp)?;
    }

    print_run_banner(&args, &session_uuid, &results_dir, &worktree_dir, &mode);

    if !args.resume {
        create_worktree(&args, &proj, &worktree_dir, &agent_workdir)?;
    }

    warm_cache(&worktree_dir);

    let lockfile = results_dir.join(".run.lock");
    acquire_lock(&lockfile)?;

    let _cleanup_ctx = CleanupContext {
        worktree_dir: worktree_dir.clone(),
        name: args.name.clone(),
        lockfile: lockfile.clone(),
    };

    let (agent_exit, level_times) = execute_mode(
        &mode, &args, &agent_workdir, &worktree_dir, &results_dir,
        prompt, &session_uuid,
    )?;

    let score = run_scoring(&args, &proj)?;

    finalize_run(
        &args, &results_dir, &session_uuid, &mode, prompt,
        &start_time, &timestamp, agent_exit, &score, &level_times,
    )?;

    push_branch(&args.name, &worktree_dir);

    let _ = fs::remove_file(&lockfile);

    println!();
    println!("=== Run complete ===");
    println!("Results: {}", results_dir.display());
    println!("Score:   {score}");

    Ok(())
}

fn write_initial_meta(
    args: &RunAgentArgs, results_dir: &Path, session_uuid: &str,
    mode: &str, prompt: &str, start_time: &str, timestamp: &str,
) -> Result<()> {
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
    write_meta(&results_dir.join("meta.json"), &meta)
}

fn print_run_banner(
    args: &RunAgentArgs, session_uuid: &str, results_dir: &Path,
    worktree_dir: &Path, mode: &str,
) {
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
}

fn create_worktree(
    args: &RunAgentArgs, proj: &Path, worktree_dir: &Path, agent_workdir: &Path,
) -> Result<()> {
    println!("Creating worktree from '{}'...", args.base);
    let exit = run_cmd(
        "git",
        &[
            "worktree", "add", "-b", &args.name,
            worktree_dir.to_str().expect("worktree path not utf8"),
            &args.base, "--quiet",
        ],
        proj,
    )?;
    if exit != 0 {
        return Err(Error::CommandFailed {
            cmd: "git worktree add".to_string(),
            exit_code: exit,
        });
    }
    println!("Worktree created.");

    symlink_strategy(args, agent_workdir)?;
    copy_strategy_clippy(args, agent_workdir)?;
    // quality-gate lints are now enforced at runtime via `cargo xtask test`
    // (clippy flags in test_level.rs), not via Cargo feature patching.
    Ok(())
}

fn symlink_strategy(args: &RunAgentArgs, agent_workdir: &Path) -> Result<()> {
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
    let _ = fs::remove_file(&claude_md);
    std::os::unix::fs::symlink(&strategy_src, &claude_md)
        .map_err(|e| Error::io(&claude_md, e))?;
    let agents_md = agent_workdir.join("AGENTS.md");
    let _ = fs::remove_file(&agents_md);
    std::os::unix::fs::symlink("CLAUDE.md", &agents_md)
        .map_err(|e| Error::io(&agents_md, e))?;
    println!("Strategy:   {strategy_src} → CLAUDE.md");
    Ok(())
}

fn copy_strategy_clippy(args: &RunAgentArgs, agent_workdir: &Path) -> Result<()> {
    let strategy_clippy = agent_workdir.join(format!("strategies/{}.clippy.toml", args.strategy));
    let clippy_toml = agent_workdir.join("clippy.toml");
    if strategy_clippy.is_file() {
        fs::copy(&strategy_clippy, &clippy_toml)
            .map_err(|e| Error::io(&clippy_toml, e))?;
        println!("Copied {}.clippy.toml → clippy.toml", args.strategy);
    }
    Ok(())
}


fn warm_cache(worktree_dir: &Path) {
    println!("Pre-building dependencies in worktree (release mode)...");
    let prebuild = run_cmd("cargo", &["build", "--release"], worktree_dir);
    match prebuild {
        Ok(0) => println!("Pre-build complete."),
        _ => println!("WARNING: Pre-build failed. Agent may hit cold cache issues."),
    }
}

fn execute_mode(
    mode: &str, args: &RunAgentArgs, agent_workdir: &Path,
    worktree_dir: &Path, results_dir: &Path,
    prompt: &str, session_uuid: &str,
) -> Result<(i32, Vec<(String, i64, String)>)> {
    if mode == "levels" {
        run_levels_mode(args, agent_workdir, worktree_dir, results_dir)
    } else if mode == "full" {
        let exit = run_full_mode(args, agent_workdir, worktree_dir, results_dir, prompt, session_uuid);
        Ok((exit, Vec::new()))
    } else {
        eprintln!("ERROR: Unknown mode '{mode}'. Use 'full' or 'levels'.");
        Err(Error::CommandFailed {
            cmd: "run-agent".to_string(),
            exit_code: 1,
        })
    }
}

fn run_full_mode(
    args: &RunAgentArgs, agent_workdir: &Path, worktree_dir: &Path,
    results_dir: &Path, prompt: &str, session_uuid: &str,
) -> i32 {
    println!("=== Full run mode ===");

    let agent_exit = launch_agent(
        &args.agent, agent_workdir, prompt, session_uuid,
        &results_dir.join("agent-output.txt"), args.max_turns, args.model.as_deref(),
    );

    println!();
    println!("Agent exited with code: {agent_exit}");

    let _ = run_cmd("git", &["add", "-A"], worktree_dir);
    let _ = run_cmd(
        "git",
        &["commit", "-m", "agent: full run complete", "--allow-empty"],
        worktree_dir,
    );

    capture_session(&args.agent, session_uuid, results_dir);
    agent_exit
}

fn run_levels_mode(
    args: &RunAgentArgs, agent_workdir: &Path, worktree_dir: &Path,
    results_dir: &Path,
) -> Result<(i32, Vec<(String, i64, String)>)> {
    println!("=== Level-by-level mode ===");

    let start_level: u32 = args
        .from_level
        .as_ref()
        .and_then(|s| s.parse().ok())
        .unwrap_or(1);

    let mut agent_exit = 0i32;
    let mut level_times: Vec<(String, i64, String)> = Vec::new();

    for level in &LEVELS {
        if INTERRUPTED.load(Ordering::Relaxed) {
            println!("Interrupted — stopping.");
            break;
        }

        let level_num: u32 = level.parse().expect("level constant not a number");
        if level_num < start_level {
            println!("Skipping L{level} (before --from-level {start_level})");
            continue;
        }

        let level_dir = results_dir.join(format!("L{level}"));
        if should_skip_level(args.resume, &level_dir, level) {
            continue;
        }

        let result = run_single_level(args, agent_workdir, worktree_dir, &level_dir, level)?;
        agent_exit = result.0;
        level_times.push((format!("L{level}"), result.1, result.2.clone()));

        commit_checkpoint(level, &result.2, result.1, worktree_dir);

        if result.2 == "FAILED" {
            println!("Level {level} FAILED — stopping");
            break;
        }
    }

    Ok((agent_exit, level_times))
}

fn should_skip_level(resume: bool, level_dir: &Path, level: &str) -> bool {
    if !resume {
        return false;
    }
    let status_file = level_dir.join("status.txt");
    if !status_file.is_file() {
        return false;
    }
    if let Ok(content) = fs::read_to_string(&status_file) {
        if content.contains("PASSED") {
            println!("Skipping L{level} — already PASSED (resume mode)");
            return true;
        }
    }
    println!("Retrying L{level} — previous attempt was not PASSED");
    let _ = fs::remove_dir_all(level_dir);
    false
}

/// Maximum number of automatic retries when the agent fails for infrastructure
/// reasons (API timeout, 529, crash) rather than exhausting its turn budget.
const MAX_INFRA_RETRIES: u32 = 2;

/// Check whether the agent output indicates it ran out of turns (not retryable)
/// vs an infrastructure failure like timeout/529/crash (retryable).
fn agent_exhausted_turns(output_file: &Path) -> bool {
    fs::read_to_string(output_file)
        .map(|c| c.contains("Reached max turns"))
        .unwrap_or(false)
}

/// Returns (agent_exit, duration_secs, status_label).
fn run_single_level(
    args: &RunAgentArgs, agent_workdir: &Path, worktree_dir: &Path,
    level_dir: &Path, level: &str,
) -> Result<(i32, i64, String)> {
    let level_start = Instant::now();
    let mut attempt = 0u32;

    loop {
        if attempt > 0 {
            // Clean up previous attempt's level_dir contents for a fresh retry
            let _ = fs::remove_dir_all(level_dir);
        }
        fs::create_dir_all(level_dir).map_err(|e| Error::io(level_dir, e))?;

        let level_num: u32 = level.parse().expect("level constant not a number");
        let level_uuid = uuid_v4();
        let level_turns = turns_for_level(level_num, args.max_turns);

        if attempt == 0 {
            println!();
            println!("--- Level {level} (max {level_turns} turns) ---");
        } else {
            println!();
            println!("--- Level {level} RETRY {attempt}/{MAX_INFRA_RETRIES} (max {level_turns} turns) ---");
        }

        let level_prompt = build_level_prompt(level, worktree_dir, level_dir.parent().expect("level_dir has parent"));
        let agent_exit = launch_agent(
            &args.agent, agent_workdir, &level_prompt, &level_uuid,
            &level_dir.join("agent-output.txt"), Some(level_turns), args.model.as_deref(),
        );

        capture_session(&args.agent, &level_uuid, level_dir);

        let mut test_exit = run_level_tests(worktree_dir, level);

        if test_exit == 0 && worktree_dir.join("bench/clippy.toml").is_file() {
            test_exit = run_quality_gate_cleanup(args, agent_workdir, worktree_dir, level_dir, level);
        }

        if test_exit == 0 {
            // Success
            let level_duration = level_start.elapsed().as_secs() as i64;
            let status_msg = format!("Level {level} PASSED ({level_duration}s)");
            println!("{status_msg}");
            let _ = fs::write(level_dir.join("status.txt"), &status_msg);
            return Ok((agent_exit, level_duration, "PASSED".to_string()));
        }

        // Failed — decide whether to retry
        let output_file = level_dir.join("agent-output.txt");
        let exhausted = agent_exhausted_turns(&output_file);

        if exhausted || attempt >= MAX_INFRA_RETRIES {
            let reason = if exhausted { "turns exhausted" } else { "max retries reached" };
            let level_duration = level_start.elapsed().as_secs() as i64;
            let status_msg = format!("Level {level} FAILED ({level_duration}s) [{reason}]");
            println!("{status_msg}");
            let _ = fs::write(level_dir.join("status.txt"), &status_msg);
            return Ok((agent_exit, level_duration, "FAILED".to_string()));
        }

        println!("Level {level} failed (infra issue, not turns) — will retry");
        attempt += 1;
    }
}

fn run_level_tests(worktree_dir: &Path, level: &str) -> i32 {
    // Tests only — no quality gate. Gate is enforced in the cleanup pass.
    run_cmd("cargo", &["xtask", "test", level], worktree_dir).unwrap_or(1)
}

fn run_quality_gate_cleanup(
    args: &RunAgentArgs, agent_workdir: &Path, worktree_dir: &Path,
    level_dir: &Path, level: &str,
) -> i32 {
    println!("--- Level {level} cleanup (max 15 turns) ---");
    let cleanup_uuid = uuid_v4();
    let cleanup_prompt = format!(
        "Level {level} tests pass. Fix any clippy/quality-gate warnings.\n\
         Run `cargo xtask test {level} --gate` to verify. Do not change test behavior."
    );
    let _cleanup_exit = launch_agent(
        &args.agent, agent_workdir, &cleanup_prompt, &cleanup_uuid,
        &level_dir.join("agent-output-cleanup.txt"), Some(15), args.model.as_deref(),
    );
    capture_session_as(&args.agent, &cleanup_uuid, level_dir, "session-cleanup.jsonl");

    run_cmd("cargo", &["xtask", "test", level, "--gate"], worktree_dir).unwrap_or(1)
}

fn commit_checkpoint(level: &str, status: &str, duration: i64, worktree_dir: &Path) {
    println!("Committing checkpoint for L{level}...");
    let commit_msg = format!("checkpoint: L{level} {status} ({duration}s)");
    let _ = run_cmd("git", &["add", "-A"], worktree_dir);
    let _ = run_cmd(
        "git",
        &["commit", "-m", &commit_msg, "--allow-empty"],
        worktree_dir,
    );
}

fn run_scoring(args: &RunAgentArgs, proj: &Path) -> Result<String> {
    if args.skip_bench {
        return Ok("skipped".to_string());
    }

    println!();
    println!("=== Scoring ===");
    let bench_log = proj.join("results/bench.log");
    let bench_exit = run_cmd(
        "cargo",
        &["xtask", "bench", &args.name, "--run-id", "bench"],
        proj,
    )
    .unwrap_or(1);

    if bench_log.is_file() {
        return Ok(extract_score_from_log(&bench_log));
    }
    if bench_exit == 0 {
        Ok("completed".to_string())
    } else {
        Ok("unknown".to_string())
    }
}

fn finalize_run(
    args: &RunAgentArgs, results_dir: &Path, session_uuid: &str,
    mode: &str, prompt: &str, start_time: &str, timestamp: &str,
    agent_exit: i32, score: &str, level_times: &[(String, i64, String)],
) -> Result<()> {
    let end_time = iso_now();
    let mut level_times_json = serde_json::Map::new();
    for (level, duration, status) in level_times {
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
    write_meta(&results_dir.join("meta.json"), &meta)
}

fn push_branch(name: &str, worktree_dir: &Path) {
    println!("Pushing branch to origin...");
    let push_exit = run_cmd("git", &["push", "-u", "origin", name], worktree_dir);
    match push_exit {
        Ok(0) => println!("Branch pushed: origin/{name}"),
        _ => println!("WARNING: Push to origin failed (no remote or auth issue)"),
    }
}

// ---------------------------------------------------------------------------
// Setup helpers
// ---------------------------------------------------------------------------

fn find_resume_dir(proj: &Path, strategy: &str, name: &str) -> Result<PathBuf> {
    let results_dir = proj.join("results");
    let prefix = format!("{strategy}_{name}_");

    let mut matching: Vec<PathBuf> = fs::read_dir(&results_dir)
        .into_iter()
        .flatten()
        .flatten()
        .filter(|e| {
            let fname = e.file_name().to_string_lossy().into_owned();
            fname.starts_with(&prefix) && e.path().is_dir()
        })
        .map(|e| e.path())
        .collect();
    matching.sort();
    matching.last().cloned().ok_or(Error::RunNotFound {
        path: results_dir.join(format!("{prefix}*")),
    })
}

fn clean_stale_worktree(proj: &Path, worktree_dir: &Path, name: &str) -> Result<()> {
    if worktree_dir.is_dir() {
        println!("--clean: removing worktree dir {}", worktree_dir.display());
        let _ = run_cmd("git", &["worktree", "remove", "--force", &worktree_dir.to_string_lossy()], proj);
        if worktree_dir.is_dir() {
            fs::remove_dir_all(worktree_dir).map_err(|e| Error::io(worktree_dir, e))?;
        }
    }
    let _ = run_cmd("git", &["worktree", "prune"], proj);
    let _ = run_cmd("git", &["branch", "-D", name], proj);
    Ok(())
}

fn setup_fresh_run(proj: &Path, args: &RunAgentArgs, worktree_dir: &Path) -> Result<PathBuf> {
    if args.clean {
        clean_stale_worktree(proj, worktree_dir, &args.name)?;
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
        if is_lock_held(lockfile) {
            let pid = fs::read_to_string(lockfile)
                .ok()
                .and_then(|c| c.trim().parse::<u32>().ok())
                .unwrap_or(0);
            return Err(Error::LockConflict {
                path: lockfile.to_path_buf(),
                pid,
            });
        }
        println!("WARNING: Stale lockfile found. Removing.");
        let _ = fs::remove_file(lockfile);
    }
    fs::write(lockfile, format!("{}", std::process::id())).map_err(|e| Error::io(lockfile, e))
}

fn is_lock_held(lockfile: &Path) -> bool {
    let Ok(content) = fs::read_to_string(lockfile) else {
        return false;
    };
    let Ok(pid) = content.trim().parse::<u32>() else {
        return false;
    };
    Path::new(&format!("/proc/{pid}")).exists()
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

fn tee_stdout_to_file(stdout: Option<std::process::ChildStdout>, out_path: &Path) {
    use std::io::{BufRead, BufReader, Write};
    let mut file = fs::File::create(out_path).expect("cannot create output file");
    let Some(stdout) = stdout else { return };
    let reader = BufReader::new(stdout);
    for line in reader.lines() {
        let Ok(line) = line else { break };
        println!("{line}");
        let _ = writeln!(file, "{line}");
    }
}

fn forward_stderr(stderr: Option<std::process::ChildStderr>) {
    use std::io::{BufRead, BufReader};
    let Some(stderr) = stderr else { return };
    let reader = BufReader::new(stderr);
    for line in reader.lines() {
        let Ok(line) = line else { break };
        eprintln!("{line}");
    }
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
        tee_stdout_to_file(stdout, &out_path);
    });

    let err_thread = std::thread::spawn(move || {
        forward_stderr(stderr);
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

use crate::model::turns_for_level;

fn append_file_listing(summary: &mut String, scheme_dir: &Path) {
    let Ok(entries) = fs::read_dir(scheme_dir) else { return };
    let mut files: Vec<(String, usize)> = entries
        .flatten()
        .filter_map(|e| {
            let name = e.file_name().to_string_lossy().into_owned();
            if !name.ends_with(".rs") {
                return None;
            }
            let content = fs::read_to_string(e.path()).ok()?;
            Some((name, content.lines().count()))
        })
        .collect();
    files.sort_by_key(|(_, lines)| *lines);
    for (name, lines) in &files {
        summary.push_str(&format!("  {lines:>5} {name}\n"));
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
    append_file_listing(&mut summary, &worktree_dir.join("bench/src/scheme"));

    summary.push_str("\nLevels already passing:\n");
    let level_num: u32 = level.parse().unwrap_or(1);
    for prev_num in 1..level_num {
        let prev = format!("{prev_num:02}");
        let status_file = results_dir.join(format!("L{prev}/status.txt"));
        let passed = fs::read_to_string(&status_file)
            .map(|c| c.contains("PASSED"))
            .unwrap_or(false);
        if passed {
            summary.push_str(&format!("  L{prev}: PASSED\n"));
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

/// Capture session with a custom output filename (for cleanup passes).
fn capture_session_as(agent: &str, session_id: &str, dest: &Path, filename: &str) {
    if agent != "claude" {
        return;
    }
    let home = std::env::var("HOME").unwrap_or_default();
    let projects_dir = PathBuf::from(&home).join(".claude/projects");
    let Some(found) = find_session_jsonl(&projects_dir, session_id) else {
        eprintln!("WARNING: Session JSONL not found for {session_id}");
        return;
    };
    let target = dest.join(filename);
    match fs::copy(&found, &target) {
        Ok(_) => println!("Session captured: {}", target.display()),
        Err(e) => eprintln!("WARNING: Failed to copy session: {e}"),
    }
}

fn capture_session(agent: &str, session_id: &str, dest: &Path) {
    match agent {
        "claude" => capture_session_as(agent, session_id, dest, "session.jsonl"),
        "codex" => capture_newest_session("codex", ".codex", "log", "session.log", dest),
        "opencode" => capture_newest_session("opencode", ".opencode", "json", "session.json", dest),
        _ => {}
    }
}

fn capture_newest_session(
    label: &str, home_subdir: &str, ext: &str, target_name: &str, dest: &Path,
) {
    let home = std::env::var("HOME").unwrap_or_default();
    let dir = PathBuf::from(&home).join(home_subdir);
    if !dir.is_dir() {
        return;
    }
    let Some(latest) = newest_file(&dir, ext) else {
        return;
    };
    let target = dest.join(target_name);
    let _ = fs::copy(&latest, &target);
    let label_cap = label.chars().next().unwrap_or('?').to_uppercase().to_string()
        + &label[1..];
    println!("{label_cap} session captured: {}", target.display());
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
        if path.extension().and_then(|e| e.to_str()) != Some(extension) {
            continue;
        }
        let Ok(meta) = path.metadata() else { continue };
        let Ok(modified) = meta.modified() else { continue };
        if best.as_ref().is_none_or(|(_, prev)| modified > *prev) {
            best = Some((path, modified));
        }
    }
    best.map(|(p, _)| p)
}

fn extract_score_from_log(log_path: &Path) -> String {
    let Ok(content) = fs::read_to_string(log_path) else {
        return "unknown".to_string();
    };
    for line in content.lines() {
        let Some(pos) = line.find("Score: ") else { continue };
        let rest = &line[pos + 7..];
        let score: String = rest
            .chars()
            .take_while(|c| c.is_ascii_digit() || *c == '/')
            .collect();
        if !score.is_empty() {
            return score;
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
        if !INTERRUPTED.load(Ordering::Relaxed) {
            return;
        }
        eprintln!("\n=== Cleaning up ===");

        if self.worktree_dir.is_dir() {
            commit_and_push_interrupted(&self.worktree_dir, &self.name);
        }

        let _ = fs::remove_file(&self.lockfile);
    }
}

fn commit_and_push_interrupted(worktree_dir: &Path, name: &str) {
    eprintln!("Committing agent work before exit...");
    let _ = run_cmd("git", &["add", "-A"], worktree_dir);
    let _ = run_cmd(
        "git",
        &["commit", "-m", "checkpoint: interrupted/cleanup", "--allow-empty"],
        worktree_dir,
    );
    let _ = run_cmd("git", &["push", "-u", "origin", name], worktree_dir);
}
