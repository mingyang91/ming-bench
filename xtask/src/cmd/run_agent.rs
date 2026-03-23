use crate::cmd::test_level;
use crate::codex;
use crate::model::{
    compact_timestamp, iso_now, output_tokens_for_level, project_dir, run_cmd, run_cmd_capture,
    uuid_v4, write_meta, Error, Lang, Result, LEVELS, SAFETY_MAX_TURNS, TOKEN_POLL_INTERVAL_SECS,
};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicBool, AtomicU32, AtomicU64, Ordering};
use std::time::{Duration, Instant};

static INTERRUPTED: AtomicBool = AtomicBool::new(false);
static CHILD_PID: AtomicU32 = AtomicU32::new(0);
/// Running output-token total, updated by the token monitor thread.
static TOKEN_TOTAL: AtomicU64 = AtomicU64::new(0);

/// Per-level timing entry: (label, duration_s, status).
type LevelTimes = Vec<(String, i64, String)>;

pub struct RunAgentArgs {
    pub base: String,
    pub strategy: String,
    pub name: String,
    pub prompt: Option<String>,
    pub model: Option<String>,
    pub agent: String,
    pub mode: String,
    pub max_turns: Option<u32>,
    pub max_tokens: Option<u64>,
    pub skip_bench: bool,
    pub resume: bool,
    pub from_level: Option<String>,
    pub clean: bool,
    pub lang: String,
}

const DEFAULT_PROMPT: &str = "Implement the Scheme interpreter by following CLAUDE.md exactly.
Work through levels 1 through 26 in order.
After implementing each level, run cargo xtask test NN to verify.
Fix failures before proceeding. Do not skip levels.";

pub fn run(args: RunAgentArgs) -> Result<()> {
    install_signal_handlers();
    let ctx = build_run_context(&args)?;
    prepare_run(&args, &ctx)?;
    let _cleanup_ctx = cleanup_context(&args, &ctx);
    let (agent_exit, level_times) = execute_mode(
        &ctx.mode,
        &args,
        &ctx.agent_workdir,
        &ctx.worktree_dir,
        &ctx.results_dir,
        &ctx.prompt,
        &ctx.session_uuid,
    )?;
    let score = run_scoring(&args, &ctx.proj)?;
    finalize_agent_run(&args, &ctx, agent_exit, &score, &level_times)?;
    Ok(())
}

struct RunContext {
    proj: PathBuf,
    session_uuid: String,
    timestamp: String,
    prompt: String,
    mode: String,
    worktree_dir: PathBuf,
    agent_workdir: PathBuf,
    results_dir: PathBuf,
    start_time: String,
    lockfile: PathBuf,
}

fn build_run_context(args: &RunAgentArgs) -> Result<RunContext> {
    let proj = project_dir();
    let session_uuid = uuid_v4();
    let timestamp = compact_timestamp();
    let prompt = args
        .prompt
        .clone()
        .unwrap_or_else(|| DEFAULT_PROMPT.to_string());
    let worktree_dir = proj
        .parent()
        .expect("project has no parent dir")
        .join("workspace")
        .join(&args.name);
    let parsed_lang =
        crate::model::Lang::from_str(&args.lang).map_err(|msg| Error::CommandFailed {
            cmd: msg,
            exit_code: 1,
        })?;
    let agent_workdir = worktree_dir.join("bench").join(parsed_lang.dir_name());
    let results_dir = if args.resume {
        find_resume_dir(&proj, &args.strategy, &args.name)?
    } else {
        setup_fresh_run(&proj, args, &worktree_dir)?
    };
    let start_time = resume_start_time(args.resume, &results_dir);
    let lockfile = results_dir.join(".run.lock");

    Ok(RunContext {
        proj,
        session_uuid,
        timestamp,
        prompt,
        mode: args.mode.clone(),
        worktree_dir,
        agent_workdir,
        results_dir,
        start_time,
        lockfile,
    })
}

fn prepare_run(args: &RunAgentArgs, ctx: &RunContext) -> Result<()> {
    if !args.resume {
        write_initial_meta(
            args,
            &ctx.results_dir,
            &ctx.session_uuid,
            &ctx.mode,
            &ctx.prompt,
            &ctx.start_time,
            &ctx.timestamp,
        )?;
    }

    print_run_banner(
        args,
        &ctx.session_uuid,
        &ctx.results_dir,
        &ctx.worktree_dir,
        &ctx.mode,
    );

    if !args.resume {
        create_worktree(args, &ctx.proj, &ctx.worktree_dir, &ctx.agent_workdir)?;
    }

    warm_cache(&ctx.worktree_dir);
    acquire_lock(&ctx.lockfile)
}

fn cleanup_context(args: &RunAgentArgs, ctx: &RunContext) -> CleanupContext {
    CleanupContext {
        worktree_dir: ctx.worktree_dir.clone(),
        name: args.name.clone(),
        lockfile: ctx.lockfile.clone(),
    }
}

fn finalize_agent_run(
    args: &RunAgentArgs,
    ctx: &RunContext,
    agent_exit: i32,
    score: &str,
    level_times: &LevelTimes,
) -> Result<()> {
    finalize_run(
        args,
        &ctx.results_dir,
        &FinalizeContext {
            session_uuid: &ctx.session_uuid,
            mode: &ctx.mode,
            prompt: &ctx.prompt,
            start_time: &ctx.start_time,
            timestamp: &ctx.timestamp,
            agent_exit,
            score,
            level_times,
        },
    )?;

    push_branch(&args.name, &ctx.worktree_dir);
    let _ = fs::remove_file(&ctx.lockfile);

    println!();
    println!("=== Run complete ===");
    println!("Results: {}", ctx.results_dir.display());
    println!("Score:   {score}");
    Ok(())
}

fn write_initial_meta(
    args: &RunAgentArgs,
    results_dir: &Path,
    session_uuid: &str,
    mode: &str,
    prompt: &str,
    start_time: &str,
    timestamp: &str,
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
    args: &RunAgentArgs,
    session_uuid: &str,
    results_dir: &Path,
    worktree_dir: &Path,
    mode: &str,
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
    args: &RunAgentArgs,
    proj: &Path,
    worktree_dir: &Path,
    agent_workdir: &Path,
) -> Result<()> {
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
    // Look for per-language strategy first, fallback to base.
    // Strategies live at bench/strategies/ — one level up from bench/{lang}/.
    let lang = crate::model::Lang::from_str(&args.lang).unwrap_or(crate::model::Lang::Rust);
    let bench_dir = agent_workdir.parent().expect("agent_workdir has parent");

    let lang_rel = format!("strategies/{}/{}.md", lang.dir_name(), args.strategy);
    let base_rel = format!("strategies/{}.md", args.strategy);

    // Check which file exists on the filesystem (bench_dir = bench/)
    let strategy_rel = if bench_dir.join(&lang_rel).is_file() {
        lang_rel
    } else if bench_dir.join(&base_rel).is_file() {
        base_rel
    } else {
        return Err(Error::CommandFailed {
            cmd: format!(
                "strategy file not found: {} or {} (in {})",
                lang_rel,
                base_rel,
                bench_dir.display()
            ),
            exit_code: 1,
        });
    };

    // Symlink target is relative to agent_workdir (bench/{lang}/), so prepend ../
    let symlink_target = format!("../{strategy_rel}");

    let claude_md = agent_workdir.join("CLAUDE.md");
    let _ = fs::remove_file(&claude_md);
    std::os::unix::fs::symlink(&symlink_target, &claude_md)
        .map_err(|e| Error::io(&claude_md, e))?;
    let agents_md = agent_workdir.join("AGENTS.md");
    let _ = fs::remove_file(&agents_md);
    std::os::unix::fs::symlink("CLAUDE.md", &agents_md).map_err(|e| Error::io(&agents_md, e))?;
    println!("Strategy:   {strategy_rel} → CLAUDE.md");
    Ok(())
}

fn copy_strategy_clippy(args: &RunAgentArgs, agent_workdir: &Path) -> Result<()> {
    let bench_dir = agent_workdir.parent().expect("agent_workdir has parent");
    let strategy_clippy = bench_dir.join(format!("strategies/{}.clippy.toml", args.strategy));
    let clippy_toml = agent_workdir.join("clippy.toml");
    if strategy_clippy.is_file() {
        fs::copy(&strategy_clippy, &clippy_toml).map_err(|e| Error::io(&clippy_toml, e))?;
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
    mode: &str,
    args: &RunAgentArgs,
    agent_workdir: &Path,
    worktree_dir: &Path,
    results_dir: &Path,
    prompt: &str,
    session_uuid: &str,
) -> Result<(i32, LevelTimes)> {
    if mode == "levels" {
        run_levels_mode(args, agent_workdir, worktree_dir, results_dir)
    } else if mode == "full" {
        let exit = run_full_mode(
            args,
            agent_workdir,
            worktree_dir,
            results_dir,
            prompt,
            session_uuid,
        );
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
    args: &RunAgentArgs,
    agent_workdir: &Path,
    worktree_dir: &Path,
    results_dir: &Path,
    prompt: &str,
    session_uuid: &str,
) -> i32 {
    println!("=== Full run mode ===");
    let output_file = results_dir.join("agent-output.txt");

    let budget = BudgetOpts {
        token_budget: args.max_tokens,
        level_dir: Some(results_dir.to_path_buf()),
    };
    let agent_exit = launch_agent(
        &args.agent,
        agent_workdir,
        prompt,
        session_uuid,
        &output_file,
        args.max_turns,
        args.model.as_deref(),
        &budget,
    );

    println!();
    println!("Agent exited with code: {agent_exit}");

    let _ = run_cmd("git", &["add", "bench/"], worktree_dir);
    let _ = run_cmd(
        "git",
        &["commit", "-m", "agent: full run complete", "--allow-empty"],
        worktree_dir,
    );

    capture_session(&args.agent, session_uuid, &output_file, results_dir);
    agent_exit
}

// ---------------------------------------------------------------------------
// Regression checking
// ---------------------------------------------------------------------------

// Regression fix uses output_tokens_for_level() — same budget as coding.

/// Run all previously-passed levels against the current worktree code.
/// Returns a list of (level_label, test_output) for any that now fail.
fn regression_check(
    passed_levels: &[&str],
    lang: &Lang,
    worktree_dir: &Path,
) -> Result<Vec<(String, String)>> {
    let mut failures = Vec::new();
    for level in passed_levels {
        let (exit_code, output) = test_level::run_level_capture(level, lang, worktree_dir)?;
        if exit_code != 0 {
            failures.push((format!("L{level}"), output));
        }
    }
    Ok(failures)
}

/// Build a prompt telling the agent which levels regressed and asking it to fix them.
fn build_regression_prompt(
    regressions: &[(String, String)],
    current_level: &str,
    lang: &str,
) -> String {
    let mut prompt = format!(
        "REGRESSION DETECTED: Your level {current_level} changes broke earlier tests.\n\
         Fix the regressions below while keeping level {current_level} tests passing.\n\
         Do NOT remove or skip any tests. Do NOT make all strings immutable.\n\n"
    );
    for (level, output) in regressions {
        prompt.push_str(&format!("=== {level} FAILING ===\n"));
        // Include last 40 lines of test output (enough for failure details)
        let lines: Vec<&str> = output.lines().collect();
        let start = lines.len().saturating_sub(40);
        for line in &lines[start..] {
            prompt.push_str(line);
            prompt.push('\n');
        }
        prompt.push('\n');
    }
    prompt.push_str(&format!(
        "Run `cargo xtask test <NN> --lang {lang}` for each failing level to verify your fix.\n\
         Then run `cargo xtask test {current_level} --lang {lang}` to confirm no new breakage."
    ));
    prompt
}

/// Launch an agent pass to fix regressions, returning the agent exit code.
fn run_regression_fix(
    args: &RunAgentArgs,
    agent_workdir: &Path,
    level_dir: &Path,
    regressions: &[(String, String)],
    current_level: &str,
    token_budget: u64,
) -> i32 {
    let prompt = build_regression_prompt(regressions, current_level, &args.lang);
    let regression_uuid = uuid_v4();
    let output_file = level_dir.join("agent-output-regression.txt");

    let budget = BudgetOpts {
        token_budget: Some(token_budget),
        level_dir: Some(level_dir.to_path_buf()),
    };
    let agent_exit = launch_agent(
        &args.agent,
        agent_workdir,
        &prompt,
        &regression_uuid,
        &output_file,
        None, // safety-net turns only
        args.model.as_deref(),
        &budget,
    );

    capture_session_as(
        &args.agent,
        &regression_uuid,
        &output_file,
        level_dir,
        "session-regression.jsonl",
    );

    agent_exit
}

fn maybe_run_gate_cleanup(
    args: &RunAgentArgs,
    agent_workdir: &Path,
    worktree_dir: &Path,
    level_dir: &Path,
    level: &str,
) {
    if !args.strategy.contains("quality-gate") {
        return;
    }
    let gate_exit = run_quality_gate_cleanup(args, agent_workdir, worktree_dir, level_dir, level);
    if gate_exit != 0 {
        println!("Quality gate cleanup failed for level {level} (non-blocking)");
    }
}

enum RegCheckOutcome {
    NoPriorLevels,
    Clean,
    Fixed,
    Broken,
}

fn check_and_fix_regressions(
    args: &RunAgentArgs,
    agent_workdir: &Path,
    worktree_dir: &Path,
    level_dir: &Path,
    level: &str,
    passed_levels: &[&str],
    lang: &Lang,
) -> Result<RegCheckOutcome> {
    if passed_levels.is_empty() {
        return Ok(RegCheckOutcome::NoPriorLevels);
    }

    println!();
    println!(
        "--- Regression check: L01..L{} ---",
        passed_levels.last().expect("passed_levels is non-empty")
    );
    let regressions = regression_check(passed_levels, lang, worktree_dir)?;

    if regressions.is_empty() {
        println!("No regressions");
        return Ok(RegCheckOutcome::Clean);
    }

    let regressed_names: Vec<&str> = regressions.iter().map(|(l, _)| l.as_str()).collect();
    println!("WARNING: Regressions detected in: {}", regressed_names.join(", "));

    let level_num: u32 = level.parse().expect("level should be a number");
    let fix_budget = output_tokens_for_level(level_num, args.max_tokens);
    println!("--- Regression fix pass ({fix_budget} output tokens) ---");
    let _fix_exit = run_regression_fix(args, agent_workdir, level_dir, &regressions, level, fix_budget);

    // Only re-test the originally regressed levels + the current level (not ALL passed levels).
    // This avoids flaky timeouts on unrelated levels and is faster.
    let regressed_level_ids: Vec<&str> = regressions
        .iter()
        .map(|(l, _)| l.strip_prefix('L').unwrap_or(l.as_str()))
        .collect();
    let mut verify_levels: Vec<&str> = regressed_level_ids;
    verify_levels.push(level);
    verify_levels.sort();
    verify_levels.dedup();

    println!(
        "--- Post-fix verify: {} ---",
        verify_levels.iter().map(|l| format!("L{l}")).collect::<Vec<_>>().join(", ")
    );
    let still_broken = regression_check(&verify_levels, lang, worktree_dir)?;

    if still_broken.is_empty() {
        println!("Regressions fixed successfully");
        commit_checkpoint(level, "REGFIX", 0, worktree_dir);
        return Ok(RegCheckOutcome::Fixed);
    }

    let broken_names: Vec<&str> = still_broken.iter().map(|(l, _)| l.as_str()).collect();
    println!("Regressions unfixed: {} — halting run", broken_names.join(", "));
    let status_msg = format!("Level {level} REGRESSION (regressions in {})", broken_names.join(", "));
    let _ = fs::write(level_dir.join("status.txt"), &status_msg);
    commit_checkpoint(level, "REGRESSION", 0, worktree_dir);
    Ok(RegCheckOutcome::Broken)
}

// ---------------------------------------------------------------------------
// Level-by-level mode
// ---------------------------------------------------------------------------

fn run_levels_mode(
    args: &RunAgentArgs,
    agent_workdir: &Path,
    worktree_dir: &Path,
    results_dir: &Path,
) -> Result<(i32, LevelTimes)> {
    println!("=== Level-by-level mode ===");

    let start_level: u32 = args
        .from_level
        .as_ref()
        .and_then(|s| s.parse().ok())
        .unwrap_or(1);

    let lang = Lang::from_str(&args.lang).map_err(|msg| Error::CommandFailed {
        cmd: msg,
        exit_code: 1,
    })?;

    let mut agent_exit = 0i32;
    let mut level_times: Vec<(String, i64, String)> = Vec::new();
    let mut passed_levels: Vec<&str> = Vec::new();

    for level in &LEVELS {
        if INTERRUPTED.load(Ordering::Relaxed) {
            println!("Interrupted — stopping.");
            break;
        }

        let level_num: u32 = level.parse().expect("level constant not a number");
        if level_num < start_level {
            println!("Skipping L{level} (before --from-level {start_level})");
            // Still track as passed for regression checks
            passed_levels.push(level);
            continue;
        }

        let level_dir = results_dir.join(format!("L{level}"));
        if should_skip_level(args.resume, &level_dir, level) {
            passed_levels.push(level);
            continue;
        }

        let result = run_single_level(args, agent_workdir, worktree_dir, &level_dir, level)?;
        agent_exit = result.0;
        level_times.push((format!("L{level}"), result.1, result.2.clone()));

        if result.2 == "FAILED" {
            commit_checkpoint(level, &result.2, result.1, worktree_dir);
            println!("Level {level} FAILED — stopping");
            break;
        }

        // --- Step 1: Regression check (BEFORE quality gate) ---
        if let RegCheckOutcome::Broken = check_and_fix_regressions(
            args, agent_workdir, worktree_dir, &level_dir, level, &passed_levels, &lang,
        )? {
            if let Some(last) = level_times.last_mut() {
                last.2 = "REGRESSION".to_string();
            }
            break;
        }

        // --- Step 2: Quality gate cleanup (AFTER regression is clean) ---
        maybe_run_gate_cleanup(args, agent_workdir, worktree_dir, &level_dir, level);

        commit_checkpoint(level, &result.2, result.1, worktree_dir);
        passed_levels.push(level);
    }

    // --- Surprise levels: inject L27-L28 after L26 passes ---
    if passed_levels.len() == LEVELS.len() && !INTERRUPTED.load(Ordering::Relaxed) {
        let proj = project_dir();
        let surprise_levels = run_surprise_levels(
            args, agent_workdir, worktree_dir, results_dir, &proj, &lang,
            &mut passed_levels, &mut level_times, &mut agent_exit,
        )?;
        if !surprise_levels {
            println!("Surprise levels: not all passed");
        }
    }

    Ok((agent_exit, level_times))
}

// ---------------------------------------------------------------------------
// Surprise levels (L27-L28) — injected after L26 passes
// ---------------------------------------------------------------------------

/// Hidden levels that test tech debt accumulated during L01-L26.
/// The agent has no prior knowledge of these requirements.
const SURPRISE_LEVELS: &[(&str, &str)] = &[
    ("27", "Concurrent Evaluation"),
    ("28", "Performance & Memory Stress"),
];

/// Inject hidden test files into the agent's worktree and run surprise levels.
fn run_surprise_levels(
    args: &RunAgentArgs,
    agent_workdir: &Path,
    worktree_dir: &Path,
    results_dir: &Path,
    proj: &Path,
    lang: &Lang,
    _passed_levels: &mut Vec<&str>,
    level_times: &mut Vec<(String, i64, String)>,
    agent_exit: &mut i32,
) -> Result<bool> {
    let hidden_dir = proj.join("bench/hidden");
    if !hidden_dir.is_dir() {
        println!("No hidden levels found at {}", hidden_dir.display());
        return Ok(false);
    }

    println!();
    println!("=== SURPRISE: Hidden levels unlocked after L26 ===");

    for &(level, name) in SURPRISE_LEVELS {
        if INTERRUPTED.load(Ordering::Relaxed) {
            break;
        }

        println!();
        println!("--- Injecting L{level}: {name} ---");

        // Inject test files and fixtures for this level
        inject_surprise_level(level, lang, worktree_dir, &hidden_dir)?;

        // Append spec text to SPEC.md in the worktree
        let spec_file = hidden_dir.join(format!("SPEC_L{level}.md"));
        if spec_file.is_file() {
            let spec_text = fs::read_to_string(&spec_file).unwrap_or_default();
            let worktree_spec = worktree_dir.join("bench/SPEC.md");
            if let Ok(mut existing) = fs::read_to_string(&worktree_spec) {
                existing.push('\n');
                existing.push_str(&spec_text);
                let _ = fs::write(&worktree_spec, existing);
            }
        }

        let level_dir = results_dir.join(format!("L{level}"));
        if should_skip_level(args.resume, &level_dir, level) {
            // leaked: need 'static lifetime for passed_levels
            // just skip tracking for surprise levels
            continue;
        }

        let result = run_single_level(args, agent_workdir, worktree_dir, &level_dir, level)?;
        *agent_exit = result.0;
        level_times.push((format!("L{level}"), result.1, result.2.clone()));

        if result.2 == "FAILED" {
            commit_checkpoint(level, &result.2, result.1, worktree_dir);
            println!("Surprise L{level} FAILED — stopping");
            return Ok(false);
        }

        commit_checkpoint(level, &result.2, result.1, worktree_dir);
    }

    Ok(true)
}

/// Copy hidden test files into the agent's worktree for a surprise level.
fn inject_surprise_level(
    level: &str,
    lang: &Lang,
    worktree_dir: &Path,
    hidden_dir: &Path,
) -> Result<()> {
    let bench_dir = worktree_dir.join("bench");

    // --- L27: language-specific test files ---
    if level == "27" {
        match lang {
            Lang::Rust => {
                // Copy level27.rs and add mod declaration
                let src = hidden_dir.join("rust/level27.rs");
                let dst = bench_dir.join("rust/src/scheme/tests/level27.rs");
                if src.is_file() {
                    let _ = fs::copy(&src, &dst);
                    // Append mod declaration to mod.rs
                    let mod_rs = bench_dir.join("rust/src/scheme/tests/mod.rs");
                    if let Ok(mut content) = fs::read_to_string(&mod_rs) {
                        if !content.contains("mod level27") {
                            content.push_str("\n// Level 27 (concurrent eval) — injected as surprise level.\nmod level27;\n");
                            let _ = fs::write(&mod_rs, content);
                        }
                    }
                }
            }
            Lang::Java => {
                // Copy L27Tests.java as a standalone main class (not JUnit)
                let src = hidden_dir.join("java/L27Tests.java");
                let dst = bench_dir.join("java/src/main/java/ming/L27Tests.java");
                if src.is_file() {
                    let _ = fs::create_dir_all(dst.parent().expect("has parent"));
                    let _ = fs::copy(&src, &dst);
                    // Force clean so shadowJar includes the new class
                    let _ = run_cmd("./gradlew", &["clean"], &bench_dir.join("java"));
                }
            }
            Lang::Scala => {
                let src = hidden_dir.join("scala/ConcurrencySpec.scala");
                let dst = bench_dir.join("scala/src/test/scala/ming/ConcurrencySpec.scala");
                if src.is_file() {
                    let _ = fs::create_dir_all(dst.parent().expect("has parent"));
                    let _ = fs::copy(&src, &dst);
                }
            }
            Lang::Go => {
                let src = hidden_dir.join("go/concurrency_test.go");
                let dst = bench_dir.join("go/concurrency_test.go");
                if src.is_file() {
                    let _ = fs::copy(&src, &dst);
                }
            }
            Lang::TypeScript => {
                let src = hidden_dir.join("ts/concurrent.test.ts");
                let dst = bench_dir.join("ts/test/concurrent.test.ts");
                if src.is_file() {
                    let _ = fs::create_dir_all(dst.parent().expect("has parent"));
                    let _ = fs::copy(&src, &dst);
                }
            }
        }
    }

    // --- L28: fixtures + tests.json entries ---
    if level == "28" {
        // Copy L28 fixtures
        let fixtures_src = hidden_dir.join("fixtures");
        let fixtures_dst = bench_dir.join("fixtures");
        if fixtures_src.is_dir() {
            for entry in fs::read_dir(&fixtures_src).into_iter().flatten() {
                if let Ok(entry) = entry {
                    let name = entry.file_name();
                    let _ = fs::copy(entry.path(), fixtures_dst.join(&name));
                }
            }
        }

        // Append L28 test entries to tests.json
        let extra_tests_path = hidden_dir.join("tests_l28.json");
        let tests_json_path = bench_dir.join("tests.json");
        if extra_tests_path.is_file() && tests_json_path.is_file() {
            if let (Ok(main_str), Ok(extra_str)) = (
                fs::read_to_string(&tests_json_path),
                fs::read_to_string(&extra_tests_path),
            ) {
                // Parse both as JSON arrays and merge
                if let (Ok(mut main_arr), Ok(extra_arr)) = (
                    serde_json::from_str::<Vec<serde_json::Value>>(&main_str),
                    serde_json::from_str::<Vec<serde_json::Value>>(&extra_str),
                ) {
                    main_arr.extend(extra_arr);
                    if let Ok(merged) = serde_json::to_string_pretty(&main_arr) {
                        let _ = fs::write(&tests_json_path, merged);
                    }
                }
            }
        }
    }

    println!("Injected L{level} test files into worktree");
    Ok(())
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

/// Parse duration in seconds from status text like "Level NN PASSED (123s)".
fn parse_status_duration(status_text: &str) -> i64 {
    status_text
        .split('(')
        .nth(1)
        .and_then(|s| s.split('s').next())
        .and_then(|s| s.parse().ok())
        .unwrap_or(0)
}

/// Maximum number of automatic retries when the agent fails for infrastructure
/// reasons (API timeout, 529, crash) rather than exhausting its turn budget.
const MAX_INFRA_RETRIES: u32 = 2;

/// Check whether the agent output indicates it ran out of turns (not retryable)
/// vs an infrastructure failure like timeout/529/crash (retryable).
fn agent_exhausted_budget(output_file: &Path, level_dir: &Path) -> bool {
    // Check token-exhausted marker (written by monitor thread)
    if level_dir.join("token-exhausted.txt").exists() {
        return true;
    }
    // Fallback: check if Claude hit the safety-net turn limit
    fs::read_to_string(output_file)
        .map(|c| c.contains("Reached max turns"))
        .unwrap_or(false)
}

/// Returns (agent_exit, duration_secs, status_label).
fn run_single_level(
    args: &RunAgentArgs,
    agent_workdir: &Path,
    worktree_dir: &Path,
    level_dir: &Path,
    level: &str,
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
        let token_budget = output_tokens_for_level(level_num, args.max_tokens);
        let output_file = level_dir.join("agent-output.txt");

        if attempt == 0 {
            println!();
            println!("--- Level {level} (budget {token_budget} output tokens) ---");
        } else {
            println!();
            println!("--- Level {level} RETRY {attempt}/{MAX_INFRA_RETRIES} (budget {token_budget} output tokens) ---");
        }

        let level_prompt = build_level_prompt(
            level,
            worktree_dir,
            level_dir.parent().expect("level_dir has parent"),
        );
        let budget = BudgetOpts {
            token_budget: Some(token_budget),
            level_dir: Some(level_dir.to_path_buf()),
        };
        let agent_exit = launch_agent(
            &args.agent,
            agent_workdir,
            &level_prompt,
            &level_uuid,
            &output_file,
            None, // safety-net turns only
            args.model.as_deref(),
            &budget,
        );

        capture_session(&args.agent, &level_uuid, &output_file, level_dir);

        let test_exit = run_level_tests(worktree_dir, level, &args.lang);

        // Quality gate cleanup moved to run_levels_mode() — after regression check.

        if test_exit == 0 {
            // Success
            let level_duration = level_start.elapsed().as_secs() as i64;
            let status_msg = format!("Level {level} PASSED ({level_duration}s)");
            println!("{status_msg}");
            let _ = fs::write(level_dir.join("status.txt"), &status_msg);
            return Ok((agent_exit, level_duration, "PASSED".to_string()));
        }

        // Failed — decide whether to retry
        let exhausted = agent_exhausted_budget(&level_dir.join("agent-output.txt"), level_dir);
        let should_stop = exhausted || attempt >= MAX_INFRA_RETRIES;

        if should_stop {
            let reason = failure_reason(exhausted);
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

fn failure_reason(exhausted: bool) -> &'static str {
    if exhausted {
        "turns exhausted"
    } else {
        "max retries reached"
    }
}

fn run_level_tests(worktree_dir: &Path, level: &str, lang: &str) -> i32 {
    // Tests only — no quality gate. Gate is enforced in the cleanup pass.
    // Use the host's xtask binary to avoid worktree xtask version mismatch.
    run_host_xtask(&["test", level, "--lang", lang], worktree_dir).unwrap_or(1)
}

/// Run the host's pre-built xtask binary with PROJECT_DIR pointing to the worktree.
///
/// Worktrees contain old source code, so `cargo xtask` inside a worktree would build
/// an outdated xtask that may lack new flags (e.g. --lang, --gate). Instead we run the
/// host's binary directly and set PROJECT_DIR so `project_dir()` resolves to the worktree.
fn run_host_xtask(args: &[&str], worktree_dir: &Path) -> Result<i32> {
    let host_bin = project_dir().join("target").join("debug").join("xtask");
    if !host_bin.is_file() {
        return Err(Error::BinaryNotFound {
            name: format!("host xtask at {}", host_bin.display()),
        });
    }
    let status = Command::new(&host_bin)
        .args(args)
        .current_dir(worktree_dir)
        .env("PROJECT_DIR", worktree_dir)
        .status()
        .map_err(|e| Error::Io {
            path: host_bin,
            source: e,
        })?;
    Ok(status.code().unwrap_or(1))
}

fn run_quality_gate_cleanup(
    args: &RunAgentArgs,
    agent_workdir: &Path,
    worktree_dir: &Path,
    level_dir: &Path,
    level: &str,
) -> i32 {
    use crate::model::GATE_CLEANUP_TOKEN_BUDGET;
    println!("--- Level {level} cleanup (budget {GATE_CLEANUP_TOKEN_BUDGET} output tokens) ---");
    let cleanup_uuid = uuid_v4();
    let cleanup_prompt = format!(
        "Level {level} tests pass. Fix any quality-gate warnings.\n\
         Run `cargo xtask test {level} --lang {} --gate` to verify. Do not change test behavior.",
        args.lang
    );
    let output_file = level_dir.join("agent-output-cleanup.txt");
    let budget = BudgetOpts {
        token_budget: Some(GATE_CLEANUP_TOKEN_BUDGET),
        level_dir: Some(level_dir.to_path_buf()),
    };
    let _cleanup_exit = launch_agent(
        &args.agent,
        agent_workdir,
        &cleanup_prompt,
        &cleanup_uuid,
        &output_file,
        None, // safety-net turns only
        args.model.as_deref(),
        &budget,
    );
    capture_session_as(
        &args.agent,
        &cleanup_uuid,
        &output_file,
        level_dir,
        "session-cleanup.jsonl",
    );

    run_host_xtask(
        &["test", level, "--lang", &args.lang, "--gate"],
        worktree_dir,
    )
    .unwrap_or(1)
}

fn commit_checkpoint(level: &str, status: &str, duration: i64, worktree_dir: &Path) {
    println!("Committing checkpoint for L{level}...");
    let commit_msg = format!("checkpoint: L{level} {status} ({duration}s)");
    let _ = run_cmd("git", &["add", "bench/"], worktree_dir);
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
        &["xtask", "bench", &args.name, "--run-id", "bench", "--lang", &args.lang],
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

struct FinalizeContext<'a> {
    session_uuid: &'a str,
    mode: &'a str,
    prompt: &'a str,
    start_time: &'a str,
    timestamp: &'a str,
    agent_exit: i32,
    score: &'a str,
    level_times: &'a [(String, i64, String)],
}

fn finalize_run(args: &RunAgentArgs, results_dir: &Path, ctx: &FinalizeContext) -> Result<()> {
    let end_time = iso_now();

    // Build level_times from disk first (covers original run + previous resumes),
    // then overlay the current session's in-memory data (more accurate for just-run levels).
    let mut level_times_json = serde_json::Map::new();
    for entry in fs::read_dir(results_dir).into_iter().flatten().flatten() {
        let name = entry.file_name().to_string_lossy().into_owned();
        if !name.starts_with('L') || !entry.path().is_dir() {
            continue;
        }
        let Ok(content) = fs::read_to_string(entry.path().join("status.txt")) else {
            continue;
        };
        let duration = parse_status_duration(&content);
        let status = if content.contains("REGRESSION") {
            "REGRESSION"
        } else if content.contains("PASSED") {
            "PASSED"
        } else {
            "FAILED"
        };
        level_times_json.insert(
            name,
            serde_json::json!({"duration_s": duration, "status": status}),
        );
    }
    // Overlay current session's level_times (more accurate timing for just-run levels)
    for (level, duration, status) in ctx.level_times {
        level_times_json.insert(
            level.clone(),
            serde_json::json!({"duration_s": duration, "status": status}),
        );
    }

    let meta = serde_json::json!({
        "base": args.base,
        "name": args.name,
        "session_id": ctx.session_uuid,
        "agent": args.agent,
        "mode": ctx.mode,
        "model": args.model.as_deref().unwrap_or("default"),
        "prompt": ctx.prompt,
        "start_time": ctx.start_time,
        "timestamp": ctx.timestamp,
        "end_time": end_time,
        "exit_code": ctx.agent_exit,
        "score": ctx.score,
        "level_times": level_times_json,
    });
    write_meta(&results_dir.join("meta.json"), &meta)
}

/// On resume, preserve the original start_time from meta.json so that
/// elapsed time reflects the full run, not just the resumed portion.
fn resume_start_time(resume: bool, results_dir: &Path) -> String {
    if !resume {
        return iso_now();
    }
    fs::read_to_string(results_dir.join("meta.json"))
        .ok()
        .and_then(|c| serde_json::from_str::<serde_json::Value>(&c).ok())
        .and_then(|v| v["start_time"].as_str().map(String::from))
        .unwrap_or_else(iso_now)
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
        let _ = run_cmd(
            "git",
            &[
                "worktree",
                "remove",
                "--force",
                &worktree_dir.to_string_lossy(),
            ],
            proj,
        );
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

/// Options for token-based budget enforcement.
struct BudgetOpts {
    /// Output token budget (None = unlimited).
    token_budget: Option<u64>,
    /// Path to write "token-exhausted.txt" marker if budget exceeded.
    level_dir: Option<PathBuf>,
}

#[allow(clippy::too_many_arguments)]
fn launch_agent(
    agent: &str,
    workdir: &Path,
    prompt: &str,
    session_id: &str,
    output_file: &Path,
    max_turns: Option<u32>,
    model: Option<&str>,
    budget: &BudgetOpts,
) -> i32 {
    let result = match agent {
        "claude" => launch_claude(workdir, prompt, session_id, output_file, max_turns, model, budget),
        "codex" => launch_codex(workdir, prompt, output_file, budget),
        "opencode" => launch_opencode(workdir, prompt, output_file, max_turns),
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
    budget: &BudgetOpts,
) -> Result<i32> {
    let claude_bin = resolve_agent_binary("claude")?;

    let safety_turns = max_turns.unwrap_or(SAFETY_MAX_TURNS);
    let mut cmd_args: Vec<String> = vec![
        "-p".to_string(),
        "--session-id".to_string(),
        session_id.to_string(),
        "--dangerously-skip-permissions".to_string(),
        "--max-turns".to_string(),
        safety_turns.to_string(),
    ];
    if let Some(m) = model {
        cmd_args.push("--model".to_string());
        cmd_args.push(m.to_string());
    }
    cmd_args.push(prompt.to_string());

    // Start token monitor for Claude (polls session.jsonl)
    let monitor = budget.token_budget.map(|b| {
        let sid = session_id.to_string();
        let level_dir = budget.level_dir.clone();
        TOKEN_TOTAL.store(0, Ordering::Relaxed);
        std::thread::spawn(move || monitor_claude_tokens(&sid, b, level_dir.as_deref()))
    });

    let args_ref: Vec<&str> = cmd_args.iter().map(|s| s.as_str()).collect();
    let result = run_agent_with_tee(&claude_bin, &args_ref, workdir, output_file);

    if let Some(handle) = monitor {
        let _ = handle.join();
    }
    result
}

/// Resolve an agent binary name to an absolute path.
/// Checks PATH first, then common locations (~/.local/bin, ~/.cargo/bin).
fn resolve_agent_binary(name: &str) -> Result<String> {
    // Try PATH first (works in interactive terminals)
    if let Ok(output) = Command::new("which").arg(name).output() {
        let path = String::from_utf8_lossy(&output.stdout).trim().to_string();
        if output.status.success() && !path.is_empty() {
            return Ok(path);
        }
    }
    // Fallback: check common locations
    let home = std::env::var("HOME").unwrap_or_default();
    for dir in &[".local/bin", ".cargo/bin"] {
        let candidate = PathBuf::from(&home).join(dir).join(name);
        if candidate.is_file() {
            return Ok(candidate.to_string_lossy().into_owned());
        }
    }
    Err(Error::BinaryNotFound {
        name: format!(
            "{name} not found in PATH — ensure ~/.local/bin and ~/.cargo/bin are in PATH"
        ),
    })
}

fn launch_codex(
    workdir: &Path,
    prompt: &str,
    output_file: &Path,
    budget: &BudgetOpts,
) -> Result<i32> {
    // Use --json for structured JSONL output so we can monitor token usage
    let json_flag = if budget.token_budget.is_some() {
        " --json"
    } else {
        ""
    };
    let script_cmd = format!(
        "cd '{}' && codex exec --full-auto{json_flag} '{}'",
        workdir.display(),
        prompt.replace('\'', "'\\''"),
    );
    let out_str = output_file.to_str().expect("output file path not utf8");

    // Start token monitor for Codex (scans agent-output.txt for token_count events)
    let monitor = budget.token_budget.map(|b| {
        let out_path = output_file.to_path_buf();
        let level_dir = budget.level_dir.clone();
        TOKEN_TOTAL.store(0, Ordering::Relaxed);
        std::thread::spawn(move || monitor_codex_tokens(&out_path, b, level_dir.as_deref()))
    });

    let result = run_cmd("script", &["-qec", &script_cmd, out_str], workdir);

    if let Some(handle) = monitor {
        let _ = handle.join();
    }
    result
}

fn launch_opencode(
    workdir: &Path,
    prompt: &str,
    output_file: &Path,
    max_turns: Option<u32>,
) -> Result<i32> {
    // OpenCode has no structured token output — use turn-based fallback.
    let turn_limit = max_turns.unwrap_or(SAFETY_MAX_TURNS);
    eprintln!("NOTE: OpenCode uses turn-based limit ({turn_limit} turns) — no token monitoring");
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
                Error::BinaryNotFound {
                    name: cmd.to_string(),
                }
            } else {
                Error::Io {
                    path: PathBuf::from(cmd),
                    source: e,
                }
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
// Token budget monitors
// ---------------------------------------------------------------------------

/// Monitor Claude session.jsonl for output token usage.
/// Polls `~/.claude/projects/*/<session_id>.jsonl` every N seconds.
/// Sends SIGTERM to the child process when budget is exceeded.
fn monitor_claude_tokens(session_id: &str, budget: u64, level_dir: Option<&Path>) {
    let home = std::env::var("HOME").unwrap_or_default();
    let projects_dir = PathBuf::from(&home).join(".claude/projects");

    // Wait for session file to appear (up to 30s)
    let jsonl_path = wait_for_session_file(&projects_dir, session_id, 30);
    let Some(jsonl_path) = jsonl_path else {
        eprintln!("WARNING: Token monitor could not find session JSONL for {session_id}");
        return;
    };

    poll_token_file(
        &jsonl_path,
        budget,
        level_dir,
        extract_claude_output_tokens,
    );
}

/// Monitor Codex JSONL output (written to agent-output.txt via --json flag).
/// Looks for `token_count` events with `output_tokens`.
fn monitor_codex_tokens(output_file: &Path, budget: u64, level_dir: Option<&Path>) {
    // Wait for output file to appear (up to 30s)
    let deadline = Instant::now() + Duration::from_secs(30);
    while !output_file.exists() && Instant::now() < deadline {
        std::thread::sleep(Duration::from_secs(2));
        if INTERRUPTED.load(Ordering::Relaxed) || CHILD_PID.load(Ordering::Relaxed) == 0 {
            return;
        }
    }
    if !output_file.exists() {
        eprintln!(
            "WARNING: Token monitor could not find Codex output at {}",
            output_file.display()
        );
        return;
    }

    poll_token_file(
        output_file,
        budget,
        level_dir,
        extract_codex_output_tokens,
    );
}

/// Generic poll loop: reads new lines from a file, calls `extractor` on each,
/// accumulates output tokens, and kills the child when budget exceeded.
fn poll_token_file<F>(path: &Path, budget: u64, level_dir: Option<&Path>, extractor: F)
where
    F: Fn(&str) -> u64,
{
    use std::io::{BufRead, BufReader, Seek, SeekFrom};

    let mut offset: u64 = 0;
    let poll = Duration::from_secs(TOKEN_POLL_INTERVAL_SECS);

    loop {
        std::thread::sleep(poll);

        // Stop if child already exited
        if CHILD_PID.load(Ordering::Relaxed) == 0 {
            break;
        }
        if INTERRUPTED.load(Ordering::Relaxed) {
            break;
        }

        // Read new lines from offset
        let Ok(file) = fs::File::open(path) else {
            continue;
        };
        let mut reader = BufReader::new(file);
        if reader.seek(SeekFrom::Start(offset)).is_err() {
            continue;
        }

        let mut new_tokens: u64 = 0;
        let mut buf = String::new();
        while reader.read_line(&mut buf).unwrap_or(0) > 0 {
            new_tokens += extractor(buf.trim());
            buf.clear();
        }
        offset = reader.stream_position().unwrap_or(offset);

        if new_tokens == 0 {
            continue;
        }
        let total = TOKEN_TOTAL.fetch_add(new_tokens, Ordering::Relaxed) + new_tokens;
        if total < budget {
            continue;
        }
        eprintln!("TOKEN BUDGET EXCEEDED: {total}/{budget} output tokens — sending SIGTERM");
        if let Some(dir) = level_dir {
            let msg = format!("Budget exceeded: {total}/{budget} output tokens");
            let _ = fs::write(dir.join("token-exhausted.txt"), msg);
        }
        kill_child();
        break;
    }
}

fn kill_child() {
    let pid = CHILD_PID.load(Ordering::Relaxed);
    if pid != 0 {
        let _ = Command::new("kill")
            .args(["-TERM", &pid.to_string()])
            .status();
    }
}

fn wait_for_session_file(
    projects_dir: &Path,
    session_id: &str,
    timeout_secs: u64,
) -> Option<PathBuf> {
    let deadline = Instant::now() + Duration::from_secs(timeout_secs);
    let target = format!("{session_id}.jsonl");
    loop {
        if let Some(found) = find_session_in_projects(projects_dir, &target) {
            return Some(found);
        }
        if Instant::now() >= deadline || INTERRUPTED.load(Ordering::Relaxed) {
            return None;
        }
        std::thread::sleep(Duration::from_secs(2));
    }
}

fn find_session_in_projects(projects_dir: &Path, target: &str) -> Option<PathBuf> {
    let entries = fs::read_dir(projects_dir).ok()?;
    for entry in entries.flatten() {
        if !entry.path().is_dir() {
            continue;
        }
        let candidate = entry.path().join(target);
        if candidate.is_file() {
            return Some(candidate);
        }
    }
    None
}

/// Extract output_tokens from a Claude session JSONL line.
/// Format: `{"message":{"usage":{"output_tokens":N,...},...},...}`
fn extract_claude_output_tokens(line: &str) -> u64 {
    let obj: serde_json::Value = match serde_json::from_str(line) {
        Ok(v) => v,
        Err(_) => return 0,
    };
    obj.pointer("/message/usage/output_tokens")
        .and_then(|v| v.as_u64())
        .unwrap_or(0)
}

/// Extract output_tokens from a Codex JSONL line.
/// Format: `{"type":"event_msg","payload":{"type":"token_count","output_tokens":N,...}}`
fn extract_codex_output_tokens(line: &str) -> u64 {
    let obj: serde_json::Value = match serde_json::from_str(line) {
        Ok(v) => v,
        Err(_) => return 0,
    };
    if obj.pointer("/payload/type").and_then(|v| v.as_str()) != Some("token_count") {
        return 0;
    }
    obj.pointer("/payload/output_tokens")
        .and_then(|v| v.as_u64())
        .unwrap_or(0)
}

// ---------------------------------------------------------------------------
// Level helpers
// ---------------------------------------------------------------------------

fn append_file_listing(summary: &mut String, scheme_dir: &Path) {
    let Ok(entries) = fs::read_dir(scheme_dir) else {
        return;
    };
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
    append_file_listing(&mut summary, &worktree_dir.join("bench/rust/src/scheme"));

    summary.push_str("\nLevels already passing:\n");
    let level_num: u32 = level.parse().expect("level should be a number");
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
fn capture_session_as(
    agent: &str,
    session_id: &str,
    output_file: &Path,
    dest: &Path,
    filename: &str,
) {
    match agent {
        "claude" => {
            let home = std::env::var("HOME").unwrap_or_default();
            let projects_dir = PathBuf::from(&home).join(".claude/projects");
            let Some(found) = find_session_jsonl(&projects_dir, session_id) else {
                eprintln!("WARNING: Session JSONL not found for {session_id}");
                return;
            };
            copy_captured_session(&found, &dest.join(filename), "Session");
        }
        "codex" => capture_codex_session(output_file, &dest.join(filename)),
        _ => {}
    }
}

fn capture_session(agent: &str, session_id: &str, output_file: &Path, dest: &Path) {
    match agent {
        "claude" | "codex" => {
            capture_session_as(agent, session_id, output_file, dest, "session.jsonl")
        }
        "opencode" => capture_newest_session("opencode", ".opencode", "json", "session.json", dest),
        _ => {}
    }
}

fn capture_codex_session(output_file: &Path, target: &Path) {
    let Some(found) = codex::resolve_rollout_from_output_file(output_file) else {
        eprintln!(
            "WARNING: Codex rollout not found for output file {}",
            output_file.display()
        );
        return;
    };
    copy_captured_session(&found, target, "Codex");
}

fn copy_captured_session(found: &Path, target: &Path, label: &str) {
    match fs::copy(found, target) {
        Ok(_) => println!("{label} session captured: {}", target.display()),
        Err(e) => eprintln!("WARNING: Failed to copy session: {e}"),
    }
}

fn capture_newest_session(
    label: &str,
    home_subdir: &str,
    ext: &str,
    target_name: &str,
    dest: &Path,
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
    let label_cap = label
        .chars()
        .next()
        .unwrap_or('?')
        .to_uppercase()
        .to_string()
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
        let Ok(modified) = meta.modified() else {
            continue;
        };
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
        let Some(pos) = line.find("Score: ") else {
            continue;
        };
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
    let _ = run_cmd("git", &["add", "bench/"], worktree_dir);
    let _ = run_cmd(
        "git",
        &[
            "commit",
            "-m",
            "checkpoint: interrupted/cleanup",
            "--allow-empty",
        ],
        worktree_dir,
    );
    let _ = run_cmd("git", &["push", "-u", "origin", name], worktree_dir);
}
