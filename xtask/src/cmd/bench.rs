use crate::cmd::test_level;
use crate::model::{compact_timestamp, project_dir, run_cmd_capture_all, Error, Lang, Result, LEVELS};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Instant;

const TIMEOUT: u32 = 45;

struct BenchLevelResult {
    status: &'static str,
    duration: u64,
    passed: u32,
    failed: u32,
    output: String,
}

fn run_bench_level(proj: &Path, level: &str, lang: &Lang) -> Result<BenchLevelResult> {
    let start = Instant::now();

    let (exit_code, output) = test_level::run_level_capture(level, lang, proj)?;

    let duration = start.elapsed().as_secs();
    let (passed, failed) = parse_test_output(&output, lang);

    let status = if exit_code == 124 || duration >= TIMEOUT as u64 {
        "TIMEOUT"
    } else if exit_code == 0 {
        "PASS"
    } else {
        "FAIL"
    };

    Ok(BenchLevelResult {
        status,
        duration,
        passed,
        failed,
        output,
    })
}

fn setup_bench(
    proj: &Path,
    branch: &str,
    worktree_dir: &Path,
    worktree_branch: &str,
    lang: &Lang,
) -> Result<()> {
    println!("=== MING Bench: branch={branch} lang={} ===", lang.display_name());
    println!("Creating worktree from '{branch}'...");

    let exit = crate::model::run_cmd(
        "git",
        &[
            "worktree",
            "add",
            "-b",
            worktree_branch,
            worktree_dir.to_str().expect("worktree path not utf8"),
            branch,
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

    let image = lang.container_image();
    let (img_exit, _) =
        run_cmd_capture_all("sudo", &["podman", "image", "exists", image], proj)?;
    if img_exit != 0 {
        eprintln!("Image '{image}' not found. Run `cargo xtask setup` first.");
        return Err(Error::CommandFailed {
            cmd: "podman image exists".to_string(),
            exit_code: img_exit,
        });
    }

    Ok(())
}

fn log_level_output(log: &mut String, level: &str, output: &str) {
    log.push_str(&format!("  --- L{level} output ---\n"));
    for line in output.lines() {
        log.push_str(&format!("  {line}\n"));
    }
    log.push_str(&format!("  --- end L{level} ---\n"));
}

fn append_log(log: &mut String, msg: &str) {
    println!("{msg}");
    log.push_str(msg);
    log.push('\n');
}

pub fn run(branch: &str, run_id: Option<&str>, lang_str: &str) -> Result<()> {
    let lang = Lang::from_str(lang_str).map_err(|msg| Error::CommandFailed {
        cmd: msg,
        exit_code: 1,
    })?;
    let proj = project_dir();
    let run_id = bench_run_id(run_id);
    let timestamp = compact_timestamp();

    let results_dir = proj.join("results");
    fs::create_dir_all(&results_dir).map_err(|e| Error::io(&results_dir, e))?;
    let result_file = results_dir.join(format!("{branch}_{run_id}_{timestamp}.log"));

    let worktree_dir =
        std::env::temp_dir().join(format!("bench-{}-{}", run_id, std::process::id()));
    let worktree_branch = format!("bench-{}-{}", run_id, std::process::id());

    let _guard = WorktreeGuard {
        proj_dir: proj.clone(),
        worktree_dir: worktree_dir.clone(),
        branch_name: worktree_branch.clone(),
    };

    setup_bench(&proj, branch, &worktree_dir, &worktree_branch, &lang)?;

    let mut log = bench_log_header(branch, &run_id, &lang);
    let summary = run_all_levels(&worktree_dir, &lang, &mut log)?;
    append_summary(&mut log, &summary, &result_file);

    fs::write(&result_file, &log).map_err(|e| Error::io(&result_file, e))?;

    println!("=== Bench complete ===");
    Ok(())
}

fn bench_run_id(run_id: Option<&str>) -> String {
    run_id
        .map(str::to_string)
        .unwrap_or_else(|| format!("run-{}", crate::model::now_epoch()))
}

fn bench_log_header(branch: &str, run_id: &str, lang: &Lang) -> String {
    println!("Branch: {branch}");
    println!("Lang:   {}", lang.display_name());
    println!("Run ID: {run_id}");
    println!("Timeout: {TIMEOUT}s per level");
    println!("---");
    format!(
        "Branch: {branch}\nLang: {}\nRun ID: {run_id}\nTimeout: {TIMEOUT}s per level\n---\n",
        lang.display_name()
    )
}

fn run_all_levels(proj: &Path, lang: &Lang, log: &mut String) -> Result<BenchSummary> {
    let mut summary = BenchSummary::default();
    for level in &LEVELS {
        let result = run_bench_level(proj, level, lang)?;
        summary.record(level, &result);
        append_level_result(log, level, &result);
    }
    Ok(summary)
}

fn append_level_result(log: &mut String, level: &str, result: &BenchLevelResult) {
    append_log(
        log,
        &format!(
            "L{level}: {} ({}s) [{}/{} tests]",
            result.status,
            result.duration,
            result.passed,
            result.passed + result.failed
        ),
    );
    if result.status != "PASS" {
        log_level_output(log, level, &result.output);
    }
}

fn append_summary(log: &mut String, summary: &BenchSummary, result_file: &Path) {
    log.push_str("---\n");
    append_log(
        log,
        &format!(
            "Score: {}/{} tests passed",
            summary.passed_tests, summary.total_tests
        ),
    );
    if !summary.failed_levels.is_empty() {
        append_log(
            log,
            &format!("Failed:  {}", summary.failed_levels.join(" ")),
        );
    }
    if !summary.timeout_levels.is_empty() {
        append_log(
            log,
            &format!("Timeout: {}", summary.timeout_levels.join(" ")),
        );
    }
    append_log(log, &format!("Results: {}", result_file.display()));
}

#[derive(Default)]
struct BenchSummary {
    total_tests: u32,
    passed_tests: u32,
    failed_levels: Vec<String>,
    timeout_levels: Vec<String>,
}

impl BenchSummary {
    fn record(&mut self, level: &str, result: &BenchLevelResult) {
        self.total_tests += result.passed + result.failed;
        self.passed_tests += result.passed;

        match result.status {
            "TIMEOUT" => self.timeout_levels.push(format!("L{level}")),
            "FAIL" => self.failed_levels.push(format!("L{level}")),
            _ => {}
        }
    }
}

// ---------------------------------------------------------------------------
// Per-language test output parsers
// ---------------------------------------------------------------------------

fn parse_test_output(output: &str, lang: &Lang) -> (u32, u32) {
    match lang {
        Lang::Rust => parse_rust_output(output),
        Lang::Go => parse_go_output(output),
        Lang::Java | Lang::Scala => parse_jvm_output(output),
        Lang::TypeScript => parse_ts_output(output),
    }
}

/// Rust: "test result: ok. N passed; M failed; ..."
fn parse_rust_output(output: &str) -> (u32, u32) {
    for line in output.lines().rev() {
        if line.starts_with("test result:") {
            let passed = extract_count(line, "passed");
            let failed = extract_count(line, "failed");
            return (passed, failed);
        }
    }
    (0, 0)
}

/// Go: count "--- PASS:" and "--- FAIL:" lines
fn parse_go_output(output: &str) -> (u32, u32) {
    let mut passed = 0u32;
    let mut failed = 0u32;
    for line in output.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("--- PASS:") {
            passed += 1;
        } else if trimmed.starts_with("--- FAIL:") {
            failed += 1;
        }
    }
    (passed, failed)
}

/// Java/Scala TestRunner: "N passed, M failed out of T tests"
fn parse_jvm_output(output: &str) -> (u32, u32) {
    for line in output.lines().rev() {
        if line.contains("passed") && line.contains("failed") && line.contains("out of") {
            let passed = extract_count(line, "passed,");
            let failed = extract_count(line, "failed");
            return (passed, failed);
        }
    }
    (0, 0)
}

/// TypeScript vitest: parse summary line or count individual results
fn parse_ts_output(output: &str) -> (u32, u32) {
    // Try summary line first: "Tests  5 passed | 2 failed (7)"
    if let Some(summary) = parse_ts_summary(output) {
        return summary;
    }
    // Fallback: count individual "✓" / "×" lines
    let mut passed = 0u32;
    let mut failed = 0u32;
    for line in output.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('✓') || trimmed.starts_with('√') {
            passed += 1;
        } else if trimmed.starts_with('×') || trimmed.starts_with('✗') {
            failed += 1;
        }
    }
    (passed, failed)
}

fn parse_ts_summary(output: &str) -> Option<(u32, u32)> {
    for line in output.lines() {
        let trimmed = line.trim();
        if !trimmed.contains("Tests") || !trimmed.contains("passed") {
            continue;
        }
        let p = extract_count(trimmed, "passed");
        let f = extract_count(trimmed, "failed");
        if p > 0 || f > 0 {
            return Some((p, f));
        }
    }
    None
}

fn extract_count(line: &str, label: &str) -> u32 {
    line.split_whitespace()
        .collect::<Vec<_>>()
        .windows(2)
        .filter(|w| w[1] == label || w[1].starts_with(label))
        .find_map(|w| w[0].parse::<u32>().ok())
        .unwrap_or(0)
}

/// RAII guard that removes the temporary worktree on drop.
struct WorktreeGuard {
    proj_dir: PathBuf,
    worktree_dir: PathBuf,
    branch_name: String,
}

impl Drop for WorktreeGuard {
    fn drop(&mut self) {
        if self.worktree_dir.is_dir() {
            let _ = std::process::Command::new("git")
                .args(["worktree", "remove", "--force"])
                .arg(&self.worktree_dir)
                .current_dir(&self.proj_dir)
                .status();
        }
        let _ = std::process::Command::new("git")
            .args(["branch", "-D", &self.branch_name])
            .current_dir(&self.proj_dir)
            .status();
    }
}
