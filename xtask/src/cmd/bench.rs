use crate::model::{compact_timestamp, project_dir, run_cmd_capture_all, Error, Result, LEVELS};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Instant;

const IMAGE_NAME: &str = "ming";
const TIMEOUT: u32 = 30;

struct BenchLevelResult {
    status: &'static str,
    duration: u64,
    passed: u32,
    failed: u32,
    output: String,
}

fn run_bench_level(proj: &Path, test_bin: &Path, level: &str) -> Result<BenchLevelResult> {
    let start = Instant::now();

    let bash_cmd =
        format!("timeout {TIMEOUT}s /bench/test_bin test_l{level} --test-threads=1 2>&1");
    let mount_spec = format!("{}:/bench/test_bin:ro,Z", test_bin.display());

    let (exit_code, output) = run_cmd_capture_all(
        "sudo",
        &[
            "podman",
            "run",
            "--rm",
            "--memory=1g",
            "--cpus=1",
            "--pids-limit=256",
            "-v",
            &mount_spec,
            IMAGE_NAME,
            &bash_cmd,
        ],
        proj,
    )?;

    let duration = start.elapsed().as_secs();
    let (passed, failed) = parse_test_result(&output);

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
) -> Result<PathBuf> {
    println!("=== MING Bench: branch={branch} ===");
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

    let (img_exit, _) =
        run_cmd_capture_all("sudo", &["podman", "image", "exists", IMAGE_NAME], proj)?;
    if img_exit != 0 {
        eprintln!("Image '{IMAGE_NAME}' not found. Run `cargo xtask setup` first.");
        return Err(Error::CommandFailed {
            cmd: "podman image exists".to_string(),
            exit_code: img_exit,
        });
    }

    println!("Compiling tests on host...");
    let test_bin = find_test_binary(worktree_dir)?;
    println!("Test binary: {}", test_bin.display());
    Ok(test_bin)
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

pub fn run(branch: &str, run_id: Option<&str>) -> Result<()> {
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

    let test_bin = setup_bench(&proj, branch, &worktree_dir, &worktree_branch)?;

    let mut log = bench_log_header(branch, &run_id);
    let summary = run_all_levels(&proj, &test_bin, &mut log)?;
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

fn bench_log_header(branch: &str, run_id: &str) -> String {
    println!("Branch: {branch}");
    println!("Run ID: {run_id}");
    println!("Timeout: {TIMEOUT}s per level");
    println!("---");
    format!("Branch: {branch}\nRun ID: {run_id}\nTimeout: {TIMEOUT}s per level\n---\n")
}

fn run_all_levels(proj: &Path, test_bin: &Path, log: &mut String) -> Result<BenchSummary> {
    let mut summary = BenchSummary::default();
    for level in &LEVELS {
        let result = run_bench_level(proj, test_bin, level)?;
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

fn parse_test_result(output: &str) -> (u32, u32) {
    // Look for "test result: ok. N passed; M failed; ..."
    for line in output.lines().rev() {
        if line.starts_with("test result:") {
            let passed = extract_count(line, "passed");
            let failed = extract_count(line, "failed");
            return (passed, failed);
        }
    }
    (0, 0)
}

fn extract_count(line: &str, label: &str) -> u32 {
    line.split_whitespace()
        .collect::<Vec<_>>()
        .windows(2)
        .filter(|w| w[1] == label || w[1].starts_with(label))
        .find_map(|w| w[0].parse::<u32>().ok())
        .unwrap_or(0)
}

fn is_path_char(c: char) -> bool {
    c.is_alphanumeric() || c == '/' || c == '-' || c == '_' || c == '.'
}

/// Extract a test binary path from `cargo test --no-run --message-format=json` output.
fn parse_json_test_binary(json_output: &str) -> Option<PathBuf> {
    for line in json_output.lines() {
        let Ok(obj) = serde_json::from_str::<serde_json::Value>(line) else {
            continue;
        };
        if obj.get("reason").and_then(|r| r.as_str()) != Some("compiler-artifact") {
            continue;
        }
        let is_lib = obj
            .get("target")
            .and_then(|t| t.get("kind"))
            .and_then(|k| k.as_array())
            .is_some_and(|kinds| kinds.iter().any(|k| k.as_str() == Some("lib")));
        if !is_lib {
            continue;
        }
        let exe = obj.get("executable").and_then(|e| e.as_str()).unwrap_or("");
        if exe.is_empty() {
            continue;
        }
        let path = PathBuf::from(exe);
        if path.is_file() {
            return Some(path);
        }
    }
    None
}

/// Extract a test binary path by grepping `cargo test --no-run` output for target/ paths.
fn parse_fallback_test_binary(output: &str, base: &Path) -> Option<PathBuf> {
    for line in output.lines() {
        let Some(start) = line.find("target/") else {
            continue;
        };
        let bin: String = line[start..]
            .chars()
            .take_while(|c| is_path_char(*c))
            .collect();
        let path = base.join(&bin);
        if path.is_file() {
            return Some(path);
        }
    }
    None
}

fn find_test_binary(worktree_dir: &Path) -> Result<PathBuf> {
    let (_, json_output) = run_cmd_capture_all(
        "cargo",
        &["test", "--no-run", "--message-format=json"],
        worktree_dir,
    )?;
    if let Some(bin) = parse_json_test_binary(&json_output) {
        return Ok(bin);
    }

    let (_, fallback_output) = run_cmd_capture_all("cargo", &["test", "--no-run"], worktree_dir)?;
    if let Some(bin) = parse_fallback_test_binary(&fallback_output, worktree_dir) {
        return Ok(bin);
    }

    Err(Error::TestBinaryNotFound)
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
