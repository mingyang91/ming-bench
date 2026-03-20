use crate::model::{compact_timestamp, project_dir, run_cmd_capture_all, Error, Result, LEVELS};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Instant;

const IMAGE_NAME: &str = "ming";
const TIMEOUT: u32 = 30;

pub fn run(branch: &str, run_id: Option<&str>) -> Result<()> {
    let proj = project_dir();
    let run_id = run_id
        .map(|s| s.to_string())
        .unwrap_or_else(|| format!("run-{}", crate::model::now_epoch()));
    let timestamp = compact_timestamp();

    // --- Results setup ---
    let results_dir = proj.join("results");
    fs::create_dir_all(&results_dir).map_err(|e| Error::io(&results_dir, e))?;
    let result_file = results_dir.join(format!("{branch}_{run_id}_{timestamp}.log"));

    // --- Create temp worktree ---
    let worktree_dir =
        std::env::temp_dir().join(format!("bench-{}-{}", run_id, std::process::id()));
    let worktree_branch = format!("bench-{}-{}", run_id, std::process::id());

    // Cleanup guard
    let _guard = WorktreeGuard {
        proj_dir: proj.clone(),
        worktree_dir: worktree_dir.clone(),
        branch_name: worktree_branch.clone(),
    };

    println!("=== MING Bench: branch={branch} run={run_id} ===");
    println!("Creating worktree from '{branch}'...");

    let exit = crate::model::run_cmd(
        "git",
        &[
            "worktree",
            "add",
            "-b",
            &worktree_branch,
            worktree_dir.to_str().expect("worktree path not utf8"),
            branch,
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

    // --- Check image exists ---
    let (img_exit, _) =
        run_cmd_capture_all("sudo", &["podman", "image", "exists", IMAGE_NAME], &proj)?;
    if img_exit != 0 {
        eprintln!("Image '{IMAGE_NAME}' not found. Run `cargo xtask setup` first.");
        return Err(Error::CommandFailed {
            cmd: "podman image exists".to_string(),
            exit_code: img_exit,
        });
    }

    // --- Compile tests on host ---
    println!("Compiling tests on host...");
    let test_bin = find_test_binary(&worktree_dir)?;
    println!("Test binary: {}", test_bin.display());

    // --- Run tests level by level ---
    let mut log =
        format!("Branch: {branch}\nRun ID: {run_id}\nTimeout: {TIMEOUT}s per level\n---\n");
    println!("Branch: {branch}");
    println!("Run ID: {run_id}");
    println!("Timeout: {TIMEOUT}s per level");
    println!("---");

    let mut total_tests: u32 = 0;
    let mut passed_tests: u32 = 0;
    let mut failed_levels: Vec<String> = Vec::new();
    let mut timeout_levels: Vec<String> = Vec::new();

    for level in &LEVELS {
        let start = Instant::now();

        let bash_cmd = format!(
            "timeout {TIMEOUT}s /bench/test_bin test_l{level} --test-threads=1 2>&1"
        );
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
            &proj,
        )?;

        let duration = start.elapsed().as_secs();

        // Parse test results
        let (level_passed, level_failed) = parse_test_result(&output);
        let level_total = level_passed + level_failed;

        let status = if exit_code == 124 || duration >= TIMEOUT as u64 {
            timeout_levels.push(format!("L{level}"));
            "TIMEOUT"
        } else if exit_code == 0 {
            "PASS"
        } else {
            failed_levels.push(format!("L{level}"));
            "FAIL"
        };

        total_tests += level_total;
        passed_tests += level_passed;

        let result_line =
            format!("L{level}: {status} ({duration}s) [{level_passed}/{level_total} tests]");
        println!("{result_line}");
        log.push_str(&result_line);
        log.push('\n');

        // Append full output for failed/timeout levels
        if status != "PASS" {
            log.push_str(&format!("  --- L{level} output ---\n"));
            for line in output.lines() {
                log.push_str(&format!("  {line}\n"));
            }
            log.push_str(&format!("  --- end L{level} ---\n"));
        }
    }

    // --- Summary ---
    log.push_str("---\n");
    let summary = format!("Score: {passed_tests}/{total_tests} tests passed");
    println!("---");
    println!("{summary}");
    log.push_str(&summary);
    log.push('\n');

    if !failed_levels.is_empty() {
        let msg = format!("Failed:  {}", failed_levels.join(" "));
        println!("{msg}");
        log.push_str(&msg);
        log.push('\n');
    }
    if !timeout_levels.is_empty() {
        let msg = format!("Timeout: {}", timeout_levels.join(" "));
        println!("{msg}");
        log.push_str(&msg);
        log.push('\n');
    }

    let msg = format!("Results: {}", result_file.display());
    println!("{msg}");
    log.push_str(&msg);
    log.push('\n');

    fs::write(&result_file, &log).map_err(|e| Error::io(&result_file, e))?;

    println!("=== Bench complete ===");
    Ok(())
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
    // Find "N <label>" pattern
    for part in line.split_whitespace().collect::<Vec<_>>().windows(2) {
        if part[1] == label || part[1].starts_with(label) {
            if let Ok(n) = part[0].parse::<u32>() {
                return n;
            }
        }
    }
    0
}

fn find_test_binary(worktree_dir: &Path) -> Result<PathBuf> {
    // Try JSON output first
    let (_, json_output) = run_cmd_capture_all(
        "cargo",
        &["test", "--no-run", "--message-format=json"],
        worktree_dir,
    )?;

    for line in json_output.lines() {
        let obj: serde_json::Value = match serde_json::from_str(line) {
            Ok(v) => v,
            Err(_) => continue,
        };
        if obj.get("reason").and_then(|r| r.as_str()) != Some("compiler-artifact") {
            continue;
        }
        let kinds = obj
            .get("target")
            .and_then(|t| t.get("kind"))
            .and_then(|k| k.as_array());
        if let Some(kinds) = kinds {
            if kinds.iter().any(|k| k.as_str() == Some("lib")) {
                if let Some(exe) = obj.get("executable").and_then(|e| e.as_str()) {
                    if !exe.is_empty() {
                        let path = PathBuf::from(exe);
                        if path.is_file() {
                            return Ok(path);
                        }
                    }
                }
            }
        }
    }

    // Fallback: build and grep for binary path
    let (_, fallback_output) = run_cmd_capture_all("cargo", &["test", "--no-run"], worktree_dir)?;

    for line in fallback_output.lines() {
        if let Some(start) = line.find("target/") {
            let bin = &line[start..];
            let bin: String = bin
                .chars()
                .take_while(|c| {
                    c.is_alphanumeric() || *c == '/' || *c == '-' || *c == '_' || *c == '.'
                })
                .collect();
            let path = worktree_dir.join(&bin);
            if path.is_file() {
                return Ok(path);
            }
        }
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
