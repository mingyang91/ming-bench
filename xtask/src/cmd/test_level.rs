use crate::model::{project_dir, run_cmd, run_cmd_capture_all, Error, Result};
use std::fs;
use std::path::Path;

const IMAGE_NAME: &str = "cs61a-bench";

pub fn run(level: &str) -> Result<()> {
    let proj = project_dir();

    // --- Quality gates (only when clippy.toml exists) ---
    let clippy_toml = proj.join("bench/clippy.toml");
    if clippy_toml.is_file() {
        quality_gates(&proj, level)?;
    }

    // --- Build test binary ---
    let bin = find_test_binary(&proj)?;

    // --- Build filter and timeout ---
    let (filter, timeout) = if level == "all" {
        (String::new(), 300)
    } else {
        (format!("test_l{level}"), 30)
    };

    // --- Run inside container ---
    let timeout_str = format!("{timeout}s");
    let mount_spec = format!("./{}:/bench/test_bin:ro,Z", bin);
    let bash_cmd = if filter.is_empty() {
        format!("timeout {timeout_str} /bench/test_bin --test-threads=1 2>&1")
    } else {
        format!("timeout {timeout_str} /bench/test_bin {filter} --test-threads=1 2>&1")
    };

    let exit = run_cmd(
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

    if exit != 0 {
        return Err(Error::CommandFailed {
            cmd: "podman run (test)".to_string(),
            exit_code: exit,
        });
    }

    Ok(())
}

fn quality_gates(proj: &Path, level: &str) -> Result<()> {
    let clippy_toml = proj.join("bench/clippy.toml");
    let clippy_bak = proj.join("bench/clippy.toml.bak");

    // Determine limits based on level
    let (fn_limit, allow_dead_code) = if level != "all" {
        let ln: u32 = level.parse().unwrap_or(99);
        if ln <= 5 {
            (80, true)
        } else {
            (60, false)
        }
    } else {
        (60, false)
    };

    // Backup and write leveled clippy.toml
    if clippy_toml.is_file() {
        let _ = fs::copy(&clippy_toml, &clippy_bak);
    }
    fs::write(
        &clippy_toml,
        format!("too-many-lines-threshold = {fn_limit}\nexcessive-nesting-threshold = 3\n"),
    )
    .map_err(|e| Error::io(&clippy_toml, e))?;

    let restore = || {
        if clippy_bak.is_file() {
            let _ = fs::rename(&clippy_bak, &clippy_toml);
        }
    };

    // Run clippy --fix
    println!("Running clippy --fix (auto-fixing trivial lints)...");
    let mut clippy_args = vec![
        "clippy",
        "--fix",
        "--allow-dirty",
        "--allow-staged",
        "--",
        "-D",
        "warnings",
    ];
    if allow_dead_code {
        clippy_args.extend_from_slice(&["-A", "dead_code"]);
    }
    let _ = run_cmd("cargo", &clippy_args, proj);

    // Run clippy (verify)
    println!("Running clippy (verify, fn limit={fn_limit})...");
    let mut verify_args = vec!["clippy", "--", "-D", "warnings"];
    if allow_dead_code {
        verify_args.extend_from_slice(&["-A", "dead_code"]);
    }
    let exit = run_cmd("cargo", &verify_args, proj)?;
    if exit != 0 {
        restore();
        eprintln!("ERROR: clippy failed — fix warnings before testing");
        return Err(Error::CommandFailed {
            cmd: "cargo clippy".to_string(),
            exit_code: exit,
        });
    }

    restore();

    // --- mod.rs size check ---
    if level != "all" {
        let ln: u32 = level.parse().unwrap_or(99);
        check_mod_size(proj, ln)?;
    }

    Ok(())
}

fn check_mod_size(proj: &Path, level: u32) -> Result<()> {
    let mod_limit: usize = if level <= 3 {
        300
    } else if level <= 6 {
        200
    } else {
        100
    };

    let mod_file = proj.join("bench/src/scheme/mod.rs");
    if !mod_file.is_file() {
        return Ok(());
    }

    let content = fs::read_to_string(&mod_file).map_err(|e| Error::io(&mod_file, e))?;

    // Count lines up to #[cfg(test)]
    let mut impl_lines = 0;
    for line in content.lines() {
        if line.starts_with("#[cfg(test)]") {
            break;
        }
        impl_lines += 1;
    }

    if impl_lines > mod_limit {
        eprintln!(
            "ERROR: mod.rs has {} impl lines (limit for L{:02}: {}).",
            impl_lines, level, mod_limit
        );
        eprintln!("  Extract implementation logic into submodules.");
        return Err(Error::CommandFailed {
            cmd: "mod.rs size check".to_string(),
            exit_code: 1,
        });
    }

    Ok(())
}

fn find_test_binary(proj: &Path) -> Result<String> {
    // Build test binary in release mode
    let (exit, output) = run_cmd_capture_all("cargo", &["test", "--no-run", "--release"], proj)?;

    // Parse output for binary path
    for line in output.lines() {
        if let Some(start) = line.find("target/release/deps/cs61a_bench-") {
            let bin = &line[start..];
            // Take until non-path character
            let bin: String = bin
                .chars()
                .take_while(|c| {
                    c.is_alphanumeric() || *c == '/' || *c == '-' || *c == '_' || *c == '.'
                })
                .collect();
            if proj.join(&bin).is_file() {
                return Ok(bin);
            }
        }
    }

    if exit != 0 {
        eprintln!("Build output:\n{output}");
    }

    Err(Error::TestBinaryNotFound)
}
