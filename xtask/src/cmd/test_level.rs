use crate::model::{project_dir, run_cmd, run_cmd_capture_all, Error, Result, LEVELS};
use std::fs;
use std::path::Path;

const IMAGE_NAME: &str = "ming";
const NODE_IMAGE: &str = "ming-node";

pub fn run(level: &str, gate: bool, lang: &str) -> Result<()> {
    let parsed_lang = crate::model::Lang::from_str(lang).map_err(|msg| Error::CommandFailed {
        cmd: msg,
        exit_code: 1,
    })?;

    match parsed_lang {
        crate::model::Lang::Rust => run_rust(level, gate),
        crate::model::Lang::Scala => run_mill(level, gate),
        crate::model::Lang::Java => run_gradle(level, gate),
        crate::model::Lang::Go => run_go(level, gate),
        crate::model::Lang::TypeScript => run_ts(level, gate),
    }
}

fn run_rust(level: &str, gate: bool) -> Result<()> {
    let proj = project_dir();

    // --- AST rules (always enforced) ---
    let scheme_src = proj.join("bench/rust/src/scheme");
    let violations = crate::ast_check::check_ast_rules(&scheme_src);
    if !violations.is_empty() {
        eprintln!("ERROR: AST rule violations found:");
        for v in &violations {
            eprintln!("  {}:{}: {}", v.file, v.line, v.message);
        }
        return Err(Error::CommandFailed {
            cmd: "AST rule check".to_string(),
            exit_code: 1,
        });
    }

    // --- Quality gates (only when --gate is set and clippy.toml exists) ---
    let clippy_toml = proj.join("bench/rust/clippy.toml");
    if gate && clippy_toml.is_file() {
        quality_gates(&proj, level)?;
    }

    // --- Build test binary ---
    let bin = find_test_binary(&proj)?;

    let exit = run_container_test(&proj, &bin, level)?;

    if exit != 0 {
        return Err(Error::CommandFailed {
            cmd: "podman run (test)".to_string(),
            exit_code: exit,
        });
    }

    Ok(())
}

const JVM_IMAGE: &str = "ming-jvm";

fn mill_quality_gate(lang_dir: &Path) -> Result<()> {
    println!("Running scalafix (quality gate)...");
    let exit = run_cmd("./mill", &["fix", "--check"], lang_dir)?;
    if exit != 0 {
        return Err(Error::CommandFailed {
            cmd: "mill fix --check".to_string(),
            exit_code: exit,
        });
    }

    println!("Running scalafmt check (quality gate)...");
    let exit = run_cmd("./mill", &["checkFormat"], lang_dir)?;
    if exit != 0 {
        return Err(Error::CommandFailed {
            cmd: "mill checkFormat".to_string(),
            exit_code: exit,
        });
    }
    Ok(())
}

fn run_mill_container(proj: &Path, jar: &Path, level: &str) -> Result<()> {
    run_jvm_container(proj, jar, level, "Scala")
}

/// Run Scala tests: compile + assembly on host, execute JAR in container.
fn run_mill(level: &str, gate: bool) -> Result<()> {
    let proj = project_dir();
    let lang_dir = proj.join("bench/scala");

    if gate {
        mill_quality_gate(&lang_dir)?;
    }

    println!("Building Scala assembly...");
    let exit = run_cmd("./mill", &["assembly"], &lang_dir)?;
    if exit != 0 {
        return Err(Error::CommandFailed {
            cmd: "mill assembly".to_string(),
            exit_code: exit,
        });
    }

    let jar = lang_dir.join("out/assembly.dest/out.jar");
    if !jar.is_file() {
        return Err(Error::CommandFailed {
            cmd: "assembly JAR not found after build".to_string(),
            exit_code: 1,
        });
    }

    run_mill_container(&proj, &jar, level)
}

/// Run Java tests: gradle shadowJar on host, execute fat JAR in container.
fn run_gradle(level: &str, gate: bool) -> Result<()> {
    let proj = project_dir();
    let lang_dir = proj.join("bench/java");

    // Build shadow JAR
    println!("Building Java shadow JAR...");
    let exit = run_cmd("./gradlew", &["shadowJar"], &lang_dir)?;
    if exit != 0 {
        return Err(Error::CommandFailed {
            cmd: "gradlew shadowJar".to_string(),
            exit_code: exit,
        });
    }

    let jar = lang_dir.join("build/libs/ming-test.jar");
    if !jar.is_file() {
        return Err(Error::CommandFailed {
            cmd: "shadow JAR not found after build".to_string(),
            exit_code: 1,
        });
    }

    if gate {
        println!("Running Java quality gate (Checkstyle)...");
        let gate_exit = run_cmd(
            "./gradlew",
            &["qualityGate", "--no-daemon"],
            &lang_dir,
        )?;
        if gate_exit != 0 {
            return Err(Error::CommandFailed {
                cmd: "gradlew qualityGate".to_string(),
                exit_code: gate_exit,
            });
        }
    }

    run_jvm_container(&proj, &jar, level, "Java")
}

/// Run a fat JAR inside the JVM container (shared by Java and Scala).
/// Mounts bench/ as a single read-only volume.
fn run_jvm_container(proj: &Path, jar: &Path, level: &str, lang_label: &str) -> Result<()> {
    let (timeout, level_arg) = if level == "all" {
        (450, "all".to_string())
    } else {
        (45, level.to_string())
    };

    let bench_mount = format!("{}:/bench:ro,Z", proj.join("bench").display());
    let jar_path = format!("/bench/{}", jar.strip_prefix(proj.join("bench")).unwrap_or(jar).display());
    let java_cmd = format!("timeout {timeout}s java -jar {jar_path} {level_arg}");

    println!("Running {lang_label} tests (level {level}) in container...");
    let exit = run_cmd(
        "sudo",
        &[
            "podman", "run", "--rm",
            "--memory=2g", "--cpus=1", "--pids-limit=256",
            "-v", &bench_mount,
            "-e", "TESTS_JSON=/bench/tests.json",
            "-e", "FIXTURES_DIR=/bench/fixtures",
            JVM_IMAGE,
            &java_cmd,
        ],
        proj,
    )?;
    if exit != 0 {
        return Err(Error::CommandFailed {
            cmd: format!("podman run ({lang_label} test)"),
            exit_code: exit,
        });
    }
    Ok(())
}

/// Run Go tests: compile static binary on host, execute in container.
fn run_go(level: &str, gate: bool) -> Result<()> {
    let proj = project_dir();
    let lang_dir = proj.join("bench/go");

    // Build test binary on host
    println!("Building Go test binary...");
    let exit = run_cmd("bash", &["build.sh"], &lang_dir)?;
    if exit != 0 {
        return Err(Error::CommandFailed {
            cmd: "build.sh (Go)".to_string(),
            exit_code: exit,
        });
    }

    let test_bin = lang_dir.join("test_bin");
    if !test_bin.is_file() {
        return Err(Error::CommandFailed {
            cmd: "Go test binary not found after build".to_string(),
            exit_code: 1,
        });
    }

    if gate {
        println!("No quality gate configured for Go (skipping).");
    }

    run_go_container(&proj, &lang_dir, level)
}

fn run_go_container(proj: &Path, _lang_dir: &Path, level: &str) -> Result<()> {
    let (timeout, filter) = if level == "all" {
        (300, String::new())
    } else {
        (30, format!("TestL{level}"))
    };

    let bench_level = if level == "all" {
        LEVELS.last().expect("no levels defined").to_string()
    } else {
        level.to_string()
    };

    let bench_mount = format!("{}:/bench:ro,Z", proj.join("bench").display());
    let bench_level_env = format!("BENCH_LEVEL={bench_level}");

    let bash_cmd = if filter.is_empty() {
        format!("cd /bench/go && timeout {timeout}s ./test_bin -test.v -test.timeout {timeout}s 2>&1")
    } else {
        format!("cd /bench/go && timeout {timeout}s ./test_bin -test.run '{filter}' -test.v -test.timeout {timeout}s 2>&1")
    };

    println!("Running Go tests (level {level}) in container...");
    let exit = run_cmd(
        "sudo",
        &[
            "podman", "run", "--rm",
            "--memory=1g", "--cpus=1", "--pids-limit=256",
            "-v", &bench_mount,
            "-e", &bench_level_env,
            IMAGE_NAME,
            &bash_cmd,
        ],
        proj,
    )?;
    if exit != 0 {
        return Err(Error::CommandFailed {
            cmd: "podman run (Go test)".to_string(),
            exit_code: exit,
        });
    }
    Ok(())
}

/// Run TypeScript tests: npm install on host, execute vitest in container.
fn run_ts(level: &str, gate: bool) -> Result<()> {
    let proj = project_dir();
    let lang_dir = proj.join("bench/ts");

    // Build (npm install + tsc) on host
    println!("Building TypeScript tests...");
    let exit = run_cmd("bash", &["build.sh"], &lang_dir)?;
    if exit != 0 {
        return Err(Error::CommandFailed {
            cmd: "build.sh (TypeScript)".to_string(),
            exit_code: exit,
        });
    }

    if !lang_dir.join("node_modules").is_dir() {
        return Err(Error::CommandFailed {
            cmd: "node_modules not found after build".to_string(),
            exit_code: 1,
        });
    }

    if gate {
        println!("No quality gate configured for TypeScript (skipping).");
    }

    run_node_container(&proj, &lang_dir, level)
}

fn run_node_container(proj: &Path, _lang_dir: &Path, level: &str) -> Result<()> {
    let (timeout, name_pattern) = if level == "all" {
        (300, String::new())
    } else {
        (30, format!("l{level}"))
    };

    let bench_level = if level == "all" {
        LEVELS.last().expect("no levels defined").to_string()
    } else {
        level.to_string()
    };

    // TS needs read-write for node_modules/.cache
    let bench_mount = format!("{}:/bench:Z", proj.join("bench").display());
    let bench_level_env = format!("BENCH_LEVEL={bench_level}");

    let bash_cmd = if name_pattern.is_empty() {
        format!("cd /bench/ts && timeout {timeout}s npx vitest run --reporter=verbose 2>&1")
    } else {
        format!("cd /bench/ts && timeout {timeout}s npx vitest run --reporter=verbose --testNamePattern '{name_pattern}' 2>&1")
    };

    println!("Running TypeScript tests (level {level}) in container...");
    let exit = run_cmd(
        "sudo",
        &[
            "podman", "run", "--rm",
            "--memory=2g", "--cpus=1", "--pids-limit=256",
            "-v", &bench_mount,
            "-e", &bench_level_env,
            NODE_IMAGE,
            &bash_cmd,
        ],
        proj,
    )?;
    if exit != 0 {
        return Err(Error::CommandFailed {
            cmd: "podman run (TypeScript test)".to_string(),
            exit_code: exit,
        });
    }
    Ok(())
}

/// Lint flags enforced by the quality gate. These were previously compile-time
/// attributes via `cfg_attr(feature = "quality-gate", ...)` in bench/src/lib.rs.
/// Now they live here so agents only see lint errors during `cargo xtask test`,
/// not on every `cargo build`.
const GATE_LINT_FLAGS: &[&str] = &[
    // Deny lints (hard errors — prevent correctness bugs)
    "-D", "clippy::unwrap_used",
    "-D", "clippy::result_unit_err",
    "-D", "clippy::manual_assert",
    "-D", "clippy::disallowed_macros",
    // Structural lints (catch true monoliths, not cosmetic style)
    "-W", "clippy::too_many_lines",
    "-W", "clippy::excessive_nesting",
];

fn quality_gates(proj: &Path, level: &str) -> Result<()> {
    let clippy_toml = proj.join("bench/rust/clippy.toml");
    let clippy_bak = proj.join("bench/rust/clippy.toml.bak");
    let config = gate_config(level);

    backup_clippy_config(&clippy_toml, &clippy_bak);
    write_gate_clippy_config(&clippy_toml, config.fn_limit)?;

    println!("Running clippy --fix (auto-fixing trivial lints)...");
    let _ = run_cmd("cargo", &clippy_args(true, config.allow_dead_code), proj);

    println!("Running clippy (verify, fn limit={})...", config.fn_limit);
    let exit = run_cmd("cargo", &clippy_args(false, config.allow_dead_code), proj)?;
    if exit != 0 {
        restore_clippy_config(&clippy_toml, &clippy_bak);
        eprintln!("ERROR: clippy failed — fix warnings before testing");
        return Err(Error::CommandFailed {
            cmd: "cargo clippy".to_string(),
            exit_code: exit,
        });
    }

    restore_clippy_config(&clippy_toml, &clippy_bak);

    if level != "all" {
        let ln: u32 = level.parse().unwrap_or(99);
        check_mod_size(proj, ln)?;
    }

    Ok(())
}

struct GateConfig {
    fn_limit: u32,
    allow_dead_code: bool,
}

fn gate_config(level: &str) -> GateConfig {
    if level == "all" {
        return GateConfig {
            fn_limit: 300,
            allow_dead_code: false,
        };
    }

    let ln: u32 = level.parse().unwrap_or(99);
    GateConfig {
        fn_limit: 300,
        allow_dead_code: ln <= 3,
    }
}

fn backup_clippy_config(clippy_toml: &Path, clippy_bak: &Path) {
    if clippy_toml.is_file() {
        let _ = fs::copy(clippy_toml, clippy_bak);
    }
}

fn write_gate_clippy_config(clippy_toml: &Path, fn_limit: u32) -> Result<()> {
    fs::write(
        clippy_toml,
        format!("too-many-lines-threshold = {fn_limit}\nexcessive-nesting-threshold = 6\n"),
    )
    .map_err(|e| Error::io(clippy_toml, e))
}

fn restore_clippy_config(clippy_toml: &Path, clippy_bak: &Path) {
    if clippy_bak.is_file() {
        let _ = fs::rename(clippy_bak, clippy_toml);
    }
}

fn clippy_args(fix: bool, allow_dead_code: bool) -> Vec<&'static str> {
    let mut args = vec!["clippy", "--package", "ming"];
    if fix {
        args.extend_from_slice(&["--fix", "--allow-dirty", "--allow-staged"]);
    }
    args.extend_from_slice(&["--", "-D", "warnings"]);
    args.extend_from_slice(GATE_LINT_FLAGS);
    if allow_dead_code {
        args.extend_from_slice(&["-A", "dead_code"]);
    }
    args
}

fn check_mod_size(proj: &Path, level: u32) -> Result<()> {
    let mod_limit: usize = 1500;

    let mod_file = proj.join("bench/rust/src/scheme/mod.rs");
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
            "ERROR: mod.rs has {impl_lines} impl lines (limit for L{level:02}: {mod_limit})."
        );
        eprintln!("  Extract implementation logic into submodules.");
        return Err(Error::CommandFailed {
            cmd: "mod.rs size check".to_string(),
            exit_code: 1,
        });
    }

    Ok(())
}

fn run_container_test(proj: &Path, bin: &str, level: &str) -> Result<i32> {
    let (filter, timeout) = if level == "all" {
        (String::new(), 300)
    } else {
        (format!("test_l{level}"), 30)
    };

    let timeout_str = format!("{timeout}s");
    let mount_spec = format!("./{bin}:/bench/test_bin:ro,Z");
    let bash_cmd = if filter.is_empty() {
        format!("timeout {timeout_str} /bench/test_bin --test-threads=1 2>&1")
    } else {
        format!("timeout {timeout_str} /bench/test_bin {filter} --test-threads=1 2>&1")
    };

    let bench_level = if level == "all" {
        LEVELS.last().expect("no levels defined").to_string()
    } else {
        level.to_string()
    };
    let bench_level_env = format!("BENCH_LEVEL={bench_level}");

    run_cmd(
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
            "-e",
            &bench_level_env,
            IMAGE_NAME,
            &bash_cmd,
        ],
        proj,
    )
}

// ---------------------------------------------------------------------------
// Capture API — used by bench.rs for scoring (build + run, return output)
// ---------------------------------------------------------------------------

use crate::model::Lang;

/// Build and run tests for a single level, capturing output.
/// `proj` is the project root (may be a worktree, not `project_dir()`).
/// Returns `(exit_code, captured_output)`.
pub fn run_level_capture(level: &str, lang: &Lang, proj: &Path) -> Result<(i32, String)> {
    match lang {
        Lang::Rust => run_rust_capture(level, proj),
        Lang::Go => run_go_capture(level, proj),
        Lang::Java => run_gradle_capture(level, proj),
        Lang::TypeScript => run_ts_capture(level, proj),
        Lang::Scala => run_mill_capture(level, proj),
    }
}

fn run_rust_capture(level: &str, proj: &Path) -> Result<(i32, String)> {
    let bin = find_test_binary_in(proj)?;
    run_container_test_capture(proj, &bin, level)
}

fn run_container_test_capture(proj: &Path, bin: &str, level: &str) -> Result<(i32, String)> {
    let (filter, timeout) = if level == "all" {
        (String::new(), 300)
    } else {
        (format!("test_l{level}"), 30)
    };

    let timeout_str = format!("{timeout}s");
    let mount_spec = format!("./{bin}:/bench/test_bin:ro,Z");
    let bash_cmd = if filter.is_empty() {
        format!("timeout {timeout_str} /bench/test_bin --test-threads=1 2>&1")
    } else {
        format!("timeout {timeout_str} /bench/test_bin {filter} --test-threads=1 2>&1")
    };

    let bench_level = if level == "all" {
        LEVELS.last().expect("no levels defined").to_string()
    } else {
        level.to_string()
    };
    let bench_level_env = format!("BENCH_LEVEL={bench_level}");

    run_cmd_capture_all(
        "sudo",
        &[
            "podman", "run", "--rm",
            "--memory=1g", "--cpus=1", "--pids-limit=256",
            "-v", &mount_spec,
            "-e", &bench_level_env,
            IMAGE_NAME,
            &bash_cmd,
        ],
        proj,
    )
}

fn run_go_capture(level: &str, proj: &Path) -> Result<(i32, String)> {
    let lang_dir = proj.join("bench/go");

    let exit = run_cmd("bash", &["build.sh"], &lang_dir)?;
    if exit != 0 {
        return Ok((exit, "Go build failed".to_string()));
    }

    let (timeout, filter) = if level == "all" {
        (300, String::new())
    } else {
        (30, format!("TestL{level}"))
    };
    let bench_level = if level == "all" {
        LEVELS.last().expect("no levels defined").to_string()
    } else {
        level.to_string()
    };

    let bench_dir = proj.join("bench");
    let bin_mount = format!("{}:/bench/go/test_bin:ro,Z", lang_dir.join("test_bin").display());
    let fixtures_mount = format!("{}:/bench/fixtures:ro,Z", bench_dir.join("fixtures").display());
    let tests_mount = format!("{}:/bench/tests.json:ro,Z", bench_dir.join("tests.json").display());
    let bench_level_env = format!("BENCH_LEVEL={bench_level}");

    let bash_cmd = if filter.is_empty() {
        format!("cd /bench/go && timeout {timeout}s ./test_bin -test.v -test.timeout {timeout}s 2>&1")
    } else {
        format!("cd /bench/go && timeout {timeout}s ./test_bin -test.run '{filter}' -test.v -test.timeout {timeout}s 2>&1")
    };

    run_cmd_capture_all(
        "sudo",
        &[
            "podman", "run", "--rm",
            "--memory=1g", "--cpus=1", "--pids-limit=256",
            "-v", &bin_mount,
            "-v", &fixtures_mount,
            "-v", &tests_mount,
            "-e", &bench_level_env,
            IMAGE_NAME,
            &bash_cmd,
        ],
        proj,
    )
}

fn run_gradle_capture(level: &str, proj: &Path) -> Result<(i32, String)> {
    let lang_dir = proj.join("bench/java");

    let exit = run_cmd("./gradlew", &["shadowJar"], &lang_dir)?;
    if exit != 0 {
        return Ok((exit, "Java build failed".to_string()));
    }

    let jar = lang_dir.join("build/libs/ming-test.jar");
    if !jar.is_file() {
        return Ok((1, "shadow JAR not found after build".to_string()));
    }

    run_jvm_container_capture(proj, &jar, level)
}

fn run_mill_capture(level: &str, proj: &Path) -> Result<(i32, String)> {
    let lang_dir = proj.join("bench/scala");

    let exit = run_cmd("./mill", &["assembly"], &lang_dir)?;
    if exit != 0 {
        return Ok((exit, "Scala build failed".to_string()));
    }

    let jar = lang_dir.join("out/assembly.dest/out.jar");
    if !jar.is_file() {
        return Ok((1, "assembly JAR not found after build".to_string()));
    }

    run_jvm_container_capture(proj, &jar, level)
}

fn run_jvm_container_capture(proj: &Path, jar: &Path, level: &str) -> Result<(i32, String)> {
    let (timeout, level_arg) = if level == "all" {
        (450, "all".to_string())
    } else {
        (45, level.to_string())
    };

    let bench_dir = proj.join("bench");
    let jar_mount = format!("{}:/bench/test.jar:ro,Z", jar.display());
    let fixtures_mount = format!("{}:/bench/fixtures:ro,Z", bench_dir.join("fixtures").display());
    let tests_mount = format!("{}:/bench/tests.json:ro,Z", bench_dir.join("tests.json").display());
    let java_cmd = format!("timeout {timeout}s java -jar /bench/test.jar {level_arg}");

    run_cmd_capture_all(
        "sudo",
        &[
            "podman", "run", "--rm",
            "--memory=2g", "--cpus=1", "--pids-limit=256",
            "-v", &jar_mount,
            "-v", &fixtures_mount,
            "-v", &tests_mount,
            "-e", "TESTS_JSON=/bench/tests.json",
            "-e", "FIXTURES_DIR=/bench/fixtures",
            JVM_IMAGE,
            &java_cmd,
        ],
        proj,
    )
}

fn run_ts_capture(level: &str, proj: &Path) -> Result<(i32, String)> {
    let lang_dir = proj.join("bench/ts");

    let exit = run_cmd("bash", &["build.sh"], &lang_dir)?;
    if exit != 0 {
        return Ok((exit, "TypeScript build failed".to_string()));
    }

    let (timeout, name_pattern) = if level == "all" {
        (300, String::new())
    } else {
        (30, format!("l{level}"))
    };
    let bench_level = if level == "all" {
        LEVELS.last().expect("no levels defined").to_string()
    } else {
        level.to_string()
    };

    let bench_dir = proj.join("bench");
    let ts_mount = format!("{}:/bench/ts:Z", lang_dir.display());
    let fixtures_mount = format!("{}:/bench/fixtures:ro,Z", bench_dir.join("fixtures").display());
    let tests_mount = format!("{}:/bench/tests.json:ro,Z", bench_dir.join("tests.json").display());
    let bench_level_env = format!("BENCH_LEVEL={bench_level}");

    let bash_cmd = if name_pattern.is_empty() {
        format!("cd /bench/ts && timeout {timeout}s npx vitest run --reporter=verbose 2>&1")
    } else {
        format!("cd /bench/ts && timeout {timeout}s npx vitest run --reporter=verbose --testNamePattern '{name_pattern}' 2>&1")
    };

    run_cmd_capture_all(
        "sudo",
        &[
            "podman", "run", "--rm",
            "--memory=2g", "--cpus=1", "--pids-limit=256",
            "-v", &ts_mount,
            "-v", &fixtures_mount,
            "-v", &tests_mount,
            "-e", &bench_level_env,
            NODE_IMAGE,
            &bash_cmd,
        ],
        proj,
    )
}

/// Find test binary — used by both run_rust() and run_rust_capture().
fn find_test_binary_in(proj: &Path) -> Result<String> {
    let (exit, output) = run_cmd_capture_all("cargo", &["test", "--no-run", "--release"], proj)?;

    for line in output.lines() {
        let Some(start) = line.find("target/release/deps/ming-") else {
            continue;
        };
        let bin: String = line[start..]
            .chars()
            .take_while(|c| c.is_alphanumeric() || matches!(c, '/' | '-' | '_' | '.'))
            .collect();
        if proj.join(&bin).is_file() {
            return Ok(bin);
        }
    }

    if exit != 0 {
        eprintln!("Build output:\n{output}");
    }

    Err(Error::TestBinaryNotFound)
}

fn find_test_binary(proj: &Path) -> Result<String> {
    // Build test binary in release mode
    let (exit, output) = run_cmd_capture_all("cargo", &["test", "--no-run", "--release"], proj)?;

    // Parse output for binary path
    for line in output.lines() {
        let Some(start) = line.find("target/release/deps/ming-") else {
            continue;
        };
        let bin: String = line[start..]
            .chars()
            .take_while(|c| c.is_alphanumeric() || matches!(c, '/' | '-' | '_' | '.'))
            .collect();
        if proj.join(&bin).is_file() {
            return Ok(bin);
        }
    }

    if exit != 0 {
        eprintln!("Build output:\n{output}");
    }

    Err(Error::TestBinaryNotFound)
}
