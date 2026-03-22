use crate::model::{project_dir, run_cmd, run_cmd_capture_all, Error, Result, LEVELS};
use std::fs;
use std::path::Path;

const IMAGE_NAME: &str = "ming";

pub fn run(level: &str, gate: bool, lang: &str) -> Result<()> {
    let parsed_lang = crate::model::Lang::from_str(lang).map_err(|msg| Error::CommandFailed {
        cmd: msg,
        exit_code: 1,
    })?;

    match parsed_lang {
        crate::model::Lang::Rust => run_rust(level, gate),
        crate::model::Lang::Scala => run_mill(level, gate),
        _ => run_script(level, gate, &parsed_lang),
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
    let (timeout, tag_arg) = if level == "all" {
        (450, String::new())
    } else {
        (45, format!(" --include-tags=l{level}"))
    };

    let bench_level = if level == "all" {
        LEVELS.last().expect("no levels defined").to_string()
    } else {
        level.to_string()
    };

    let bench_dir = proj.join("bench");
    let jar_mount = format!("{}:/bench/test.jar:ro,Z", jar.display());
    let fixtures_mount = format!(
        "{}:/bench/fixtures:ro,Z",
        bench_dir.join("fixtures").display()
    );
    let tests_mount = format!(
        "{}:/bench/tests.json:ro,Z",
        bench_dir.join("tests.json").display()
    );

    let java_cmd = format!(
        "timeout {timeout}s java -Dbench.dir=/bench -jar /bench/test.jar --bench-level {bench_level}{tag_arg}"
    );

    println!("Running Scala tests (level {level}) in container...");
    let exit = run_cmd(
        "sudo",
        &[
            "podman",
            "run",
            "--rm",
            "--memory=2g",
            "--cpus=1",
            "--pids-limit=256",
            "-v",
            &jar_mount,
            "-v",
            &fixtures_mount,
            "-v",
            &tests_mount,
            JVM_IMAGE,
            &java_cmd,
        ],
        proj,
    )?;
    if exit != 0 {
        return Err(Error::CommandFailed {
            cmd: "podman run (scala test)".to_string(),
            exit_code: exit,
        });
    }
    Ok(())
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

/// Run tests for non-Rust languages by calling build.sh + test.sh scripts.
fn run_script(level: &str, gate: bool, lang: &crate::model::Lang) -> Result<()> {
    let proj = project_dir();
    let lang_dir = lang.bench_dir(&proj);

    if !lang_dir.is_dir() {
        return Err(Error::CommandFailed {
            cmd: format!("bench/{} directory not found", lang.dir_name()),
            exit_code: 1,
        });
    }

    // Build
    let build_script = lang_dir.join("build.sh");
    if build_script.is_file() {
        println!("Building {} tests...", lang.display_name());
        let exit = run_cmd("bash", &["build.sh"], &lang_dir)?;
        if exit != 0 {
            return Err(Error::CommandFailed {
                cmd: format!("build.sh ({})", lang.display_name()),
                exit_code: exit,
            });
        }
    }

    // Test
    let test_script = lang_dir.join("test.sh");
    if !test_script.is_file() {
        return Err(Error::CommandFailed {
            cmd: format!("bench/{}/test.sh not found", lang.dir_name()),
            exit_code: 1,
        });
    }

    println!("Running {} tests (level {level})...", lang.display_name());
    let mut test_args = vec!["test.sh", level];
    if gate {
        test_args.push("--gate");
    }
    let exit = run_cmd("bash", &test_args, &lang_dir)?;

    if exit != 0 {
        return Err(Error::CommandFailed {
            cmd: format!("test.sh ({})", lang.display_name()),
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
    let mod_limit: usize = 500;

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
