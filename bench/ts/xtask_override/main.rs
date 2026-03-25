use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};

const PROJECT_ROOT: &str = "/home/my/.zeroclaw/workspace/workspace/cx-ts-def-r27";
const MANIFEST_PATH: &str = "/home/my/.zeroclaw/workspace/workspace/cx-ts-def-r27/bench/rust/Cargo.toml";
const TARGET_DIR: &str = "/home/my/.zeroclaw/workspace/workspace/cx-ts-def-r27/bench/ts/.cargo-target";
const WRAPPER_PATH: &str =
    "/home/my/.zeroclaw/workspace/workspace/cx-ts-def-r27/bench/ts/.cargo/rustc-wrapper.sh";

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(message) => {
            eprintln!("error: {message}");
            ExitCode::FAILURE
        }
    }
}

fn run() -> Result<(), String> {
    let args: Vec<String> = std::env::args().collect();
    let Some(command) = args.get(1).map(String::as_str) else {
        return Err("missing subcommand".to_string());
    };

    if command != "test" {
        return Err("only `cargo xtask test <level>` is supported in this sandbox".to_string());
    }

    let Some(level) = args.get(2).map(String::as_str) else {
        return Err("missing test level".to_string());
    };

    if level != "25" {
        return Err("only level 25 is supported for this task".to_string());
    }

    build_ming()?;
    let test_binary = find_test_binary(Path::new(TARGET_DIR).join("release").join("deps"))?;
    run_test_binary(&test_binary, level)
}

fn build_ming() -> Result<(), String> {
    let status = Command::new("cargo")
        .arg("test")
        .arg("--manifest-path")
        .arg(MANIFEST_PATH)
        .arg("--no-run")
        .arg("--release")
        .env("CARGO_TARGET_DIR", TARGET_DIR)
        .env("RUSTC_WRAPPER", WRAPPER_PATH)
        .current_dir(PROJECT_ROOT)
        .status()
        .map_err(|error| format!("failed to start cargo test --no-run: {error}"))?;

    if status.success() {
        Ok(())
    } else {
        Err(format!(
            "cargo test --no-run failed with status {}",
            status.code().unwrap_or(1)
        ))
    }
}

fn find_test_binary(deps_dir: PathBuf) -> Result<PathBuf, String> {
    let entries = fs::read_dir(&deps_dir)
        .map_err(|error| format!("failed to read {}: {error}", deps_dir.display()))?;

    for entry in entries {
        let entry = entry.map_err(|error| format!("failed to read target entry: {error}"))?;
        let path = entry.path();
        let Some(name) = path.file_name().and_then(OsStr::to_str) else {
            continue;
        };

        if !name.starts_with("ming-") || name.ends_with(".d") || name.ends_with(".rlib") {
            continue;
        }

        if path.is_file() && is_executable(&path) {
            return Ok(path);
        }
    }

    Err("test binary not found after compilation".to_string())
}

fn is_executable(path: &Path) -> bool {
    use std::os::unix::fs::PermissionsExt;

    fs::metadata(path)
        .map(|metadata| metadata.permissions().mode() & 0o111 != 0)
        .unwrap_or(false)
}

fn run_test_binary(test_binary: &Path, level: &str) -> Result<(), String> {
    let filter = format!("test_l{level}");
    let status = Command::new(test_binary)
        .arg(filter)
        .arg("--test-threads=1")
        .env("BENCH_LEVEL", level)
        .status()
        .map_err(|error| format!("failed to run {}: {error}", test_binary.display()))?;

    if status.success() {
        Ok(())
    } else {
        Err(format!(
            "test binary failed with status {}",
            status.code().unwrap_or(1)
        ))
    }
}
