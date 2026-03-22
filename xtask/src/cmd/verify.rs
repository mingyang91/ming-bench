use crate::model::{command_exists, fixtures_dir, load_tests_json, run_cmd_capture, Error, Result, TestEntry};
use std::path::Path;

/// Supported ground-truth Scheme implementations.
fn impl_command(name: &str) -> Result<(&'static str, Vec<&'static str>)> {
    match name {
        "guile" => Ok(("guile", vec!["--no-auto-compile", "-c"])),
        "chez" => Ok(("chez-scheme", vec!["--quiet", "--script"])),
        _ => Err(Error::CommandFailed {
            cmd: format!("unknown implementation: {name}"),
            exit_code: 1,
        }),
    }
}

/// Run a fixture file through the ground-truth implementation.
/// Returns (exit_code, stdout).
fn run_fixture(
    bin: &str,
    implementation: &str,
    fixture_path: &std::path::Path,
    kind: &str,
    cwd: &std::path::Path,
) -> std::result::Result<(i32, String), Error> {
    match kind {
        "eval_str_ok" => {
            // Load the fixture, write the result.
            // Guile: (write (load "path")) (newline)
            let load_expr = format!(
                "(write (load \"{}\")) (newline)",
                fixture_path.display()
            );
            if implementation == "chez" {
                run_chez(bin, &load_expr, cwd)
            } else {
                run_cmd_capture(bin, &["--no-auto-compile", "-c", &load_expr], cwd)
            }
        }
        "eval_str_err" | "eval_str_err_with_position" => {
            // Load the fixture — expect non-zero exit
            let load_expr = format!("(load \"{}\")", fixture_path.display());
            if implementation == "chez" {
                run_chez(bin, &load_expr, cwd)
            } else {
                run_cmd_capture(bin, &["--no-auto-compile", "-c", &load_expr], cwd)
            }
        }
        _ => {
            // Default: just load
            let load_expr = format!("(load \"{}\")", fixture_path.display());
            if implementation == "chez" {
                run_chez(bin, &load_expr, cwd)
            } else {
                run_cmd_capture(bin, &["--no-auto-compile", "-c", &load_expr], cwd)
            }
        }
    }
}

pub fn run(level: Option<&str>, implementation: &str) -> Result<()> {
    let (bin, _base_args) = impl_command(implementation)?;

    if !command_exists(bin) {
        return Err(Error::BinaryNotFound {
            name: bin.to_string(),
        });
    }

    let tests = load_tests_json()?;
    let fix_dir = fixtures_dir();
    let cwd = std::env::current_dir().expect("cannot read cwd");

    // Filter by level if specified
    let filtered: Vec<&TestEntry> = tests
        .iter()
        .filter(|t| match level {
            Some(l) => format!("{:02}", t.level) == l || t.level.to_string() == l,
            None => true,
        })
        .collect();

    if filtered.is_empty() {
        println!("No tests match level filter '{}'", level.unwrap_or("all"));
        return Ok(());
    }

    let mut passed = 0u32;
    let mut failed = 0u32;
    let mut skipped = 0u32;
    let mut errors: Vec<String> = Vec::new();
    let mut current_level = 0u32;

    for test in &filtered {
        // Print section headers
        if test.level != current_level {
            current_level = test.level;
            println!("=== Level {current_level} ===");
        }

        // Load fixture
        let fixture_path = fix_dir.join(&test.fixture);
        if !fixture_path.is_file() {
            skipped += 1;
            errors.push(format!("  SKIP {}: fixture not found: {}", test.name, fixture_path.display()));
            continue;
        }

        let result = run_fixture(bin, implementation, &fixture_path, &test.kind, &cwd);

        match result {
            Ok((exit_code, actual)) => {
                let actual = actual.trim_end();
                match test.kind.as_str() {
                    "eval_str_ok" => {
                        let expected = test.expected.as_deref().unwrap_or("");
                        if exit_code != 0 {
                            failed += 1;
                            errors.push(format!("  FAIL {}: {implementation} error (exit {exit_code})", test.name));
                        } else if actual == expected {
                            passed += 1;
                        } else {
                            failed += 1;
                            errors.push(format!(
                                "  FAIL {}: expected '{}', got '{actual}'",
                                test.name, expected
                            ));
                        }
                    }
                    "eval_str_err" | "eval_str_err_with_position" => {
                        if exit_code != 0 {
                            passed += 1; // error expected
                        } else {
                            failed += 1;
                            errors.push(format!(
                                "  FAIL {}: expected error but got success: '{actual}'",
                                test.name
                            ));
                        }
                    }
                    "eval_str_with_output" => {
                        // Check expected_output against stdout
                        let expected_output = test.expected_output.as_deref().unwrap_or("");
                        if exit_code != 0 {
                            failed += 1;
                            errors.push(format!("  FAIL {}: {implementation} error (exit {exit_code})", test.name));
                        } else if actual.contains(expected_output) {
                            passed += 1;
                        } else {
                            failed += 1;
                            errors.push(format!(
                                "  FAIL {}: expected output containing '{}', got '{actual}'",
                                test.name, expected_output
                            ));
                        }
                    }
                    _ => {
                        skipped += 1;
                    }
                }
            }
            Err(_) => {
                failed += 1;
                errors.push(format!("  FAIL {}: failed to run {implementation}", test.name));
            }
        }
    }

    println!();
    println!("=== Results ({implementation}) ===");
    println!("  Passed:  {passed}");
    println!("  Failed:  {failed}");
    if skipped > 0 {
        println!("  Skipped: {skipped}");
    }

    if failed > 0 {
        for err in &errors {
            eprintln!("{err}");
        }
        Err(Error::CommandFailed {
            cmd: "verify".to_string(),
            exit_code: 1,
        })
    } else {
        println!("  All {passed} tests match ground truth!");
        Ok(())
    }
}

/// Chez Scheme needs a temp file (no -c flag for eval).
fn run_chez(bin: &str, expr: &str, cwd: &Path) -> std::result::Result<(i32, String), Error> {
    let tmp = std::env::temp_dir().join("ming_verify.scm");
    std::fs::write(&tmp, expr).map_err(|e| Error::CommandFailed {
        cmd: format!("write temp: {e}"),
        exit_code: 1,
    })?;
    let result = run_cmd_capture(bin, &["--quiet", "--script", tmp.to_str().unwrap()], cwd);
    let _ = std::fs::remove_file(&tmp);
    result
}
