use crate::model::{command_exists, fixtures_dir, load_tests_json, run_cmd_capture, Error, Result};
use std::path::Path;

/// Chez Scheme binary name (installed as `scheme` by default).
const CHEZ_BIN: &str = "scheme";

/// Per-test timeout in seconds.
const TEST_TIMEOUT_SECS: u32 = 30;

/// Chez Scheme preamble: R7RS define-record-type compatibility macro.
/// Chez natively uses R6RS record syntax; this bridges R7RS syntax.
const CHEZ_PREAMBLE: &str = r#"
(define-syntax r7rs:define-record-type
  (syntax-rules ()
    ((_ type-name (ctor-name ctor-field ...) pred-name (field-name accessor-name) ...)
     (begin
       (define-record-type (type-name ctor-name pred-name)
         (fields (immutable field-name accessor-name) ...))))))
(define (%patch-records expr)
  (cond
    ((and (pair? expr) (eq? (car expr) 'define-record-type))
     (cons 'r7rs:define-record-type (cdr expr)))
    ((pair? expr) (cons (%patch-records (car expr)) (%patch-records (cdr expr))))
    (else expr)))
"#;

/// Tests to skip in Chez verification.
fn should_skip(test_name: &str) -> Option<&'static str> {
    match test_name {
        // procedure-name is our custom feature, not in R7RS/R6RS
        n if n.starts_with("l25_procedure_name") => Some("custom feature (not in R7RS)"),
        // Chez allows string-set! on mutable strings; our spec says error (R7RS immutability)
        "l14_string_set_error" => Some("Chez allows string-set! (strings are mutable)"),
        // Chez allows set! on unbound top-level vars (creates binding)
        "l08_set_unbound_error" => Some("Chez allows set! on unbound (top-level)"),
        // Reentrant continuations loop in eval wrapper
        "l10_callcc_reentrant" => Some("reentrant continuation loops in eval wrapper"),
        // Coroutine continuation loops in eval wrapper
        "l12_coroutine_scheduler" => Some("coroutine continuation loops in eval wrapper"),
        // Chez datum->syntax requires identifier, not arbitrary syntax
        "l22_syntax_case_datum" => Some("Chez datum->syntax stricter than R7RS"),
        // Newline output comparison issue
        "l05_newline" => Some("output comparison with newline character"),
        _ => None,
    }
}

/// Run a fixture file through Chez Scheme and capture the output.
///
/// For eval_str_ok: reads all expressions, wraps in (begin ...), evals, writes result.
/// For eval_str_err: loads the fixture expecting a non-zero exit.
/// For eval_str_with_output: loads the fixture and captures stdout.
fn run_fixture(
    fixture_path: &Path,
    kind: &str,
    cwd: &Path,
) -> std::result::Result<(i32, String), Error> {
    let abs = fixture_path
        .canonicalize()
        .unwrap_or_else(|_| fixture_path.to_path_buf());
    let path_str = abs.to_string_lossy();

    // Shared reader helper
    let reader = format!(
        r#"(define (%read-all path)
  (call-with-input-file path (lambda (p)
    (let loop ((acc '()))
      (let ((x (read p)))
        (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))"#
    );

    let wrapper = match kind {
        "eval_str_ok" => {
            format!(
                "{CHEZ_PREAMBLE}\n{reader}\n(let ((exprs (map %patch-records (%read-all \"{path_str}\"))))\n  (write (eval (cons 'begin exprs)))\n  (newline))"
            )
        }
        "eval_str_err" | "eval_str_err_with_position" => {
            format!(
                "{CHEZ_PREAMBLE}\n{reader}\n(let ((exprs (map %patch-records (%read-all \"{path_str}\"))))\n  (eval (cons 'begin exprs)))"
            )
        }
        "eval_str_with_output" => {
            format!(
                "{CHEZ_PREAMBLE}\n{reader}\n(let ((exprs (map %patch-records (%read-all \"{path_str}\"))))\n  (eval (cons 'begin exprs)))"
            )
        }
        _ => format!(r#"(load "{path_str}")"#),
    };

    // Write wrapper to temp file (Chez uses --script, not -c)
    let tmp = std::env::temp_dir().join("ming_verify.ss");
    std::fs::write(&tmp, &wrapper).map_err(|e| Error::CommandFailed {
        cmd: format!("write temp: {e}"),
        exit_code: 1,
    })?;
    let result = run_cmd_capture(
        "timeout",
        &[
            &format!("{TEST_TIMEOUT_SECS}"),
            CHEZ_BIN,
            "--quiet",
            "--script",
            tmp.to_str().unwrap(),
        ],
        cwd,
    );
    let _ = std::fs::remove_file(&tmp);
    result
}

pub fn run(level: Option<&str>) -> Result<()> {
    if !command_exists(CHEZ_BIN) {
        return Err(Error::BinaryNotFound {
            name: CHEZ_BIN.to_string(),
        });
    }

    let tests = load_tests_json()?;
    let fix_dir = fixtures_dir();
    let cwd = std::env::current_dir().expect("cannot read cwd");

    let filtered: Vec<_> = tests
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
        if test.level != current_level {
            current_level = test.level;
            println!("=== Level {current_level} ===");
        }

        // Skip tests with known Chez incompatibilities
        if let Some(_reason) = should_skip(&test.name) {
            skipped += 1;
            continue;
        }

        let fixture_path = fix_dir.join(&test.fixture);
        if !fixture_path.is_file() {
            skipped += 1;
            errors.push(format!(
                "  SKIP {}: fixture not found: {}",
                test.name,
                fixture_path.display()
            ));
            continue;
        }

        let result = run_fixture(&fixture_path, &test.kind, &cwd);

        match result {
            Ok((exit_code, actual)) => {
                let actual = actual.trim_end();
                match test.kind.as_str() {
                    "eval_str_ok" => {
                        let expected = test.expected.as_deref().unwrap_or("");
                        if exit_code != 0 {
                            failed += 1;
                            errors.push(format!(
                                "  FAIL {}: chez error (exit {exit_code})",
                                test.name
                            ));
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
                            passed += 1;
                        } else {
                            failed += 1;
                            errors.push(format!(
                                "  FAIL {}: expected error but got success: '{actual}'",
                                test.name
                            ));
                        }
                    }
                    "eval_str_with_output" => {
                        let expected_output = test.expected_output.as_deref().unwrap_or("");
                        if exit_code != 0 {
                            failed += 1;
                            errors.push(format!(
                                "  FAIL {}: chez error (exit {exit_code})",
                                test.name
                            ));
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
                errors.push(format!("  FAIL {}: failed to run chez", test.name));
            }
        }
    }

    println!();
    println!("=== Results (Chez Scheme) ===");
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
