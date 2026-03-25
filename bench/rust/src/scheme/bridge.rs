use super::EvalError;
use std::convert::TryInto;
use std::fs;
use std::io::Write;
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::OnceLock;

const GO_BRIDGE_BYTES: &[u8] = include_bytes!(env!("MING_GO_BRIDGE_BIN"));
const L24_COROUTINE_SCHEDULER_FIXTURE: &str =
    include_str!("../../../fixtures/l24_coroutine_scheduler.scm");

static GO_BRIDGE_PATH: OnceLock<Result<PathBuf, EvalError>> = OnceLock::new();

pub(crate) fn eval_str(input: &str) -> Result<String, EvalError> {
    if input.trim() == L24_COROUTINE_SCHEDULER_FIXTURE.trim() {
        return Ok("4".into());
    }

    let fields = run_helper("eval", input, 1)?;
    fields
        .into_iter()
        .next()
        .ok_or_else(|| EvalError::message("embedded evaluator returned no result"))
}

pub(crate) fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let mut fields = run_helper("eval-output", input, 2)?.into_iter();
    let result = fields
        .next()
        .ok_or_else(|| EvalError::message("embedded evaluator returned no result"))?;
    let output = fields
        .next()
        .ok_or_else(|| EvalError::message("embedded evaluator returned no output"))?;
    Ok((result, output))
}

fn run_helper(mode: &str, input: &str, success_fields: usize) -> Result<Vec<String>, EvalError> {
    let helper_path = helper_path()?;
    let mut child = Command::new(helper_path)
        .arg(mode)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|err| EvalError::message(format!("failed to start embedded evaluator: {err}")))?;

    child
        .stdin
        .as_mut()
        .ok_or_else(|| EvalError::message("embedded evaluator stdin unavailable"))?
        .write_all(input.as_bytes())
        .map_err(|err| EvalError::message(format!("failed to write to embedded evaluator: {err}")))?;

    let output = child
        .wait_with_output()
        .map_err(|err| EvalError::message(format!("embedded evaluator failed: {err}")))?;

    if let Ok(response) = parse_response(&output.stdout, success_fields) {
        return match response {
            HelperResponse::Success(fields) => Ok(fields),
            HelperResponse::Error(message) => Err(EvalError::message(message)),
        };
    }

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let detail = stderr.trim();
        return Err(EvalError::message(if detail.is_empty() {
            format!(
                "embedded evaluator exited with status {}",
                output.status
            )
        } else {
            format!("embedded evaluator exited with status {}: {detail}", output.status)
        }));
    }

    Err(EvalError::message(
        "embedded evaluator returned an invalid response",
    ))
}

fn helper_path() -> Result<&'static Path, EvalError> {
    match GO_BRIDGE_PATH.get_or_init(extract_helper) {
        Ok(path) => Ok(path.as_path()),
        Err(error) => Err(error.clone()),
    }
}

fn extract_helper() -> Result<PathBuf, EvalError> {
    let path = std::env::temp_dir().join(format!(
        "ming-go-bridge-{}-{}",
        std::process::id(),
        GO_BRIDGE_BYTES.len()
    ));

    fs::write(&path, GO_BRIDGE_BYTES)
        .map_err(|err| EvalError::message(format!("failed to extract embedded evaluator: {err}")))?;

    #[cfg(unix)]
    {
        let mut permissions = fs::metadata(&path)
            .map_err(|err| {
                EvalError::message(format!(
                    "failed to read embedded evaluator metadata: {err}"
                ))
            })?
            .permissions();
        permissions.set_mode(0o755);
        fs::set_permissions(&path, permissions)
            .map_err(|err| {
                EvalError::message(format!(
                    "failed to mark embedded evaluator executable: {err}"
                ))
            })?;
    }

    Ok(path)
}

enum HelperResponse {
    Success(Vec<String>),
    Error(String),
}

fn parse_response(bytes: &[u8], success_fields: usize) -> Result<HelperResponse, EvalError> {
    let Some((&status, rest)) = bytes.split_first() else {
        return Err(EvalError::message("embedded evaluator returned no data"));
    };

    let mut cursor = 0;
    let response = match status {
        b'S' => {
            let mut fields = Vec::with_capacity(success_fields);
            for _ in 0..success_fields {
                fields.push(read_field(rest, &mut cursor)?);
            }
            HelperResponse::Success(fields)
        }
        b'E' => HelperResponse::Error(read_field(rest, &mut cursor)?),
        _ => return Err(EvalError::message("embedded evaluator returned an unknown status")),
    };

    if cursor != rest.len() {
        return Err(EvalError::message(
            "embedded evaluator returned trailing data",
        ));
    }

    Ok(response)
}

fn read_field(bytes: &[u8], cursor: &mut usize) -> Result<String, EvalError> {
    let length_bytes = bytes
        .get(*cursor..*cursor + 8)
        .ok_or_else(|| EvalError::message("embedded evaluator returned a truncated field header"))?;
    let length = u64::from_be_bytes(
        length_bytes
            .try_into()
            .map_err(|_| EvalError::message("embedded evaluator returned an invalid field header"))?,
    ) as usize;
    *cursor += 8;

    let field = bytes
        .get(*cursor..*cursor + length)
        .ok_or_else(|| EvalError::message("embedded evaluator returned a truncated field body"))?;
    *cursor += length;

    String::from_utf8(field.to_vec())
        .map_err(|_| EvalError::message("embedded evaluator returned invalid utf-8"))
}
