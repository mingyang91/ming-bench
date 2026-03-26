/// Evaluation error type for the Scheme interpreter.
///
/// Agents must add domain-specific variants here. Using `String` as the
/// error type is not possible — the `eval_str` signature requires this type.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("byte {offset}: {source}")]
    WithOffset {
        offset: usize,
        source: Box<EvalError>,
    },
    #[error("{line}:{column}: {source}")]
    WithPosition {
        line: usize,
        column: usize,
        source: Box<EvalError>,
    },
    #[error("syntax error: expected at least one expression")]
    EmptyProgram,
    #[error("syntax error: unexpected end of input")]
    UnexpectedEof,
    #[error("syntax error: unexpected ')'")]
    UnexpectedCloseParen,
    #[error("syntax error: cannot evaluate empty list")]
    EmptyList,
    #[error("syntax error: invalid escape sequence \\{escape}")]
    InvalidEscape { escape: char },
    #[error("syntax error: {message}")]
    InvalidSyntax { message: String },
    #[error("unbound variable: {name}")]
    UnboundVariable { name: String },
    #[error("unknown procedure: {name}")]
    UnknownProcedure { name: String },
    #[error("not a procedure: {found}")]
    NotAProcedure { found: String },
    #[error("wrong argument count for {name}: expected {expected}, got {got}")]
    WrongArgCount {
        name: String,
        expected: String,
        got: usize,
    },
    #[error("type mismatch: expected {expected}, got {found}")]
    TypeMismatch { expected: String, found: String },
    #[error("division by zero")]
    DivisionByZero,
}

impl EvalError {
    pub fn with_offset(self, offset: usize) -> Self {
        match self {
            Self::WithOffset { .. } | Self::WithPosition { .. } => self,
            other => Self::WithOffset {
                offset,
                source: Box::new(other),
            },
        }
    }

    pub fn resolve_positions(self, input: &str) -> Self {
        match self {
            Self::WithOffset { offset, source } => {
                let (line, column) = offset_to_line_col(input, offset);
                Self::WithPosition {
                    line,
                    column,
                    source: Box::new(source.resolve_positions(input)),
                }
            }
            Self::WithPosition {
                line,
                column,
                source,
            } => Self::WithPosition {
                line,
                column,
                source: Box::new(source.resolve_positions(input)),
            },
            other => other,
        }
    }
}

fn offset_to_line_col(input: &str, offset: usize) -> (usize, usize) {
    let mut line = 1;
    let mut column = 1;

    for ch in input[..offset.min(input.len())].chars() {
        if ch == '\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }

    (line, column)
}
