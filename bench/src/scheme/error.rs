/// Evaluation error type for the Scheme interpreter.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("unexpected end of input")]
    ParseUnexpectedEnd,
    #[error("unexpected token `{token}` at byte {position}")]
    ParseUnexpectedToken { token: String, position: usize },
    #[error("invalid number `{lexeme}`")]
    ParseInvalidNumber { lexeme: String },
    #[error("unterminated string starting at byte {position}")]
    ParseUnclosedString { position: usize },
    #[error("invalid syntax in {form}: {detail}")]
    InvalidSyntax { form: String, detail: String },
    #[error("invalid binding target in {context}")]
    InvalidBindingTarget { context: String },
    #[error("duplicate parameter `{name}`")]
    DuplicateParameter { name: String },
    #[error("unbound variable `{name}`")]
    UnboundVariable { name: String },
    #[error("value of type `{actual}` is not callable")]
    NotCallable { actual: String },
    #[error("wrong number of arguments for `{procedure}`: expected {expected}, got {actual}")]
    ArgumentCountExact {
        procedure: String,
        expected: usize,
        actual: usize,
    },
    #[error("wrong number of arguments for `{procedure}`: expected at least {minimum}, got {actual}")]
    ArgumentCountAtLeast {
        procedure: String,
        minimum: usize,
        actual: usize,
    },
    #[error("type mismatch: expected `{expected}`, got `{actual}`")]
    TypeMismatch {
        expected: String,
        actual: String,
    },
    #[error("division by zero in `{procedure}`")]
    DivisionByZero { procedure: String },
    #[error("arithmetic overflow in `{procedure}`")]
    ArithmeticOverflow { procedure: String },
    #[error("improper list in `{operation}`")]
    ImproperList { operation: String },
    #[error("no matching syntax-rules clause for `{name}`")]
    MacroNoMatch { name: String },
    #[error("missing {kind} capture {id}")]
    MissingCapture { kind: String, id: u64 },
}
