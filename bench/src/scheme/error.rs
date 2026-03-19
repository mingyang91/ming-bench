use thiserror::Error;

use crate::scheme::value::Value;

#[derive(Debug, Error, Clone)]
pub(crate) enum SchemeError {
    #[error("input did not contain any expressions")]
    EmptyInput,
    #[error("unexpected end of input while parsing {context}")]
    UnexpectedEndOfInput { context: &'static str },
    #[error("unexpected `)` at byte {index}")]
    UnexpectedCloseParen { index: usize },
    #[error("invalid token starting with `{ch}` at byte {index}")]
    InvalidTokenStart { ch: char, index: usize },
    #[error("invalid boolean literal `{literal}` at byte {index}")]
    InvalidBooleanLiteral { literal: String, index: usize },
    #[error("invalid integer literal `{literal}` at byte {index}")]
    InvalidInteger { literal: String, index: usize },
    #[error("unterminated string literal starting at byte {start}")]
    UnterminatedString { start: usize },
    #[error("invalid escape sequence `\\{escape}` at byte {index}")]
    InvalidEscape { escape: char, index: usize },
    #[error("application requires at least one expression")]
    EmptyApplication,
    #[error("unbound symbol `{name}`")]
    UnboundSymbol { name: String },
    #[error("cannot call a {kind}")]
    NonCallable { kind: &'static str },
    #[error("`{operator}` expected a number but got {found}")]
    ExpectedNumber {
        operator: &'static str,
        found: Value,
    },
    #[error("`{operator}` expected a pair but got {found}")]
    ExpectedPair {
        operator: &'static str,
        found: Value,
    },
    #[error("`{operator}` expected a list but got {found}")]
    ExpectedList {
        operator: &'static str,
        found: Value,
    },
    #[error("`{operator}` expected at least {min} argument(s) but got {actual}")]
    TooFewArguments {
        operator: &'static str,
        min: usize,
        actual: usize,
    },
    #[error("`{operator}` expected exactly {expected} argument(s) but got {actual}")]
    WrongArgumentCount {
        operator: &'static str,
        expected: usize,
        actual: usize,
    },
    #[error("`define` expected a symbol name but got {found}")]
    InvalidDefinitionTarget { found: &'static str },
    #[error("`set!` expected a symbol name but got {found}")]
    InvalidAssignmentTarget { found: &'static str },
    #[error("`{operator}` expected a binding list but got {found}")]
    InvalidBindingList {
        operator: &'static str,
        found: &'static str,
    },
    #[error("`{operator}` expected a binding pair but got {found}")]
    InvalidBinding {
        operator: &'static str,
        found: &'static str,
    },
    #[error("`{operator}` binding expected a symbol name but got {found}")]
    InvalidBindingName {
        operator: &'static str,
        found: &'static str,
    },
    #[error("`{operator}` has duplicate binding `{name}`")]
    DuplicateBinding {
        operator: &'static str,
        name: String,
    },
    #[error("`{operator}` expected a parameter list but got {found}")]
    InvalidParameterList {
        operator: &'static str,
        found: &'static str,
    },
    #[error("`{operator}` has an invalid dotted parameter list")]
    InvalidDottedParameterList { operator: &'static str },
    #[error("`{operator}` parameter expected a symbol but got {found}")]
    InvalidParameterName {
        operator: &'static str,
        found: &'static str,
    },
    #[error("`{operator}` has duplicate parameter `{name}`")]
    DuplicateParameter {
        operator: &'static str,
        name: String,
    },
    #[error("procedure expected at least {min} argument(s) but got {actual}")]
    TooFewProcedureArguments { min: usize, actual: usize },
    #[error("procedure expected exactly {expected} argument(s) but got {actual}")]
    WrongProcedureArgumentCount { expected: usize, actual: usize },
    #[error("`cond` expected a clause list but got {found}")]
    InvalidCondClause { found: &'static str },
    #[error("`cond` else clause must be last")]
    CondElseNotLast,
    #[error("cannot divide by zero")]
    DivisionByZero,
}
