use crate::scheme::env::Env;
use crate::scheme::value::Value;

/// Continuation frames for the CEK machine.
#[derive(Debug, Clone)]
pub enum Frame {
    /// Operator just evaluated; args still need evaluation (right-to-left).
    EvalOp {
        todo: Vec<Value>,
        env: Env,
    },
    /// Evaluating arguments right-to-left for a call.
    EvalArgs {
        op: Value,
        done: Vec<Value>,
        todo: Vec<Value>,
        env: Env,
    },
    /// `(define name <value being evaluated>)`
    Define {
        name: String,
        env: Env,
    },
    /// `(if <test evaluated> then_expr else_expr)`
    If {
        then_expr: Value,
        else_expr: Value,
        env: Env,
    },
    /// `(set! name <value being evaluated>)`
    SetBang {
        name: String,
        env: Env,
    },
    /// Sequence of expressions; discard current value, eval next.
    Seq {
        remaining: Vec<Value>,
        env: Env,
    },
    /// Short-circuit `and`: remaining expressions to test.
    And {
        remaining: Vec<Value>,
        env: Env,
    },
    /// Short-circuit `or`: remaining expressions to test.
    Or {
        remaining: Vec<Value>,
        env: Env,
    },
}
