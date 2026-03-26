use std::rc::Rc;

mod builtins;
pub mod error;
mod macros;
mod model;
mod number;
mod parser;

use builtins::{apply_builtin, eqv_values};
pub use error::EvalError;
use macros::{env_with_expansion_aliases, expand_macro_call, parse_syntax_rules};
use model::{
    Builtin, Env, EnvRef, Expr, Params, Procedure, ProcedureClause, ProcedureKind, RecordInstance,
    RecordProcedure, RecordProcedureKind, RecordType, SchemeString, Value,
};
use parser::Parser;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let (value, _) = eval_program(input)?;
    Ok(value.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let (value, output) = eval_program(input)?;
    Ok((value.render(), output))
}

fn eval_program(input: &str) -> Result<(Value, String), EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    let env = initial_env();
    let mut output = String::new();
    let value = eval_sequence(&exprs, &env, &mut output)?;
    Ok((value, output))
}

fn initial_env() -> EnvRef {
    let env = Env::new(None);

    for builtin in [
        Builtin::Add,
        Builtin::Sub,
        Builtin::Mul,
        Builtin::Div,
        Builtin::Abs,
        Builtin::Modulo,
        Builtin::Remainder,
        Builtin::Quotient,
        Builtin::Min,
        Builtin::Max,
        Builtin::Expt,
        Builtin::ZeroPred,
        Builtin::PositivePred,
        Builtin::NegativePred,
        Builtin::OddPred,
        Builtin::EvenPred,
        Builtin::ExactPred,
        Builtin::InexactPred,
        Builtin::IntegerPred,
        Builtin::RationalPred,
        Builtin::ExactToInexact,
        Builtin::InexactToExact,
        Builtin::Numerator,
        Builtin::Denominator,
        Builtin::Less,
        Builtin::Greater,
        Builtin::Equal,
        Builtin::LessEqual,
        Builtin::EqPred,
        Builtin::EqvPred,
        Builtin::EqualPred,
        Builtin::Not,
        Builtin::Display,
        Builtin::Write,
        Builtin::Newline,
        Builtin::Cons,
        Builtin::Car,
        Builtin::Cdr,
        Builtin::Append,
        Builtin::List,
        Builtin::Length,
        Builtin::ListRef,
        Builtin::ListTail,
        Builtin::ListPred,
        Builtin::Vector,
        Builtin::MakeVector,
        Builtin::VectorRef,
        Builtin::VectorSet,
        Builtin::VectorLength,
        Builtin::VectorPred,
        Builtin::VectorToList,
        Builtin::ListToVector,
        Builtin::Assoc,
        Builtin::Map,
        Builtin::StringAppend,
        Builtin::StringLength,
        Builtin::Substring,
        Builtin::StringToNumber,
        Builtin::NumberToString,
        Builtin::SymbolToString,
        Builtin::StringToSymbol,
        Builtin::StringRef,
        Builtin::StringSet,
        Builtin::StringCopy,
        Builtin::NullPred,
        Builtin::NumberPred,
        Builtin::StringPred,
        Builtin::BooleanPred,
        Builtin::ProcedurePred,
        Builtin::PairPred,
        Builtin::SymbolPred,
        Builtin::CharPred,
        Builtin::CharAlphabeticPred,
        Builtin::CharNumericPred,
        Builtin::CharUpcase,
        Builtin::CharDowncase,
        Builtin::CharEqual,
        Builtin::CharLess,
        Builtin::StringEqual,
        Builtin::StringLess,
        Builtin::StringCiEqual,
        Builtin::StringUpcase,
        Builtin::StringDowncase,
        Builtin::Apply,
    ] {
        env.define(builtin.name().into(), Value::Builtin(builtin));
    }

    env
}

fn eval_sequence(exprs: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let mut result = Value::Void;

    for expr in exprs {
        result = eval(expr, env, output)?;
    }

    Ok(result)
}

fn eval(expr: &Expr, env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let pos = expr.pos();

    match expr {
        Expr::Number(value, _) => Ok(Value::Number(*value)),
        Expr::Boolean(value, _) => Ok(Value::Boolean(*value)),
        Expr::String(value, _) => Ok(Value::String(SchemeString::literal(value))),
        Expr::Char(value, _) => Ok(Value::Char(*value)),
        Expr::Symbol(name, _) => env
            .lookup(name)
            .ok_or_else(|| EvalError::UnboundVariable { name: name.clone() })
            .map_err(|error| error.with_position(pos)),
        Expr::List(items, _) => {
            eval_list(items, env, output).map_err(|error| error.with_position(pos))
        }
    }
}

fn eval_list(items: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::Syntax {
            message: "cannot evaluate empty list".into(),
        });
    };

    if let Expr::Symbol(name, _) = head {
        match name.as_str() {
            "define" => return eval_define(tail, env, output),
            "define-syntax" => return eval_define_syntax(tail, env),
            "define-record-type" => return eval_define_record_type(tail, env),
            "set!" => return eval_set(tail, env, output),
            "if" => return eval_if(tail, env, output),
            "quote" => return eval_quote(tail),
            "lambda" => return build_lambda(tail, env, None),
            "case-lambda" => return build_case_lambda(tail, env, None),
            "and" => return eval_and(tail, env, output),
            "or" => return eval_or(tail, env, output),
            "begin" => return eval_begin(tail, env, output),
            "cond" => return eval_cond(tail, env, output),
            "let" => return eval_let(tail, env, output),
            "letrec" => return eval_letrec(tail, env, output, false),
            "letrec*" => return eval_letrec(tail, env, output, true),
            "case" => return eval_case(tail, env, output),
            "do" => return eval_do(tail, env, output),
            _ => {}
        }

        if let Some(transformer) = env.lookup_macro(name) {
            let expansion = expand_macro_call(items, &transformer)?;
            let expanded_env = env_with_expansion_aliases(env, &expansion);
            return eval(&expansion.expr, &expanded_env, output);
        }
    }

    let callable = eval(head, env, output)?;
    let args = eval_args(tail, env, output)?;
    apply(callable, &args, output)
}

fn eval_define(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name, _), value_expr] => {
            let value = if let Some(parts) = lambda_parts(value_expr) {
                build_lambda(parts, env, Some(name.clone()))?
            } else if let Some(clauses) = case_lambda_clauses(value_expr) {
                build_case_lambda(clauses, env, Some(name.clone()))?
            } else {
                eval(value_expr, env, output)?
            };
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        [Expr::List(signature, _), body @ ..] => {
            let Some((Expr::Symbol(name, _), params)) = signature.split_first() else {
                return Err(EvalError::Syntax {
                    message: "define: expected function name".into(),
                });
            };
            if body.is_empty() {
                return Err(EvalError::Syntax {
                    message: "define: expected function body".into(),
                });
            }

            let value = new_procedure(Some(name.clone()), parse_param_list(params)?, body, env);
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Syntax {
            message: "define: invalid syntax".into(),
        }),
    }
}

fn eval_define_syntax(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name, _), transformer_expr] => {
            let transformer = parse_syntax_rules(transformer_expr, env)?;
            env.define_macro(name.clone(), transformer);
            Ok(Value::Void)
        }
        [_, _] => Err(EvalError::Syntax {
            message: "define-syntax: expected transformer name".into(),
        }),
        _ => Err(wrong_arg_count("define-syntax", "2", args.len())),
    }
}

fn eval_define_record_type(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let [type_name_expr, constructor_expr, predicate_expr, field_exprs @ ..] = args else {
        return Err(EvalError::Syntax {
            message: "define-record-type: invalid syntax".into(),
        });
    };

    let type_name = match type_name_expr {
        Expr::Symbol(name, _) => name.clone(),
        _ => {
            return Err(EvalError::Syntax {
                message: "define-record-type: expected type name".into(),
            });
        }
    };

    let (constructor_name, constructor_arity) = parse_record_constructor(constructor_expr)?;
    let predicate_name = match predicate_expr {
        Expr::Symbol(name, _) => name.clone(),
        _ => {
            return Err(EvalError::Syntax {
                message: "define-record-type: expected predicate name".into(),
            });
        }
    };

    let accessor_names = field_exprs
        .iter()
        .map(parse_record_field)
        .collect::<Result<Vec<_>, _>>()?;

    if constructor_arity != accessor_names.len() {
        return Err(EvalError::Syntax {
            message: "define-record-type: constructor and field count must match".into(),
        });
    }

    let record_type = Rc::new(RecordType {
        name: type_name,
        field_count: accessor_names.len(),
    });

    env.define(
        constructor_name.clone(),
        Value::RecordProcedure(Rc::new(RecordProcedure {
            name: constructor_name,
            kind: RecordProcedureKind::Constructor {
                record_type: record_type.clone(),
            },
        })),
    );

    env.define(
        predicate_name.clone(),
        Value::RecordProcedure(Rc::new(RecordProcedure {
            name: predicate_name,
            kind: RecordProcedureKind::Predicate {
                record_type: record_type.clone(),
            },
        })),
    );

    for (field_index, accessor_name) in accessor_names.into_iter().enumerate() {
        env.define(
            accessor_name.clone(),
            Value::RecordProcedure(Rc::new(RecordProcedure {
                name: accessor_name,
                kind: RecordProcedureKind::Accessor {
                    record_type: record_type.clone(),
                    field_index,
                },
            })),
        );
    }

    Ok(Value::Void)
}

fn parse_record_constructor(expr: &Expr) -> Result<(String, usize), EvalError> {
    let Expr::List(items, _) = expr else {
        return Err(EvalError::Syntax {
            message: "define-record-type: expected constructor spec".into(),
        });
    };

    let Some((Expr::Symbol(name, _), params)) = items.split_first() else {
        return Err(EvalError::Syntax {
            message: "define-record-type: expected constructor name".into(),
        });
    };

    for param in params {
        if !matches!(param, Expr::Symbol(_, _)) {
            return Err(EvalError::Syntax {
                message: "define-record-type: expected constructor field name".into(),
            });
        }
    }

    Ok((name.clone(), params.len()))
}

fn parse_record_field(expr: &Expr) -> Result<String, EvalError> {
    let Expr::List(items, _) = expr else {
        return Err(EvalError::Syntax {
            message: "define-record-type: expected field spec".into(),
        });
    };

    match items.as_slice() {
        [Expr::Symbol(_, _), Expr::Symbol(accessor, _)] => Ok(accessor.clone()),
        _ => Err(EvalError::Syntax {
            message: "define-record-type: expected (field accessor)".into(),
        }),
    }
}

fn eval_set(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name, _), value_expr] => {
            let value = eval(value_expr, env, output)?;
            if env.set(name, value) {
                Ok(Value::Void)
            } else {
                Err(EvalError::UnboundVariable { name: name.clone() })
            }
        }
        [_, _] => Err(EvalError::Syntax {
            message: "set!: expected variable name".into(),
        }),
        _ => Err(wrong_arg_count("set!", "2", args.len())),
    }
}

fn eval_if(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [condition, then_branch] => {
            if eval(condition, env, output)?.is_truthy() {
                eval(then_branch, env, output)
            } else {
                Ok(Value::Void)
            }
        }
        [condition, then_branch, else_branch] => {
            if eval(condition, env, output)?.is_truthy() {
                eval(then_branch, env, output)
            } else {
                eval(else_branch, env, output)
            }
        }
        _ => Err(wrong_arg_count("if", "2 or 3", args.len())),
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    match args {
        [expr] => Ok(quote_expr(expr)),
        _ => Err(wrong_arg_count("quote", "1", args.len())),
    }
}

fn eval_args(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Vec<Value>, EvalError> {
    let mut values = Vec::with_capacity(args.len());
    for expr in args {
        values.push(eval(expr, env, output)?);
    }
    Ok(values)
}

fn eval_and(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let mut last = Value::Boolean(true);

    for arg in args {
        let value = eval(arg, env, output)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_or(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    for arg in args {
        let value = eval(arg, env, output)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
}

fn eval_begin(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    eval_sequence(args, env, output)
}

fn eval_cond(clauses: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    for (index, clause) in clauses.iter().enumerate() {
        let Expr::List(items, _) = clause else {
            return Err(EvalError::Syntax {
                message: "cond: expected clause".into(),
            });
        };
        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::Syntax {
                message: "cond: expected clause".into(),
            });
        };

        if matches!(test, Expr::Symbol(name, _) if name == "else") {
            if index + 1 != clauses.len() {
                return Err(EvalError::Syntax {
                    message: "cond: else must be last".into(),
                });
            }
            return eval_sequence(body, env, output);
        }

        let value = eval(test, env, output)?;
        if value.is_truthy() {
            return if body.is_empty() {
                Ok(value)
            } else {
                eval_sequence(body, env, output)
            };
        }
    }

    Ok(Value::Void)
}

fn eval_let(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name, _), bindings, body @ ..] => {
            eval_named_let(name, bindings, body, env, output)
        }
        [bindings, body @ ..] => eval_plain_let(bindings, body, env, output),
        _ => Err(EvalError::Syntax {
            message: "let: invalid syntax".into(),
        }),
    }
}

fn eval_plain_let(
    bindings_expr: &Expr,
    body: &[Expr],
    env: &EnvRef,
    output: &mut String,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "let: expected body".into(),
        });
    }

    let bindings = parse_let_bindings(bindings_expr)?;
    let mut values = Vec::with_capacity(bindings.len());
    for (_, value_expr) in &bindings {
        values.push(eval(value_expr, env, output)?);
    }

    let let_env = Env::new(Some(env.clone()));
    for ((name, _), value) in bindings.into_iter().zip(values) {
        let_env.define(name, value);
    }

    eval_sequence(body, &let_env, output)
}

fn eval_named_let(
    name: &str,
    bindings_expr: &Expr,
    body: &[Expr],
    env: &EnvRef,
    output: &mut String,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "let: expected body".into(),
        });
    }

    let bindings = parse_let_bindings(bindings_expr)?;
    let mut args = Vec::with_capacity(bindings.len());
    for (_, value_expr) in &bindings {
        args.push(eval(value_expr, env, output)?);
    }
    let params = bindings.iter().map(|(param, _)| param.clone()).collect();

    let let_env = Env::new(Some(env.clone()));
    let procedure = new_procedure(Some(name.into()), Params::fixed(params), body, &let_env);
    let_env.define(name.into(), procedure.clone());
    apply(procedure, &args, output)
}

fn parse_let_bindings(bindings_expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let Expr::List(bindings, _) = bindings_expr else {
        return Err(EvalError::Syntax {
            message: "let: expected bindings".into(),
        });
    };

    let mut parsed = Vec::with_capacity(bindings.len());
    for binding in bindings {
        match binding {
            Expr::List(parts, _) => match parts.as_slice() {
                [Expr::Symbol(name, _), value_expr] => {
                    parsed.push((name.clone(), value_expr.clone()));
                }
                _ => {
                    return Err(EvalError::Syntax {
                        message: "let: expected binding pair".into(),
                    });
                }
            },
            _ => {
                return Err(EvalError::Syntax {
                    message: "let: expected binding pair".into(),
                });
            }
        }
    }

    Ok(parsed)
}

fn eval_letrec(
    args: &[Expr],
    env: &EnvRef,
    output: &mut String,
    sequential: bool,
) -> Result<Value, EvalError> {
    let [bindings_expr, body @ ..] = args else {
        return Err(EvalError::Syntax {
            message: "letrec: invalid syntax".into(),
        });
    };

    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "letrec: expected body".into(),
        });
    }

    let bindings = parse_let_bindings(bindings_expr)?;
    let letrec_env = Env::new(Some(env.clone()));

    if sequential {
        for (name, value_expr) in bindings {
            letrec_env.define(name.clone(), Value::Void);
            let value = eval_letrec_initializer(&name, &value_expr, &letrec_env, output)?;
            let updated = letrec_env.set(&name, value);
            debug_assert!(updated, "letrec* binding defined before initialization");
        }
    } else {
        for (name, _) in &bindings {
            letrec_env.define(name.clone(), Value::Void);
        }

        let mut values = Vec::with_capacity(bindings.len());
        for (name, value_expr) in &bindings {
            values.push(eval_letrec_initializer(
                name,
                value_expr,
                &letrec_env,
                output,
            )?);
        }

        for ((name, _), value) in bindings.into_iter().zip(values) {
            let updated = letrec_env.set(&name, value);
            debug_assert!(updated, "letrec binding defined before initialization");
        }
    }

    eval_sequence(body, &letrec_env, output)
}

fn eval_letrec_initializer(
    name: &str,
    value_expr: &Expr,
    env: &EnvRef,
    output: &mut String,
) -> Result<Value, EvalError> {
    if let Some(parts) = lambda_parts(value_expr) {
        build_lambda(parts, env, Some(name.into()))
    } else if let Some(clauses) = case_lambda_clauses(value_expr) {
        build_case_lambda(clauses, env, Some(name.into()))
    } else {
        eval(value_expr, env, output)
    }
}

fn eval_case(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let [key_expr, clauses @ ..] = args else {
        return Err(EvalError::Syntax {
            message: "case: invalid syntax".into(),
        });
    };

    let key = eval(key_expr, env, output)?;

    for (index, clause) in clauses.iter().enumerate() {
        let Expr::List(items, _) = clause else {
            return Err(EvalError::Syntax {
                message: "case: expected clause".into(),
            });
        };
        let Some((datum_expr, body)) = items.split_first() else {
            return Err(EvalError::Syntax {
                message: "case: expected clause".into(),
            });
        };

        if matches!(datum_expr, Expr::Symbol(name, _) if name == "else") {
            if index + 1 != clauses.len() {
                return Err(EvalError::Syntax {
                    message: "case: else must be last".into(),
                });
            }
            return if body.is_empty() {
                Ok(Value::Void)
            } else {
                eval_sequence(body, env, output)
            };
        }

        let Expr::List(datums, _) = datum_expr else {
            return Err(EvalError::Syntax {
                message: "case: expected datum list".into(),
            });
        };

        if datums
            .iter()
            .map(quote_expr)
            .any(|datum| eqv_values(&key, &datum))
        {
            return if body.is_empty() {
                Ok(Value::Void)
            } else {
                eval_sequence(body, env, output)
            };
        }
    }

    Ok(Value::Void)
}

fn eval_do(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let [bindings_expr, test_clause_expr, body @ ..] = args else {
        return Err(EvalError::Syntax {
            message: "do: invalid syntax".into(),
        });
    };

    let bindings = parse_do_bindings(bindings_expr)?;
    let (test_expr, result_exprs) = parse_do_test_clause(test_clause_expr)?;

    let loop_env = Env::new(Some(env.clone()));
    let mut initial_values = Vec::with_capacity(bindings.len());
    for binding in &bindings {
        initial_values.push(eval(&binding.init, env, output)?);
    }
    for (binding, value) in bindings.iter().zip(initial_values) {
        loop_env.define(binding.name.clone(), value);
    }

    loop {
        if eval(&test_expr, &loop_env, output)?.is_truthy() {
            return if result_exprs.is_empty() {
                Ok(Value::Void)
            } else {
                eval_sequence(&result_exprs, &loop_env, output)
            };
        }

        if !body.is_empty() {
            eval_sequence(body, &loop_env, output)?;
        }

        let mut next_values = Vec::with_capacity(bindings.len());
        for binding in &bindings {
            let value = match &binding.step {
                Some(step) => eval(step, &loop_env, output)?,
                None => loop_env
                    .lookup(&binding.name)
                    .expect("do binding is always present"),
            };
            next_values.push(value);
        }

        for (binding, value) in bindings.iter().zip(next_values) {
            let updated = loop_env.set(&binding.name, value);
            debug_assert!(updated, "do binding defined before loop step");
        }
    }
}

struct DoBinding {
    name: String,
    init: Expr,
    step: Option<Expr>,
}

fn parse_do_bindings(bindings_expr: &Expr) -> Result<Vec<DoBinding>, EvalError> {
    let Expr::List(bindings, _) = bindings_expr else {
        return Err(EvalError::Syntax {
            message: "do: expected bindings".into(),
        });
    };

    let mut parsed = Vec::with_capacity(bindings.len());
    for binding in bindings {
        match binding {
            Expr::List(parts, _) => match parts.as_slice() {
                [Expr::Symbol(name, _), init] => parsed.push(DoBinding {
                    name: name.clone(),
                    init: init.clone(),
                    step: None,
                }),
                [Expr::Symbol(name, _), init, step] => parsed.push(DoBinding {
                    name: name.clone(),
                    init: init.clone(),
                    step: Some(step.clone()),
                }),
                _ => {
                    return Err(EvalError::Syntax {
                        message: "do: expected (name init [step])".into(),
                    });
                }
            },
            _ => {
                return Err(EvalError::Syntax {
                    message: "do: expected (name init [step])".into(),
                });
            }
        }
    }

    Ok(parsed)
}

fn parse_do_test_clause(test_clause_expr: &Expr) -> Result<(Expr, Vec<Expr>), EvalError> {
    let Expr::List(items, _) = test_clause_expr else {
        return Err(EvalError::Syntax {
            message: "do: expected termination clause".into(),
        });
    };
    let Some((test_expr, result_exprs)) = items.split_first() else {
        return Err(EvalError::Syntax {
            message: "do: expected termination test".into(),
        });
    };

    Ok((test_expr.clone(), result_exprs.to_vec()))
}

fn build_lambda(parts: &[Expr], env: &EnvRef, name: Option<String>) -> Result<Value, EvalError> {
    let [params_expr, body @ ..] = parts else {
        return Err(EvalError::Syntax {
            message: "lambda: expected parameters and body".into(),
        });
    };

    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "lambda: expected body".into(),
        });
    }

    Ok(new_procedure(
        name,
        parse_params_expr(params_expr)?,
        body,
        env,
    ))
}

fn build_case_lambda(
    clauses: &[Expr],
    env: &EnvRef,
    name: Option<String>,
) -> Result<Value, EvalError> {
    if clauses.is_empty() {
        return Err(EvalError::Syntax {
            message: "case-lambda: expected at least one clause".into(),
        });
    }

    let mut parsed_clauses = Vec::with_capacity(clauses.len());
    for clause in clauses {
        let Expr::List(items, _) = clause else {
            return Err(EvalError::Syntax {
                message: "case-lambda: expected clause".into(),
            });
        };

        let [params_expr, body @ ..] = items.as_slice() else {
            return Err(EvalError::Syntax {
                message: "case-lambda: expected parameters and body".into(),
            });
        };

        if body.is_empty() {
            return Err(EvalError::Syntax {
                message: "case-lambda: expected body".into(),
            });
        }

        parsed_clauses.push(ProcedureClause {
            params: parse_params_expr(params_expr)?,
            body: body.to_vec(),
        });
    }

    Ok(new_case_procedure(name, parsed_clauses, env))
}

fn new_procedure(name: Option<String>, params: Params, body: &[Expr], env: &EnvRef) -> Value {
    Value::Procedure(Rc::new(Procedure {
        kind: ProcedureKind::Lambda,
        name,
        clauses: vec![ProcedureClause {
            params,
            body: body.to_vec(),
        }],
        env: env.clone(),
    }))
}

fn new_case_procedure(name: Option<String>, clauses: Vec<ProcedureClause>, env: &EnvRef) -> Value {
    Value::Procedure(Rc::new(Procedure {
        kind: ProcedureKind::CaseLambda,
        name,
        clauses,
        env: env.clone(),
    }))
}

fn parse_params_expr(params: &Expr) -> Result<Params, EvalError> {
    let Expr::List(items, _) = params else {
        return Err(EvalError::Syntax {
            message: "lambda: expected parameter list".into(),
        });
    };

    parse_param_list(items)
}

fn parse_param_list(params: &[Expr]) -> Result<Params, EvalError> {
    let mut required = Vec::with_capacity(params.len());
    let mut index = 0;

    while index < params.len() {
        match &params[index] {
            Expr::Symbol(name, _) if name == "." => {
                if index + 2 != params.len() {
                    return Err(EvalError::Syntax {
                        message: "lambda: expected parameter name".into(),
                    });
                }

                return match &params[index + 1] {
                    Expr::Symbol(name, _) if name != "." => Ok(Params {
                        required,
                        rest: Some(name.clone()),
                    }),
                    _ => Err(EvalError::Syntax {
                        message: "lambda: expected parameter name".into(),
                    }),
                };
            }
            Expr::Symbol(name, _) => required.push(name.clone()),
            _ => {
                return Err(EvalError::Syntax {
                    message: "lambda: expected parameter name".into(),
                });
            }
        }

        index += 1;
    }

    Ok(Params {
        required,
        rest: None,
    })
}

fn lambda_parts(expr: &Expr) -> Option<&[Expr]> {
    let Expr::List(items, _) = expr else {
        return None;
    };
    let (Expr::Symbol(name, _), tail) = items.split_first()? else {
        return None;
    };

    if name == "lambda" {
        Some(tail)
    } else {
        None
    }
}

fn case_lambda_clauses(expr: &Expr) -> Option<&[Expr]> {
    let Expr::List(items, _) = expr else {
        return None;
    };
    let (Expr::Symbol(name, _), tail) = items.split_first()? else {
        return None;
    };

    if name == "case-lambda" {
        Some(tail)
    } else {
        None
    }
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Number(value, _) => Value::Number(*value),
        Expr::Boolean(value, _) => Value::Boolean(*value),
        Expr::String(value, _) => Value::String(SchemeString::literal(value)),
        Expr::Char(value, _) => Value::Char(*value),
        Expr::Symbol(value, _) => Value::Symbol(value.clone()),
        Expr::List(items, _) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn apply(callable: Value, args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    match callable {
        Value::Builtin(builtin) => apply_builtin(builtin, args, output),
        Value::Procedure(procedure) => apply_procedure(&procedure, args, output),
        Value::RecordProcedure(procedure) => apply_record_procedure(&procedure, args),
        value => Err(EvalError::NotAProcedure {
            got: value.type_name().into(),
        }),
    }
}

fn apply_procedure(
    procedure: &Procedure,
    args: &[Value],
    output: &mut String,
) -> Result<Value, EvalError> {
    let Some(clause) = procedure
        .clauses
        .iter()
        .find(|clause| clause.params.matches_arity(args.len()))
    else {
        let expected = procedure.expected_args();
        return Err(wrong_arg_count(
            procedure.error_name(),
            &expected,
            args.len(),
        ));
    };

    let call_env = Env::new(Some(procedure.env.clone()));
    for (param, arg) in clause.params.required.iter().zip(args.iter()) {
        call_env.define(param.clone(), arg.clone());
    }
    if let Some(rest) = &clause.params.rest {
        call_env.define(
            rest.clone(),
            Value::List(args[clause.params.required.len()..].to_vec()),
        );
    }

    eval_sequence(&clause.body, &call_env, output)
}

fn apply_record_procedure(procedure: &RecordProcedure, args: &[Value]) -> Result<Value, EvalError> {
    match &procedure.kind {
        RecordProcedureKind::Constructor { record_type } => {
            if args.len() != record_type.field_count {
                return Err(wrong_arg_count(
                    &procedure.name,
                    &record_type.field_count.to_string(),
                    args.len(),
                ));
            }

            Ok(Value::Record(Rc::new(RecordInstance {
                record_type: record_type.clone(),
                fields: args.to_vec(),
            })))
        }
        RecordProcedureKind::Predicate { record_type } => match args {
            [Value::Record(record)] => {
                Ok(Value::Boolean(Rc::ptr_eq(&record.record_type, record_type)))
            }
            [_] => Ok(Value::Boolean(false)),
            _ => Err(wrong_arg_count(&procedure.name, "1", args.len())),
        },
        RecordProcedureKind::Accessor {
            record_type,
            field_index,
        } => match args {
            [Value::Record(record)] if Rc::ptr_eq(&record.record_type, record_type) => {
                Ok(record.fields[*field_index].clone())
            }
            [value] => Err(EvalError::TypeMismatch {
                name: procedure.name.clone(),
                expected: format!("{} record", record_type.name),
                got: value.type_name().into(),
            }),
            _ => Err(wrong_arg_count(&procedure.name, "1", args.len())),
        },
    }
}

fn wrong_arg_count(name: &str, expected: &str, got: usize) -> EvalError {
    EvalError::WrongArgCount {
        name: name.into(),
        expected: expected.into(),
        got,
    }
}

#[cfg(test)]
mod tests;
