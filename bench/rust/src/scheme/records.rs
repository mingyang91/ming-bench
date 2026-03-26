use std::rc::Rc;

use super::{
    error::EvalError,
    model::{
        EnvRef, Expr, RecordInstance, RecordProcedure, RecordProcedureKind, RecordType, Value,
    },
    wrong_arg_count,
};

pub(super) fn eval_define_record_type(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let [type_name_expr, constructor_expr, predicate_expr, field_exprs @ ..] = args else {
        return Err(EvalError::Syntax {
            message: "define-record-type: invalid syntax".into(),
        });
    };

    let type_name = parse_named_symbol(type_name_expr, "define-record-type: expected type name")?;
    let (constructor_name, constructor_arity) = parse_record_constructor(constructor_expr)?;
    let predicate_name = parse_named_symbol(
        predicate_expr,
        "define-record-type: expected predicate name",
    )?;
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

pub(super) fn apply_record_procedure(
    procedure: &RecordProcedure,
    args: &[Value],
) -> Result<Value, EvalError> {
    match &procedure.kind {
        RecordProcedureKind::Constructor { record_type } => {
            apply_record_constructor(procedure, record_type, args)
        }
        RecordProcedureKind::Predicate { record_type } => {
            apply_record_predicate(procedure, record_type, args)
        }
        RecordProcedureKind::Accessor {
            record_type,
            field_index,
        } => apply_record_accessor(procedure, record_type, *field_index, args),
    }
}

fn parse_named_symbol(expr: &Expr, message: &str) -> Result<String, EvalError> {
    match expr {
        Expr::Symbol(name, _) => Ok(name.clone()),
        Expr::Number(_, _)
        | Expr::Boolean(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::List(_, _) => Err(EvalError::Syntax {
            message: message.into(),
        }),
    }
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

    if !params
        .iter()
        .all(|param| matches!(param, Expr::Symbol(_, _)))
    {
        return Err(EvalError::Syntax {
            message: "define-record-type: expected constructor field name".into(),
        });
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
        [Expr::Number(_, _), _]
        | [Expr::Boolean(_, _), _]
        | [Expr::String(_, _), _]
        | [Expr::Char(_, _), _]
        | [Expr::List(_, _), _]
        | [_, Expr::Number(_, _)]
        | [_, Expr::Boolean(_, _)]
        | [_, Expr::String(_, _)]
        | [_, Expr::Char(_, _)]
        | [_, Expr::List(_, _)]
        | []
        | [_]
        | [_, _, ..] => Err(EvalError::Syntax {
            message: "define-record-type: expected (field accessor)".into(),
        }),
    }
}

fn apply_record_constructor(
    procedure: &RecordProcedure,
    record_type: &Rc<RecordType>,
    args: &[Value],
) -> Result<Value, EvalError> {
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

fn apply_record_predicate(
    procedure: &RecordProcedure,
    record_type: &Rc<RecordType>,
    args: &[Value],
) -> Result<Value, EvalError> {
    match args {
        [Value::Record(record)] => Ok(Value::Boolean(Rc::ptr_eq(&record.record_type, record_type))),
        [_] => Ok(Value::Boolean(false)),
        _ => Err(wrong_arg_count(&procedure.name, "1", args.len())),
    }
}

fn apply_record_accessor(
    procedure: &RecordProcedure,
    record_type: &Rc<RecordType>,
    field_index: usize,
    args: &[Value],
) -> Result<Value, EvalError> {
    match args {
        [Value::Record(record)] if Rc::ptr_eq(&record.record_type, record_type) => {
            Ok(record.fields[field_index].clone())
        }
        [value] => Err(EvalError::TypeMismatch {
            name: procedure.name.clone(),
            expected: format!("{} record", record_type.name),
            got: value.type_name().into(),
        }),
        _ => Err(wrong_arg_count(&procedure.name, "1", args.len())),
    }
}
