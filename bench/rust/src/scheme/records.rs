use super::helpers::{syntax_error, type_mismatch, wrong_arg_count};
use super::value_ops::value_type_name;
use super::{
    env_define, EnvRef, EvalError, Expr, Procedure, Rc, RecordType, RecordValue, SourcePos, Value,
};

pub(super) fn eval_define_record_type(
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
) -> Result<Value, EvalError> {
    let (type_name_expr, constructor_expr, predicate_expr, field_exprs) = match args {
        [type_name_expr, constructor_expr, predicate_expr, field_exprs @ ..] => (
            type_name_expr,
            constructor_expr,
            predicate_expr,
            field_exprs,
        ),
        _ => return Err(syntax_error(pos, "invalid define-record-type form")),
    };

    let type_name = type_name_expr
        .symbol_name()
        .ok_or_else(|| syntax_error(type_name_expr.pos, "record type name must be a symbol"))?
        .to_string();
    let (constructor_name, constructor_field_count) = parse_record_constructor(constructor_expr)?;
    let predicate_name = predicate_expr
        .symbol_name()
        .ok_or_else(|| syntax_error(predicate_expr.pos, "record predicate name must be a symbol"))?
        .to_string();
    let accessor_names = field_exprs
        .iter()
        .map(parse_record_field)
        .collect::<Result<Vec<_>, _>>()?;

    if constructor_field_count != accessor_names.len() {
        return Err(syntax_error(
            constructor_expr.pos,
            "record constructor arity must match field count",
        ));
    }

    let record_type = Rc::new(RecordType {
        type_name,
        constructor_name: constructor_name.clone(),
        field_count: accessor_names.len(),
    });

    env_define(
        env,
        constructor_name,
        Value::Procedure(Procedure::RecordConstructor(Rc::clone(&record_type))),
    );
    env_define(
        env,
        predicate_name,
        Value::Procedure(Procedure::RecordPredicate(Rc::clone(&record_type))),
    );

    for (field_index, accessor_name) in accessor_names.into_iter().enumerate() {
        env_define(
            env,
            accessor_name.clone(),
            Value::Procedure(Procedure::RecordAccessor {
                record_type: Rc::clone(&record_type),
                field_index,
                name: accessor_name,
            }),
        );
    }

    Ok(Value::Void)
}

fn parse_record_constructor(constructor: &Expr) -> Result<(String, usize), EvalError> {
    let items = constructor
        .list_items()
        .ok_or_else(|| syntax_error(constructor.pos, "record constructor must be a list"))?;
    let (name_expr, params) = items
        .split_first()
        .ok_or_else(|| syntax_error(constructor.pos, "record constructor cannot be empty"))?;
    let name = name_expr
        .symbol_name()
        .ok_or_else(|| syntax_error(name_expr.pos, "record constructor name must be a symbol"))?
        .to_string();

    for param in params {
        if param.symbol_name().is_none() {
            return Err(syntax_error(
                param.pos,
                "record constructor parameter must be a symbol",
            ));
        }
    }

    Ok((name, params.len()))
}

fn parse_record_field(field: &Expr) -> Result<String, EvalError> {
    let items = field
        .list_items()
        .ok_or_else(|| syntax_error(field.pos, "record field must be a list"))?;

    match items {
        [name_expr, accessor_expr] => {
            if name_expr.symbol_name().is_none() {
                return Err(syntax_error(
                    name_expr.pos,
                    "record field name must be a symbol",
                ));
            }

            accessor_expr
                .symbol_name()
                .ok_or_else(|| {
                    syntax_error(accessor_expr.pos, "record accessor name must be a symbol")
                })
                .map(str::to_string)
        }
        _ => Err(syntax_error(
            field.pos,
            "record field must contain a name and accessor",
        )),
    }
}

pub(super) fn apply_record_constructor(
    record_type: Rc<RecordType>,
    args: &[Value],
    pos: SourcePos,
) -> Result<Value, EvalError> {
    if args.len() != record_type.field_count {
        return Err(wrong_arg_count(
            pos,
            record_type.constructor_name.clone(),
            format!("exactly {} arguments", record_type.field_count),
            args.len(),
        ));
    }

    Ok(Value::Record(Rc::new(RecordValue {
        record_type,
        fields: args.to_vec(),
    })))
}

pub(super) fn apply_record_predicate(
    record_type: Rc<RecordType>,
    args: &[Value],
    pos: SourcePos,
) -> Result<Value, EvalError> {
    match args {
        [Value::Record(record)] => Ok(Value::Boolean(Rc::ptr_eq(
            &record.record_type,
            &record_type,
        ))),
        [_] => Ok(Value::Boolean(false)),
        _ => Err(wrong_arg_count(
            pos,
            "record predicate",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

pub(super) fn apply_record_accessor(
    record_type: Rc<RecordType>,
    field_index: usize,
    name: &str,
    args: &[Value],
    pos: SourcePos,
) -> Result<Value, EvalError> {
    match args {
        [Value::Record(record)] if Rc::ptr_eq(&record.record_type, &record_type) => {
            Ok(record.fields[field_index].clone())
        }
        [other] => Err(type_mismatch(
            pos,
            name,
            record_type.type_name.clone(),
            value_type_name(other),
        )),
        _ => Err(wrong_arg_count(pos, name, "exactly 1 argument", args.len())),
    }
}
