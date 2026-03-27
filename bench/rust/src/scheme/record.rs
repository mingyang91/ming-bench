use super::{env_define, EnvRef, EvalError, Expr, Value};
use std::cell::RefCell;
use std::rc::Rc;

pub(super) type RecordRef = Rc<RecordInstance>;
type RecordTypeRef = Rc<RecordType>;

struct RecordType {
    name: String,
    field_count: usize,
}

pub(super) struct RecordInstance {
    record_type: RecordTypeRef,
    fields: RefCell<Vec<Value>>,
}

pub(super) struct NativeProcedure {
    name: String,
    kind: NativeProcedureKind,
}

enum NativeProcedureKind {
    Constructor {
        record_type: RecordTypeRef,
    },
    Predicate {
        record_type: RecordTypeRef,
    },
    Accessor {
        record_type: RecordTypeRef,
        field_index: usize,
    },
}

impl NativeProcedure {
    fn new(name: String, kind: NativeProcedureKind) -> Self {
        Self { name, kind }
    }

    pub(super) fn apply(&self, args: &[Value]) -> Result<Value, EvalError> {
        match &self.kind {
            NativeProcedureKind::Constructor { record_type } => {
                if args.len() != record_type.field_count {
                    return Err(EvalError::WrongArgCount {
                        name: self.name.clone(),
                        expected: format!("exactly {} argument(s)", record_type.field_count),
                        got: args.len(),
                    });
                }

                Ok(Value::Record(Rc::new(RecordInstance {
                    record_type: Rc::clone(record_type),
                    fields: RefCell::new(args.to_vec()),
                })))
            }
            NativeProcedureKind::Predicate { record_type } => {
                let [value] = args else {
                    return Err(EvalError::WrongArgCount {
                        name: self.name.clone(),
                        expected: "exactly 1 argument".into(),
                        got: args.len(),
                    });
                };

                Ok(Value::Boolean(matches!(
                    value,
                    Value::Record(record) if Rc::ptr_eq(&record.record_type, record_type)
                )))
            }
            NativeProcedureKind::Accessor {
                record_type,
                field_index,
            } => {
                let [value] = args else {
                    return Err(EvalError::WrongArgCount {
                        name: self.name.clone(),
                        expected: "exactly 1 argument".into(),
                        got: args.len(),
                    });
                };

                let Value::Record(record) = value else {
                    return Err(EvalError::RecordTypeMismatch {
                        expected: record_type.name.clone(),
                        found: value.type_name().into(),
                    });
                };

                if !Rc::ptr_eq(&record.record_type, record_type) {
                    return Err(EvalError::RecordTypeMismatch {
                        expected: record_type.name.clone(),
                        found: record.record_type.name.clone(),
                    });
                }

                Ok(record
                    .fields
                    .borrow()
                    .get(*field_index)
                    .cloned()
                    .expect("record accessor index must be valid"))
            }
        }
    }
}

pub(super) fn render_record(record: &RecordRef) -> String {
    format!("#<record {}>", record.record_type.name)
}

pub(super) fn define_record_type(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let [record_name, constructor_spec, predicate_name, field_specs @ ..] = args else {
        return Err(EvalError::SyntaxError {
            message: "invalid define-record-type".into(),
        });
    };

    let record_name = parse_record_symbol(record_name, "record type name must be a symbol")?;
    let (constructor_name, field_count) = parse_record_constructor_spec(constructor_spec)?;
    let predicate_name =
        parse_record_symbol(predicate_name, "record predicate name must be a symbol")?;
    let accessor_names = parse_record_field_specs(field_specs)?;

    if field_count != accessor_names.len() {
        return Err(EvalError::SyntaxError {
            message: "record constructor and field definitions must have matching arity".into(),
        });
    }

    let record_type = Rc::new(RecordType {
        name: record_name,
        field_count,
    });

    env_define(
        env,
        constructor_name.clone(),
        Value::NativeProcedure(Rc::new(NativeProcedure::new(
            constructor_name,
            NativeProcedureKind::Constructor {
                record_type: Rc::clone(&record_type),
            },
        ))),
    );

    env_define(
        env,
        predicate_name.clone(),
        Value::NativeProcedure(Rc::new(NativeProcedure::new(
            predicate_name,
            NativeProcedureKind::Predicate {
                record_type: Rc::clone(&record_type),
            },
        ))),
    );

    for (field_index, accessor_name) in accessor_names.into_iter().enumerate() {
        env_define(
            env,
            accessor_name.clone(),
            Value::NativeProcedure(Rc::new(NativeProcedure::new(
                accessor_name,
                NativeProcedureKind::Accessor {
                    record_type: Rc::clone(&record_type),
                    field_index,
                },
            ))),
        );
    }

    Ok(Value::Void)
}

fn parse_record_symbol(item: &Expr, message: &str) -> Result<String, EvalError> {
    match item {
        Expr::Symbol(name, _) => Ok(name.clone()),
        _ => Err(EvalError::SyntaxError {
            message: message.into(),
        }),
    }
}

fn parse_record_constructor_spec(item: &Expr) -> Result<(String, usize), EvalError> {
    let Expr::List(items, _) = item else {
        return Err(EvalError::SyntaxError {
            message: "record constructor spec must be a list".into(),
        });
    };

    let Some((name, fields)) = items.split_first() else {
        return Err(EvalError::SyntaxError {
            message: "record constructor spec cannot be empty".into(),
        });
    };

    let constructor_name = parse_record_symbol(name, "record constructor name must be a symbol")?;
    let fields = super::parse_required_param_names(fields)?;
    Ok((constructor_name, fields.len()))
}

fn parse_record_field_specs(field_specs: &[Expr]) -> Result<Vec<String>, EvalError> {
    let mut accessors = Vec::with_capacity(field_specs.len());
    for field in field_specs {
        let Expr::List(items, _) = field else {
            return Err(EvalError::SyntaxError {
                message: "record field spec must be a list".into(),
            });
        };

        let [_, accessor] = items.as_slice() else {
            return Err(EvalError::SyntaxError {
                message: "record field spec must be (field accessor)".into(),
            });
        };

        accessors.push(parse_record_symbol(
            accessor,
            "record accessor name must be a symbol",
        )?);
    }

    Ok(accessors)
}
