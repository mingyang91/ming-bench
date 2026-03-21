use std::cell::RefCell;
use std::rc::Rc;

use crate::scheme::ast::{Expr, SourceLocation};
use crate::scheme::environment::Environment;
use crate::scheme::error::{ArgCount, EvalError};
use crate::scheme::value::Value;

#[derive(Clone)]
pub(crate) struct SchemeRecord(Rc<RecordState>);

struct RecordState {
    record_type: Rc<RecordType>,
    fields: RefCell<Vec<Value>>,
}

struct RecordType {
    name: String,
    field_tags: Vec<String>,
}

struct RecordDefinition {
    type_name: String,
    constructor_name: String,
    constructor_fields: Vec<String>,
    predicate_name: String,
    fields: Vec<RecordFieldSpec>,
}

struct RecordFieldSpec {
    tag: String,
    accessor_name: String,
    mutator_name: Option<String>,
}

#[derive(Clone)]
pub(crate) struct RecordProcedure(Rc<RecordProcedureImpl>);

enum RecordProcedureImpl {
    Constructor {
        name: String,
        record_type: Rc<RecordType>,
    },
    Predicate {
        name: String,
        record_type: Rc<RecordType>,
    },
    Accessor {
        name: String,
        record_type: Rc<RecordType>,
        field_index: usize,
    },
    Mutator {
        name: String,
        record_type: Rc<RecordType>,
        field_index: usize,
    },
}

impl SchemeRecord {
    fn new(record_type: Rc<RecordType>, fields: Vec<Value>) -> Self {
        Self(Rc::new(RecordState {
            record_type,
            fields: RefCell::new(fields),
        }))
    }

    pub(crate) fn is_same_object(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    pub(crate) fn render(&self) -> String {
        format!("#<record:{}>", self.type_name())
    }

    pub(crate) fn type_name(&self) -> &str {
        &self.0.record_type.name
    }

    fn is_type(&self, record_type: &Rc<RecordType>) -> bool {
        Rc::ptr_eq(&self.0.record_type, record_type)
    }

    fn field(&self, field_index: usize) -> Value {
        self.0
            .fields
            .borrow()
            .get(field_index)
            .cloned()
            .expect("record field index should be valid")
    }

    fn set_field(&self, field_index: usize, value: Value) {
        let mut fields = self.0.fields.borrow_mut();
        let slot = fields
            .get_mut(field_index)
            .expect("record field index should be valid");
        *slot = value;
    }
}

impl RecordType {
    fn new(name: String, field_tags: Vec<String>) -> Self {
        Self { name, field_tags }
    }

    fn field_count(&self) -> usize {
        self.field_tags.len()
    }
}

impl RecordProcedure {
    fn constructor(name: String, record_type: Rc<RecordType>) -> Self {
        Self(Rc::new(RecordProcedureImpl::Constructor {
            name,
            record_type,
        }))
    }

    fn predicate(name: String, record_type: Rc<RecordType>) -> Self {
        Self(Rc::new(RecordProcedureImpl::Predicate {
            name,
            record_type,
        }))
    }

    fn accessor(name: String, record_type: Rc<RecordType>, field_index: usize) -> Self {
        Self(Rc::new(RecordProcedureImpl::Accessor {
            name,
            record_type,
            field_index,
        }))
    }

    fn mutator(name: String, record_type: Rc<RecordType>, field_index: usize) -> Self {
        Self(Rc::new(RecordProcedureImpl::Mutator {
            name,
            record_type,
            field_index,
        }))
    }

    pub(crate) fn is_same_object(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    pub(crate) fn name(&self) -> &str {
        match &*self.0 {
            RecordProcedureImpl::Constructor { name, .. }
            | RecordProcedureImpl::Predicate { name, .. }
            | RecordProcedureImpl::Accessor { name, .. }
            | RecordProcedureImpl::Mutator { name, .. } => name,
        }
    }

    pub(crate) fn apply(
        &self,
        arguments: &[Value],
        location: SourceLocation,
    ) -> Result<Value, EvalError> {
        match &*self.0 {
            RecordProcedureImpl::Constructor { name, record_type } => {
                apply_constructor(name, record_type, arguments, location)
            }
            RecordProcedureImpl::Predicate { name, record_type } => {
                apply_predicate(name, record_type, arguments, location)
            }
            RecordProcedureImpl::Accessor {
                name,
                record_type,
                field_index,
            } => apply_accessor(name, record_type, *field_index, arguments, location),
            RecordProcedureImpl::Mutator {
                name,
                record_type,
                field_index,
            } => apply_mutator(name, record_type, *field_index, arguments, location),
        }
    }
}

pub(crate) fn define_record_type(
    arguments: &[Expr],
    location: SourceLocation,
    environment: &Environment,
) -> Result<(), EvalError> {
    let definition = parse_record_definition(arguments, location)?;
    let record_type = Rc::new(RecordType::new(
        definition.type_name,
        definition.constructor_fields,
    ));

    environment.define(
        definition.constructor_name.clone(),
        Value::RecordProcedure(RecordProcedure::constructor(
            definition.constructor_name,
            record_type.clone(),
        )),
    );
    environment.define(
        definition.predicate_name.clone(),
        Value::RecordProcedure(RecordProcedure::predicate(
            definition.predicate_name,
            record_type.clone(),
        )),
    );

    definition
        .fields
        .into_iter()
        .enumerate()
        .for_each(|(field_index, field)| {
            environment.define(
                field.accessor_name.clone(),
                Value::RecordProcedure(RecordProcedure::accessor(
                    field.accessor_name,
                    record_type.clone(),
                    field_index,
                )),
            );

            if let Some(mutator_name) = field.mutator_name {
                environment.define(
                    mutator_name.clone(),
                    Value::RecordProcedure(RecordProcedure::mutator(
                        mutator_name,
                        record_type.clone(),
                        field_index,
                    )),
                );
            }
        });

    Ok(())
}

fn apply_constructor(
    name: &str,
    record_type: &Rc<RecordType>,
    arguments: &[Value],
    location: SourceLocation,
) -> Result<Value, EvalError> {
    if arguments.len() != record_type.field_count() {
        return Err(wrong_record_arg_count(
            name,
            ArgCount::Exactly(record_type.field_count()),
            arguments.len(),
            location,
        ));
    }

    Ok(Value::Record(SchemeRecord::new(
        record_type.clone(),
        arguments.to_vec(),
    )))
}

fn apply_predicate(
    name: &str,
    record_type: &Rc<RecordType>,
    arguments: &[Value],
    location: SourceLocation,
) -> Result<Value, EvalError> {
    let [value] = arguments else {
        return Err(wrong_record_arg_count(
            name,
            ArgCount::Exactly(1),
            arguments.len(),
            location,
        ));
    };

    Ok(Value::Boolean(matches!(
        value,
        Value::Record(record) if record.is_type(record_type)
    )))
}

fn apply_accessor(
    name: &str,
    record_type: &Rc<RecordType>,
    field_index: usize,
    arguments: &[Value],
    location: SourceLocation,
) -> Result<Value, EvalError> {
    let [record_value] = arguments else {
        return Err(wrong_record_arg_count(
            name,
            ArgCount::Exactly(1),
            arguments.len(),
            location,
        ));
    };

    Ok(expect_record(record_type, record_value, location)?.field(field_index))
}

fn apply_mutator(
    name: &str,
    record_type: &Rc<RecordType>,
    field_index: usize,
    arguments: &[Value],
    location: SourceLocation,
) -> Result<Value, EvalError> {
    let [record_value, updated_value] = arguments else {
        return Err(wrong_record_arg_count(
            name,
            ArgCount::Exactly(2),
            arguments.len(),
            location,
        ));
    };

    expect_record(record_type, record_value, location)?
        .set_field(field_index, updated_value.clone());
    Ok(Value::Void)
}

fn expect_record(
    record_type: &Rc<RecordType>,
    value: &Value,
    location: SourceLocation,
) -> Result<SchemeRecord, EvalError> {
    let Value::Record(record) = value else {
        return Err(EvalError::RecordTypeMismatch {
            location,
            expected: record_type.name.clone(),
            found: value_type_name(value),
        });
    };

    if record.is_type(record_type) {
        return Ok(record.clone());
    }

    Err(EvalError::RecordTypeMismatch {
        location,
        expected: record_type.name.clone(),
        found: record.type_name().to_string(),
    })
}

fn parse_record_definition(
    arguments: &[Expr],
    location: SourceLocation,
) -> Result<RecordDefinition, EvalError> {
    let [type_name, constructor, predicate, field_specs @ ..] = arguments else {
        return Err(malformed_define_record_type(location));
    };

    let constructor_fields = parse_constructor_spec(constructor)?;
    let fields = field_specs
        .iter()
        .map(parse_record_field_spec)
        .collect::<Result<Vec<_>, _>>()?;

    validate_record_fields(&constructor_fields.1, &fields, location)?;

    Ok(RecordDefinition {
        type_name: parse_symbol(type_name)?,
        constructor_name: constructor_fields.0,
        constructor_fields: constructor_fields.1,
        predicate_name: parse_symbol(predicate)?,
        fields,
    })
}

fn parse_constructor_spec(expression: &Expr) -> Result<(String, Vec<String>), EvalError> {
    let Expr::List { items, location } = expression else {
        return Err(malformed_define_record_type(expression.location()));
    };
    let Some((constructor, fields)) = items.split_first() else {
        return Err(malformed_define_record_type(*location));
    };

    Ok((
        parse_symbol(constructor)?,
        fields
            .iter()
            .map(parse_symbol)
            .collect::<Result<Vec<_>, _>>()?,
    ))
}

fn parse_record_field_spec(expression: &Expr) -> Result<RecordFieldSpec, EvalError> {
    let Expr::List { items, .. } = expression else {
        return Err(malformed_define_record_type(expression.location()));
    };

    match items.as_slice() {
        [tag, accessor] => Ok(RecordFieldSpec {
            tag: parse_symbol(tag)?,
            accessor_name: parse_symbol(accessor)?,
            mutator_name: None,
        }),
        [tag, accessor, mutator] => Ok(RecordFieldSpec {
            tag: parse_symbol(tag)?,
            accessor_name: parse_symbol(accessor)?,
            mutator_name: Some(parse_symbol(mutator)?),
        }),
        _ => Err(malformed_define_record_type(expression.location())),
    }
}

fn validate_record_fields(
    constructor_fields: &[String],
    fields: &[RecordFieldSpec],
    location: SourceLocation,
) -> Result<(), EvalError> {
    let field_tags = fields.iter().map(|field| field.tag.as_str());

    if constructor_fields.iter().map(String::as_str).eq(field_tags) {
        return Ok(());
    }

    Err(malformed_define_record_type(location))
}

fn parse_symbol(expression: &Expr) -> Result<String, EvalError> {
    let Expr::Symbol { name, .. } = expression else {
        return Err(malformed_define_record_type(expression.location()));
    };

    Ok(name.clone())
}

fn malformed_define_record_type(location: SourceLocation) -> EvalError {
    EvalError::MalformedSpecialForm {
        location,
        form: "define-record-type",
    }
}

fn wrong_record_arg_count(
    procedure: &str,
    expected: ArgCount,
    got: usize,
    location: SourceLocation,
) -> EvalError {
    EvalError::WrongArgumentCountNamed {
        location,
        procedure: procedure.to_string(),
        expected,
        got,
    }
}

fn value_type_name(value: &Value) -> String {
    match value {
        Value::Record(record) => record.type_name().to_string(),
        _ => value.type_name().to_string(),
    }
}
