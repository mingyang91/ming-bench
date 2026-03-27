use super::error::EvalError;
use super::{env_define, gensym, Env, Expr, ExprKind, Value};

pub(crate) fn eval_define_record_type(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    // (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
    if args.len() < 3 {
        return Err(EvalError::Arity("define-record-type requires at least 3 arguments".into()));
    }
    // Type name (used for unique tag)
    let type_name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("define-record-type: expected type name symbol".into())),
    };
    let type_tag = gensym(&type_name);

    // Constructor: (make-foo field1 field2 ...)
    let (constructor_name, constructor_fields) = match &args[1].kind {
        ExprKind::List(items) if !items.is_empty() => {
            let name = match &items[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Type("define-record-type: constructor name must be symbol".into())),
            };
            let fields: Vec<String> = items[1..].iter().map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type("define-record-type: field name must be symbol".into())),
            }).collect::<Result<_, _>>()?;
            (name, fields)
        }
        _ => return Err(EvalError::Type("define-record-type: expected constructor spec".into())),
    };

    // Predicate name
    let predicate_name = match &args[2].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("define-record-type: expected predicate name".into())),
    };

    // Field accessors: (field accessor) ...
    let mut field_accessors: Vec<(String, String)> = Vec::new();
    for arg in &args[3..] {
        match &arg.kind {
            ExprKind::List(items) if items.len() == 2 => {
                let field = match &items[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("define-record-type: field name must be symbol".into())),
                };
                let accessor = match &items[1].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("define-record-type: accessor name must be symbol".into())),
                };
                field_accessors.push((field, accessor));
            }
            _ => return Err(EvalError::Type("define-record-type: invalid field spec".into())),
        }
    }

    // Define constructor as a builtin-like procedure
    let tag = type_tag.clone();
    let fields = constructor_fields.clone();
    let constructor_tag = tag.clone();
    let constructor_fields_clone = fields.clone();

    // Register constructor
    let ctor_id = format!("##record-ctor##{constructor_tag}");
    let meta_key = format!("##record-meta##{constructor_tag}");
    env_define(env, meta_key, Value::List(
        constructor_fields_clone.iter().map(|f| Value::Symbol(f.clone())).collect()
    ));
    env_define(env, constructor_name, Value::Builtin(ctor_id));

    // Register predicate
    let pred_id = format!("##record-pred##{tag}");
    env_define(env, predicate_name, Value::Builtin(pred_id));

    // Register accessors
    for (field_name, accessor_name) in &field_accessors {
        let idx = constructor_fields.iter().position(|f| f == field_name)
            .ok_or_else(|| EvalError::Generic(format!(
                "define-record-type: field {field_name} not in constructor"
            )))?;
        let acc_id = format!("##record-acc##{tag}##{idx}");
        env_define(env, accessor_name.clone(), Value::Builtin(acc_id));
    }

    Ok(Value::Boolean(false))
}
