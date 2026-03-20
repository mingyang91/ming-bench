use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::rc::Rc;

use crate::scheme::error::EvalError;

type Cell = Rc<RefCell<Value>>;
type ContRef = Rc<Cont>;
type MacroRef = Rc<MacroTransformer>;
type PatternBindings = HashMap<String, MatchBinding>;
type LocalBindings = HashMap<String, Ident>;

pub fn eval_str_impl(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let program = parser.parse_program()?;
    if program.is_empty() {
        return Err(EvalError::EmptyProgram);
    }

    let env = Env::new_root();
    install_builtins(&env);

    let mut machine = Machine::default();
    let result = machine.eval_program(program, env)?;
    Ok(format_value(&result))
}

#[derive(Clone, Debug)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(Ident),
    List(Vec<Expr>),
    DottedList(Vec<Expr>, Box<Expr>),
}

impl Expr {
    fn symbol(name: &str) -> Self {
        Self::Symbol(Ident::raw(name))
    }

    fn as_symbol(&self) -> Option<&Ident> {
        match self {
            Self::Symbol(ident) => Some(ident),
            _ => None,
        }
    }

    fn symbol_name(&self) -> Option<&str> {
        self.as_symbol().map(|ident| ident.name.as_str())
    }

    fn is_ellipsis(&self) -> bool {
        self.symbol_name() == Some("...")
    }
}

#[derive(Clone, Debug)]
struct Ident {
    name: String,
    uid: Option<u64>,
    resolution: Option<IdentResolution>,
}

impl Ident {
    fn raw(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            uid: None,
            resolution: None,
        }
    }

    fn fresh(name: impl Into<String>, uid: u64) -> Self {
        Self {
            name: name.into(),
            uid: Some(uid),
            resolution: None,
        }
    }

    fn with_value(name: impl Into<String>, cell: Cell) -> Self {
        Self {
            name: name.into(),
            uid: None,
            resolution: Some(IdentResolution::Value(cell)),
        }
    }

    fn with_syntax(name: impl Into<String>, transformer: MacroRef) -> Self {
        Self {
            name: name.into(),
            uid: None,
            resolution: Some(IdentResolution::Syntax(transformer)),
        }
    }

    fn binding_key(&self) -> BindingKey {
        BindingKey {
            name: self.name.clone(),
            uid: self.uid,
        }
    }

    fn as_binding_ident(&self) -> Self {
        Self {
            name: self.name.clone(),
            uid: self.uid,
            resolution: None,
        }
    }
}

#[derive(Clone, Debug)]
enum IdentResolution {
    Value(Cell),
    Syntax(MacroRef),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct BindingKey {
    name: String,
    uid: Option<u64>,
}

#[derive(Clone, Debug)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    Nil,
    Pair(Rc<Pair>),
    Procedure(Rc<Procedure>),
    Void,
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    fn type_name(&self) -> &'static str {
        match self {
            Self::Integer(_) => "number",
            Self::Boolean(_) => "boolean",
            Self::String(_) => "string",
            Self::Symbol(_) => "symbol",
            Self::Nil => "null",
            Self::Pair(_) => "pair",
            Self::Procedure(_) => "procedure",
            Self::Void => "void",
        }
    }
}

#[derive(Clone, Debug)]
struct Pair {
    car: Value,
    cdr: Value,
}

#[derive(Clone, Debug)]
enum Procedure {
    Builtin(Builtin),
    Lambda(Rc<Lambda>),
    Continuation(ContRef),
}

#[derive(Clone, Copy, Debug)]
enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    Less,
    Greater,
    NumericEqual,
    LessEqual,
    Not,
    Cons,
    Car,
    Cdr,
    NullPred,
    List,
    Length,
    StringPred,
    NumberPred,
    BooleanPred,
    PairPred,
    SymbolPred,
    Apply,
    CallCc,
}

#[derive(Clone, Debug)]
struct Lambda {
    formals: Formals,
    body: Vec<Expr>,
    env: Env,
}

#[derive(Clone, Debug)]
struct Formals {
    required: Vec<Ident>,
    rest: Option<Ident>,
}

#[derive(Clone, Debug)]
struct CondClause {
    test: Expr,
    body: Vec<Expr>,
    is_else: bool,
}

#[derive(Clone, Debug)]
struct MacroTransformer {
    name: String,
    literals: HashSet<String>,
    rules: Vec<MacroRule>,
    env: Env,
}

#[derive(Clone, Debug)]
struct MacroRule {
    pattern: Expr,
    template: Expr,
}

#[derive(Clone, Debug)]
enum MatchBinding {
    Single(Expr),
    Repeat(Vec<Expr>),
}

#[derive(Clone, Debug)]
struct Env(Rc<EnvFrame>);

#[derive(Debug)]
struct EnvFrame {
    parent: Option<Env>,
    values: RefCell<HashMap<BindingKey, Cell>>,
    syntax: RefCell<HashMap<BindingKey, MacroRef>>,
}

impl Env {
    fn new_root() -> Self {
        Self(Rc::new(EnvFrame {
            parent: None,
            values: RefCell::new(HashMap::new()),
            syntax: RefCell::new(HashMap::new()),
        }))
    }

    fn child(&self) -> Self {
        Self(Rc::new(EnvFrame {
            parent: Some(self.clone()),
            values: RefCell::new(HashMap::new()),
            syntax: RefCell::new(HashMap::new()),
        }))
    }

    fn ensure_value_binding_here(&self, ident: &Ident) -> Cell {
        let key = ident.binding_key();
        if let Some(existing) = self.0.values.borrow().get(&key) {
            return existing.clone();
        }

        let cell = Rc::new(RefCell::new(Value::Void));
        self.0.values.borrow_mut().insert(key, cell.clone());
        cell
    }

    fn bind_value_here(&self, ident: &Ident, value: Value) {
        let key = ident.binding_key();
        let mut values = self.0.values.borrow_mut();
        if let Some(existing) = values.get(&key) {
            *existing.borrow_mut() = value;
        } else {
            values.insert(key, Rc::new(RefCell::new(value)));
        }
    }

    fn define_syntax_here(&self, ident: &Ident, transformer: MacroRef) {
        self.0
            .syntax
            .borrow_mut()
            .insert(ident.binding_key(), transformer);
    }

    fn lookup_value_cell(&self, ident: &Ident) -> Option<Cell> {
        if let Some(IdentResolution::Value(cell)) = &ident.resolution {
            return Some(cell.clone());
        }

        self.lookup_value_by_key(&ident.binding_key())
    }

    fn lookup_value_name(&self, name: &str) -> Option<Cell> {
        self.lookup_value_by_key(&BindingKey {
            name: name.to_string(),
            uid: None,
        })
    }

    fn lookup_syntax(&self, ident: &Ident) -> Option<MacroRef> {
        if let Some(IdentResolution::Syntax(transformer)) = &ident.resolution {
            return Some(transformer.clone());
        }

        self.lookup_syntax_by_key(&ident.binding_key())
    }

    fn lookup_syntax_name(&self, name: &str) -> Option<MacroRef> {
        self.lookup_syntax_by_key(&BindingKey {
            name: name.to_string(),
            uid: None,
        })
    }

    fn set_existing(&self, ident: &Ident, value: Value) -> Result<(), EvalError> {
        if let Some(IdentResolution::Value(cell)) = &ident.resolution {
            *cell.borrow_mut() = value;
            return Ok(());
        }

        let key = ident.binding_key();
        let Some(cell) = self.lookup_value_by_key(&key) else {
            return Err(EvalError::UnboundVariable {
                name: ident.name.clone(),
            });
        };
        *cell.borrow_mut() = value;
        Ok(())
    }

    fn lookup_value_by_key(&self, key: &BindingKey) -> Option<Cell> {
        if let Some(value) = self.0.values.borrow().get(key) {
            return Some(value.clone());
        }

        self.0
            .parent
            .as_ref()
            .and_then(|parent| parent.lookup_value_by_key(key))
    }

    fn lookup_syntax_by_key(&self, key: &BindingKey) -> Option<MacroRef> {
        if let Some(value) = self.0.syntax.borrow().get(key) {
            return Some(value.clone());
        }

        self.0
            .parent
            .as_ref()
            .and_then(|parent| parent.lookup_syntax_by_key(key))
    }
}

#[derive(Clone, Debug)]
enum Control {
    Eval {
        expr: Expr,
        env: Env,
        cont: ContRef,
    },
    Apply {
        proc: Value,
        args: Vec<Value>,
        cont: ContRef,
    },
    Return {
        value: Value,
        cont: ContRef,
    },
}

#[derive(Clone, Debug)]
enum Cont {
    Done,
    CallOperator {
        operands: Vec<Expr>,
        env: Env,
        next: ContRef,
    },
    CallOperands {
        proc: Value,
        operands: Vec<Expr>,
        index: usize,
        values: Vec<Value>,
        env: Env,
        next: ContRef,
    },
    If {
        consequent: Expr,
        alternative: Option<Expr>,
        env: Env,
        next: ContRef,
    },
    Sequence {
        exprs: Vec<Expr>,
        index: usize,
        env: Env,
        next: ContRef,
    },
    Define {
        cell: Cell,
        next: ContRef,
    },
    ProcedureBoundary {
        next: ContRef,
    },
    CallCcReturn {
        current: ContRef,
    },
    Set {
        target: Ident,
        env: Env,
        next: ContRef,
    },
    And {
        exprs: Vec<Expr>,
        index: usize,
        env: Env,
        next: ContRef,
    },
    Or {
        exprs: Vec<Expr>,
        index: usize,
        env: Env,
        next: ContRef,
    },
    Let {
        names: Vec<Ident>,
        exprs: Vec<Expr>,
        index: usize,
        values: Vec<Value>,
        body: Vec<Expr>,
        eval_env: Env,
        next: ContRef,
    },
    NamedLet {
        name: Ident,
        params: Vec<Ident>,
        exprs: Vec<Expr>,
        index: usize,
        values: Vec<Value>,
        body: Vec<Expr>,
        env: Env,
        next: ContRef,
    },
    Cond {
        clauses: Vec<CondClause>,
        index: usize,
        env: Env,
        next: ContRef,
    },
}

#[derive(Clone, Debug)]
enum Step {
    Continue(Control),
    Halt(Value),
}

#[derive(Debug, Default)]
struct Machine {
    next_uid: u64,
}

impl Machine {
    fn eval_program(&mut self, exprs: Vec<Expr>, env: Env) -> Result<Value, EvalError> {
        let mut control = self.enter_sequence(exprs, env, Rc::new(Cont::Done));

        loop {
            control = match control {
                Control::Eval { expr, env, cont } => self.eval_expr(expr, env, cont)?,
                Control::Apply { proc, args, cont } => self.apply_procedure(proc, args, cont)?,
                Control::Return { value, cont } => match self.resume(value, cont)? {
                    Step::Continue(next) => next,
                    Step::Halt(result) => return Ok(result),
                },
            };
        }
    }

    fn eval_expr(&mut self, expr: Expr, env: Env, cont: ContRef) -> Result<Control, EvalError> {
        match expr {
            Expr::Integer(value) => Ok(Control::Return {
                value: Value::Integer(value),
                cont,
            }),
            Expr::Boolean(value) => Ok(Control::Return {
                value: Value::Boolean(value),
                cont,
            }),
            Expr::String(value) => Ok(Control::Return {
                value: Value::String(value),
                cont,
            }),
            Expr::Symbol(ident) => {
                let Some(cell) = env.lookup_value_cell(&ident) else {
                    return Err(EvalError::UnboundVariable { name: ident.name });
                };
                let value = cell.borrow().clone();
                Ok(Control::Return { value, cont })
            }
            Expr::DottedList(_, _) => Err(EvalError::InvalidForm {
                message: "cannot evaluate dotted list directly".to_string(),
            }),
            Expr::List(items) => self.eval_list(items, env, cont),
        }
    }

    fn eval_list(
        &mut self,
        items: Vec<Expr>,
        env: Env,
        cont: ContRef,
    ) -> Result<Control, EvalError> {
        if items.is_empty() {
            return Ok(Control::Return {
                value: Value::Nil,
                cont,
            });
        }

        let expanded = self.expand_macros(Expr::List(items), &env)?;
        let Expr::List(items) = expanded else {
            return Ok(Control::Eval {
                expr: expanded,
                env,
                cont,
            });
        };

        if items.is_empty() {
            return Ok(Control::Return {
                value: Value::Nil,
                cont,
            });
        }

        if let Some(operator) = items[0].as_symbol() {
            match operator.name.as_str() {
                "quote" => return self.eval_quote(&items[1..], cont),
                "if" => return self.eval_if(&items[1..], env, cont),
                "define" => return self.eval_define(&items[1..], env, cont),
                "lambda" => return self.eval_lambda(&items[1..], env, cont),
                "begin" => return Ok(self.enter_sequence(items[1..].to_vec(), env, cont)),
                "and" => return Ok(self.enter_and(items[1..].to_vec(), env, cont)),
                "or" => return Ok(self.enter_or(items[1..].to_vec(), env, cont)),
                "let" => return self.eval_let(&items[1..], env, cont),
                "cond" => return self.eval_cond(&items[1..], env, cont),
                "set!" => return self.eval_set(&items[1..], env, cont),
                "define-syntax" => return self.eval_define_syntax(&items[1..], env, cont),
                _ => {}
            }
        }

        Ok(Control::Eval {
            expr: items[0].clone(),
            env: env.clone(),
            cont: Rc::new(Cont::CallOperator {
                operands: items[1..].to_vec(),
                env,
                next: cont,
            }),
        })
    }

    fn eval_quote(&self, args: &[Expr], cont: ContRef) -> Result<Control, EvalError> {
        if args.len() != 1 {
            return Err(EvalError::ArityError {
                expected: "1".to_string(),
                got: args.len(),
            });
        }

        Ok(Control::Return {
            value: quote_to_value(&args[0]),
            cont,
        })
    }

    fn eval_if(&self, args: &[Expr], env: Env, cont: ContRef) -> Result<Control, EvalError> {
        if args.len() != 2 && args.len() != 3 {
            return Err(EvalError::ArityError {
                expected: "2 or 3".to_string(),
                got: args.len(),
            });
        }

        Ok(Control::Eval {
            expr: args[0].clone(),
            env: env.clone(),
            cont: Rc::new(Cont::If {
                consequent: args[1].clone(),
                alternative: args.get(2).cloned(),
                env,
                next: cont,
            }),
        })
    }

    fn eval_define(&self, args: &[Expr], env: Env, cont: ContRef) -> Result<Control, EvalError> {
        let (name, value_expr) = parse_define(args)?;
        let cell = env.ensure_value_binding_here(&name);
        Ok(Control::Eval {
            expr: value_expr,
            env,
            cont: Rc::new(Cont::Define { cell, next: cont }),
        })
    }

    fn eval_lambda(&self, args: &[Expr], env: Env, cont: ContRef) -> Result<Control, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::InvalidForm {
                message: "lambda requires formals and body".to_string(),
            });
        }

        let formals = parse_formals(&args[0])?;
        let lambda = Lambda {
            formals,
            body: args[1..].to_vec(),
            env,
        };

        Ok(Control::Return {
            value: Value::Procedure(Rc::new(Procedure::Lambda(Rc::new(lambda)))),
            cont,
        })
    }

    fn eval_let(&self, args: &[Expr], env: Env, cont: ContRef) -> Result<Control, EvalError> {
        if args.is_empty() {
            return Err(EvalError::InvalidForm {
                message: "let requires bindings".to_string(),
            });
        }

        if let Expr::Symbol(name) = &args[0] {
            if args.len() < 2 {
                return Err(EvalError::InvalidForm {
                    message: "named let requires bindings and body".to_string(),
                });
            }

            let bindings = parse_let_bindings(&args[1])?;
            let params = bindings
                .iter()
                .map(|(ident, _)| ident.clone())
                .collect::<Vec<_>>();
            validate_unique_bindings(&params)?;
            let exprs = bindings
                .into_iter()
                .map(|(_, expr)| expr)
                .collect::<Vec<_>>();
            let body = args[2..].to_vec();

            if exprs.is_empty() {
                return self.finish_named_let(name.clone(), params, Vec::new(), body, env, cont);
            }

            Ok(Control::Eval {
                expr: exprs[0].clone(),
                env: env.clone(),
                cont: Rc::new(Cont::NamedLet {
                    name: name.clone(),
                    params,
                    exprs,
                    index: 1,
                    values: Vec::new(),
                    body,
                    env,
                    next: cont,
                }),
            })
        } else {
            let bindings = parse_let_bindings(&args[0])?;
            let names = bindings
                .iter()
                .map(|(ident, _)| ident.clone())
                .collect::<Vec<_>>();
            validate_unique_bindings(&names)?;
            let exprs = bindings
                .into_iter()
                .map(|(_, expr)| expr)
                .collect::<Vec<_>>();
            let body = args[1..].to_vec();

            if exprs.is_empty() {
                let let_env = env.child();
                return Ok(self.enter_sequence(body, let_env, cont));
            }

            Ok(Control::Eval {
                expr: exprs[0].clone(),
                env: env.clone(),
                cont: Rc::new(Cont::Let {
                    names,
                    exprs,
                    index: 1,
                    values: Vec::new(),
                    body,
                    eval_env: env,
                    next: cont,
                }),
            })
        }
    }

    fn eval_cond(&self, args: &[Expr], env: Env, cont: ContRef) -> Result<Control, EvalError> {
        let clauses = parse_cond_clauses(args)?;
        Ok(self.enter_cond(clauses, 0, env, cont)?)
    }

    fn eval_set(&self, args: &[Expr], env: Env, cont: ContRef) -> Result<Control, EvalError> {
        if args.len() != 2 {
            return Err(EvalError::ArityError {
                expected: "2".to_string(),
                got: args.len(),
            });
        }

        let target = expect_symbol_expr(&args[0])?.clone();
        if env.lookup_value_cell(&target).is_none() {
            return Err(EvalError::UnboundVariable {
                name: target.name.clone(),
            });
        }

        Ok(Control::Eval {
            expr: args[1].clone(),
            env: env.clone(),
            cont: Rc::new(Cont::Set {
                target,
                env,
                next: cont,
            }),
        })
    }

    fn eval_define_syntax(
        &self,
        args: &[Expr],
        env: Env,
        cont: ContRef,
    ) -> Result<Control, EvalError> {
        if args.len() != 2 {
            return Err(EvalError::ArityError {
                expected: "2".to_string(),
                got: args.len(),
            });
        }

        let name = expect_symbol_expr(&args[0])?.clone();
        let transformer = Rc::new(parse_syntax_rules(&name, &args[1], env.clone())?);
        env.define_syntax_here(&name, transformer);

        Ok(Control::Return {
            value: Value::Void,
            cont,
        })
    }

    fn apply_procedure(
        &mut self,
        proc: Value,
        args: Vec<Value>,
        cont: ContRef,
    ) -> Result<Control, EvalError> {
        let Value::Procedure(procedure) = proc else {
            return Err(EvalError::TypeError {
                expected: "procedure".to_string(),
                found: proc.type_name().to_string(),
            });
        };

        match procedure.as_ref() {
            Procedure::Builtin(builtin) => self.apply_builtin(*builtin, args, cont),
            Procedure::Lambda(lambda) => self.apply_lambda(lambda.clone(), args, cont),
            Procedure::Continuation(saved) => {
                if args.len() != 1 {
                    return Err(EvalError::ArityError {
                        expected: "1".to_string(),
                        got: args.len(),
                    });
                }

                Ok(Control::Return {
                    value: args[0].clone(),
                    cont: saved.clone(),
                })
            }
        }
    }

    fn apply_builtin(
        &mut self,
        builtin: Builtin,
        args: Vec<Value>,
        cont: ContRef,
    ) -> Result<Control, EvalError> {
        match builtin {
            Builtin::Add => Ok(Control::Return {
                value: Value::Integer(fold_numbers(&args, 0, checked_add)?),
                cont,
            }),
            Builtin::Sub => Ok(Control::Return {
                value: Value::Integer(apply_sub(&args)?),
                cont,
            }),
            Builtin::Mul => Ok(Control::Return {
                value: Value::Integer(fold_numbers(&args, 1, checked_mul)?),
                cont,
            }),
            Builtin::Div => Ok(Control::Return {
                value: Value::Integer(apply_div(&args)?),
                cont,
            }),
            Builtin::Less => Ok(Control::Return {
                value: Value::Boolean(compare_numbers(&args, |left, right| left < right)?),
                cont,
            }),
            Builtin::Greater => Ok(Control::Return {
                value: Value::Boolean(compare_numbers(&args, |left, right| left > right)?),
                cont,
            }),
            Builtin::NumericEqual => Ok(Control::Return {
                value: Value::Boolean(compare_numbers(&args, |left, right| left == right)?),
                cont,
            }),
            Builtin::LessEqual => Ok(Control::Return {
                value: Value::Boolean(compare_numbers(&args, |left, right| left <= right)?),
                cont,
            }),
            Builtin::Not => {
                ensure_exact_arity(&args, 1)?;
                Ok(Control::Return {
                    value: Value::Boolean(!args[0].is_truthy()),
                    cont,
                })
            }
            Builtin::Cons => {
                ensure_exact_arity(&args, 2)?;
                Ok(Control::Return {
                    value: cons(args[0].clone(), args[1].clone()),
                    cont,
                })
            }
            Builtin::Car => {
                ensure_exact_arity(&args, 1)?;
                let pair = expect_pair(&args[0])?;
                Ok(Control::Return {
                    value: pair.car.clone(),
                    cont,
                })
            }
            Builtin::Cdr => {
                ensure_exact_arity(&args, 1)?;
                let pair = expect_pair(&args[0])?;
                Ok(Control::Return {
                    value: pair.cdr.clone(),
                    cont,
                })
            }
            Builtin::NullPred => {
                ensure_exact_arity(&args, 1)?;
                Ok(Control::Return {
                    value: Value::Boolean(matches!(args[0], Value::Nil)),
                    cont,
                })
            }
            Builtin::List => Ok(Control::Return {
                value: list_from_values(args),
                cont,
            }),
            Builtin::Length => {
                ensure_exact_arity(&args, 1)?;
                Ok(Control::Return {
                    value: Value::Integer(proper_list_to_vec(&args[0])?.len() as i64),
                    cont,
                })
            }
            Builtin::StringPred => {
                ensure_exact_arity(&args, 1)?;
                Ok(Control::Return {
                    value: Value::Boolean(matches!(args[0], Value::String(_))),
                    cont,
                })
            }
            Builtin::NumberPred => {
                ensure_exact_arity(&args, 1)?;
                Ok(Control::Return {
                    value: Value::Boolean(matches!(args[0], Value::Integer(_))),
                    cont,
                })
            }
            Builtin::BooleanPred => {
                ensure_exact_arity(&args, 1)?;
                Ok(Control::Return {
                    value: Value::Boolean(matches!(args[0], Value::Boolean(_))),
                    cont,
                })
            }
            Builtin::PairPred => {
                ensure_exact_arity(&args, 1)?;
                Ok(Control::Return {
                    value: Value::Boolean(matches!(args[0], Value::Pair(_))),
                    cont,
                })
            }
            Builtin::SymbolPred => {
                ensure_exact_arity(&args, 1)?;
                Ok(Control::Return {
                    value: Value::Boolean(matches!(args[0], Value::Symbol(_))),
                    cont,
                })
            }
            Builtin::Apply => {
                ensure_min_arity(&args, 2)?;
                let proc = args[0].clone();
                let mut flattened = args[1..args.len() - 1].to_vec();
                let Some(last) = args.last() else {
                    return Err(EvalError::ArityError {
                        expected: "at least 2".to_string(),
                        got: args.len(),
                    });
                };
                let mut rest = proper_list_to_vec(last)?;
                flattened.append(&mut rest);
                Ok(Control::Apply {
                    proc,
                    args: flattened,
                    cont,
                })
            }
            Builtin::CallCc => {
                ensure_exact_arity(&args, 1)?;
                Ok(Control::Apply {
                    proc: args[0].clone(),
                    args: vec![Value::Procedure(Rc::new(Procedure::Continuation(
                        cont.clone(),
                    )))],
                    cont: Rc::new(Cont::CallCcReturn { current: cont }),
                })
            }
        }
    }

    fn apply_lambda(
        &self,
        lambda: Rc<Lambda>,
        args: Vec<Value>,
        cont: ContRef,
    ) -> Result<Control, EvalError> {
        let call_env = lambda.env.child();
        bind_formals(&lambda.formals, args, &call_env)?;
        Ok(self.enter_sequence(
            lambda.body.clone(),
            call_env,
            Rc::new(Cont::ProcedureBoundary { next: cont }),
        ))
    }

    fn resume(&mut self, value: Value, cont: ContRef) -> Result<Step, EvalError> {
        match cont.as_ref() {
            Cont::Done => Ok(Step::Halt(value)),
            Cont::CallOperator {
                operands,
                env,
                next,
            } => {
                if operands.is_empty() {
                    Ok(Step::Continue(Control::Apply {
                        proc: value,
                        args: Vec::new(),
                        cont: next.clone(),
                    }))
                } else {
                    let last_index = operands.len() - 1;
                    Ok(Step::Continue(Control::Eval {
                        expr: operands[last_index].clone(),
                        env: env.clone(),
                        cont: Rc::new(Cont::CallOperands {
                            proc: value,
                            operands: operands.clone(),
                            index: last_index,
                            values: Vec::new(),
                            env: env.clone(),
                            next: next.clone(),
                        }),
                    }))
                }
            }
            Cont::CallOperands {
                proc,
                operands,
                index,
                values,
                env,
                next,
            } => {
                let mut values = values.clone();
                values.push(value);
                if *index == 0 {
                    values.reverse();
                    Ok(Step::Continue(Control::Apply {
                        proc: proc.clone(),
                        args: values,
                        cont: next.clone(),
                    }))
                } else {
                    let next_index = index - 1;
                    Ok(Step::Continue(Control::Eval {
                        expr: operands[next_index].clone(),
                        env: env.clone(),
                        cont: Rc::new(Cont::CallOperands {
                            proc: proc.clone(),
                            operands: operands.clone(),
                            index: next_index,
                            values,
                            env: env.clone(),
                            next: next.clone(),
                        }),
                    }))
                }
            }
            Cont::If {
                consequent,
                alternative,
                env,
                next,
            } => {
                if value.is_truthy() {
                    Ok(Step::Continue(Control::Eval {
                        expr: consequent.clone(),
                        env: env.clone(),
                        cont: next.clone(),
                    }))
                } else if let Some(alternative) = alternative {
                    Ok(Step::Continue(Control::Eval {
                        expr: alternative.clone(),
                        env: env.clone(),
                        cont: next.clone(),
                    }))
                } else {
                    Ok(Step::Continue(Control::Return {
                        value: Value::Void,
                        cont: next.clone(),
                    }))
                }
            }
            Cont::Sequence {
                exprs,
                index,
                env,
                next,
            } => {
                if *index == exprs.len() - 1 {
                    Ok(Step::Continue(Control::Eval {
                        expr: exprs[*index].clone(),
                        env: env.clone(),
                        cont: next.clone(),
                    }))
                } else {
                    Ok(Step::Continue(Control::Eval {
                        expr: exprs[*index].clone(),
                        env: env.clone(),
                        cont: Rc::new(Cont::Sequence {
                            exprs: exprs.clone(),
                            index: index + 1,
                            env: env.clone(),
                            next: next.clone(),
                        }),
                    }))
                }
            }
            Cont::Define { cell, next } => {
                *cell.borrow_mut() = value;
                Ok(Step::Continue(Control::Return {
                    value: Value::Void,
                    cont: next.clone(),
                }))
            }
            Cont::ProcedureBoundary { next } => Ok(Step::Continue(Control::Return {
                value,
                cont: next.clone(),
            })),
            Cont::CallCcReturn { current } => {
                let target = if matches!(value, Value::Void) {
                    procedure_return_cont(current)
                } else {
                    current.clone()
                };
                Ok(Step::Continue(Control::Return {
                    value,
                    cont: target,
                }))
            }
            Cont::Set { target, env, next } => {
                env.set_existing(target, value)?;
                Ok(Step::Continue(Control::Return {
                    value: Value::Void,
                    cont: next.clone(),
                }))
            }
            Cont::And {
                exprs,
                index,
                env,
                next,
            } => {
                if !value.is_truthy() {
                    Ok(Step::Continue(Control::Return {
                        value,
                        cont: next.clone(),
                    }))
                } else if *index == exprs.len() - 1 {
                    Ok(Step::Continue(Control::Eval {
                        expr: exprs[*index].clone(),
                        env: env.clone(),
                        cont: next.clone(),
                    }))
                } else {
                    Ok(Step::Continue(Control::Eval {
                        expr: exprs[*index].clone(),
                        env: env.clone(),
                        cont: Rc::new(Cont::And {
                            exprs: exprs.clone(),
                            index: index + 1,
                            env: env.clone(),
                            next: next.clone(),
                        }),
                    }))
                }
            }
            Cont::Or {
                exprs,
                index,
                env,
                next,
            } => {
                if value.is_truthy() {
                    Ok(Step::Continue(Control::Return {
                        value,
                        cont: next.clone(),
                    }))
                } else if *index == exprs.len() - 1 {
                    Ok(Step::Continue(Control::Eval {
                        expr: exprs[*index].clone(),
                        env: env.clone(),
                        cont: next.clone(),
                    }))
                } else {
                    Ok(Step::Continue(Control::Eval {
                        expr: exprs[*index].clone(),
                        env: env.clone(),
                        cont: Rc::new(Cont::Or {
                            exprs: exprs.clone(),
                            index: index + 1,
                            env: env.clone(),
                            next: next.clone(),
                        }),
                    }))
                }
            }
            Cont::Let {
                names,
                exprs,
                index,
                values,
                body,
                eval_env,
                next,
            } => {
                let mut values = values.clone();
                values.push(value);
                if *index == exprs.len() {
                    let let_env = eval_env.child();
                    for (name, value) in names.iter().zip(values) {
                        let_env.bind_value_here(name, value);
                    }
                    Ok(Step::Continue(self.enter_sequence(
                        body.clone(),
                        let_env,
                        next.clone(),
                    )))
                } else {
                    Ok(Step::Continue(Control::Eval {
                        expr: exprs[*index].clone(),
                        env: eval_env.clone(),
                        cont: Rc::new(Cont::Let {
                            names: names.clone(),
                            exprs: exprs.clone(),
                            index: index + 1,
                            values,
                            body: body.clone(),
                            eval_env: eval_env.clone(),
                            next: next.clone(),
                        }),
                    }))
                }
            }
            Cont::NamedLet {
                name,
                params,
                exprs,
                index,
                values,
                body,
                env,
                next,
            } => {
                let mut values = values.clone();
                values.push(value);
                if *index == exprs.len() {
                    self.finish_named_let(
                        name.clone(),
                        params.clone(),
                        values,
                        body.clone(),
                        env.clone(),
                        next.clone(),
                    )
                    .map(Step::Continue)
                } else {
                    Ok(Step::Continue(Control::Eval {
                        expr: exprs[*index].clone(),
                        env: env.clone(),
                        cont: Rc::new(Cont::NamedLet {
                            name: name.clone(),
                            params: params.clone(),
                            exprs: exprs.clone(),
                            index: index + 1,
                            values,
                            body: body.clone(),
                            env: env.clone(),
                            next: next.clone(),
                        }),
                    }))
                }
            }
            Cont::Cond {
                clauses,
                index,
                env,
                next,
            } => {
                let clause = &clauses[*index];
                if value.is_truthy() {
                    if clause.body.is_empty() {
                        Ok(Step::Continue(Control::Return {
                            value,
                            cont: next.clone(),
                        }))
                    } else {
                        Ok(Step::Continue(self.enter_sequence(
                            clause.body.clone(),
                            env.clone(),
                            next.clone(),
                        )))
                    }
                } else {
                    self.enter_cond(clauses.clone(), index + 1, env.clone(), next.clone())
                        .map(Step::Continue)
                }
            }
        }
    }

    fn enter_sequence(&self, exprs: Vec<Expr>, env: Env, next: ContRef) -> Control {
        if exprs.is_empty() {
            return Control::Return {
                value: Value::Void,
                cont: next,
            };
        }

        if exprs.len() == 1 {
            return Control::Eval {
                expr: exprs[0].clone(),
                env,
                cont: next,
            };
        }

        Control::Eval {
            expr: exprs[0].clone(),
            env: env.clone(),
            cont: Rc::new(Cont::Sequence {
                exprs,
                index: 1,
                env,
                next,
            }),
        }
    }

    fn enter_and(&self, exprs: Vec<Expr>, env: Env, next: ContRef) -> Control {
        if exprs.is_empty() {
            return Control::Return {
                value: Value::Boolean(true),
                cont: next,
            };
        }

        if exprs.len() == 1 {
            return Control::Eval {
                expr: exprs[0].clone(),
                env,
                cont: next,
            };
        }

        Control::Eval {
            expr: exprs[0].clone(),
            env: env.clone(),
            cont: Rc::new(Cont::And {
                exprs,
                index: 1,
                env,
                next,
            }),
        }
    }

    fn enter_or(&self, exprs: Vec<Expr>, env: Env, next: ContRef) -> Control {
        if exprs.is_empty() {
            return Control::Return {
                value: Value::Boolean(false),
                cont: next,
            };
        }

        if exprs.len() == 1 {
            return Control::Eval {
                expr: exprs[0].clone(),
                env,
                cont: next,
            };
        }

        Control::Eval {
            expr: exprs[0].clone(),
            env: env.clone(),
            cont: Rc::new(Cont::Or {
                exprs,
                index: 1,
                env,
                next,
            }),
        }
    }

    fn enter_cond(
        &self,
        clauses: Vec<CondClause>,
        index: usize,
        env: Env,
        next: ContRef,
    ) -> Result<Control, EvalError> {
        if index >= clauses.len() {
            return Ok(Control::Return {
                value: Value::Void,
                cont: next,
            });
        }

        let clause = &clauses[index];
        if clause.is_else {
            if index + 1 != clauses.len() {
                return Err(EvalError::InvalidForm {
                    message: "else must be the final cond clause".to_string(),
                });
            }
            return Ok(self.enter_sequence(clause.body.clone(), env, next));
        }

        Ok(Control::Eval {
            expr: clause.test.clone(),
            env: env.clone(),
            cont: Rc::new(Cont::Cond {
                clauses,
                index,
                env,
                next,
            }),
        })
    }

    fn finish_named_let(
        &self,
        name: Ident,
        params: Vec<Ident>,
        args: Vec<Value>,
        body: Vec<Expr>,
        env: Env,
        cont: ContRef,
    ) -> Result<Control, EvalError> {
        let let_env = env.child();
        let name_cell = let_env.ensure_value_binding_here(&name);
        let lambda = Lambda {
            formals: Formals {
                required: params,
                rest: None,
            },
            body,
            env: let_env.clone(),
        };
        let proc = Value::Procedure(Rc::new(Procedure::Lambda(Rc::new(lambda))));
        *name_cell.borrow_mut() = proc.clone();
        Ok(Control::Apply { proc, args, cont })
    }

    fn expand_macros(&mut self, expr: Expr, env: &Env) -> Result<Expr, EvalError> {
        let mut current = expr;

        loop {
            let Some(operator) = current
                .as_list()
                .and_then(|items| items.first())
                .and_then(Expr::as_symbol)
            else {
                return Ok(current);
            };

            let Some(transformer) = env.lookup_syntax(operator) else {
                return Ok(current);
            };

            current = self.expand_macro_call(transformer, &current)?;
        }
    }

    fn expand_macro_call(&mut self, transformer: MacroRef, call: &Expr) -> Result<Expr, EvalError> {
        for rule in &transformer.rules {
            let mut bindings = HashMap::new();
            if self.match_pattern(&transformer, &rule.pattern, call, &mut bindings, false)? {
                let mut introduced = HashMap::new();
                return self.expand_template(
                    &rule.template,
                    &bindings,
                    &transformer,
                    &mut introduced,
                    &HashMap::new(),
                );
            }
        }

        Err(EvalError::MacroExpansion {
            message: format!("no matching syntax-rules clause for `{}`", transformer.name),
        })
    }

    fn match_pattern(
        &self,
        transformer: &MacroTransformer,
        pattern: &Expr,
        input: &Expr,
        bindings: &mut PatternBindings,
        force_literal: bool,
    ) -> Result<bool, EvalError> {
        match (pattern, input) {
            (Expr::Integer(left), Expr::Integer(right)) => Ok(left == right),
            (Expr::Boolean(left), Expr::Boolean(right)) => Ok(left == right),
            (Expr::String(left), Expr::String(right)) => Ok(left == right),
            (Expr::Symbol(symbol), _) => {
                self.match_pattern_symbol(transformer, symbol, input, bindings, force_literal)
            }
            (Expr::List(patterns), Expr::List(inputs)) => {
                self.match_list_pattern(transformer, patterns, inputs, bindings)
            }
            (Expr::DottedList(patterns, tail), Expr::DottedList(inputs, input_tail)) => {
                if patterns.len() != inputs.len() {
                    return Ok(false);
                }

                for (pattern, input) in patterns.iter().zip(inputs) {
                    if !self.match_pattern(transformer, pattern, input, bindings, false)? {
                        return Ok(false);
                    }
                }

                self.match_pattern(transformer, tail, input_tail, bindings, false)
            }
            _ => Ok(false),
        }
    }

    fn match_pattern_symbol(
        &self,
        transformer: &MacroTransformer,
        symbol: &Ident,
        input: &Expr,
        bindings: &mut PatternBindings,
        force_literal: bool,
    ) -> Result<bool, EvalError> {
        if symbol.name == "..." {
            return Err(EvalError::MacroExpansion {
                message: "ellipsis cannot appear alone in a pattern".to_string(),
            });
        }

        let is_literal = force_literal || transformer.literals.contains(&symbol.name);
        if is_literal {
            return Ok(matches!(input, Expr::Symbol(input_sym) if input_sym.name == symbol.name));
        }

        match bindings.get(&symbol.name) {
            None => {
                bindings.insert(symbol.name.clone(), MatchBinding::Single(input.clone()));
                Ok(true)
            }
            Some(MatchBinding::Single(existing)) => Ok(syntax_eq(existing, input)),
            Some(MatchBinding::Repeat(existing)) => {
                Ok(existing.len() == 1 && syntax_eq(&existing[0], input))
            }
        }
    }

    fn match_list_pattern(
        &self,
        transformer: &MacroTransformer,
        patterns: &[Expr],
        inputs: &[Expr],
        bindings: &mut PatternBindings,
    ) -> Result<bool, EvalError> {
        let mut pattern_index = 0;
        let mut input_index = 0;

        while pattern_index < patterns.len() {
            if pattern_index + 1 < patterns.len() && patterns[pattern_index + 1].is_ellipsis() {
                if pattern_index + 2 != patterns.len() {
                    return Err(EvalError::MacroExpansion {
                        message: "only trailing ellipsis patterns are supported".to_string(),
                    });
                }

                let Expr::Symbol(symbol) = &patterns[pattern_index] else {
                    return Err(EvalError::MacroExpansion {
                        message: "ellipsis must follow a pattern variable".to_string(),
                    });
                };
                if transformer.literals.contains(&symbol.name) {
                    return Err(EvalError::MacroExpansion {
                        message: "literal identifiers cannot be repeated with ellipsis".to_string(),
                    });
                }

                let repeated = inputs[input_index..].to_vec();
                match bindings.get(&symbol.name) {
                    None => {
                        bindings.insert(symbol.name.clone(), MatchBinding::Repeat(repeated));
                        return Ok(true);
                    }
                    Some(MatchBinding::Repeat(existing)) => {
                        return Ok(existing.len() == repeated.len()
                            && existing
                                .iter()
                                .zip(&repeated)
                                .all(|(left, right)| syntax_eq(left, right)));
                    }
                    Some(MatchBinding::Single(_)) => return Ok(false),
                }
            }

            if input_index >= inputs.len() {
                return Ok(false);
            }

            let force_literal = pattern_index == 0;
            if !self.match_pattern(
                transformer,
                &patterns[pattern_index],
                &inputs[input_index],
                bindings,
                force_literal,
            )? {
                return Ok(false);
            }

            pattern_index += 1;
            input_index += 1;
        }

        Ok(input_index == inputs.len())
    }

    fn expand_template(
        &mut self,
        template: &Expr,
        bindings: &PatternBindings,
        transformer: &MacroRef,
        introduced: &mut LocalBindings,
        local: &LocalBindings,
    ) -> Result<Expr, EvalError> {
        match template {
            Expr::Integer(_) | Expr::Boolean(_) | Expr::String(_) => Ok(template.clone()),
            Expr::Symbol(symbol) => {
                self.expand_template_symbol(symbol, bindings, transformer, introduced, local)
            }
            Expr::List(items) => {
                self.expand_template_list(items, bindings, transformer, introduced, local)
            }
            Expr::DottedList(items, tail) => {
                let expanded_items = items
                    .iter()
                    .map(|item| {
                        self.expand_template(item, bindings, transformer, introduced, local)
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let expanded_tail =
                    self.expand_template(tail, bindings, transformer, introduced, local)?;
                Ok(Expr::DottedList(expanded_items, Box::new(expanded_tail)))
            }
        }
    }

    fn expand_template_symbol(
        &mut self,
        symbol: &Ident,
        bindings: &PatternBindings,
        transformer: &MacroRef,
        introduced: &mut LocalBindings,
        local: &LocalBindings,
    ) -> Result<Expr, EvalError> {
        if let Some(binding) = bindings.get(&symbol.name) {
            return match binding {
                MatchBinding::Single(expr) => Ok(expr.clone()),
                MatchBinding::Repeat(_) => Err(EvalError::MacroExpansion {
                    message: format!(
                        "pattern variable `{}` must be followed by ellipsis",
                        symbol.name
                    ),
                }),
            };
        }

        if let Some(bound) = local.get(&symbol.name) {
            return Ok(Expr::Symbol(bound.clone()));
        }

        if is_core_keyword(&symbol.name) || symbol.name == "else" || symbol.name == "..." {
            return Ok(Expr::Symbol(Ident::raw(&symbol.name)));
        }

        if symbol.name == transformer.name {
            return Ok(Expr::Symbol(Ident::with_syntax(
                &symbol.name,
                transformer.clone(),
            )));
        }

        if let Some(syntax) = transformer.env.lookup_syntax_name(&symbol.name) {
            return Ok(Expr::Symbol(Ident::with_syntax(&symbol.name, syntax)));
        }

        if let Some(value) = transformer.env.lookup_value_name(&symbol.name) {
            return Ok(Expr::Symbol(Ident::with_value(&symbol.name, value)));
        }

        if let Some(existing) = introduced.get(&symbol.name) {
            return Ok(Expr::Symbol(existing.clone()));
        }

        let fresh = self.fresh_ident(&symbol.name);
        introduced.insert(symbol.name.clone(), fresh.clone());
        Ok(Expr::Symbol(fresh))
    }

    fn expand_template_list(
        &mut self,
        items: &[Expr],
        bindings: &PatternBindings,
        transformer: &MacroRef,
        introduced: &mut LocalBindings,
        local: &LocalBindings,
    ) -> Result<Expr, EvalError> {
        if let Some(head) = items.first().and_then(Expr::symbol_name) {
            match head {
                "lambda" => {
                    return self.expand_lambda_template(
                        items,
                        bindings,
                        transformer,
                        introduced,
                        local,
                    )
                }
                "let" => {
                    return self.expand_let_template(
                        items,
                        bindings,
                        transformer,
                        introduced,
                        local,
                    )
                }
                "define" => {
                    return self.expand_define_template(
                        items,
                        bindings,
                        transformer,
                        introduced,
                        local,
                    )
                }
                _ => {}
            }
        }

        let mut expanded = Vec::new();
        let mut index = 0;
        while index < items.len() {
            if index + 1 < items.len() && items[index + 1].is_ellipsis() {
                self.expand_ellipsis_template_item(&items[index], bindings, &mut expanded)?;
                index += 2;
            } else {
                expanded.push(self.expand_template(
                    &items[index],
                    bindings,
                    transformer,
                    introduced,
                    local,
                )?);
                index += 1;
            }
        }

        Ok(Expr::List(expanded))
    }

    fn expand_lambda_template(
        &mut self,
        items: &[Expr],
        bindings: &PatternBindings,
        transformer: &MacroRef,
        introduced: &mut LocalBindings,
        local: &LocalBindings,
    ) -> Result<Expr, EvalError> {
        if items.len() < 3 {
            return Err(EvalError::MacroExpansion {
                message: "lambda template requires formals and body".to_string(),
            });
        }

        let (formals, bound) = self.expand_binding_shape(&items[1], bindings)?;
        let body_locals = merge_local_bindings(local, &bound);
        let mut expanded = vec![Expr::symbol("lambda"), formals];
        for item in &items[2..] {
            expanded.push(self.expand_template(
                item,
                bindings,
                transformer,
                introduced,
                &body_locals,
            )?);
        }
        Ok(Expr::List(expanded))
    }

    fn expand_let_template(
        &mut self,
        items: &[Expr],
        bindings: &PatternBindings,
        transformer: &MacroRef,
        introduced: &mut LocalBindings,
        local: &LocalBindings,
    ) -> Result<Expr, EvalError> {
        if items.len() < 3 {
            return Err(EvalError::MacroExpansion {
                message: "let template requires bindings and body".to_string(),
            });
        }

        if items[1].as_symbol().is_some() && items.len() >= 4 {
            let name = self.expand_binding_identifier(&items[1], bindings)?;
            let binding_list = expect_list_expr(&items[2])?;
            let (expanded_bindings, binding_locals) =
                self.expand_let_bindings(binding_list, bindings, transformer, introduced, local)?;
            let mut body_locals = merge_local_bindings(local, &binding_locals);
            body_locals.insert(name.name.clone(), name.clone());

            let mut result = vec![
                Expr::symbol("let"),
                Expr::Symbol(name),
                Expr::List(expanded_bindings),
            ];
            for body in &items[3..] {
                result.push(self.expand_template(
                    body,
                    bindings,
                    transformer,
                    introduced,
                    &body_locals,
                )?);
            }
            return Ok(Expr::List(result));
        }

        let binding_list = expect_list_expr(&items[1])?;
        let (expanded_bindings, binding_locals) =
            self.expand_let_bindings(binding_list, bindings, transformer, introduced, local)?;
        let body_locals = merge_local_bindings(local, &binding_locals);

        let mut result = vec![Expr::symbol("let"), Expr::List(expanded_bindings)];
        for body in &items[2..] {
            result.push(self.expand_template(
                body,
                bindings,
                transformer,
                introduced,
                &body_locals,
            )?);
        }
        Ok(Expr::List(result))
    }

    fn expand_define_template(
        &mut self,
        items: &[Expr],
        bindings: &PatternBindings,
        transformer: &MacroRef,
        introduced: &mut LocalBindings,
        local: &LocalBindings,
    ) -> Result<Expr, EvalError> {
        if items.len() < 3 {
            return Err(EvalError::MacroExpansion {
                message: "define template requires target and value".to_string(),
            });
        }

        if items[1].as_symbol().is_some() {
            let name = self.expand_binding_identifier(&items[1], bindings)?;
            introduced.insert(name.name.clone(), name.clone());
            let mut value_locals = local.clone();
            value_locals.insert(name.name.clone(), name.clone());
            let value =
                self.expand_template(&items[2], bindings, transformer, introduced, &value_locals)?;
            return Ok(Expr::List(vec![
                Expr::symbol("define"),
                Expr::Symbol(name),
                value,
            ]));
        }

        let (target, mut bound) = self.expand_define_signature(&items[1], bindings)?;
        if let Some(name) = leading_define_name(&target) {
            introduced.insert(name.name.clone(), name.clone());
        }
        let body_locals = {
            let mut locals = local.clone();
            locals.extend(bound.drain());
            locals
        };

        let mut result = vec![Expr::symbol("define"), target];
        for body in &items[2..] {
            result.push(self.expand_template(
                body,
                bindings,
                transformer,
                introduced,
                &body_locals,
            )?);
        }
        Ok(Expr::List(result))
    }

    fn expand_define_signature(
        &mut self,
        target: &Expr,
        bindings: &PatternBindings,
    ) -> Result<(Expr, LocalBindings), EvalError> {
        match target {
            Expr::List(items) => {
                if items.is_empty() {
                    return Err(EvalError::MacroExpansion {
                        message: "define function signature cannot be empty".to_string(),
                    });
                }

                let name = self.expand_binding_identifier(&items[0], bindings)?;
                let (formals, mut locals) =
                    self.expand_binding_shape(&Expr::List(items[1..].to_vec()), bindings)?;
                locals.insert(name.name.clone(), name.clone());

                let mut expanded = vec![Expr::Symbol(name)];
                let Expr::List(formal_items) = formals else {
                    return Err(EvalError::MacroExpansion {
                        message: "function formals must expand to a proper list".to_string(),
                    });
                };
                expanded.extend(formal_items);
                Ok((Expr::List(expanded), locals))
            }
            Expr::DottedList(prefix, tail) => {
                if prefix.is_empty() {
                    return Err(EvalError::MacroExpansion {
                        message: "define function signature cannot be empty".to_string(),
                    });
                }

                let name = self.expand_binding_identifier(&prefix[0], bindings)?;
                let formals = if prefix.len() == 1 {
                    tail.as_ref().clone()
                } else {
                    Expr::DottedList(prefix[1..].to_vec(), tail.clone())
                };
                let (expanded_formals, mut locals) =
                    self.expand_binding_shape(&formals, bindings)?;
                locals.insert(name.name.clone(), name.clone());

                let expanded_target = match expanded_formals {
                    Expr::List(items) => {
                        let mut result = vec![Expr::Symbol(name)];
                        result.extend(items);
                        Expr::List(result)
                    }
                    Expr::DottedList(items, tail) => {
                        let mut result = vec![Expr::Symbol(name)];
                        result.extend(items);
                        Expr::DottedList(result, tail)
                    }
                    Expr::Symbol(symbol) => {
                        Expr::DottedList(vec![Expr::Symbol(name)], Box::new(Expr::Symbol(symbol)))
                    }
                    _ => {
                        return Err(EvalError::MacroExpansion {
                            message: "invalid function formal expansion".to_string(),
                        })
                    }
                };

                Ok((expanded_target, locals))
            }
            _ => Err(EvalError::MacroExpansion {
                message: "define function target must be a list".to_string(),
            }),
        }
    }

    fn expand_binding_shape(
        &mut self,
        formals: &Expr,
        bindings: &PatternBindings,
    ) -> Result<(Expr, LocalBindings), EvalError> {
        match formals {
            Expr::Symbol(_) => {
                let ident = self.expand_binding_identifier(formals, bindings)?;
                let mut locals = HashMap::new();
                locals.insert(ident.name.clone(), ident.clone());
                Ok((Expr::Symbol(ident), locals))
            }
            Expr::List(items) => {
                let mut expanded = Vec::with_capacity(items.len());
                let mut locals = HashMap::new();
                for item in items {
                    let ident = self.expand_binding_identifier(item, bindings)?;
                    locals.insert(ident.name.clone(), ident.clone());
                    expanded.push(Expr::Symbol(ident));
                }
                Ok((Expr::List(expanded), locals))
            }
            Expr::DottedList(items, tail) => {
                let mut expanded_items = Vec::with_capacity(items.len());
                let mut locals = HashMap::new();
                for item in items {
                    let ident = self.expand_binding_identifier(item, bindings)?;
                    locals.insert(ident.name.clone(), ident.clone());
                    expanded_items.push(Expr::Symbol(ident));
                }

                let tail_ident = self.expand_binding_identifier(tail, bindings)?;
                locals.insert(tail_ident.name.clone(), tail_ident.clone());
                Ok((
                    Expr::DottedList(expanded_items, Box::new(Expr::Symbol(tail_ident))),
                    locals,
                ))
            }
            _ => Err(EvalError::MacroExpansion {
                message: "binding shape must be a symbol or list".to_string(),
            }),
        }
    }

    fn expand_binding_identifier(
        &mut self,
        target: &Expr,
        bindings: &PatternBindings,
    ) -> Result<Ident, EvalError> {
        let symbol = expect_symbol_expr(target)?;
        if let Some(binding) = bindings.get(&symbol.name) {
            return match binding {
                MatchBinding::Single(Expr::Symbol(ident)) => Ok(ident.as_binding_ident()),
                MatchBinding::Single(_) => Err(EvalError::MacroExpansion {
                    message: format!(
                        "pattern variable `{}` must expand to an identifier in binding position",
                        symbol.name
                    ),
                }),
                MatchBinding::Repeat(_) => Err(EvalError::MacroExpansion {
                    message: format!(
                        "pattern variable `{}` cannot be repeated in binding position",
                        symbol.name
                    ),
                }),
            };
        }

        Ok(self.fresh_ident(&symbol.name))
    }

    fn expand_let_bindings(
        &mut self,
        binding_list: &[Expr],
        bindings: &PatternBindings,
        transformer: &MacroRef,
        introduced: &mut LocalBindings,
        local: &LocalBindings,
    ) -> Result<(Vec<Expr>, LocalBindings), EvalError> {
        let mut expanded_bindings = Vec::new();
        let mut bound_locals = HashMap::new();

        for binding in binding_list {
            let binding_items = expect_list_expr(binding)?;
            if binding_items.len() != 2 {
                return Err(EvalError::MacroExpansion {
                    message: "let binding templates must have two elements".to_string(),
                });
            }

            let name = self.expand_binding_identifier(&binding_items[0], bindings)?;
            let value =
                self.expand_template(&binding_items[1], bindings, transformer, introduced, local)?;
            bound_locals.insert(name.name.clone(), name.clone());
            expanded_bindings.push(Expr::List(vec![Expr::Symbol(name), value]));
        }

        Ok((expanded_bindings, bound_locals))
    }

    fn expand_ellipsis_template_item(
        &self,
        item: &Expr,
        bindings: &PatternBindings,
        out: &mut Vec<Expr>,
    ) -> Result<(), EvalError> {
        let symbol = expect_symbol_expr(item)?;
        let Some(binding) = bindings.get(&symbol.name) else {
            return Err(EvalError::MacroExpansion {
                message: format!(
                    "ellipsis template item `{}` must be a repeated pattern variable",
                    symbol.name
                ),
            });
        };

        let MatchBinding::Repeat(values) = binding else {
            return Err(EvalError::MacroExpansion {
                message: format!(
                    "ellipsis template item `{}` must be bound to repeated syntax",
                    symbol.name
                ),
            });
        };

        out.extend(values.iter().cloned());
        Ok(())
    }

    fn fresh_ident(&mut self, name: &str) -> Ident {
        let ident = Ident::fresh(name, self.next_uid);
        self.next_uid += 1;
        ident
    }
}

impl Expr {
    fn as_list(&self) -> Option<&[Expr]> {
        match self {
            Expr::List(items) => Some(items),
            _ => None,
        }
    }
}

struct Parser<'a> {
    chars: Vec<char>,
    pos: usize,
    _source: &'a str,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().collect(),
            pos: 0,
            _source: source,
        }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        while self.skip_ws_and_comments() {
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        let Some(ch) = self.peek() else {
            return Err(EvalError::Parse {
                message: "unexpected end of input".to_string(),
            });
        };

        match ch {
            '(' => self.parse_list(),
            '\'' => {
                self.pos += 1;
                let expr = self.parse_expr()?;
                Ok(Expr::List(vec![Expr::symbol("quote"), expr]))
            }
            '"' => self.parse_string(),
            ')' => Err(EvalError::Parse {
                message: "unexpected `)`".to_string(),
            }),
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.expect('(')?;
        let mut items = Vec::new();

        loop {
            self.skip_ws_and_comments();
            match self.peek() {
                Some(')') => {
                    self.pos += 1;
                    return Ok(Expr::List(items));
                }
                Some('.')
                    if match self.peek_next() {
                        None => true,
                        Some(next) => is_token_delimiter(next),
                    } =>
                {
                    self.pos += 1;
                    if items.is_empty() {
                        return Err(EvalError::Parse {
                            message: "unexpected dotted list marker".to_string(),
                        });
                    }
                    self.skip_ws_and_comments();
                    let tail = self.parse_expr()?;
                    self.skip_ws_and_comments();
                    self.expect(')')?;
                    return Ok(Expr::DottedList(items, Box::new(tail)));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => {
                    return Err(EvalError::Parse {
                        message: "unterminated list".to_string(),
                    })
                }
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.expect('"')?;
        let mut out = String::new();

        loop {
            let Some(ch) = self.next() else {
                return Err(EvalError::Parse {
                    message: "unterminated string literal".to_string(),
                });
            };

            match ch {
                '"' => return Ok(Expr::String(out)),
                '\\' => {
                    let Some(escaped) = self.next() else {
                        return Err(EvalError::Parse {
                            message: "unterminated string escape".to_string(),
                        });
                    };
                    match escaped {
                        'n' => out.push('\n'),
                        't' => out.push('\t'),
                        '"' => out.push('"'),
                        '\\' => out.push('\\'),
                        other => out.push(other),
                    }
                }
                other => out.push(other),
            }
        }
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let token = self.read_token();
        if token.is_empty() {
            return Err(EvalError::Parse {
                message: "expected expression".to_string(),
            });
        }

        match token.as_str() {
            "#t" => Ok(Expr::Boolean(true)),
            "#f" => Ok(Expr::Boolean(false)),
            "." => Err(EvalError::Parse {
                message: "unexpected `.`".to_string(),
            }),
            _ if is_integer_token(&token) => {
                token
                    .parse::<i64>()
                    .map(Expr::Integer)
                    .map_err(|_| EvalError::Parse {
                        message: format!("invalid integer literal `{token}`"),
                    })
            }
            _ => Ok(Expr::Symbol(Ident::raw(token))),
        }
    }

    fn read_token(&mut self) -> String {
        let mut token = String::new();
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() || matches!(ch, '(' | ')' | '\'' | '"' | ';') {
                break;
            }
            token.push(ch);
            self.pos += 1;
        }
        token
    }

    fn skip_ws_and_comments(&mut self) -> bool {
        loop {
            while matches!(self.peek(), Some(ch) if ch.is_whitespace()) {
                self.pos += 1;
            }

            if self.peek() == Some(';') {
                while let Some(ch) = self.next() {
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }

            break;
        }

        self.peek().is_some()
    }

    fn expect(&mut self, expected: char) -> Result<(), EvalError> {
        let Some(found) = self.next() else {
            return Err(EvalError::Parse {
                message: format!("expected `{expected}`, found end of input"),
            });
        };

        if found == expected {
            Ok(())
        } else {
            Err(EvalError::Parse {
                message: format!("expected `{expected}`, found `{found}`"),
            })
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn next(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.pos += 1;
        Some(ch)
    }

    fn peek_next(&self) -> Option<char> {
        self.chars.get(self.pos + 1).copied()
    }
}

fn install_builtins(env: &Env) {
    let builtins = [
        ("+", Builtin::Add),
        ("-", Builtin::Sub),
        ("*", Builtin::Mul),
        ("/", Builtin::Div),
        ("<", Builtin::Less),
        (">", Builtin::Greater),
        ("=", Builtin::NumericEqual),
        ("<=", Builtin::LessEqual),
        ("not", Builtin::Not),
        ("cons", Builtin::Cons),
        ("car", Builtin::Car),
        ("cdr", Builtin::Cdr),
        ("null?", Builtin::NullPred),
        ("list", Builtin::List),
        ("length", Builtin::Length),
        ("string?", Builtin::StringPred),
        ("number?", Builtin::NumberPred),
        ("boolean?", Builtin::BooleanPred),
        ("pair?", Builtin::PairPred),
        ("symbol?", Builtin::SymbolPred),
        ("apply", Builtin::Apply),
        ("call/cc", Builtin::CallCc),
    ];

    for (name, builtin) in builtins {
        env.bind_value_here(
            &Ident::raw(name),
            Value::Procedure(Rc::new(Procedure::Builtin(builtin))),
        );
    }
}

fn parse_define(args: &[Expr]) -> Result<(Ident, Expr), EvalError> {
    if args.is_empty() {
        return Err(EvalError::InvalidForm {
            message: "define requires a target".to_string(),
        });
    }

    if let Expr::Symbol(name) = &args[0] {
        if args.len() != 2 {
            return Err(EvalError::ArityError {
                expected: "2".to_string(),
                got: args.len(),
            });
        }
        return Ok((name.clone(), args[1].clone()));
    }

    if args.len() < 2 {
        return Err(EvalError::InvalidForm {
            message: "function define requires a body".to_string(),
        });
    }

    let (name, formals) = parse_function_signature(&args[0])?;
    let mut lambda = vec![Expr::symbol("lambda"), formals];
    lambda.extend(args[1..].iter().cloned());
    Ok((name, Expr::List(lambda)))
}

fn parse_function_signature(target: &Expr) -> Result<(Ident, Expr), EvalError> {
    match target {
        Expr::List(items) => {
            if items.is_empty() {
                return Err(EvalError::InvalidForm {
                    message: "function signature cannot be empty".to_string(),
                });
            }
            let name = expect_symbol_expr(&items[0])?.clone();
            Ok((name, Expr::List(items[1..].to_vec())))
        }
        Expr::DottedList(prefix, tail) => {
            if prefix.is_empty() {
                return Err(EvalError::InvalidForm {
                    message: "function signature cannot be empty".to_string(),
                });
            }
            let name = expect_symbol_expr(&prefix[0])?.clone();
            let formals = if prefix.len() == 1 {
                tail.as_ref().clone()
            } else {
                Expr::DottedList(prefix[1..].to_vec(), tail.clone())
            };
            Ok((name, formals))
        }
        _ => Err(EvalError::InvalidForm {
            message: "invalid define target".to_string(),
        }),
    }
}

fn parse_formals(expr: &Expr) -> Result<Formals, EvalError> {
    let formals = match expr {
        Expr::Symbol(rest) => Formals {
            required: Vec::new(),
            rest: Some(rest.as_binding_ident()),
        },
        Expr::List(items) => Formals {
            required: items
                .iter()
                .map(expect_symbol_expr)
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .map(Ident::as_binding_ident)
                .collect(),
            rest: None,
        },
        Expr::DottedList(items, tail) => Formals {
            required: items
                .iter()
                .map(expect_symbol_expr)
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .map(Ident::as_binding_ident)
                .collect(),
            rest: Some(expect_symbol_expr(tail)?.as_binding_ident()),
        },
        _ => {
            return Err(EvalError::InvalidForm {
                message: "lambda formals must be identifiers".to_string(),
            })
        }
    };

    validate_unique_bindings(&formals.required)?;
    if let Some(rest) = &formals.rest {
        if formals.required.iter().any(|ident| ident.name == rest.name) {
            return Err(EvalError::InvalidForm {
                message: format!("duplicate binding `{}`", rest.name),
            });
        }
    }

    Ok(formals)
}

fn parse_let_bindings(expr: &Expr) -> Result<Vec<(Ident, Expr)>, EvalError> {
    let bindings = expect_list_expr(expr)?;
    let mut parsed = Vec::with_capacity(bindings.len());

    for binding in bindings {
        let items = expect_list_expr(binding)?;
        if items.len() != 2 {
            return Err(EvalError::InvalidForm {
                message: "let bindings must have two elements".to_string(),
            });
        }

        parsed.push((expect_symbol_expr(&items[0])?.clone(), items[1].clone()));
    }

    Ok(parsed)
}

fn parse_cond_clauses(args: &[Expr]) -> Result<Vec<CondClause>, EvalError> {
    let mut clauses = Vec::with_capacity(args.len());
    for clause in args {
        let items = expect_list_expr(clause)?;
        if items.is_empty() {
            return Err(EvalError::InvalidForm {
                message: "cond clauses cannot be empty".to_string(),
            });
        }

        let is_else = items[0].symbol_name() == Some("else");
        clauses.push(CondClause {
            test: items[0].clone(),
            body: items[1..].to_vec(),
            is_else,
        });
    }
    Ok(clauses)
}

fn parse_syntax_rules(name: &Ident, expr: &Expr, env: Env) -> Result<MacroTransformer, EvalError> {
    let items = expect_list_expr(expr)?;
    if items.is_empty() || items[0].symbol_name() != Some("syntax-rules") {
        return Err(EvalError::InvalidForm {
            message: "define-syntax only supports syntax-rules".to_string(),
        });
    }

    if items.len() < 3 {
        return Err(EvalError::InvalidForm {
            message: "syntax-rules requires literals and at least one rule".to_string(),
        });
    }

    let literals = expect_list_expr(&items[1])?
        .iter()
        .map(expect_symbol_expr)
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .map(|ident| ident.name.clone())
        .collect::<HashSet<_>>();

    let mut rules = Vec::with_capacity(items.len() - 2);
    for rule in &items[2..] {
        let rule_items = expect_list_expr(rule)?;
        if rule_items.len() != 2 {
            return Err(EvalError::InvalidForm {
                message: "syntax-rules clauses must have a pattern and template".to_string(),
            });
        }
        rules.push(MacroRule {
            pattern: rule_items[0].clone(),
            template: rule_items[1].clone(),
        });
    }

    Ok(MacroTransformer {
        name: name.name.clone(),
        literals,
        rules,
        env,
    })
}

fn bind_formals(formals: &Formals, args: Vec<Value>, env: &Env) -> Result<(), EvalError> {
    let min_arity = formals.required.len();
    if args.len() < min_arity {
        return Err(EvalError::ArityError {
            expected: format!("at least {min_arity}"),
            got: args.len(),
        });
    }

    if formals.rest.is_none() && args.len() != min_arity {
        return Err(EvalError::ArityError {
            expected: min_arity.to_string(),
            got: args.len(),
        });
    }

    for (ident, value) in formals.required.iter().zip(args.iter()) {
        env.bind_value_here(ident, value.clone());
    }

    if let Some(rest) = &formals.rest {
        env.bind_value_here(rest, list_from_values(args[min_arity..].to_vec()));
    }

    Ok(())
}

fn validate_unique_bindings(bindings: &[Ident]) -> Result<(), EvalError> {
    let mut seen = HashSet::new();
    for binding in bindings {
        if !seen.insert(binding.binding_key()) {
            return Err(EvalError::InvalidForm {
                message: format!("duplicate binding `{}`", binding.name),
            });
        }
    }
    Ok(())
}

fn ensure_exact_arity(args: &[Value], expected: usize) -> Result<(), EvalError> {
    if args.len() == expected {
        Ok(())
    } else {
        Err(EvalError::ArityError {
            expected: expected.to_string(),
            got: args.len(),
        })
    }
}

fn ensure_min_arity(args: &[Value], min: usize) -> Result<(), EvalError> {
    if args.len() >= min {
        Ok(())
    } else {
        Err(EvalError::ArityError {
            expected: format!("at least {min}"),
            got: args.len(),
        })
    }
}

fn expect_symbol_expr(expr: &Expr) -> Result<&Ident, EvalError> {
    expr.as_symbol().ok_or_else(|| EvalError::InvalidForm {
        message: "expected identifier".to_string(),
    })
}

fn expect_list_expr(expr: &Expr) -> Result<&[Expr], EvalError> {
    expr.as_list().ok_or_else(|| EvalError::InvalidForm {
        message: "expected proper list".to_string(),
    })
}

fn expect_number(value: &Value) -> Result<i64, EvalError> {
    if let Value::Integer(number) = value {
        Ok(*number)
    } else {
        Err(EvalError::TypeError {
            expected: "number".to_string(),
            found: value.type_name().to_string(),
        })
    }
}

fn expect_pair(value: &Value) -> Result<&Pair, EvalError> {
    if let Value::Pair(pair) = value {
        Ok(pair)
    } else {
        Err(EvalError::TypeError {
            expected: "pair".to_string(),
            found: value.type_name().to_string(),
        })
    }
}

fn fold_numbers(
    args: &[Value],
    init: i64,
    op: fn(i64, i64) -> Result<i64, EvalError>,
) -> Result<i64, EvalError> {
    let mut acc = init;
    for arg in args {
        acc = op(acc, expect_number(arg)?)?;
    }
    Ok(acc)
}

fn apply_sub(args: &[Value]) -> Result<i64, EvalError> {
    ensure_min_arity(args, 1)?;
    let first = expect_number(&args[0])?;
    if args.len() == 1 {
        checked_sub(0, first)
    } else {
        let mut acc = first;
        for arg in &args[1..] {
            acc = checked_sub(acc, expect_number(arg)?)?;
        }
        Ok(acc)
    }
}

fn apply_div(args: &[Value]) -> Result<i64, EvalError> {
    ensure_min_arity(args, 2)?;
    let mut acc = expect_number(&args[0])?;
    for arg in &args[1..] {
        let divisor = expect_number(arg)?;
        if divisor == 0 {
            return Err(EvalError::DivisionByZero);
        }
        acc /= divisor;
    }
    Ok(acc)
}

fn compare_numbers(args: &[Value], cmp: fn(i64, i64) -> bool) -> Result<bool, EvalError> {
    if args.len() < 2 {
        return Ok(true);
    }

    let numbers = args
        .iter()
        .map(expect_number)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(numbers.windows(2).all(|pair| cmp(pair[0], pair[1])))
}

fn checked_add(left: i64, right: i64) -> Result<i64, EvalError> {
    left.checked_add(right).ok_or(EvalError::ArithmeticOverflow)
}

fn checked_sub(left: i64, right: i64) -> Result<i64, EvalError> {
    left.checked_sub(right).ok_or(EvalError::ArithmeticOverflow)
}

fn checked_mul(left: i64, right: i64) -> Result<i64, EvalError> {
    left.checked_mul(right).ok_or(EvalError::ArithmeticOverflow)
}

fn cons(car: Value, cdr: Value) -> Value {
    Value::Pair(Rc::new(Pair { car, cdr }))
}

fn list_from_values(values: Vec<Value>) -> Value {
    values
        .into_iter()
        .rev()
        .fold(Value::Nil, |cdr, car| cons(car, cdr))
}

fn proper_list_to_vec(value: &Value) -> Result<Vec<Value>, EvalError> {
    let mut items = Vec::new();
    let mut current = value.clone();
    loop {
        match current {
            Value::Nil => return Ok(items),
            Value::Pair(pair) => {
                items.push(pair.car.clone());
                current = pair.cdr.clone();
            }
            other => {
                return Err(EvalError::TypeError {
                    expected: "proper list".to_string(),
                    found: other.type_name().to_string(),
                })
            }
        }
    }
}

fn quote_to_value(expr: &Expr) -> Value {
    match expr {
        Expr::Integer(value) => Value::Integer(*value),
        Expr::Boolean(value) => Value::Boolean(*value),
        Expr::String(value) => Value::String(value.clone()),
        Expr::Symbol(symbol) => Value::Symbol(symbol.name.clone()),
        Expr::List(items) => {
            let values = items.iter().map(quote_to_value).collect::<Vec<_>>();
            list_from_values(values)
        }
        Expr::DottedList(items, tail) => {
            items.iter().rev().fold(quote_to_value(tail), |cdr, car| {
                cons(quote_to_value(car), cdr)
            })
        }
    }
}

fn format_value(value: &Value) -> String {
    match value {
        Value::Integer(number) => number.to_string(),
        Value::Boolean(true) => "#t".to_string(),
        Value::Boolean(false) => "#f".to_string(),
        Value::String(string) => format!("\"{}\"", escape_string(string)),
        Value::Symbol(symbol) => symbol.clone(),
        Value::Nil => "()".to_string(),
        Value::Pair(pair) => {
            let mut out = String::from("(");
            format_pair(pair, &mut out);
            out.push(')');
            out
        }
        Value::Procedure(_) => "#<procedure>".to_string(),
        Value::Void => String::new(),
    }
}

fn format_pair(pair: &Pair, out: &mut String) {
    out.push_str(&format_value(&pair.car));
    match &pair.cdr {
        Value::Nil => {}
        Value::Pair(next) => {
            out.push(' ');
            format_pair(next, out);
        }
        other => {
            out.push_str(" . ");
            out.push_str(&format_value(other));
        }
    }
}

fn escape_string(value: &str) -> String {
    let mut escaped = String::new();
    for ch in value.chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            '\t' => escaped.push_str("\\t"),
            other => {
                let _ = escaped.write_char(other);
            }
        }
    }
    escaped
}

fn syntax_eq(left: &Expr, right: &Expr) -> bool {
    match (left, right) {
        (Expr::Integer(left), Expr::Integer(right)) => left == right,
        (Expr::Boolean(left), Expr::Boolean(right)) => left == right,
        (Expr::String(left), Expr::String(right)) => left == right,
        (Expr::Symbol(left), Expr::Symbol(right)) => left.name == right.name,
        (Expr::List(left), Expr::List(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right)
                    .all(|(left, right)| syntax_eq(left, right))
        }
        (Expr::DottedList(left_items, left_tail), Expr::DottedList(right_items, right_tail)) => {
            left_items.len() == right_items.len()
                && left_items
                    .iter()
                    .zip(right_items)
                    .all(|(left, right)| syntax_eq(left, right))
                && syntax_eq(left_tail, right_tail)
        }
        _ => false,
    }
}

fn merge_local_bindings(base: &LocalBindings, additions: &LocalBindings) -> LocalBindings {
    let mut merged = base.clone();
    merged.extend(additions.clone());
    merged
}

fn leading_define_name(target: &Expr) -> Option<Ident> {
    match target {
        Expr::List(items) => items.first().and_then(Expr::as_symbol).cloned(),
        Expr::DottedList(items, _) => items.first().and_then(Expr::as_symbol).cloned(),
        _ => None,
    }
}

fn procedure_return_cont(cont: &ContRef) -> ContRef {
    match cont.as_ref() {
        Cont::Done => cont.clone(),
        Cont::ProcedureBoundary { next } => next.clone(),
        Cont::CallOperator { next, .. }
        | Cont::CallOperands { next, .. }
        | Cont::If { next, .. }
        | Cont::Sequence { next, .. }
        | Cont::Define { next, .. }
        | Cont::Set { next, .. }
        | Cont::And { next, .. }
        | Cont::Or { next, .. }
        | Cont::Let { next, .. }
        | Cont::NamedLet { next, .. }
        | Cont::Cond { next, .. } => procedure_return_cont(next),
        Cont::CallCcReturn { current } => procedure_return_cont(current),
    }
}

fn is_core_keyword(name: &str) -> bool {
    matches!(
        name,
        "quote"
            | "if"
            | "define"
            | "lambda"
            | "begin"
            | "and"
            | "or"
            | "let"
            | "cond"
            | "set!"
            | "define-syntax"
            | "syntax-rules"
    )
}

fn is_integer_token(token: &str) -> bool {
    if token.is_empty() || token == "-" {
        return false;
    }

    let digits = token.strip_prefix('-').unwrap_or(token);
    !digits.is_empty() && digits.chars().all(|ch| ch.is_ascii_digit())
}

fn is_token_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | '\'' | '"' | ';')
}
