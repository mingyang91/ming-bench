use std::rc::Rc;

pub type ExprRef = Rc<Expr>;

#[derive(Clone, Debug)]
pub enum Expr {
    Number(i64),
    Bool(bool),
    String(String),
    Symbol(Identifier),
    List(Vec<ExprRef>),
    DottedList(Vec<ExprRef>, ExprRef),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum BindingKey {
    Plain(String),
    Gensym(u64),
}

#[derive(Clone, Debug)]
pub enum Identifier {
    Plain(String),
    Gensym { name: String, id: u64 },
    CapturedValue { name: String, id: u64 },
    CapturedMacro { name: String, id: u64 },
}

pub fn expr_ref(expr: Expr) -> ExprRef {
    Rc::new(expr)
}

pub fn list_expr(items: Vec<ExprRef>) -> ExprRef {
    expr_ref(Expr::List(items))
}

pub fn symbol_expr(name: &str) -> ExprRef {
    expr_ref(Expr::Symbol(Identifier::Plain(name.to_owned())))
}

impl Expr {
    pub fn list_items(&self) -> Option<&[ExprRef]> {
        match self {
            Self::List(items) => Some(items),
            _ => None,
        }
    }

    pub fn symbol_name(&self) -> Option<&str> {
        match self {
            Self::Symbol(identifier) => Some(identifier.name()),
            _ => None,
        }
    }

    pub fn is_symbol_named(&self, name: &str) -> bool {
        self.symbol_name().is_some_and(|symbol| symbol == name)
    }
}

impl Identifier {
    pub fn name(&self) -> &str {
        match self {
            Self::Plain(name)
            | Self::Gensym { name, .. }
            | Self::CapturedValue { name, .. }
            | Self::CapturedMacro { name, .. } => name,
        }
    }

    pub fn binding_key(&self) -> Option<BindingKey> {
        match self {
            Self::Plain(name) => Some(BindingKey::Plain(name.clone())),
            Self::Gensym { id, .. } => Some(BindingKey::Gensym(*id)),
            Self::CapturedValue { .. } | Self::CapturedMacro { .. } => None,
        }
    }

    pub fn captured_value_id(&self) -> Option<u64> {
        match self {
            Self::CapturedValue { id, .. } => Some(*id),
            _ => None,
        }
    }

    pub fn captured_macro_id(&self) -> Option<u64> {
        match self {
            Self::CapturedMacro { id, .. } => Some(*id),
            _ => None,
        }
    }
}
