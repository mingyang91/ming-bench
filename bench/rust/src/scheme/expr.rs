use crate::scheme::source::SourcePos;
use crate::scheme::value::Value;

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Value, SourcePos),
    Symbol(String, SourcePos),
    List(Vec<Expr>, SourcePos),
}

impl Expr {
    pub fn position(&self) -> SourcePos {
        match self {
            Self::Literal(_, position) | Self::Symbol(_, position) | Self::List(_, position) => {
                *position
            }
        }
    }

    pub fn quote_symbol(position: SourcePos) -> Self {
        Self::Symbol("quote".to_string(), position)
    }
}
