use super::error::Position;

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub pos: Position,
}

impl Expr {
    pub fn new(kind: ExprKind, pos: Position) -> Self {
        Self { kind, pos }
    }

    pub fn symbol(name: impl Into<String>, pos: Position) -> Self {
        Self::new(ExprKind::Symbol(name.into()), pos)
    }

    pub fn list(elements: Vec<Self>, pos: Position) -> Self {
        Self::new(ExprKind::List(elements), pos)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Int(i64),
    Bool(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}
