#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}
