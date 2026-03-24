use super::position::SourcePos;

#[derive(Clone)]
pub(crate) enum Expr {
    Integer(i64, SourcePos),
    Boolean(bool, SourcePos),
    String(String, SourcePos),
    Character(char, SourcePos),
    Symbol(String, SourcePos),
    List(Vec<Expr>, SourcePos),
}

impl Expr {
    pub(crate) fn pos(&self) -> SourcePos {
        match self {
            Self::Integer(_, pos)
            | Self::Boolean(_, pos)
            | Self::String(_, pos)
            | Self::Character(_, pos)
            | Self::Symbol(_, pos)
            | Self::List(_, pos) => *pos,
        }
    }
}
