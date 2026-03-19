#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
    Void,
}

impl Expr {
    pub fn to_display(&self) -> String {
        match self {
            Expr::Integer(n) => n.to_string(),
            Expr::Boolean(true) => "#t".into(),
            Expr::Boolean(false) => "#f".into(),
            Expr::Str(s) => format!("\"{s}\""),
            Expr::Symbol(s) => s.clone(),
            Expr::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|e| e.to_display()).collect();
                format!("({})", inner.join(" "))
            }
            Expr::Void => "".into(),
        }
    }
}
