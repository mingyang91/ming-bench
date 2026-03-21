use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceLocation {
    pub line: usize,
    pub column: usize,
}

impl SourceLocation {
    pub const fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

impl Display for SourceLocation {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}:{}", self.line, self.column)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Integer {
        value: i64,
        location: SourceLocation,
    },
    Boolean {
        value: bool,
        location: SourceLocation,
    },
    String {
        value: String,
        location: SourceLocation,
    },
    Character {
        value: char,
        location: SourceLocation,
    },
    Symbol {
        name: String,
        location: SourceLocation,
    },
    List {
        items: Vec<Expr>,
        location: SourceLocation,
    },
}

impl Expr {
    pub fn integer(value: i64, location: SourceLocation) -> Self {
        Self::Integer { value, location }
    }

    pub fn boolean(value: bool, location: SourceLocation) -> Self {
        Self::Boolean { value, location }
    }

    pub fn string(value: String, location: SourceLocation) -> Self {
        Self::String { value, location }
    }

    pub fn character(value: char, location: SourceLocation) -> Self {
        Self::Character { value, location }
    }

    pub fn symbol(name: String, location: SourceLocation) -> Self {
        Self::Symbol { name, location }
    }

    pub fn list(items: Vec<Expr>, location: SourceLocation) -> Self {
        Self::List { items, location }
    }

    pub fn location(&self) -> SourceLocation {
        match self {
            Self::Integer { location, .. }
            | Self::Boolean { location, .. }
            | Self::String { location, .. }
            | Self::Character { location, .. }
            | Self::Symbol { location, .. }
            | Self::List { location, .. } => *location,
        }
    }

    pub fn symbol_name(&self) -> Option<&str> {
        match self {
            Self::Symbol { name, .. } => Some(name),
            _ => None,
        }
    }
}
