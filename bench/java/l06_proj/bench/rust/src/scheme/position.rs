use std::fmt;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct SourcePos {
    pub(crate) line: usize,
    pub(crate) column: usize,
}

impl SourcePos {
    pub(crate) const fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

impl fmt::Display for SourcePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}
