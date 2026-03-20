use crate::scheme::environment::Environment;
use crate::scheme::error::SchemeError;

#[derive(Debug, Clone)]
pub(crate) enum Expr {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    ScopedSymbol {
        name: String,
        environment: Environment,
    },
    List(Vec<Expr>),
}

impl Expr {
    pub(crate) fn symbol(name: impl Into<String>) -> Self {
        Self::Symbol(name.into())
    }

    pub(crate) fn scoped_symbol(name: impl Into<String>, environment: Environment) -> Self {
        Self::ScopedSymbol {
            name: name.into(),
            environment,
        }
    }

    pub(crate) fn symbol_name(&self) -> Option<&str> {
        match self {
            Self::Symbol(name) | Self::ScopedSymbol { name, .. } => Some(name),
            Self::Integer(_) | Self::Boolean(_) | Self::String(_) | Self::List(_) => None,
        }
    }

    pub(crate) fn symbol_environment(&self) -> Option<&Environment> {
        match self {
            Self::ScopedSymbol { environment, .. } => Some(environment),
            Self::Integer(_)
            | Self::Boolean(_)
            | Self::String(_)
            | Self::Symbol(_)
            | Self::List(_) => None,
        }
    }

    pub(crate) fn is_symbol_named(&self, expected: &str) -> bool {
        self.symbol_name() == Some(expected)
    }
}

pub(crate) fn parse_program(input: &str) -> Result<Vec<Expr>, SchemeError> {
    let mut parser = Parser::new(input);
    let expressions = parser.parse_program()?;
    if expressions.is_empty() {
        Err(SchemeError::EmptyInput)
    } else {
        Ok(expressions)
    }
}

struct Parser<'a> {
    input: &'a str,
    index: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, index: 0 }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, SchemeError> {
        let mut expressions = Vec::new();
        self.skip_ignored();

        while !self.is_eof() {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }

        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, SchemeError> {
        self.skip_ignored();

        match self.peek_char() {
            Some('(') => self.parse_list(),
            Some('\'') => self.parse_quote(),
            Some('"') => self.parse_string(),
            Some('#') => self.parse_boolean(),
            Some(')') => Err(SchemeError::UnexpectedCloseParen { index: self.index }),
            Some(_) if self.starts_integer() => self.parse_integer(),
            Some(_) => self.parse_symbol(),
            None => Err(SchemeError::UnexpectedEndOfInput {
                context: "expression",
            }),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, SchemeError> {
        let _ = self.advance_char();
        let mut expressions = Vec::new();
        self.skip_ignored();

        while self.peek_char().is_some_and(|ch| ch != ')') {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if self.peek_char() == Some(')') {
            let _ = self.advance_char();
            Ok(Expr::List(expressions))
        } else {
            Err(SchemeError::UnexpectedEndOfInput { context: "list" })
        }
    }

    fn parse_quote(&mut self) -> Result<Expr, SchemeError> {
        let _ = self.advance_char();
        let quoted = self.parse_expr()?;
        Ok(Expr::List(vec![Expr::symbol("quote"), quoted]))
    }

    fn parse_boolean(&mut self) -> Result<Expr, SchemeError> {
        let start = self.index;
        let token = self.read_token();

        match token {
            "#t" => Ok(Expr::Boolean(true)),
            "#f" => Ok(Expr::Boolean(false)),
            _ => Err(SchemeError::InvalidBooleanLiteral {
                literal: token.to_owned(),
                index: start,
            }),
        }
    }

    fn parse_integer(&mut self) -> Result<Expr, SchemeError> {
        let start = self.index;
        let token = self.read_token();
        token
            .parse::<i64>()
            .map(Expr::Integer)
            .map_err(|_| SchemeError::InvalidInteger {
                literal: token.to_owned(),
                index: start,
            })
    }

    fn parse_symbol(&mut self) -> Result<Expr, SchemeError> {
        let start = self.index;
        let token = self.read_token();

        if token.is_empty() {
            match self.peek_char() {
                Some(ch) => Err(SchemeError::InvalidTokenStart { ch, index: start }),
                None => Err(SchemeError::UnexpectedEndOfInput { context: "symbol" }),
            }
        } else {
            Ok(Expr::symbol(token))
        }
    }

    fn parse_string(&mut self) -> Result<Expr, SchemeError> {
        let start = self.index;
        let mut value = String::new();
        let _ = self.advance_char();

        while let Some(ch) = self.advance_char() {
            match ch {
                '"' => return Ok(Expr::String(value)),
                '\\' => value.push(self.parse_escape_sequence(start)?),
                _ => value.push(ch),
            }
        }

        Err(SchemeError::UnterminatedString { start })
    }

    fn parse_escape_sequence(&mut self, string_start: usize) -> Result<char, SchemeError> {
        let index = self.index;
        match self.advance_char() {
            Some('"') => Ok('"'),
            Some('\\') => Ok('\\'),
            Some('n') => Ok('\n'),
            Some('r') => Ok('\r'),
            Some('t') => Ok('\t'),
            Some(escape) => Err(SchemeError::InvalidEscape { escape, index }),
            None => Err(SchemeError::UnterminatedString {
                start: string_start,
            }),
        }
    }

    fn read_token(&mut self) -> &'a str {
        let start = self.index;

        while self.peek_char().is_some_and(|ch| !is_token_delimiter(ch)) {
            let _ = self.advance_char();
        }

        &self.input[start..self.index]
    }

    fn skip_ignored(&mut self) {
        self.skip_whitespace();

        while self.peek_char() == Some(';') {
            self.skip_comment();
            self.skip_whitespace();
        }
    }

    fn skip_comment(&mut self) {
        while self.advance_char().is_some_and(|ch| ch != '\n') {}
    }

    fn skip_whitespace(&mut self) {
        while self.peek_char().is_some_and(|ch| ch.is_whitespace()) {
            let _ = self.advance_char();
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.index..].chars().next()
    }

    fn advance_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.index += ch.len_utf8();
        Some(ch)
    }

    fn is_eof(&self) -> bool {
        self.index >= self.input.len()
    }

    fn starts_integer(&self) -> bool {
        match self.peek_char() {
            Some(ch) if ch.is_ascii_digit() => true,
            Some('+') | Some('-') => self.peek_next_char().is_some_and(|ch| ch.is_ascii_digit()),
            _ => false,
        }
    }

    fn peek_next_char(&self) -> Option<char> {
        self.input[self.index..].chars().nth(1)
    }
}

fn is_token_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | '"' | '\'' | ';')
}
