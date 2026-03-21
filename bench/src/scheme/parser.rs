use crate::scheme::error::ParseError;
use crate::scheme::value::{Span, Value};

struct Parser<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    line: usize,
    col: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars().peekable(),
            line: 1,
            col: 1,
        }
    }

    fn span(&self) -> Span {
        Span {
            line: self.line,
            col: self.col,
        }
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        if c == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(c)
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn skip_whitespace(&mut self) {
        while self.peek().is_some_and(|c| c.is_whitespace()) {
            self.advance();
        }
    }

    fn parse_expr(&mut self) -> Result<Value, ParseError> {
        self.skip_whitespace();
        let span = self.span();

        match self.peek() {
            None => Err(ParseError::UnexpectedEof),
            Some('\'') => self.parse_quote_shorthand(span),
            Some('(') => self.parse_list(span),
            Some('"') => self.parse_string(span),
            Some('#') => self.parse_hash_literal(span),
            Some(&c) if is_symbol_start(c) => self.parse_symbol(span),
            Some(&c) if c == '-' || c.is_ascii_digit() => self.parse_number_or_symbol(span),
            Some(&c) => Err(ParseError::UnexpectedChar { ch: c }),
        }
    }

    fn parse_string(&mut self, _span: Span) -> Result<Value, ParseError> {
        self.advance(); // consume opening quote
        let mut s = String::new();

        loop {
            match self.advance() {
                None => return Err(ParseError::UnterminatedString),
                Some('"') => return Ok(Value::String(s)),
                Some('\\') => match self.advance() {
                    Some('n') => s.push('\n'),
                    Some('t') => s.push('\t'),
                    Some('\\') => s.push('\\'),
                    Some('"') => s.push('"'),
                    Some(c) => s.extend(['\\', c]),
                    None => return Err(ParseError::UnterminatedString),
                },
                Some(c) => s.push(c),
            }
        }
    }

    fn parse_hash_literal(&mut self, _span: Span) -> Result<Value, ParseError> {
        self.advance(); // consume '#'
        match self.peek() {
            Some('\\') => self.parse_char_literal(),
            _ => match self.advance() {
                Some('t') => Ok(Value::Boolean(true)),
                Some('f') => Ok(Value::Boolean(false)),
                Some(c) => Err(ParseError::UnexpectedChar { ch: c }),
                None => Err(ParseError::UnexpectedEof),
            },
        }
    }

    fn parse_char_literal(&mut self) -> Result<Value, ParseError> {
        self.advance(); // consume '\\'
        let ch = self.advance().ok_or(ParseError::UnexpectedEof)?;
        if !ch.is_ascii_alphabetic() {
            return Ok(Value::Char(ch));
        }
        let name = self.read_char_name(ch);
        if name.len() == 1 {
            return Ok(Value::Char(ch));
        }
        match name.as_str() {
            "space" => Ok(Value::Char(' ')),
            "newline" => Ok(Value::Char('\n')),
            "tab" => Ok(Value::Char('\t')),
            _ => Err(ParseError::UnexpectedChar { ch }),
        }
    }

    fn read_char_name(&mut self, first: char) -> String {
        let mut name = String::from(first);
        while self.peek().is_some_and(|c| c.is_ascii_alphabetic()) {
            name.push(self.advance().expect("peeked"));
        }
        name
    }

    fn parse_symbol(&mut self, span: Span) -> Result<Value, ParseError> {
        let token = self.read_token();
        Ok(Value::Symbol(token, span))
    }

    fn parse_quote_shorthand(&mut self, span: Span) -> Result<Value, ParseError> {
        self.advance(); // consume '\''
        let inner = self.parse_expr()?;
        Ok(Value::List(
            vec![Value::Symbol("quote".to_string(), span), inner],
            span,
        ))
    }

    fn parse_list(&mut self, span: Span) -> Result<Value, ParseError> {
        self.advance(); // consume '('
        let mut items = Vec::new();

        loop {
            self.skip_whitespace();
            match self.peek() {
                None => return Err(ParseError::UnexpectedEof),
                Some(')') => break,
                _ => items.push(self.parse_expr()?),
            }
        }
        self.advance(); // consume ')'
        Ok(Value::List(items, span))
    }

    fn read_token(&mut self) -> String {
        let mut token = String::new();
        while self.peek().is_some_and(|c| !is_delimiter(*c)) {
            token.push(self.advance().expect("peeked"));
        }
        token
    }

    fn parse_number_or_symbol(&mut self, span: Span) -> Result<Value, ParseError> {
        let token = self.read_token();
        match token.parse::<i64>() {
            Ok(n) => Ok(Value::Integer(n)),
            Err(_) => Ok(Value::Symbol(token, span)),
        }
    }
}

fn is_delimiter(c: char) -> bool {
    c.is_whitespace() || c == '(' || c == ')'
}

fn is_symbol_start(c: char) -> bool {
    matches!(c, '+' | '*' | '/' | '<' | '>' | '=' | '!' | '?' | '_' | '.')
        || c.is_ascii_alphabetic()
}

/// Parse all expressions from the input string.
pub fn parse(input: &str) -> Result<Vec<Value>, ParseError> {
    let mut parser = Parser::new(input);
    let mut exprs = Vec::new();

    loop {
        parser.skip_whitespace();
        if parser.peek().is_none() {
            break;
        }
        exprs.push(parser.parse_expr()?);
    }

    Ok(exprs)
}
