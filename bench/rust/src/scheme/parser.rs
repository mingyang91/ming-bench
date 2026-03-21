use crate::scheme::ast::{Expr, SourceLocation};
use crate::scheme::error::ParseError;

pub fn parse_program(input: &str) -> Result<Vec<Expr>, ParseError> {
    let mut parser = Parser::new(input);
    let mut expressions = Vec::new();

    while parser.skip_ignored() {
        expressions.push(parser.parse_expr()?);
    }

    Ok(expressions)
}

struct Parser<'src> {
    input: &'src str,
    index: usize,
    location: SourceLocation,
}

impl<'src> Parser<'src> {
    fn new(input: &'src str) -> Self {
        Self {
            input,
            index: 0,
            location: SourceLocation::new(1, 1),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.skip_ignored();
        let location = self.location;

        let Some(ch) = self.peek_char() else {
            return Err(ParseError::UnexpectedEndOfInput { location });
        };

        match ch {
            '(' => self.parse_list(location),
            ')' => Err(ParseError::UnexpectedClosingParenthesis { location }),
            '\'' => self.parse_quote(location),
            '"' => self.parse_string(location),
            _ => Ok(self.parse_atom(location)),
        }
    }

    fn parse_list(&mut self, location: SourceLocation) -> Result<Expr, ParseError> {
        self.consume_char();
        let mut items = Vec::new();

        while let Some(expression) = self.parse_list_expression()? {
            items.push(expression);
        }

        Ok(Expr::list(items, location))
    }

    fn parse_string(&mut self, location: SourceLocation) -> Result<Expr, ParseError> {
        self.consume_char();
        let mut value = String::new();

        loop {
            match self.consume_string_char()? {
                '"' => return Ok(Expr::string(value, location)),
                '\\' => value.push(self.parse_escape_sequence()?),
                ch => value.push(ch),
            }
        }
    }

    fn parse_quote(&mut self, location: SourceLocation) -> Result<Expr, ParseError> {
        self.consume_char();

        Ok(Expr::list(
            vec![Expr::symbol("quote".into(), location), self.parse_expr()?],
            location,
        ))
    }

    fn parse_escape_sequence(&mut self) -> Result<char, ParseError> {
        let location = self.location;
        let Some(ch) = self.consume_char() else {
            return Err(ParseError::UnterminatedStringLiteral { location });
        };

        match ch {
            '\\' => Ok('\\'),
            '"' => Ok('"'),
            'n' => Ok('\n'),
            'r' => Ok('\r'),
            't' => Ok('\t'),
            _ => Err(ParseError::InvalidEscapeSequence {
                location,
                escape: ch,
            }),
        }
    }

    fn parse_atom(&mut self, location: SourceLocation) -> Expr {
        let token = self.take_while(|ch| !ch.is_whitespace() && ch != '(' && ch != ')');

        match token {
            "#t" => Expr::boolean(true, location),
            "#f" => Expr::boolean(false, location),
            _ => match token.parse::<i64>() {
                Ok(value) => Expr::integer(value, location),
                Err(_) => Expr::symbol(token.to_string(), location),
            },
        }
    }

    fn skip_ignored(&mut self) -> bool {
        while self.skip_comment() {}
        self.peek_char().is_some()
    }

    fn skip_whitespace(&mut self) {
        self.take_while(char::is_whitespace);
    }

    fn skip_comment(&mut self) -> bool {
        self.skip_whitespace();
        if self.peek_char() != Some(';') {
            return false;
        }

        self.consume_until_newline();
        true
    }

    fn consume_until_newline(&mut self) {
        self.take_while(|ch| ch != '\n');
    }

    fn take_while(&mut self, predicate: impl Fn(char) -> bool) -> &'src str {
        let start = self.index;

        while self.peek_char().is_some_and(&predicate) {
            self.consume_char();
        }

        &self.input[start..self.index]
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.index..].chars().next()
    }

    fn consume_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.index += ch.len_utf8();
        self.location = next_location(self.location, ch);
        Some(ch)
    }

    fn consume_string_char(&mut self) -> Result<char, ParseError> {
        self.consume_char()
            .ok_or(ParseError::UnterminatedStringLiteral {
                location: self.location,
            })
    }

    fn parse_list_expression(&mut self) -> Result<Option<Expr>, ParseError> {
        self.skip_ignored();
        if self.try_consume_char(')') {
            return Ok(None);
        }

        if self.peek_char().is_none() {
            return Err(ParseError::UnexpectedEndOfInput {
                location: self.location,
            });
        }

        self.parse_expr().map(Some)
    }

    fn try_consume_char(&mut self, expected: char) -> bool {
        if self.peek_char() != Some(expected) {
            return false;
        }

        self.consume_char();
        true
    }
}

fn next_location(location: SourceLocation, ch: char) -> SourceLocation {
    if ch == '\n' {
        return SourceLocation::new(location.line + 1, 1);
    }

    SourceLocation::new(location.line, location.column + 1)
}
