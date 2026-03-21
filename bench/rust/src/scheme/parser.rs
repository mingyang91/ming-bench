use crate::scheme::ast::{Expr, SourceLocation};
use crate::scheme::error::ParseError;
use crate::scheme::number::Number;

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
            '#' if self.starts_with("#'") => self.parse_syntax_quote(location),
            '"' => self.parse_string(location),
            _ => self.parse_atom(location),
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

    fn parse_syntax_quote(&mut self, location: SourceLocation) -> Result<Expr, ParseError> {
        self.consume_char();
        self.consume_char();

        Ok(Expr::list(
            vec![Expr::symbol("syntax".into(), location), self.parse_expr()?],
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

    fn parse_atom(&mut self, location: SourceLocation) -> Result<Expr, ParseError> {
        let token = self.take_while(|ch| !ch.is_whitespace() && ch != '(' && ch != ')');

        match token {
            "#t" => Ok(Expr::boolean(true, location)),
            "#f" => Ok(Expr::boolean(false, location)),
            _ if token.starts_with("#\\") => parse_character_literal(token, location),
            _ => Ok(Number::parse(token)
                .map(|value| Expr::number(value, location))
                .unwrap_or_else(|| Expr::symbol(token.to_string(), location))),
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

    fn starts_with(&self, prefix: &str) -> bool {
        self.input[self.index..].starts_with(prefix)
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

fn parse_character_literal(token: &str, location: SourceLocation) -> Result<Expr, ParseError> {
    let literal = &token[2..];
    let value = match literal {
        "space" => Some(' '),
        "newline" => Some('\n'),
        _ => {
            let mut chars = literal.chars();
            let first = chars.next();
            if chars.next().is_none() {
                first
            } else {
                None
            }
        }
    };

    value
        .map(|value| Expr::character(value, location))
        .ok_or(ParseError::InvalidCharacterLiteral {
            location,
            literal: token.to_string(),
        })
}
