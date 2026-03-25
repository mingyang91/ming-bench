use super::core::{Expr, Position};
use super::error::EvalError;
use super::number::parse_number_literal;

pub(crate) fn parse_program(input: &str) -> Result<Vec<Expr>, EvalError> {
    Parser::new(input).parse_program()
}

struct Parser<'a> {
    input: &'a str,
    cursor: usize,
    line: usize,
    col: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            cursor: 0,
            line: 1,
            col: 1,
        }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut expressions = Vec::new();
        self.skip_ignored();

        while !self.is_eof() {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if expressions.is_empty() {
            Err(EvalError::EmptyInput)
        } else {
            Ok(expressions)
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();
        let pos = self.current_position();

        match self.peek_char() {
            Some('(') => self.parse_list(pos),
            Some('\'') => self.parse_quote_shorthand(pos),
            Some('#') if self.peek_next_char() == Some('\'') => self.parse_syntax_shorthand(pos),
            Some('"') => self.parse_string(pos),
            Some(')') => Err(syntax_error(pos, "unexpected ')'")),
            Some(_) => self.parse_atom(pos),
            None => Err(syntax_error(pos, "unexpected end of input")),
        }
    }

    fn parse_list(&mut self, pos: Position) -> Result<Expr, EvalError> {
        self.expect_char('(')?;
        let mut items = Vec::new();

        loop {
            self.skip_ignored();
            match self.peek_char() {
                Some(')') => return self.finish_list(items, pos),
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(syntax_error(self.current_position(), "unterminated list")),
            }
        }
    }

    fn parse_quote_shorthand(&mut self, pos: Position) -> Result<Expr, EvalError> {
        self.expect_char('\'')?;
        let quoted = self.parse_expr()?;
        Ok(Expr::List(
            vec![Expr::Symbol("quote".into(), pos), quoted],
            pos,
        ))
    }

    fn parse_syntax_shorthand(&mut self, pos: Position) -> Result<Expr, EvalError> {
        self.expect_char('#')?;
        self.expect_char('\'')?;
        let quoted = self.parse_expr()?;
        Ok(Expr::List(
            vec![Expr::Symbol("syntax".into(), pos), quoted],
            pos,
        ))
    }

    fn parse_string(&mut self, pos: Position) -> Result<Expr, EvalError> {
        self.expect_char('"')?;
        let mut value = String::new();

        loop {
            match self.bump_char() {
                Some('"') => return Ok(Expr::String(value, pos)),
                Some('\\') => value.push(self.parse_string_escape()?),
                Some(ch) => value.push(ch),
                None => return Err(syntax_error(self.current_position(), "unterminated string")),
            }
        }
    }

    fn parse_atom(&mut self, pos: Position) -> Result<Expr, EvalError> {
        let mut token = String::new();

        while let Some(ch) = self.peek_char().filter(|ch| !is_delimiter(*ch)) {
            token.push(ch);
            self.bump_char();
        }

        if token.is_empty() {
            return Err(syntax_error(pos, "expected expression"));
        }

        match token.as_str() {
            "#t" => Ok(Expr::Bool(true, pos)),
            "#f" => Ok(Expr::Bool(false, pos)),
            _ if token.starts_with("#\\") => parse_char_literal(&token, pos),
            _ => match parse_number_literal(&token) {
                Ok(Some(value)) => Ok(Expr::Number(value, pos)),
                Ok(None) => Ok(Expr::Symbol(token, pos)),
                Err(error) => Err(pos.attach(error)),
            },
        }
    }

    fn skip_ignored(&mut self) {
        loop {
            self.skip_whitespace();
            match self.skip_comment() {
                true => continue,
                false => break,
            }
        }
    }

    fn expect_char(&mut self, expected: char) -> Result<(), EvalError> {
        let pos = self.current_position();
        match self.bump_char() {
            Some(actual) if actual == expected => Ok(()),
            Some(actual) => Err(syntax_error(
                pos,
                format!("expected '{expected}', found '{actual}'"),
            )),
            None => Err(syntax_error(
                pos,
                format!("expected '{expected}', found end of input"),
            )),
        }
    }

    fn current_position(&self) -> Position {
        Position {
            line: self.line,
            col: self.col,
        }
    }

    fn finish_list(&mut self, items: Vec<Expr>, pos: Position) -> Result<Expr, EvalError> {
        self.bump_char();
        Ok(Expr::List(items, pos))
    }

    fn parse_string_escape(&mut self) -> Result<char, EvalError> {
        let Some(ch) = self.bump_char() else {
            return Err(syntax_error(
                self.current_position(),
                "unterminated string escape",
            ));
        };

        match ch {
            '"' => Ok('"'),
            '\\' => Ok('\\'),
            'n' => Ok('\n'),
            'r' => Ok('\r'),
            't' => Ok('\t'),
            other => Err(syntax_error(
                self.current_position(),
                format!("unsupported string escape: \\{other}"),
            )),
        }
    }

    fn skip_whitespace(&mut self) {
        while self.peek_char().is_some_and(|ch| ch.is_whitespace()) {
            self.bump_char();
        }
    }

    fn skip_comment(&mut self) -> bool {
        if self.peek_char() != Some(';') {
            return false;
        }

        while self.bump_char().is_some_and(|ch| ch != '\n') {}
        true
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.cursor..].chars().next()
    }

    fn peek_next_char(&self) -> Option<char> {
        let mut chars = self.input[self.cursor..].chars();
        chars.next()?;
        chars.next()
    }

    fn bump_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.cursor += ch.len_utf8();
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(ch)
    }

    fn is_eof(&self) -> bool {
        self.cursor >= self.input.len()
    }
}

fn syntax_error(pos: Position, message: impl Into<String>) -> EvalError {
    pos.attach(EvalError::SyntaxError {
        message: message.into(),
    })
}

fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | ';')
}

fn parse_char_literal(token: &str, pos: Position) -> Result<Expr, EvalError> {
    let Some(literal) = token.strip_prefix("#\\") else {
        return Err(syntax_error(
            pos,
            format!("invalid character literal: {token}"),
        ));
    };

    let value = match literal {
        "space" => ' ',
        "newline" => '\n',
        _ => {
            let mut chars = literal.chars();
            match (chars.next(), chars.next()) {
                (Some(ch), None) => ch,
                _ => {
                    return Err(syntax_error(
                        pos,
                        format!("invalid character literal: {token}"),
                    ))
                }
            }
        }
    };

    Ok(Expr::Char(value, pos))
}
