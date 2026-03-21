use crate::scheme::ast::Expr;
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
}

impl<'src> Parser<'src> {
    fn new(input: &'src str) -> Self {
        Self { input, index: 0 }
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.skip_ignored();

        let Some(ch) = self.peek_char() else {
            return Err(ParseError::UnexpectedEndOfInput);
        };

        match ch {
            '(' => self.parse_list(),
            ')' => Err(ParseError::UnexpectedClosingParenthesis),
            '\'' => self.parse_quote(),
            '"' => self.parse_string(),
            _ => Ok(self.parse_atom()),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, ParseError> {
        self.consume_char();
        let mut items = Vec::new();

        while let Some(expression) = self.parse_list_expression()? {
            items.push(expression);
        }

        Ok(Expr::List(items))
    }

    fn parse_string(&mut self) -> Result<Expr, ParseError> {
        self.consume_char();
        let mut value = String::new();

        loop {
            match self.consume_string_char()? {
                '"' => return Ok(Expr::String(value)),
                '\\' => value.push(self.parse_escape_sequence()?),
                ch => value.push(ch),
            }
        }
    }

    fn parse_quote(&mut self) -> Result<Expr, ParseError> {
        self.consume_char();

        Ok(Expr::List(vec![
            Expr::Symbol("quote".into()),
            self.parse_expr()?,
        ]))
    }

    fn parse_escape_sequence(&mut self) -> Result<char, ParseError> {
        let Some(ch) = self.consume_char() else {
            return Err(ParseError::UnterminatedStringLiteral);
        };

        match ch {
            '\\' => Ok('\\'),
            '"' => Ok('"'),
            'n' => Ok('\n'),
            'r' => Ok('\r'),
            't' => Ok('\t'),
            _ => Err(ParseError::InvalidEscapeSequence { escape: ch }),
        }
    }

    fn parse_atom(&mut self) -> Expr {
        let token = self.take_while(|ch| !ch.is_whitespace() && ch != '(' && ch != ')');

        match token {
            "#t" => Expr::Boolean(true),
            "#f" => Expr::Boolean(false),
            _ => match token.parse::<i64>() {
                Ok(value) => Expr::Integer(value),
                Err(_) => Expr::Symbol(token.to_string()),
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
        Some(ch)
    }

    fn consume_string_char(&mut self) -> Result<char, ParseError> {
        self.consume_char()
            .ok_or(ParseError::UnterminatedStringLiteral)
    }

    fn parse_list_expression(&mut self) -> Result<Option<Expr>, ParseError> {
        self.skip_ignored();
        if self.try_consume_char(')') {
            return Ok(None);
        }

        if self.peek_char().is_none() {
            return Err(ParseError::UnexpectedEndOfInput);
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
