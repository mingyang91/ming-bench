use crate::scheme::ast::{expr_ref, list_expr, symbol_expr, Expr, ExprRef, Identifier};
use crate::scheme::error::EvalError;

pub fn parse_program(input: &str) -> Result<Vec<ExprRef>, EvalError> {
    Reader::new(input).parse_program()
}

struct Reader<'a> {
    input: &'a str,
    index: usize,
}

impl<'a> Reader<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, index: 0 }
    }

    fn parse_program(&mut self) -> Result<Vec<ExprRef>, EvalError> {
        let mut expressions = Vec::new();
        self.skip_ignored();
        while self.peek_char().is_some() {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }
        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<ExprRef, EvalError> {
        self.skip_ignored();
        match self.next_char() {
            Some('(') => self.parse_list(),
            Some('\'') => self.parse_quoted(),
            Some('"') => self.parse_string(self.index.saturating_sub(1)),
            Some(ch) => self.parse_atom(ch),
            None => Err(EvalError::ParseUnexpectedEnd),
        }
    }

    fn parse_list(&mut self) -> Result<ExprRef, EvalError> {
        let mut elements = Vec::new();
        loop {
            self.skip_ignored();
            match self.peek_char() {
                Some(')') => return self.finish_list(elements),
                Some('.') if self.starts_dotted_tail() => return self.parse_dotted_list(elements),
                Some(_) => elements.push(self.parse_expr()?),
                None => return Err(EvalError::ParseUnexpectedEnd),
            }
        }
    }

    fn parse_dotted_list(&mut self, elements: Vec<ExprRef>) -> Result<ExprRef, EvalError> {
        if elements.is_empty() {
            return Err(EvalError::InvalidSyntax {
                form: "list".to_owned(),
                detail: "dot requires a head element".to_owned(),
            });
        }
        self.next_char();
        let tail = self.parse_expr()?;
        self.skip_ignored();
        if self.next_char() != Some(')') {
            return Err(EvalError::InvalidSyntax {
                form: "list".to_owned(),
                detail: "dotted list must end with `)`".to_owned(),
            });
        }
        Ok(expr_ref(Expr::DottedList(elements, tail)))
    }

    fn parse_quoted(&mut self) -> Result<ExprRef, EvalError> {
        let quoted = self.parse_expr()?;
        Ok(list_expr(vec![symbol_expr("quote"), quoted]))
    }

    fn parse_string(&mut self, start: usize) -> Result<ExprRef, EvalError> {
        let mut value = String::new();
        while let Some(ch) = self.next_char() {
            match ch {
                '"' => return Ok(expr_ref(Expr::String(value))),
                '\\' => value.push(self.parse_escape(start)?),
                _ => value.push(ch),
            }
        }
        Err(EvalError::ParseUnclosedString { position: start })
    }

    fn parse_escape(&mut self, start: usize) -> Result<char, EvalError> {
        match self.next_char() {
            Some('"') => Ok('"'),
            Some('\\') => Ok('\\'),
            Some('n') => Ok('\n'),
            Some('t') => Ok('\t'),
            Some(other) => Ok(other),
            None => Err(EvalError::ParseUnclosedString { position: start }),
        }
    }

    fn parse_atom(&mut self, first: char) -> Result<ExprRef, EvalError> {
        let mut token = String::from(first);
        token.push_str(&self.take_while(|ch| !is_delimiter(ch)));
        parse_token(&token)
    }

    fn skip_ignored(&mut self) {
        self.skip_whitespace();
        while self.peek_char() == Some(';') {
            self.skip_comment();
            self.skip_whitespace();
        }
    }

    fn skip_whitespace(&mut self) {
        while self.peek_char().is_some_and(char::is_whitespace) {
            self.next_char();
        }
    }

    fn skip_comment(&mut self) {
        while self.next_char().is_some_and(|ch| ch != '\n') {}
    }

    fn take_while<F>(&mut self, predicate: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut text = String::new();
        while let Some(ch) = self.peek_char().filter(|ch| predicate(*ch)) {
            text.push(ch);
            self.next_char();
        }
        text
    }

    fn finish_list(&mut self, elements: Vec<ExprRef>) -> Result<ExprRef, EvalError> {
        self.next_char();
        Ok(list_expr(elements))
    }

    fn starts_dotted_tail(&self) -> bool {
        let mut chars = self.input[self.index..].chars();
        if chars.next() != Some('.') {
            return false;
        }
        match chars.next() {
            Some(ch) => is_delimiter(ch),
            None => true,
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.index..].chars().next()
    }

    fn next_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.index += ch.len_utf8();
        Some(ch)
    }
}

fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | '\'' | '"' | ';')
}

fn parse_token(token: &str) -> Result<ExprRef, EvalError> {
    match token {
        "#t" => Ok(expr_ref(Expr::Bool(true))),
        "#f" => Ok(expr_ref(Expr::Bool(false))),
        _ if looks_like_number(token) => parse_number(token),
        _ => Ok(expr_ref(Expr::Symbol(Identifier::Plain(token.to_owned())))),
    }
}

fn looks_like_number(token: &str) -> bool {
    let digits = token.strip_prefix('-').unwrap_or(token);
    !digits.is_empty() && digits.chars().all(|ch| ch.is_ascii_digit())
}

fn parse_number(token: &str) -> Result<ExprRef, EvalError> {
    token
        .parse::<i64>()
        .map(|value| expr_ref(Expr::Number(value)))
        .map_err(|_| EvalError::ParseInvalidNumber {
            lexeme: token.to_owned(),
        })
}
