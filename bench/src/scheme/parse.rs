use std::rc::Rc;

use crate::scheme::error::SchemeError;
use crate::scheme::value::Value;

enum ListToken {
    Close,
    Dot,
    Expr(Value),
}

/// Parse all expressions from `input`.
pub fn parse_all(input: &str) -> Result<Vec<Value>, SchemeError> {
    let mut p = Parser {
        input: input.as_bytes(),
        pos: 0,
    };
    let mut exprs = Vec::new();
    loop {
        p.skip_ws();
        if p.pos >= p.input.len() {
            break;
        }
        exprs.push(p.parse_expr()?);
    }
    Ok(exprs)
}

struct Parser<'a> {
    input: &'a [u8],
    pos: usize,
}

impl Parser<'_> {
    fn peek(&self) -> Option<u8> {
        self.input.get(self.pos).copied()
    }

    fn advance(&mut self) -> u8 {
        let b = self.input[self.pos];
        self.pos += 1;
        b
    }

    fn skip_ws(&mut self) {
        while self.pos < self.input.len() {
            match self.input[self.pos] {
                b';' => self.skip_line_comment(),
                b if b.is_ascii_whitespace() => self.pos += 1,
                _ => break,
            }
        }
    }

    fn skip_line_comment(&mut self) {
        while self.pos < self.input.len() && self.input[self.pos] != b'\n' {
            self.pos += 1;
        }
    }

    fn is_delimiter(&self, pos: usize) -> bool {
        if pos >= self.input.len() {
            return true;
        }
        matches!(
            self.input[pos],
            b' ' | b'\t' | b'\n' | b'\r' | b'(' | b')' | b'"' | b';'
        )
    }

    fn parse_expr(&mut self) -> Result<Value, SchemeError> {
        self.skip_ws();
        match self.peek() {
            None => Err(SchemeError::UnexpectedEof),
            Some(b'(') => self.parse_list(),
            Some(b'\'') => self.parse_quote(),
            Some(b'"') => self.parse_string(),
            Some(b'#') => self.parse_hash(),
            Some(b')') => Err(SchemeError::UnexpectedChar { ch: ')' }),
            Some(_) => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Value, SchemeError> {
        self.advance(); // skip '('
        let mut items = Vec::new();
        loop {
            self.skip_ws();
            match self.list_next()? {
                ListToken::Close => return Ok(crate::scheme::value::from_vec(items)),
                ListToken::Dot => return self.parse_dotted_tail(items),
                ListToken::Expr(v) => items.push(v),
            }
        }
    }

    fn list_next(&mut self) -> Result<ListToken, SchemeError> {
        if self.pos >= self.input.len() {
            return Err(SchemeError::UnexpectedEof);
        }
        if self.input[self.pos] == b')' {
            self.advance();
            return Ok(ListToken::Close);
        }
        if self.input[self.pos] == b'.' && self.is_delimiter(self.pos + 1) {
            return Ok(ListToken::Dot);
        }
        self.parse_expr().map(ListToken::Expr)
    }

    fn parse_dotted_tail(&mut self, items: Vec<Value>) -> Result<Value, SchemeError> {
        self.advance(); // skip '.'
        let cdr = self.parse_expr()?;
        self.skip_ws();
        if self.peek() != Some(b')') {
            return Err(SchemeError::BadSyntax {
                form: "dotted pair".into(),
            });
        }
        self.advance(); // skip ')'
        let result = items
            .into_iter()
            .rev()
            .fold(cdr, |acc, v| Value::Pair(Rc::new(v), Rc::new(acc)));
        Ok(result)
    }

    fn parse_quote(&mut self) -> Result<Value, SchemeError> {
        self.advance(); // skip '\''
        let expr = self.parse_expr()?;
        Ok(Value::Pair(
            Rc::new(Value::Symbol("quote".into())),
            Rc::new(Value::Pair(Rc::new(expr), Rc::new(Value::Nil))),
        ))
    }

    fn parse_string(&mut self) -> Result<Value, SchemeError> {
        self.advance(); // skip opening '"'
        let start = self.pos;
        while self.pos < self.input.len() && self.input[self.pos] != b'"' {
            self.pos += 1;
        }
        if self.pos >= self.input.len() {
            return Err(SchemeError::UnterminatedString);
        }
        let s = std::str::from_utf8(&self.input[start..self.pos])
            .map_err(|_| SchemeError::BadSyntax {
                form: "invalid utf8".into(),
            })?;
        self.advance(); // skip closing '"'
        Ok(Value::Str(s.to_string()))
    }

    fn parse_hash(&mut self) -> Result<Value, SchemeError> {
        self.advance(); // skip '#'
        match self.peek() {
            Some(b't') => {
                self.advance();
                Ok(Value::Boolean(true))
            }
            Some(b'f') => {
                self.advance();
                Ok(Value::Boolean(false))
            }
            Some(ch) => Err(SchemeError::UnexpectedChar { ch: ch as char }),
            None => Err(SchemeError::UnexpectedEof),
        }
    }

    fn parse_atom(&mut self) -> Result<Value, SchemeError> {
        let start = self.pos;
        while self.pos < self.input.len() && !self.is_delimiter(self.pos) {
            self.pos += 1;
        }
        let token = std::str::from_utf8(&self.input[start..self.pos])
            .map_err(|_| SchemeError::BadSyntax {
                form: "invalid utf8".into(),
            })?;
        if let Ok(n) = token.parse::<i64>() {
            return Ok(Value::Integer(n));
        }
        Ok(Value::Symbol(token.to_string()))
    }
}
