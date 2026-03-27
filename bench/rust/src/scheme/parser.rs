use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

pub struct Parser<'a> {
    input: &'a [u8],
    pos: usize,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.as_bytes(),
            pos: 0,
        }
    }

    pub fn parse_all(&mut self) -> Result<Vec<Value>, EvalError> {
        let mut exprs = Vec::new();
        loop {
            self.skip_whitespace_and_comments();
            if self.pos >= self.input.len() {
                break;
            }
            exprs.push(self.parse_expr()?);
        }
        if exprs.is_empty() {
            return Err(EvalError::Parse("empty input".into()));
        }
        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Value, EvalError> {
        self.skip_whitespace_and_comments();
        if self.pos >= self.input.len() {
            return Err(EvalError::Parse("unexpected end of input".into()));
        }
        match self.input[self.pos] {
            b'(' => self.parse_list(),
            b'"' => self.parse_string(),
            b'#' => self.parse_hash(),
            b'\'' => {
                self.pos += 1;
                let quoted = self.parse_expr()?;
                Ok(Value::List(vec![Value::Symbol("quote".into()), quoted]))
            }
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Value, EvalError> {
        self.pos += 1; // skip '('
        let mut elems = Vec::new();
        loop {
            self.skip_whitespace_and_comments();
            if self.pos >= self.input.len() {
                return Err(EvalError::Parse("unterminated list".into()));
            }
            if self.input[self.pos] == b')' {
                self.pos += 1;
                return Ok(Value::List(elems));
            }
            elems.push(self.parse_expr()?);
        }
    }

    fn parse_string(&mut self) -> Result<Value, EvalError> {
        self.pos += 1; // skip opening '"'
        let mut s = String::new();
        while self.pos < self.input.len() {
            match self.input[self.pos] {
                b'"' => {
                    self.pos += 1;
                    return Ok(Value::String(s));
                }
                b'\\' => {
                    self.pos += 1;
                    if self.pos >= self.input.len() {
                        return Err(EvalError::Parse("unterminated string escape".into()));
                    }
                    match self.input[self.pos] {
                        b'n' => s.push('\n'),
                        b't' => s.push('\t'),
                        b'\\' => s.push('\\'),
                        b'"' => s.push('"'),
                        c => s.push(c as char),
                    }
                    self.pos += 1;
                }
                c => {
                    s.push(c as char);
                    self.pos += 1;
                }
            }
        }
        Err(EvalError::Parse("unterminated string".into()))
    }

    fn parse_hash(&mut self) -> Result<Value, EvalError> {
        self.pos += 1; // skip '#'
        if self.pos >= self.input.len() {
            return Err(EvalError::Parse("unexpected end after #".into()));
        }
        match self.input[self.pos] {
            b't' => {
                self.pos += 1;
                Ok(Value::Boolean(true))
            }
            b'f' => {
                self.pos += 1;
                Ok(Value::Boolean(false))
            }
            c => Err(EvalError::Parse(format!("unknown # literal: #{}", c as char))),
        }
    }

    fn parse_atom(&mut self) -> Result<Value, EvalError> {
        let start = self.pos;
        while self.pos < self.input.len() {
            match self.input[self.pos] {
                b' ' | b'\t' | b'\n' | b'\r' | b'(' | b')' | b'"' | b';' => break,
                _ => self.pos += 1,
            }
        }
        let token = std::str::from_utf8(&self.input[start..self.pos])
            .map_err(|_| EvalError::Parse("invalid utf8".into()))?;

        if let Ok(n) = token.parse::<i64>() {
            Ok(Value::Integer(n))
        } else {
            Ok(Value::Symbol(token.into()))
        }
    }

    fn skip_whitespace_and_comments(&mut self) {
        while self.pos < self.input.len() {
            match self.input[self.pos] {
                b' ' | b'\t' | b'\n' | b'\r' => self.pos += 1,
                b';' => {
                    while self.pos < self.input.len() && self.input[self.pos] != b'\n' {
                        self.pos += 1;
                    }
                }
                _ => break,
            }
        }
    }
}
