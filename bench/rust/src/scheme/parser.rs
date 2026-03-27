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

    pub fn parse_all(&mut self) -> Result<Vec<(Value, usize, usize)>, EvalError> {
        let mut exprs = Vec::new();
        loop {
            self.skip_whitespace_and_comments();
            if self.pos >= self.input.len() {
                break;
            }
            let (line, col) = self.line_col(self.pos);
            let val = self.parse_expr().map_err(|e| {
                EvalError::WithPosition {
                    error: Box::new(e),
                    line,
                    col,
                }
            })?;
            exprs.push((val, line, col));
        }
        if exprs.is_empty() {
            return Err(EvalError::Parse("empty input".into()));
        }
        Ok(exprs)
    }

    fn line_col(&self, byte_pos: usize) -> (usize, usize) {
        let mut line = 1;
        let mut col = 1;
        for &b in &self.input[..byte_pos] {
            if b == b'\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        (line, col)
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
                    return Ok(Value::String(s, false));
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
            b'\\' => {
                self.pos += 1; // skip '\'
                if self.pos >= self.input.len() {
                    return Err(EvalError::Parse("unexpected end after #\\".into()));
                }
                // Named characters
                let start = self.pos;
                while self.pos < self.input.len() {
                    match self.input[self.pos] {
                        b' ' | b'\t' | b'\n' | b'\r' | b'(' | b')' | b'"' | b';' => break,
                        _ => self.pos += 1,
                    }
                }
                let name = std::str::from_utf8(&self.input[start..self.pos])
                    .map_err(|_| EvalError::Parse("invalid utf8 in char literal".into()))?;
                let ch = match name {
                    "space" => ' ',
                    "newline" => '\n',
                    "tab" => '\t',
                    s if s.chars().count() == 1 => s.chars().next().unwrap(),
                    _ => return Err(EvalError::Parse(format!("unknown character name: {}", name))),
                };
                Ok(Value::Char(ch))
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
        } else if let Some(idx) = token.find('/') {
            // Try rational: num/den
            if idx > 0 && idx < token.len() - 1 {
                if let (Ok(num), Ok(den)) = (token[..idx].parse::<i64>(), token[idx+1..].parse::<i64>()) {
                    if den != 0 {
                        return Ok(Value::make_rational(num, den));
                    }
                }
            }
            Ok(Value::Symbol(token.into()))
        } else if token.contains('.') {
            if let Ok(f) = token.parse::<f64>() {
                Ok(Value::Float(f))
            } else {
                Ok(Value::Symbol(token.into()))
            }
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
