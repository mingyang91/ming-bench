use crate::scheme::error::{EvalError, Span};
use crate::scheme::value::Value;

/// Position-tracking character reader.
struct Reader<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    line: usize,
    col: usize,
}

impl<'a> Reader<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars().peekable(),
            line: 1,
            col: 1,
        }
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn next(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        if c == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(c)
    }

    fn span(&self) -> Span {
        Span::new(self.line, self.col)
    }
}

/// A token with source position.
struct Token {
    text: String,
    span: Span,
}

fn is_delimiter(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';')
}

/// Skip a line comment (starting at `;`).
fn skip_comment(reader: &mut Reader<'_>) {
    while let Some(&c) = reader.peek() {
        reader.next();
        if c == '\n' {
            break;
        }
    }
}

/// Read a string literal (opening `"` already consumed).
fn read_string(reader: &mut Reader<'_>) -> String {
    let mut s = String::from('"');
    while let Some(&c) = reader.peek() {
        reader.next();
        s.push(c);
        if c == '"' {
            break;
        }
    }
    s
}

/// Read an atom token.
fn read_atom(reader: &mut Reader<'_>) -> String {
    let mut atom = String::new();
    while let Some(&c) = reader.peek() {
        if is_delimiter(c) {
            break;
        }
        atom.push(c);
        reader.next();
    }
    atom
}

/// Tokenize input into a flat list of tokens with positions.
fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut reader = Reader::new(input);

    while let Some(&ch) = reader.peek() {
        match ch {
            ' ' | '\t' | '\n' | '\r' => {
                reader.next();
            }
            ';' => skip_comment(&mut reader),
            '\'' => {
                let span = reader.span();
                reader.next();
                tokens.push(Token {
                    text: "'".into(),
                    span,
                });
            }
            '(' | ')' => {
                let span = reader.span();
                reader.next();
                tokens.push(Token {
                    text: ch.to_string(),
                    span,
                });
            }
            '"' => {
                let span = reader.span();
                reader.next();
                let text = read_string(&mut reader);
                tokens.push(Token { text, span });
            }
            _ => {
                let span = reader.span();
                let text = read_atom(&mut reader);
                tokens.push(Token { text, span });
            }
        }
    }

    tokens
}

/// Parse all expressions from input, returning each with its source span.
pub fn parse(input: &str) -> Result<Vec<(Value, Span)>, EvalError> {
    let tokens = tokenize(input);
    let mut pos = 0;
    let mut exprs = Vec::new();

    while pos < tokens.len() {
        let span = tokens[pos].span;
        let (expr, next_pos) = parse_expr(&tokens, pos)?;
        exprs.push((expr, span));
        pos = next_pos;
    }

    Ok(exprs)
}

fn parse_expr(tokens: &[Token], pos: usize) -> Result<(Value, usize), EvalError> {
    let token = tokens.get(pos).ok_or_else(|| EvalError::Parse {
        message: "unexpected end of input".into(),
        span: tokens.last().map_or(Span::default(), |t| t.span),
    })?;

    match token.text.as_str() {
        "(" => parse_list(tokens, pos + 1),
        ")" => Err(EvalError::Parse {
            message: "unexpected ')'".into(),
            span: token.span,
        }),
        "'" => {
            let (quoted, next_pos) = parse_expr(tokens, pos + 1)?;
            Ok((
                Value::List(vec![Value::Symbol("quote".into()), quoted]),
                next_pos,
            ))
        }
        _ => Ok((parse_atom(&token.text), pos + 1)),
    }
}

fn parse_list(tokens: &[Token], mut pos: usize) -> Result<(Value, usize), EvalError> {
    let mut elems = Vec::new();

    loop {
        let token = tokens.get(pos).ok_or_else(|| EvalError::Parse {
            message: "unterminated list".into(),
            span: tokens.last().map_or(Span::default(), |t| t.span),
        })?;

        if token.text == ")" {
            return Ok((Value::List(elems), pos + 1));
        }

        let (expr, next_pos) = parse_expr(tokens, pos)?;
        elems.push(expr);
        pos = next_pos;
    }
}

fn parse_atom(token: &str) -> Value {
    if let Ok(n) = token.parse::<i64>() {
        return Value::Integer(n);
    }

    match token {
        "#t" => Value::Boolean(true),
        "#f" => Value::Boolean(false),
        "#\\space" => Value::Char(' '),
        "#\\newline" => Value::Char('\n'),
        "#\\tab" => Value::Char('\t'),
        s if s.starts_with("#\\") && s.len() == 3 => {
            Value::Char(s.chars().nth(2).expect("char literal must have a character"))
        }
        s if s.starts_with('"') && s.ends_with('"') && s.len() >= 2 => {
            Value::String(s[1..s.len() - 1].to_string())
        }
        _ => Value::Symbol(token.to_string()),
    }
}
