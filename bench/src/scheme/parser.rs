use crate::scheme::EvalError;
use crate::scheme::error::Span;
use crate::scheme::value::Value;
use std::iter::Peekable;
use std::str::Chars;

/// A token with its source position.
struct Token {
    text: String,
    line: usize,
    col: usize,
}

/// Scanner that tracks line and column while iterating chars.
struct Scanner<'a> {
    chars: Peekable<Chars<'a>>,
    line: usize,
    col: usize,
}

impl<'a> Scanner<'a> {
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
        let ch = self.chars.next()?;
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(ch)
    }

    fn pos(&self) -> (usize, usize) {
        (self.line, self.col)
    }
}

/// Read a string literal (after the opening `"` has been consumed).
fn read_string(scanner: &mut Scanner<'_>) -> Result<String, EvalError> {
    let mut s = String::new();
    loop {
        match scanner.next() {
            Some('\\') => match scanner.next() {
                Some(esc) => s.push(esc),
                None => {
                    return Err(EvalError::Parse {
                        message: "unterminated string escape".into(),
                    })
                }
            },
            Some('"') => return Ok(s),
            Some(c) => s.push(c),
            None => {
                return Err(EvalError::Parse {
                    message: "unterminated string".into(),
                })
            }
        }
    }
}

/// Skip a line comment (starting from `;`).
fn skip_comment(scanner: &mut Scanner<'_>) {
    while let Some(&c) = scanner.peek() {
        if c == '\n' {
            break;
        }
        scanner.next();
    }
}

/// Read an atom token (symbol, number, boolean).
fn read_atom(scanner: &mut Scanner<'_>) -> String {
    let mut atom = String::new();
    while let Some(&c) = scanner.peek() {
        if c.is_whitespace() || c == '(' || c == ')' || c == ';' {
            break;
        }
        atom.push(c);
        scanner.next();
    }
    atom
}

/// Tokenize input into a list of tokens with positions.
fn tokenize(input: &str) -> Result<Vec<Token>, EvalError> {
    let mut tokens = Vec::new();
    let mut scanner = Scanner::new(input);

    while let Some(&ch) = scanner.peek() {
        match ch {
            ' ' | '\t' | '\n' | '\r' => {
                scanner.next();
            }
            '"' => {
                let (line, col) = scanner.pos();
                scanner.next();
                let s = read_string(&mut scanner)?;
                tokens.push(Token {
                    text: format!("\"{s}\""),
                    line,
                    col,
                });
            }
            ';' => skip_comment(&mut scanner),
            '(' | ')' | '\'' => {
                let (line, col) = scanner.pos();
                tokens.push(Token {
                    text: ch.to_string(),
                    line,
                    col,
                });
                scanner.next();
            }
            _ => {
                let (line, col) = scanner.pos();
                let atom = read_atom(&mut scanner);
                tokens.push(Token {
                    text: atom,
                    line,
                    col,
                });
            }
        }
    }
    Ok(tokens)
}

/// Parse a list from the token stream (after the opening `(` has been consumed).
fn parse_list(tokens: &[Token], start: usize) -> Result<(Value, usize), EvalError> {
    let mut elements = Vec::new();
    let mut i = start;
    loop {
        if i >= tokens.len() {
            return Err(EvalError::Parse {
                message: "unmatched opening parenthesis".into(),
            });
        }
        if tokens[i].text == ")" {
            return Ok((Value::List(elements), i + 1));
        }
        let (val, next) = parse_expr(tokens, i)?;
        elements.push(val);
        i = next;
    }
}

/// Parse a single expression from the token stream, returning the value
/// and the remaining token index.
fn parse_expr(tokens: &[Token], pos: usize) -> Result<(Value, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse {
            message: "unexpected end of input".into(),
        });
    }

    let token = &tokens[pos];

    if token.text == "(" {
        parse_list(tokens, pos + 1)
    } else if token.text == ")" {
        Err(EvalError::Parse {
            message: "unexpected closing parenthesis".into(),
        })
    } else if token.text == "'" {
        let (quoted, next) = parse_expr(tokens, pos + 1)?;
        Ok((
            Value::List(vec![Value::Symbol("quote".into()), quoted]),
            next,
        ))
    } else {
        Ok((parse_atom(&token.text), pos + 1))
    }
}

fn parse_atom(token: &str) -> Value {
    if token == "#t" {
        return Value::Boolean(true);
    }
    if token == "#f" {
        return Value::Boolean(false);
    }
    if token.starts_with('"') && token.ends_with('"') {
        let inner = &token[1..token.len() - 1];
        return Value::String(inner.to_string());
    }
    if let Ok(n) = token.parse::<i64>() {
        return Value::Integer(n);
    }
    Value::Symbol(token.to_string())
}

/// Parse all expressions from input string, returning values with their source spans.
pub fn parse_spanned(input: &str) -> Result<Vec<(Value, Span)>, EvalError> {
    let tokens = tokenize(input)?;
    let mut expressions = Vec::new();
    let mut pos = 0;
    while pos < tokens.len() {
        let span = Span {
            line: tokens[pos].line,
            col: tokens[pos].col,
        };
        let (expr, next) = parse_expr(&tokens, pos)?;
        expressions.push((expr, span));
        pos = next;
    }
    Ok(expressions)
}

/// Parse all expressions from input string.
pub fn parse(input: &str) -> Result<Vec<Value>, EvalError> {
    parse_spanned(input).map(|exprs| exprs.into_iter().map(|(v, _)| v).collect())
}
