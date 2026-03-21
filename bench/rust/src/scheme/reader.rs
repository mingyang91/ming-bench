use crate::scheme::error::EvalError;
use crate::scheme::expr::Expr;
use crate::scheme::source::SourcePos;
use crate::scheme::value::Value;

#[derive(Debug, Clone)]
enum Token {
    LeftParen(SourcePos),
    RightParen(SourcePos),
    Quote(SourcePos),
    Atom(String, SourcePos),
    Str(String, SourcePos),
}

#[derive(Clone, Copy)]
struct Cursor<'a> {
    input: &'a str,
    offset: usize,
    line: usize,
    column: usize,
}

impl<'a> Cursor<'a> {
    fn at_end(self) -> bool {
        self.offset >= self.input.len()
    }

    fn position(self) -> SourcePos {
        SourcePos::new(self.line, self.column)
    }

    fn current(self) -> char {
        self.input.as_bytes()[self.offset] as char
    }

    fn advance(self) -> Self {
        if self.current() == '\n' {
            Self {
                input: self.input,
                offset: self.offset + 1,
                line: self.line + 1,
                column: 1,
            }
        } else {
            Self {
                input: self.input,
                offset: self.offset + 1,
                line: self.line,
                column: self.column + 1,
            }
        }
    }
}

pub fn read_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input)?;
    let (expressions, index) = parse_all(&tokens, 0)?;

    if index != tokens.len() {
        return Err(EvalError::message("unexpected trailing tokens"));
    }

    if expressions.is_empty() {
        return Err(EvalError::message("expected at least one expression"));
    }

    Ok(expressions)
}

fn tokenize(input: &str) -> Result<Vec<Token>, EvalError> {
    let mut tokens = Vec::new();
    let mut cursor = Cursor {
        input,
        offset: 0,
        line: 1,
        column: 1,
    };

    while !cursor.at_end() {
        match cursor.current() {
            ch if ch.is_ascii_whitespace() => cursor = cursor.advance(),
            ';' => cursor = skip_comment(cursor),
            '(' => {
                tokens.push(Token::LeftParen(cursor.position()));
                cursor = cursor.advance();
            }
            ')' => {
                tokens.push(Token::RightParen(cursor.position()));
                cursor = cursor.advance();
            }
            '\'' => {
                tokens.push(Token::Quote(cursor.position()));
                cursor = cursor.advance();
            }
            '"' => {
                let start = cursor.position();
                let (next, body) = read_string(cursor.advance(), start, String::new())?;
                tokens.push(Token::Str(body, start));
                cursor = next;
            }
            _ => {
                let start = cursor.position();
                let (next, atom) = read_atom(cursor, String::new());
                tokens.push(Token::Atom(atom, start));
                cursor = next;
            }
        }
    }

    Ok(tokens)
}

fn skip_comment(mut cursor: Cursor<'_>) -> Cursor<'_> {
    while !cursor.at_end() && cursor.current() != '\n' {
        cursor = cursor.advance();
    }

    cursor
}

fn read_atom(mut cursor: Cursor<'_>, mut atom: String) -> (Cursor<'_>, String) {
    while !cursor.at_end() && !is_delimiter(cursor.current()) {
        atom.push(cursor.current());
        cursor = cursor.advance();
    }

    (cursor, atom)
}

fn read_string(
    mut cursor: Cursor<'_>,
    start: SourcePos,
    mut body: String,
) -> Result<(Cursor<'_>, String), EvalError> {
    while !cursor.at_end() {
        match cursor.current() {
            '"' => return Ok((cursor.advance(), body)),
            '\\' => {
                cursor = cursor.advance();
                if cursor.at_end() {
                    return Err(EvalError::at(start, "unterminated string"));
                }
                body.push(match cursor.current() {
                    '"' => '"',
                    '\\' => '\\',
                    'n' => '\n',
                    't' => '\t',
                    other => other,
                });
                cursor = cursor.advance();
            }
            ch => {
                body.push(ch);
                cursor = cursor.advance();
            }
        }
    }

    Err(EvalError::at(start, "unterminated string"))
}

fn parse_all(tokens: &[Token], mut index: usize) -> Result<(Vec<Expr>, usize), EvalError> {
    let mut expressions = Vec::new();

    while index < tokens.len() {
        let (expr, next) = parse_expr(tokens, index)?;
        expressions.push(expr);
        index = next;
    }

    Ok((expressions, index))
}

fn parse_expr(tokens: &[Token], index: usize) -> Result<(Expr, usize), EvalError> {
    let Some(token) = tokens.get(index) else {
        return Err(EvalError::message("unexpected end of input"));
    };

    match token {
        Token::LeftParen(position) => parse_list(tokens, index + 1, *position),
        Token::RightParen(position) => Err(EvalError::at(*position, "unexpected ')'")),
        Token::Quote(position) => {
            let (quoted, next) = parse_expr(tokens, index + 1)?;
            Ok((
                Expr::List(vec![Expr::quote_symbol(*position), quoted], *position),
                next,
            ))
        }
        Token::Str(value, position) => Ok((
            Expr::Literal(Value::Str(value.clone()), *position),
            index + 1,
        )),
        Token::Atom(value, position) => Ok((parse_atom(value, *position), index + 1)),
    }
}

fn parse_list(tokens: &[Token], mut index: usize, start: SourcePos) -> Result<(Expr, usize), EvalError> {
    let mut items = Vec::new();

    while let Some(token) = tokens.get(index) {
        if matches!(token, Token::RightParen(_)) {
            return Ok((Expr::List(items, start), index + 1));
        }

        let (expr, next) = parse_expr(tokens, index)?;
        items.push(expr);
        index = next;
    }

    Err(EvalError::at(start, "unterminated list"))
}

fn parse_atom(value: &str, position: SourcePos) -> Expr {
    match value {
        "#t" => Expr::Literal(Value::Bool(true), position),
        "#f" => Expr::Literal(Value::Bool(false), position),
        _ => match value.parse::<i64>() {
            Ok(number) => Expr::Literal(Value::Number(number), position),
            Err(_) => Expr::Symbol(value.to_string(), position),
        },
    }
}

fn is_delimiter(ch: char) -> bool {
    ch.is_ascii_whitespace() || matches!(ch, '(' | ')' | '\'' | ';')
}
