use super::EvalError;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Int(i64),
    Bool(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
enum Token {
    LParen,
    RParen,
    Quote,
    Atom(String),
    String(String),
}

pub fn parse_program(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input)?;
    let mut parser = Parser::new(tokens);
    let mut exprs = Vec::new();

    while !parser.is_at_end() {
        exprs.push(parser.parse_expr()?);
    }

    Ok(exprs)
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }

    fn is_at_end(&self) -> bool {
        self.index >= self.tokens.len()
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        let token = self
            .tokens
            .get(self.index)
            .ok_or_else(|| EvalError::Parse {
                message: "unexpected end of input".into(),
            })?
            .clone();
        self.index += 1;

        match token {
            Token::LParen => self.parse_list(),
            Token::RParen => Err(EvalError::Parse {
                message: "unexpected ')'".into(),
            }),
            Token::Quote => {
                let quoted = self.parse_expr()?;
                Ok(Expr::List(vec![Expr::Symbol("quote".into()), quoted]))
            }
            Token::Atom(atom) => Ok(parse_atom(&atom)),
            Token::String(value) => Ok(Expr::String(value)),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        let mut items = Vec::new();

        loop {
            match self.tokens.get(self.index) {
                Some(Token::RParen) => {
                    self.index += 1;
                    return Ok(Expr::List(items));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => {
                    return Err(EvalError::Parse {
                        message: "unterminated list".into(),
                    })
                }
            }
        }
    }
}

fn parse_atom(atom: &str) -> Expr {
    match atom {
        "#t" => Expr::Bool(true),
        "#f" => Expr::Bool(false),
        _ => match atom.parse::<i64>() {
            Ok(value) => Expr::Int(value),
            Err(_) => Expr::Symbol(atom.to_string()),
        },
    }
}

fn tokenize(input: &str) -> Result<Vec<Token>, EvalError> {
    let chars: Vec<char> = input.chars().collect();
    let mut tokens = Vec::new();
    let mut index = 0;

    while index < chars.len() {
        match chars[index] {
            ch if ch.is_whitespace() => index += 1,
            ';' => {
                index += 1;
                while index < chars.len() && chars[index] != '\n' {
                    index += 1;
                }
            }
            '(' => {
                tokens.push(Token::LParen);
                index += 1;
            }
            ')' => {
                tokens.push(Token::RParen);
                index += 1;
            }
            '\'' => {
                tokens.push(Token::Quote);
                index += 1;
            }
            '"' => {
                let (value, next_index) = read_string(&chars, index + 1)?;
                tokens.push(Token::String(value));
                index = next_index;
            }
            _ => {
                let start = index;
                while index < chars.len()
                    && !chars[index].is_whitespace()
                    && !matches!(chars[index], '(' | ')' | '\'' | ';')
                {
                    index += 1;
                }

                tokens.push(Token::Atom(chars[start..index].iter().collect()));
            }
        }
    }

    Ok(tokens)
}

fn read_string(chars: &[char], mut index: usize) -> Result<(String, usize), EvalError> {
    let mut value = String::new();

    while index < chars.len() {
        match chars[index] {
            '"' => return Ok((value, index + 1)),
            '\\' => {
                index += 1;
                let escaped = chars.get(index).ok_or_else(|| EvalError::Parse {
                    message: "unterminated string escape".into(),
                })?;
                value.push(match escaped {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\\' => '\\',
                    '"' => '"',
                    other => *other,
                });
                index += 1;
            }
            ch => {
                value.push(ch);
                index += 1;
            }
        }
    }

    Err(EvalError::Parse {
        message: "unterminated string".into(),
    })
}
