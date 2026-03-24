use std::fmt;

use super::EvalError;

// ---------- Source Position ----------

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Pos {
    line: usize,
    col: usize,
}

impl Pos {
    fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

// ---------- Tokenizer ----------

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LParen,
    RParen,
    Quote,
    Symbol(String),
    Integer(i64),
    Float(f64),
    Rational(i64, i64),
    Boolean(bool),
    Str(String),
    Char(char),
}

#[derive(Debug, Clone)]
struct SpannedToken {
    token: Token,
    pos: Pos,
}

/// Parse a character literal starting at `pos` in `chars`.
/// Returns the char and how many characters were consumed.
fn parse_char_literal(chars: &[char], pos: usize, start_pos: Pos) -> Result<(char, usize), EvalError> {
    if pos >= chars.len() {
        return Err(EvalError::Parse(format!("{start_pos}: incomplete character literal")));
    }
    if !chars[pos].is_alphabetic() {
        return Ok((chars[pos], 1));
    }
    let mut end = pos;
    while end < chars.len() && chars[end].is_alphabetic() {
        end += 1;
    }
    let name: String = chars[pos..end].iter().collect();
    let consumed = end - pos;
    if consumed == 1 {
        return Ok((chars[pos], 1));
    }
    let ch = match name.as_str() {
        "space" => ' ',
        "newline" => '\n',
        "tab" => '\t',
        _ => return Err(EvalError::Parse(format!("{start_pos}: unknown character name: {name}"))),
    };
    Ok((ch, consumed))
}

fn parse_word_token(word: &str) -> Token {
    if let Ok(n) = word.parse::<i64>() {
        return Token::Integer(n);
    }
    if let Some(slash) = word.find('/') {
        if slash > 0 && slash < word.len() - 1 {
            if let (Ok(n), Ok(d)) = (word[..slash].parse::<i64>(), word[slash+1..].parse::<i64>()) {
                if d != 0 {
                    return Token::Rational(n, d);
                }
            }
        }
        return Token::Symbol(word.to_string());
    }
    if let Ok(f) = word.parse::<f64>() {
        if word.contains('.') {
            return Token::Float(f);
        }
    }
    Token::Symbol(word.to_string())
}

fn tokenize(input: &str) -> Result<Vec<SpannedToken>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line = 1usize;
    let mut col = 1usize;

    while i < chars.len() {
        match chars[i] {
            '\n' => {
                i += 1;
                line += 1;
                col = 1;
            }
            ' ' | '\t' | '\r' => {
                i += 1;
                col += 1;
            }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                    col += 1;
                }
            }
            '(' => {
                tokens.push(SpannedToken { token: Token::LParen, pos: Pos::new(line, col) });
                i += 1;
                col += 1;
            }
            ')' => {
                tokens.push(SpannedToken { token: Token::RParen, pos: Pos::new(line, col) });
                i += 1;
                col += 1;
            }
            '\'' => {
                tokens.push(SpannedToken { token: Token::Quote, pos: Pos::new(line, col) });
                i += 1;
                col += 1;
            }
            '"' => {
                let start_pos = Pos::new(line, col);
                i += 1;
                col += 1;
                let mut s = String::new();
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        i += 1;
                        col += 1;
                        match chars[i] {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            c => {
                                s.push('\\');
                                s.push(c);
                            }
                        }
                    } else {
                        if chars[i] == '\n' {
                            line += 1;
                            col = 0;
                        }
                        s.push(chars[i]);
                    }
                    i += 1;
                    col += 1;
                }
                if i >= chars.len() {
                    return Err(EvalError::Parse(format!("{start_pos}: unterminated string")));
                }
                i += 1;
                col += 1;
                tokens.push(SpannedToken { token: Token::Str(s), pos: start_pos });
            }
            '#' => {
                let start_pos = Pos::new(line, col);
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => {
                            tokens.push(SpannedToken { token: Token::Boolean(true), pos: start_pos });
                            i += 2;
                            col += 2;
                        }
                        'f' => {
                            tokens.push(SpannedToken { token: Token::Boolean(false), pos: start_pos });
                            i += 2;
                            col += 2;
                        }
                        '\\' => {
                            let (ch, advance) = parse_char_literal(&chars, i + 2, start_pos)?;
                            i += 2 + advance;
                            col += 2 + advance;
                            tokens.push(SpannedToken { token: Token::Char(ch), pos: start_pos });
                        }
                        _ => {
                            return Err(EvalError::Parse(format!(
                                "{start_pos}: unexpected character after #: {}",
                                chars[i + 1]
                            )));
                        }
                    }
                } else {
                    return Err(EvalError::Parse(format!("{start_pos}: unexpected #")));
                }
            }
            _ => {
                let start_pos = Pos::new(line, col);
                let start = i;
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '"' | '\'')
                {
                    i += 1;
                    col += 1;
                }
                let word: String = chars[start..i].iter().collect();
                let token = parse_word_token(&word);
                tokens.push(SpannedToken { token, pos: start_pos });
            }
        }
    }
    Ok(tokens)
}

// ---------- Parser ----------

#[derive(Debug, Clone)]
pub(crate) enum Expr {
    Integer(i64, Pos),
    Float(f64, Pos),
    Rational(i64, i64, Pos),
    Boolean(bool, Pos),
    Str(String, Pos),
    Symbol(String, Pos),
    Char(char, Pos),
    List(Vec<Expr>, Pos),
}

impl Expr {
    pub(crate) fn pos(&self) -> Pos {
        match self {
            Expr::Integer(_, p)
            | Expr::Float(_, p)
            | Expr::Rational(_, _, p)
            | Expr::Boolean(_, p)
            | Expr::Str(_, p)
            | Expr::Symbol(_, p)
            | Expr::Char(_, p)
            | Expr::List(_, p) => *p,
        }
    }
}

fn parse(tokens: &[SpannedToken], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let st = &tokens[*pos];
    let src_pos = st.pos;
    match &st.token {
        Token::Integer(n) => {
            let n = *n;
            *pos += 1;
            Ok(Expr::Integer(n, src_pos))
        }
        Token::Float(f) => {
            let f = *f;
            *pos += 1;
            Ok(Expr::Float(f, src_pos))
        }
        Token::Rational(n, d) => {
            let (n, d) = (*n, *d);
            *pos += 1;
            Ok(Expr::Rational(n, d, src_pos))
        }
        Token::Boolean(b) => {
            let b = *b;
            *pos += 1;
            Ok(Expr::Boolean(b, src_pos))
        }
        Token::Str(s) => {
            let s = s.clone();
            *pos += 1;
            Ok(Expr::Str(s, src_pos))
        }
        Token::Symbol(s) => {
            let s = s.clone();
            *pos += 1;
            Ok(Expr::Symbol(s, src_pos))
        }
        Token::Char(c) => {
            let c = *c;
            *pos += 1;
            Ok(Expr::Char(c, src_pos))
        }
        Token::Quote => {
            *pos += 1;
            let inner = parse(tokens, pos)?;
            Ok(Expr::List(vec![Expr::Symbol("quote".into(), src_pos), inner], src_pos))
        }
        Token::LParen => {
            *pos += 1;
            let mut elems = Vec::new();
            while *pos < tokens.len() && tokens[*pos].token != Token::RParen {
                elems.push(parse(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse(format!("{src_pos}: missing closing paren")));
            }
            *pos += 1;
            Ok(Expr::List(elems, src_pos))
        }
        Token::RParen => Err(EvalError::Parse(format!("{src_pos}: unexpected )"))),
    }
}

pub(crate) fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input)?;
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse(&tokens, &mut pos)?);
    }
    Ok(exprs)
}
