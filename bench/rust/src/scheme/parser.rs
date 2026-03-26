use crate::scheme::EvalError;

// --- Positions ---

#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) struct Pos {
    pub line: usize,
    pub col: usize,
}

impl Pos {
    pub fn new(line: usize, col: usize) -> Self {
        Pos { line, col }
    }
}

impl std::fmt::Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

// --- Tokenizer ---

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LParen,
    RParen,
    Integer(i64),
    Float(f64),
    Rational(i64, i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
}

#[derive(Debug, Clone)]
struct SpannedToken {
    token: Token,
    pos: Pos,
}

fn tokenize(input: &str) -> Result<Vec<SpannedToken>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line = 1usize;
    let mut col = 1usize;

    while i < chars.len() {
        match chars[i] {
            '\n' => { line += 1; col = 1; i += 1; }
            ' ' | '\t' | '\r' => { col += 1; i += 1; }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                    col += 1;
                }
            }
            '(' => { tokens.push(SpannedToken { token: Token::LParen, pos: Pos::new(line, col) }); i += 1; col += 1; }
            ')' => { tokens.push(SpannedToken { token: Token::RParen, pos: Pos::new(line, col) }); i += 1; col += 1; }
            '"' => {
                let start_pos = Pos::new(line, col);
                i += 1; col += 1;
                let mut s = String::new();
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        i += 1; col += 1;
                        match chars[i] {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            c => { s.push('\\'); s.push(c); }
                        }
                    } else if chars[i] == '\n' {
                        s.push('\n');
                        line += 1; col = 0;
                    } else {
                        s.push(chars[i]);
                    }
                    i += 1; col += 1;
                }
                if i >= chars.len() {
                    return Err(EvalError::Parse(format!("{start_pos}: unterminated string")));
                }
                i += 1; col += 1;
                tokens.push(SpannedToken { token: Token::Str(s), pos: start_pos });
            }
            '\'' => {
                tokens.push(SpannedToken { token: Token::Symbol("'".to_string()), pos: Pos::new(line, col) });
                i += 1; col += 1;
            }
            '#' => {
                let start_pos = Pos::new(line, col);
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => { tokens.push(SpannedToken { token: Token::Boolean(true), pos: start_pos }); i += 2; col += 2; }
                        'f' => { tokens.push(SpannedToken { token: Token::Boolean(false), pos: start_pos }); i += 2; col += 2; }
                        '\\' => {
                            // Character literal: #\x or #\space, #\newline, #\tab
                            i += 2; col += 2;
                            if i >= chars.len() {
                                return Err(EvalError::Parse(format!("{start_pos}: incomplete character literal")));
                            }
                            // Collect the name
                            let name_start = i;
                            while i < chars.len() && !chars[i].is_whitespace() && chars[i] != ')' && chars[i] != '(' {
                                i += 1; col += 1;
                            }
                            let name: String = chars[name_start..i].iter().collect();
                            let ch = match name.as_str() {
                                "space" => ' ',
                                "newline" => '\n',
                                "tab" => '\t',
                                s if s.len() == 1 => s.chars().next().expect("single-char string guaranteed non-empty"),
                                _ => return Err(EvalError::Parse(format!("{start_pos}: unknown character name: {name}"))),
                            };
                            tokens.push(SpannedToken { token: Token::Char(ch), pos: start_pos });
                        }
                        _ => return Err(EvalError::Parse(format!("{start_pos}: unexpected #-literal")))
                    }
                } else {
                    return Err(EvalError::Parse(format!("{start_pos}: unexpected #")));
                }
            }
            _ => {
                let start_pos = Pos::new(line, col);
                let start = i;
                while i < chars.len() && !matches!(chars[i], ' '|'\t'|'\n'|'\r'|'('|')'|'"'|';') {
                    i += 1; col += 1;
                }
                let word: String = chars[start..i].iter().collect();
                if let Ok(n) = word.parse::<i64>() {
                    tokens.push(SpannedToken { token: Token::Integer(n), pos: start_pos });
                } else if let Some(slash) = word.find('/') {
                    if let (Ok(n), Ok(d)) = (word[..slash].parse::<i64>(), word[slash+1..].parse::<i64>()) {
                        if d != 0 {
                            tokens.push(SpannedToken { token: Token::Rational(n, d), pos: start_pos });
                        } else {
                            tokens.push(SpannedToken { token: Token::Symbol(word), pos: start_pos });
                        }
                    } else {
                        tokens.push(SpannedToken { token: Token::Symbol(word), pos: start_pos });
                    }
                } else if let Ok(f) = word.parse::<f64>() {
                    tokens.push(SpannedToken { token: Token::Float(f), pos: start_pos });
                } else {
                    tokens.push(SpannedToken { token: Token::Symbol(word), pos: start_pos });
                }
            }
        }
    }
    Ok(tokens)
}

// --- Parser ---

#[derive(Debug, Clone)]
pub(super) struct Expr {
    pub kind: ExprKind,
    pub pos: Pos,
}

#[derive(Debug, Clone)]
pub(super) enum ExprKind {
    Integer(i64),
    Float(f64),
    Rational(i64, i64),
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<Expr>),
}

impl Expr {
    pub fn new(kind: ExprKind, pos: Pos) -> Self {
        Expr { kind, pos }
    }
}

fn parse_tokens(tokens: &[SpannedToken], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let spos = tokens[*pos].pos;
    match &tokens[*pos].token {
        Token::LParen => {
            *pos += 1;
            let mut items = Vec::new();
            while *pos < tokens.len() && tokens[*pos].token != Token::RParen {
                items.push(parse_tokens(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse(format!("{spos}: missing closing paren")));
            }
            *pos += 1;
            Ok(Expr::new(ExprKind::List(items), spos))
        }
        Token::RParen => Err(EvalError::Parse(format!("{spos}: unexpected )"))),
        Token::Integer(n) => { let n = *n; *pos += 1; Ok(Expr::new(ExprKind::Integer(n), spos)) }
        Token::Float(f) => { let f = *f; *pos += 1; Ok(Expr::new(ExprKind::Float(f), spos)) }
        Token::Rational(n, d) => { let (n, d) = (*n, *d); *pos += 1; Ok(Expr::new(ExprKind::Rational(n, d), spos)) }
        Token::Boolean(b) => { let b = *b; *pos += 1; Ok(Expr::new(ExprKind::Boolean(b), spos)) }
        Token::Str(s) => { let s = s.clone(); *pos += 1; Ok(Expr::new(ExprKind::Str(s), spos)) }
        Token::Char(c) => { let c = *c; *pos += 1; Ok(Expr::new(ExprKind::Char(c), spos)) }
        Token::Symbol(s) if s == "'" => {
            *pos += 1;
            let inner = parse_tokens(tokens, pos)?;
            Ok(Expr::new(ExprKind::List(vec![
                Expr::new(ExprKind::Symbol("quote".to_string()), spos),
                inner,
            ]), spos))
        }
        Token::Symbol(s) => { let s = s.clone(); *pos += 1; Ok(Expr::new(ExprKind::Symbol(s), spos)) }
    }
}

pub(super) fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input)?;
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse_tokens(&tokens, &mut pos)?);
    }
    Ok(exprs)
}
