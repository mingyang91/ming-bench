use crate::scheme::error::{EvalError, Span};

/// A parsed S-expression.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Integer(i64, Span),
    Rational(i64, i64, Span), // numerator, denominator
    Float(f64, Span),
    Boolean(bool, Span),
    String(String, Span),
    Char(char, Span),
    Symbol(String, Span),
    List(Vec<Expr>, Span),
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Integer(_, s)
            | Expr::Rational(_, _, s)
            | Expr::Float(_, s)
            | Expr::Boolean(_, s)
            | Expr::String(_, s)
            | Expr::Char(_, s)
            | Expr::Symbol(_, s)
            | Expr::List(_, s) => *s,
        }
    }
}

struct Lexer<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    line: usize,
    col: usize,
}

impl<'a> Lexer<'a> {
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

    fn advance(&mut self) -> Option<char> {
        let ch = self.chars.next()?;
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(ch)
    }

    fn span(&self) -> Span {
        Span {
            line: self.line,
            col: self.col,
        }
    }

    fn tokenize(&mut self) -> Vec<(String, Span)> {
        let mut tokens = Vec::new();
        while self.peek().is_some() {
            tokenize_one(self, &mut tokens);
        }
        tokens
    }
}

fn read_string(lexer: &mut Lexer) -> String {
    let mut s = String::from('"');
    while let Some(c) = lexer.advance() {
        s.push(c);
        if c == '"' { return s; }
        if c == '\\' { s.extend(lexer.advance()); }
    }
    s
}

fn skip_comment(lexer: &mut Lexer) {
    while lexer.peek().is_some_and(|&c| c != '\n') {
        lexer.advance();
    }
    lexer.advance(); // consume the newline
}

fn is_delimiter(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\n' | '\r' | '(' | ')')
}

fn read_atom(lexer: &mut Lexer) -> String {
    let mut atom = String::new();
    while lexer.peek().is_some_and(|&c| !is_delimiter(c)) {
        atom.push(lexer.advance().expect("peeked Some"));
    }
    atom
}

fn tokenize_one(lexer: &mut Lexer, tokens: &mut Vec<(String, Span)>) {
    let &ch = lexer.peek().expect("caller checked is_some");
    match ch {
        ' ' | '\t' | '\n' | '\r' => {
            lexer.advance();
        }
        '(' | ')' => {
            let span = lexer.span();
            tokens.push((ch.to_string(), span));
            lexer.advance();
        }
        '"' => {
            let span = lexer.span();
            lexer.advance();
            tokens.push((read_string(lexer), span));
        }
        '\'' => {
            let span = lexer.span();
            tokens.push(("'".to_string(), span));
            lexer.advance();
        }
        ';' => skip_comment(lexer),
        _ => {
            let span = lexer.span();
            tokens.push((read_atom(lexer), span));
        }
    }
}

/// Parse a sequence of tokens into an Expr.
fn parse_tokens(tokens: &[(String, Span)], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }

    let (token, span) = &tokens[*pos];
    let span = *span;
    *pos += 1;

    match token.as_str() {
        "(" => {
            let mut list = Vec::new();
            while *pos < tokens.len() && tokens[*pos].0 != ")" {
                list.push(parse_tokens(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse("missing closing parenthesis".into()));
            }
            *pos += 1; // consume ')'
            Ok(Expr::List(list, span))
        }
        ")" => Err(EvalError::Parse("unexpected ')'".into())),
        "'" => {
            let quoted = parse_tokens(tokens, pos)?;
            Ok(Expr::List(
                vec![Expr::Symbol("quote".into(), span), quoted],
                span,
            ))
        }
        _ => Ok(parse_atom(token, span)),
    }
}

/// Try to parse a rational literal like `1/3` or `-5/2`.
fn parse_rational(token: &str) -> Option<(i64, i64)> {
    let slash_pos = token.find('/')?;
    let (numer_s, rest) = token.split_at(slash_pos);
    let denom_s = &rest[1..];
    let n = numer_s.parse::<i64>().ok()?;
    let d = denom_s.parse::<i64>().ok()?;
    (d != 0).then_some((n, d))
}

/// Parse an atom token into an Expr.
fn parse_atom(token: &str, span: Span) -> Expr {
    if token == "#t" {
        return Expr::Boolean(true, span);
    }
    if token == "#f" {
        return Expr::Boolean(false, span);
    }
    if let Ok(n) = token.parse::<i64>() {
        return Expr::Integer(n, span);
    }
    // Rational literal: e.g. 1/3, -5/2
    if let Some((n, d)) = parse_rational(token) {
        return Expr::Rational(n, d, span);
    }
    // Float literal
    if let Ok(x) = token.parse::<f64>() {
        return Expr::Float(x, span);
    }
    if token.starts_with('"') && token.ends_with('"') {
        return Expr::String(token[1..token.len() - 1].to_string(), span);
    }
    if let Some(rest) = token.strip_prefix("#\\") {
        let ch = match rest {
            "space" => ' ',
            "newline" => '\n',
            "tab" => '\t',
            _ => rest.chars().next().unwrap_or(' '),
        };
        return Expr::Char(ch, span);
    }
    Expr::Symbol(token.to_string(), span)
}

/// Parse input string into a list of expressions.
pub fn parse(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = Lexer::new(input).tokenize();
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse_tokens(&tokens, &mut pos)?);
    }
    Ok(exprs)
}
