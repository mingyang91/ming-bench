use crate::scheme::expr::Expr;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    Quote,
    Atom(String),
    Str(String),
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, String> {
    let mut tokenizer = Tokenizer::new(input);
    tokenizer.run()
}

struct Tokenizer {
    chars: Vec<char>,
    pos: usize,
}

impl Tokenizer {
    fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            pos: 0,
        }
    }

    fn run(&mut self) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();
        while self.pos < self.chars.len() {
            let tok = self.next_token()?;
            tokens.extend(tok);
        }
        Ok(tokens)
    }

    fn next_token(&mut self) -> Result<Option<Token>, String> {
        let c = self.chars[self.pos];
        match c {
            ' ' | '\t' | '\n' | '\r' => {
                self.pos += 1;
                Ok(None)
            }
            ';' => {
                self.skip_comment();
                Ok(None)
            }
            '(' => {
                self.pos += 1;
                Ok(Some(Token::LParen))
            }
            ')' => {
                self.pos += 1;
                Ok(Some(Token::RParen))
            }
            '\'' => {
                self.pos += 1;
                Ok(Some(Token::Quote))
            }
            '"' => self.read_string().map(|s| Some(Token::Str(s))),
            _ => Ok(Some(self.read_atom())),
        }
    }

    fn skip_comment(&mut self) {
        while self.pos < self.chars.len() && self.chars[self.pos] != '\n' {
            self.pos += 1;
        }
    }

    fn read_string(&mut self) -> Result<String, String> {
        self.pos += 1; // skip opening "
        let mut s = String::new();
        while self.pos < self.chars.len() && self.chars[self.pos] != '"' {
            self.read_string_char(&mut s);
        }
        if self.pos >= self.chars.len() {
            return Err("unterminated string".into());
        }
        self.pos += 1; // skip closing "
        Ok(s)
    }

    fn read_string_char(&mut self, s: &mut String) {
        if self.chars[self.pos] != '\\' || self.pos + 1 >= self.chars.len() {
            s.push(self.chars[self.pos]);
            self.pos += 1;
            return;
        }
        self.pos += 1;
        match self.chars[self.pos] {
            'n' => s.push('\n'),
            't' => s.push('\t'),
            '\\' => s.push('\\'),
            '"' => s.push('"'),
            c => {
                s.push('\\');
                s.push(c);
            }
        }
        self.pos += 1;
    }

    fn read_atom(&mut self) -> Token {
        let start = self.pos;
        while self.pos < self.chars.len() && !is_delimiter(self.chars[self.pos]) {
            self.pos += 1;
        }
        let atom: String = self.chars[start..self.pos].iter().collect();
        Token::Atom(atom)
    }
}

fn is_delimiter(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';')
}

pub struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn parse_all(&mut self) -> Result<Vec<Expr>, String> {
        let mut exprs = Vec::new();
        while self.pos < self.tokens.len() {
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        if self.pos >= self.tokens.len() {
            return Err("unexpected end of input".into());
        }

        match &self.tokens[self.pos] {
            Token::LParen => self.parse_list(),
            Token::RParen => Err("unexpected )".into()),
            Token::Quote => self.parse_quote(),
            Token::Str(s) => {
                let s = s.clone();
                self.pos += 1;
                Ok(Expr::Str(s))
            }
            Token::Atom(a) => {
                let a = a.clone();
                self.pos += 1;
                parse_atom(&a)
            }
        }
    }

    fn parse_list(&mut self) -> Result<Expr, String> {
        self.pos += 1; // skip (
        let mut elems = Vec::new();
        while self.pos < self.tokens.len() && self.tokens[self.pos] != Token::RParen {
            elems.push(self.parse_expr()?);
        }
        if self.pos >= self.tokens.len() {
            return Err("unmatched (".into());
        }
        self.pos += 1; // skip )
        Ok(Expr::List(elems))
    }

    fn parse_quote(&mut self) -> Result<Expr, String> {
        self.pos += 1;
        let inner = self.parse_expr()?;
        Ok(Expr::List(vec![Expr::Symbol("quote".into()), inner]))
    }
}

fn parse_atom(s: &str) -> Result<Expr, String> {
    if s == "#t" {
        return Ok(Expr::Boolean(true));
    }
    if s == "#f" {
        return Ok(Expr::Boolean(false));
    }
    if let Ok(n) = s.parse::<i64>() {
        return Ok(Expr::Integer(n));
    }
    Ok(Expr::Symbol(s.to_string()))
}
