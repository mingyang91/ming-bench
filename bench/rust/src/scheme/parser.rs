//! Tokenizer and parser for Scheme expressions.

use super::{gcd, EvalError, Expr, ExprKind, Span};

struct Token {
    text: String,
    span: Span,
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    let mut line = 1usize;
    let mut col = 1usize;
    while let Some(&c) = chars.peek() {
        match c {
            '\n' => { chars.next(); line += 1; col = 1; }
            ' ' | '\t' | '\r' | '\x0b' | '\x0c' => { chars.next(); col += 1; }
            ';' => {
                while let Some(&c2) = chars.peek() {
                    chars.next();
                    col += 1;
                    if c2 == '\n' { line += 1; col = 1; break; }
                }
            }
            '(' => { tokens.push(Token { text: "(".into(), span: Span::new(line, col) }); chars.next(); col += 1; }
            ')' => { tokens.push(Token { text: ")".into(), span: Span::new(line, col) }); chars.next(); col += 1; }
            '\'' => { tokens.push(Token { text: "'".into(), span: Span::new(line, col) }); chars.next(); col += 1; }
            '`' => { tokens.push(Token { text: "`".into(), span: Span::new(line, col) }); chars.next(); col += 1; }
            ',' => {
                let start_span = Span::new(line, col);
                chars.next();
                col += 1;
                if chars.peek() == Some(&'@') {
                    chars.next();
                    col += 1;
                    tokens.push(Token { text: ",@".into(), span: start_span });
                } else {
                    tokens.push(Token { text: ",".into(), span: start_span });
                }
            }
            '"' => {
                let start_span = Span::new(line, col);
                chars.next();
                col += 1;
                let mut s = String::new();
                loop {
                    match chars.next() {
                        Some('\\') => {
                            col += 1;
                            match chars.next() {
                                Some('n') => { s.push('\n'); col += 1; }
                                Some('t') => { s.push('\t'); col += 1; }
                                Some('"') => { s.push('"'); col += 1; }
                                Some('\\') => { s.push('\\'); col += 1; }
                                Some(other) => { s.push('\\'); s.push(other); col += 1; }
                                None => break,
                            }
                        }
                        Some('"') => { col += 1; break; }
                        Some('\n') => { s.push('\n'); line += 1; col = 1; }
                        Some(c2) => { s.push(c2); col += 1; }
                        None => break,
                    }
                }
                tokens.push(Token { text: format!("\"{s}\""), span: start_span });
            }
            '#' => {
                let start_span = Span::new(line, col);
                chars.next();
                col += 1;
                if chars.peek() == Some(&'\'') {
                    chars.next();
                    col += 1;
                    tokens.push(Token { text: "#'".into(), span: start_span });
                } else {
                    let mut tok = String::from('#');
                    while let Some(&c2) = chars.peek() {
                        if c2 == '(' || c2 == ')' || c2 == ' ' || c2 == '\t' || c2 == '\n' || c2 == '\r' || c2 == '\x0b' || c2 == '\x0c' || c2 == ';' || c2 == '\'' || c2 == '`' || c2 == ',' || c2 == '"' {
                            break;
                        }
                        tok.push(c2);
                        chars.next();
                        col += 1;
                    }
                    tokens.push(Token { text: tok, span: start_span });
                }
            }
            _ => {
                let start_span = Span::new(line, col);
                let mut tok = String::new();
                while let Some(&c2) = chars.peek() {
                    if c2 == '(' || c2 == ')' || c2 == ' ' || c2 == '\t' || c2 == '\n' || c2 == '\r' || c2 == '\x0b' || c2 == '\x0c' || c2 == ';' || c2 == '\'' || c2 == '`' || c2 == ',' || c2 == '"' {
                        break;
                    }
                    tok.push(c2);
                    chars.next();
                    col += 1;
                }
                tokens.push(Token { text: tok, span: start_span });
            }
        }
    }
    tokens
}

fn parse(tokens: &[Token]) -> Result<(Expr, usize), EvalError> {
    if tokens.is_empty() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let tok = &tokens[0];
    let span = tok.span;
    if tok.text == "'" {
        let (inner, consumed) = parse(&tokens[1..])?;
        Ok((Expr::new(ExprKind::List(vec![
            Expr::new(ExprKind::Symbol("quote".into()), span),
            inner,
        ]), span), 1 + consumed))
    } else if tok.text == "`" {
        let (inner, consumed) = parse(&tokens[1..])?;
        Ok((Expr::new(ExprKind::List(vec![
            Expr::new(ExprKind::Symbol("quasiquote".into()), span),
            inner,
        ]), span), 1 + consumed))
    } else if tok.text == "," {
        let (inner, consumed) = parse(&tokens[1..])?;
        Ok((Expr::new(ExprKind::List(vec![
            Expr::new(ExprKind::Symbol("unquote".into()), span),
            inner,
        ]), span), 1 + consumed))
    } else if tok.text == ",@" {
        let (inner, consumed) = parse(&tokens[1..])?;
        Ok((Expr::new(ExprKind::List(vec![
            Expr::new(ExprKind::Symbol("unquote-splicing".into()), span),
            inner,
        ]), span), 1 + consumed))
    } else if tok.text == "#'" {
        let (inner, consumed) = parse(&tokens[1..])?;
        Ok((Expr::new(ExprKind::List(vec![
            Expr::new(ExprKind::Symbol("syntax".into()), span),
            inner,
        ]), span), 1 + consumed))
    } else if tok.text == "(" {
        let mut elems = Vec::new();
        let mut i = 1;
        while i < tokens.len() && tokens[i].text != ")" {
            // Check for dot notation: (a b . c)
            if tokens[i].text == "." && !elems.is_empty() {
                // Peek ahead: must have exactly one expr then ")"
                i += 1; // skip the dot
                if i >= tokens.len() {
                    return Err(EvalError::Parse(format!("unexpected end after dot at {span}")));
                }
                let (tail, consumed) = parse(&tokens[i..])?;
                i += consumed;
                if i >= tokens.len() || tokens[i].text != ")" {
                    return Err(EvalError::Parse(format!("expected ) after dotted tail at {span}")));
                }
                return Ok((Expr::new(ExprKind::DottedList(elems, Box::new(tail)), span), i + 1));
            }
            let (expr, consumed) = parse(&tokens[i..])?;
            elems.push(expr);
            i += consumed;
        }
        if i >= tokens.len() {
            return Err(EvalError::Parse(format!("missing closing paren at {span}")));
        }
        Ok((Expr::new(ExprKind::List(elems), span), i + 1))
    } else if tok.text == ")" {
        Err(EvalError::Parse(format!("unexpected ) at {span}")))
    } else if tok.text.starts_with('"') {
        let s = tok.text[1..tok.text.len()-1].to_string();
        Ok((Expr::new(ExprKind::Str(s), span), 1))
    } else if tok.text == "#t" {
        Ok((Expr::new(ExprKind::Bool(true), span), 1))
    } else if tok.text == "#f" {
        Ok((Expr::new(ExprKind::Bool(false), span), 1))
    } else if tok.text.starts_with("#\\") {
        let rest = &tok.text[2..];
        let ch = match rest {
            "space" => ' ',
            "newline" => '\n',
            "tab" => '\t',
            s if s.len() == 1 => s.chars().next().expect("single-char string is non-empty"),
            _ => return Err(EvalError::Parse(format!("unknown character literal: {} at {span}", tok.text))),
        };
        Ok((Expr::new(ExprKind::Char(ch), span), 1))
    } else if let Ok(n) = tok.text.parse::<i64>() {
        Ok((Expr::new(ExprKind::Int(n), span), 1))
    } else if let Some(pos) = tok.text.find('/') {
        // Try rational literal n/d
        let num_part = &tok.text[..pos];
        let den_part = &tok.text[pos+1..];
        if let (Ok(n), Ok(d)) = (num_part.parse::<i64>(), den_part.parse::<i64>()) {
            if d != 0 {
                // Simplify the rational
                let sign = if d < 0 { -1 } else { 1 };
                let nn = n * sign;
                let dd = d * sign;
                let g = gcd(nn.abs(), dd);
                let nn = nn / g;
                let dd = dd / g;
                if dd == 1 {
                    Ok((Expr::new(ExprKind::Int(nn), span), 1))
                } else {
                    Ok((Expr::new(ExprKind::Rational(nn, dd), span), 1))
                }
            } else {
                Ok((Expr::new(ExprKind::Symbol(tok.text.clone()), span), 1))
            }
        } else {
            Ok((Expr::new(ExprKind::Symbol(tok.text.clone()), span), 1))
        }
    } else if let Ok(x) = tok.text.parse::<f64>() {
        Ok((Expr::new(ExprKind::Float(x), span), 1))
    } else {
        Ok((Expr::new(ExprKind::Symbol(tok.text.clone()), span), 1))
    }
}

pub(crate) fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input);
    let mut exprs = Vec::new();
    let mut i = 0;
    while i < tokens.len() {
        let (expr, consumed) = parse(&tokens[i..])?;
        exprs.push(expr);
        i += consumed;
    }
    Ok(exprs)
}
