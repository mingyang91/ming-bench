package ming

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
)

// token types
type tokenKind int

const (
	tokLParen tokenKind = iota
	tokRParen
	tokNumber
	tokString
	tokBool
	tokSymbol
	tokQuote
	tokChar
	tokFloat
	tokRational
	tokSyntaxQuote
	tokEOF
)

type token struct {
	kind tokenKind
	text string
	line int
	col  int
}

// tokenize splits input into tokens.
func tokenize(input string) ([]token, error) {
	var tokens []token
	i := 0
	line := 1
	col := 1

	for i < len(input) {
		ch := input[i]

		// skip whitespace
		if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
			if ch == '\n' {
				line++
				col = 1
			} else {
				col++
			}
			i++
			continue
		}

		// skip line comments
		if ch == ';' {
			for i < len(input) && input[i] != '\n' {
				i++
			}
			continue
		}

		startLine, startCol := line, col

		if ch == '(' {
			tokens = append(tokens, token{tokLParen, "(", startLine, startCol})
			i++
			col++
			continue
		}
		if ch == ')' {
			tokens = append(tokens, token{tokRParen, ")", startLine, startCol})
			i++
			col++
			continue
		}
		if ch == '\'' {
			tokens = append(tokens, token{tokQuote, "'", startLine, startCol})
			i++
			col++
			continue
		}

		// string literal
		if ch == '"' {
			var buf strings.Builder
			buf.WriteByte('"')
			i++
			col++
			for i < len(input) && input[i] != '"' {
				if input[i] == '\\' && i+1 < len(input) {
					buf.WriteByte('\\')
					buf.WriteByte(input[i+1])
					i += 2
					col += 2
					continue
				}
				if input[i] == '\n' {
					line++
					col = 1
				} else {
					col++
				}
				buf.WriteByte(input[i])
				i++
			}
			if i >= len(input) {
				return nil, fmt.Errorf("%d:%d: unterminated string", startLine, startCol)
			}
			buf.WriteByte('"')
			i++ // closing quote
			col++
			tokens = append(tokens, token{tokString, buf.String(), startLine, startCol})
			continue
		}

		// #', #t, #f, #\char
		if ch == '#' && i+1 < len(input) {
			next := input[i+1]
			if next == '\'' {
				tokens = append(tokens, token{tokSyntaxQuote, "#'", startLine, startCol})
				i += 2
				col += 2
				continue
			}
			if next == 't' || next == 'f' {
				// check it's not part of a longer symbol
				if i+2 >= len(input) || isDelimiter(input[i+2]) {
					text := string([]byte{ch, next})
					tokens = append(tokens, token{tokBool, text, startLine, startCol})
					i += 2
					col += 2
					continue
				}
			}
			if next == '\\' {
				// character literal
				if i+2 >= len(input) {
					return nil, fmt.Errorf("%d:%d: incomplete character literal", startLine, startCol)
				}
				// read the character name
				j := i + 2
				// read word characters for named chars (space, newline, tab)
				if j < len(input) && ((input[j] >= 'a' && input[j] <= 'z') || (input[j] >= 'A' && input[j] <= 'Z')) {
					start := j
					for j < len(input) && !isDelimiter(input[j]) {
						j++
					}
					name := input[start:j]
					text := input[i:j]
					col += j - i
					i = j
					tokens = append(tokens, token{tokChar, text, startLine, startCol})
					_ = name
					continue
				}
				// single non-alpha character like #\( or #\)
				text := input[i : i+3]
				i += 3
				col += 3
				tokens = append(tokens, token{tokChar, text, startLine, startCol})
				continue
			}
		}

		// number or symbol (including negative numbers)
		if isSymbolStart(ch) || ch == '-' || ch == '+' || unicode.IsDigit(rune(ch)) {
			start := i
			i++
			col++
			for i < len(input) && !isDelimiter(input[i]) {
				i++
				col++
			}
			text := input[start:i]
			// try to parse as integer
			if _, err := strconv.ParseInt(text, 10, 64); err == nil {
				tokens = append(tokens, token{tokNumber, text, startLine, startCol})
			} else if isRationalLiteral(text) {
				tokens = append(tokens, token{tokRational, text, startLine, startCol})
			} else if _, err := strconv.ParseFloat(text, 64); err == nil {
				tokens = append(tokens, token{tokFloat, text, startLine, startCol})
			} else {
				tokens = append(tokens, token{tokSymbol, text, startLine, startCol})
			}
			continue
		}

		return nil, fmt.Errorf("%d:%d: unexpected character %q", line, col, ch)
	}
	tokens = append(tokens, token{tokEOF, "", line, col})
	return tokens, nil
}

func isDelimiter(ch byte) bool {
	return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' ||
		ch == '(' || ch == ')' || ch == '"' || ch == ';'
}

// isRationalLiteral checks if text matches pattern like "1/3", "-2/5", etc.
func isRationalLiteral(text string) bool {
	slash := strings.IndexByte(text, '/')
	if slash <= 0 || slash == len(text)-1 {
		return false
	}
	numPart := text[:slash]
	denomPart := text[slash+1:]
	if _, err := strconv.ParseInt(numPart, 10, 64); err != nil {
		return false
	}
	if _, err := strconv.ParseInt(denomPart, 10, 64); err != nil {
		return false
	}
	return true
}

func isSymbolStart(ch byte) bool {
	return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') ||
		ch == '!' || ch == '$' || ch == '%' || ch == '&' || ch == '*' ||
		ch == '/' || ch == ':' || ch == '<' || ch == '=' || ch == '>' ||
		ch == '?' || ch == '_' || ch == '~' || ch == '^' || ch == '.'
}

// Expr is a parsed Scheme expression with position info.
type Expr struct {
	Kind ExprKind
	// atom value
	IVal   int64
	FVal   float64
	Num    int64 // rational numerator
	Denom  int64 // rational denominator
	SVal   string
	BVal   bool
	RVal   rune
	// list
	List []*Expr
	// position
	Line, Col int
}

type ExprKind int

const (
	ExprInt ExprKind = iota
	ExprBool
	ExprString
	ExprSymbol
	ExprList
	ExprChar
	ExprFloat
	ExprRational
)

// parser
type parser struct {
	tokens []token
	pos    int
}

func (p *parser) peek() token {
	return p.tokens[p.pos]
}

func (p *parser) next() token {
	t := p.tokens[p.pos]
	p.pos++
	return t
}

func parse(input string) ([]*Expr, error) {
	tokens, err := tokenize(input)
	if err != nil {
		return nil, err
	}
	p := &parser{tokens: tokens}
	var exprs []*Expr
	for p.peek().kind != tokEOF {
		e, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, e)
	}
	return exprs, nil
}

func (p *parser) parseExpr() (*Expr, error) {
	t := p.peek()

	switch t.kind {
	case tokNumber:
		p.next()
		n, _ := strconv.ParseInt(t.text, 10, 64)
		return &Expr{Kind: ExprInt, IVal: n, Line: t.line, Col: t.col}, nil

	case tokFloat:
		p.next()
		f, _ := strconv.ParseFloat(t.text, 64)
		return &Expr{Kind: ExprFloat, FVal: f, Line: t.line, Col: t.col}, nil

	case tokRational:
		p.next()
		slash := strings.IndexByte(t.text, '/')
		num, _ := strconv.ParseInt(t.text[:slash], 10, 64)
		denom, _ := strconv.ParseInt(t.text[slash+1:], 10, 64)
		return &Expr{Kind: ExprRational, Num: num, Denom: denom, Line: t.line, Col: t.col}, nil

	case tokBool:
		p.next()
		return &Expr{Kind: ExprBool, BVal: t.text == "#t", Line: t.line, Col: t.col}, nil

	case tokChar:
		p.next()
		name := t.text[2:] // strip #\
		var r rune
		switch strings.ToLower(name) {
		case "space":
			r = ' '
		case "newline":
			r = '\n'
		case "tab":
			r = '\t'
		default:
			runes := []rune(name)
			if len(runes) != 1 {
				return nil, fmt.Errorf("%d:%d: invalid character literal %s", t.line, t.col, t.text)
			}
			r = runes[0]
		}
		return &Expr{Kind: ExprChar, RVal: r, Line: t.line, Col: t.col}, nil

	case tokString:
		p.next()
		// unescape
		s, err := strconv.Unquote(t.text)
		if err != nil {
			return nil, fmt.Errorf("%d:%d: invalid string literal", t.line, t.col)
		}
		return &Expr{Kind: ExprString, SVal: s, Line: t.line, Col: t.col}, nil

	case tokSymbol:
		p.next()
		return &Expr{Kind: ExprSymbol, SVal: t.text, Line: t.line, Col: t.col}, nil

	case tokQuote:
		p.next()
		inner, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		return &Expr{
			Kind: ExprList,
			List: []*Expr{
				{Kind: ExprSymbol, SVal: "quote", Line: t.line, Col: t.col},
				inner,
			},
			Line: t.line, Col: t.col,
		}, nil

	case tokSyntaxQuote:
		p.next()
		inner, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		return &Expr{
			Kind: ExprList,
			List: []*Expr{
				{Kind: ExprSymbol, SVal: "syntax", Line: t.line, Col: t.col},
				inner,
			},
			Line: t.line, Col: t.col,
		}, nil

	case tokLParen:
		p.next()
		var list []*Expr
		for p.peek().kind != tokRParen {
			if p.peek().kind == tokEOF {
				return nil, fmt.Errorf("%d:%d: unexpected end of input", t.line, t.col)
			}
			e, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			list = append(list, e)
		}
		p.next() // consume ')'
		return &Expr{Kind: ExprList, List: list, Line: t.line, Col: t.col}, nil

	case tokRParen:
		return nil, fmt.Errorf("%d:%d: unexpected ')'", t.line, t.col)

	default:
		return nil, fmt.Errorf("%d:%d: unexpected token", t.line, t.col)
	}
}
