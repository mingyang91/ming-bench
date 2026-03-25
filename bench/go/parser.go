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
	tokQuote
	tokNumber
	tokString
	tokBool
	tokSymbol
	tokChar
	tokFloat
	tokRational
	tokVecOpen     // #(
	tokSyntaxQuote    // #'
	tokQuasiquote     // `
	tokUnquote        // ,
	tokUnquoteSplice  // ,@
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
		if ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n' || ch == '\f' {
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

		startCol := col

		if ch == '(' {
			tokens = append(tokens, token{tokLParen, "(", line, startCol})
			i++
			col++
			continue
		}
		if ch == ')' {
			tokens = append(tokens, token{tokRParen, ")", line, startCol})
			i++
			col++
			continue
		}
		if ch == '\'' {
			tokens = append(tokens, token{tokQuote, "'", line, startCol})
			i++
			col++
			continue
		}
		if ch == '`' {
			tokens = append(tokens, token{tokQuasiquote, "`", line, startCol})
			i++
			col++
			continue
		}
		if ch == ',' {
			if i+1 < len(input) && input[i+1] == '@' {
				tokens = append(tokens, token{tokUnquoteSplice, ",@", line, startCol})
				i += 2
				col += 2
			} else {
				tokens = append(tokens, token{tokUnquote, ",", line, startCol})
				i++
				col++
			}
			continue
		}

		// string literal
		if ch == '"' {
			var buf strings.Builder
			i++
			col++
			for i < len(input) && input[i] != '"' {
				if input[i] == '\\' && i+1 < len(input) {
					i++
					col++
					switch input[i] {
					case 'n':
						buf.WriteByte('\n')
					case 't':
						buf.WriteByte('\t')
					case '\\':
						buf.WriteByte('\\')
					case '"':
						buf.WriteByte('"')
					default:
						buf.WriteByte(input[i])
					}
				} else {
					if input[i] == '\n' {
						line++
						col = 0
					}
					buf.WriteByte(input[i])
				}
				i++
				col++
			}
			if i >= len(input) {
				return nil, fmt.Errorf("%d:%d: unterminated string", line, startCol)
			}
			i++ // skip closing "
			col++
			tokens = append(tokens, token{tokString, buf.String(), line, startCol})
			continue
		}

		// #t, #f, #\ character literals
		if ch == '#' {
			if i+1 < len(input) {
				next := input[i+1]
				if next == '\'' {
					tokens = append(tokens, token{tokSyntaxQuote, "#'", line, startCol})
					i += 2
					col += 2
					continue
				}
				if next == '(' {
					tokens = append(tokens, token{tokVecOpen, "#(", line, startCol})
					i += 2
					col += 2
					continue
				}
				if next == 't' && (i+2 >= len(input) || isDelimiter(input[i+2])) {
					tokens = append(tokens, token{tokBool, "#t", line, startCol})
					i += 2
					col += 2
					continue
				}
				if next == 'f' && (i+2 >= len(input) || isDelimiter(input[i+2])) {
					tokens = append(tokens, token{tokBool, "#f", line, startCol})
					i += 2
					col += 2
					continue
				}
				if next == '\\' {
					// character literal: #\x, #\space, #\newline, #\tab
					i += 2
					col += 2
					if i >= len(input) {
						return nil, fmt.Errorf("%d:%d: incomplete character literal", line, startCol)
					}
					// read the character name
					nameStart := i
					for i < len(input) && !isDelimiter(input[i]) {
						i++
						col++
					}
					name := input[nameStart:i]
					var ch rune
					switch strings.ToLower(name) {
					case "space":
						ch = ' '
					case "newline":
						ch = '\n'
					case "tab":
						ch = '\t'
					default:
						if len(name) == 1 {
							ch = rune(name[0])
						} else {
							return nil, fmt.Errorf("%d:%d: unknown character name: %s", line, startCol, name)
						}
					}
					tokens = append(tokens, token{tokChar, string(ch), line, startCol})
					continue
				}
			}
			// fall through to symbol
		}

		// number or symbol (including negative numbers)
		if isSymbolStart(ch) || ch == '+' || ch == '-' || unicode.IsDigit(rune(ch)) {
			start := i
			i++
			col++
			for i < len(input) && !isDelimiter(input[i]) {
				i++
				col++
			}
			text := input[start:i]

			// try to parse as integer
			if n, err := strconv.ParseInt(text, 10, 64); err == nil {
				tokens = append(tokens, token{tokNumber, text, line, startCol})
				_ = n
				continue
			}

			// try to parse as float (contains '.')
			if strings.ContainsRune(text, '.') {
				if _, err := strconv.ParseFloat(text, 64); err == nil {
					tokens = append(tokens, token{tokFloat, text, line, startCol})
					continue
				}
			}

			// try to parse as rational (e.g. 1/3, -5/2)
			if slashIdx := strings.Index(text, "/"); slashIdx > 0 && slashIdx < len(text)-1 {
				numStr := text[:slashIdx]
				denStr := text[slashIdx+1:]
				if _, err := strconv.ParseInt(numStr, 10, 64); err == nil {
					if _, err := strconv.ParseInt(denStr, 10, 64); err == nil {
						tokens = append(tokens, token{tokRational, text, line, startCol})
						continue
					}
				}
			}

			tokens = append(tokens, token{tokSymbol, text, line, startCol})
			continue
		}

		return nil, fmt.Errorf("%d:%d: unexpected character '%c'", line, col, ch)
	}

	tokens = append(tokens, token{tokEOF, "", line, col})
	return tokens, nil
}

func isDelimiter(ch byte) bool {
	return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' ||
		ch == '(' || ch == ')' || ch == '"' || ch == ';' ||
		ch == '`' || ch == ','
}

func isSymbolStart(ch byte) bool {
	return ch == '!' || ch == '$' || ch == '%' || ch == '&' || ch == '*' ||
		ch == '/' || ch == ':' || ch == '<' || ch == '=' || ch == '>' ||
		ch == '?' || ch == '_' || ch == '~' || ch == '#' || ch == '.' ||
		ch == '^' ||
		(ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
}

// Expr is a parsed S-expression.
type Expr interface {
	pos() (int, int)
}

type NumberExpr struct {
	Val  int64
	Line int
	Col  int
}

type StringExpr struct {
	Val  string
	Line int
	Col  int
}

type BoolExpr struct {
	Val  bool
	Line int
	Col  int
}

type SymbolExpr struct {
	Name string
	Line int
	Col  int
}

type ListExpr struct {
	Elems []Expr
	Dot   Expr // non-nil for dotted pair: (a b . c)
	Line  int
	Col   int
}

type VectorExpr struct {
	Elems []Expr
	Line  int
	Col   int
}

type CharExpr struct {
	Val  rune
	Line int
	Col  int
}

type FloatExpr struct {
	Val  float64
	Line int
	Col  int
}

type RationalExpr struct {
	Num, Den int64
	Line     int
	Col      int
}

func (e *NumberExpr) pos() (int, int)   { return e.Line, e.Col }
func (e *StringExpr) pos() (int, int)   { return e.Line, e.Col }
func (e *BoolExpr) pos() (int, int)     { return e.Line, e.Col }
func (e *SymbolExpr) pos() (int, int)   { return e.Line, e.Col }
func (e *ListExpr) pos() (int, int)     { return e.Line, e.Col }
func (e *CharExpr) pos() (int, int)     { return e.Line, e.Col }
func (e *FloatExpr) pos() (int, int)    { return e.Line, e.Col }
func (e *RationalExpr) pos() (int, int) { return e.Line, e.Col }
func (e *VectorExpr) pos() (int, int)   { return e.Line, e.Col }

type parser struct {
	tokens []token
	pos    int
}

func parse(input string) ([]Expr, error) {
	tokens, err := tokenize(input)
	if err != nil {
		return nil, err
	}
	p := &parser{tokens: tokens}
	var exprs []Expr
	for p.peek().kind != tokEOF {
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, expr)
	}
	return exprs, nil
}

func (p *parser) peek() token {
	if p.pos >= len(p.tokens) {
		return token{kind: tokEOF}
	}
	return p.tokens[p.pos]
}

func (p *parser) next() token {
	t := p.peek()
	p.pos++
	return t
}

func (p *parser) parseExpr() (Expr, error) {
	t := p.peek()
	switch t.kind {
	case tokNumber:
		p.next()
		n, _ := strconv.ParseInt(t.text, 10, 64)
		return &NumberExpr{Val: n, Line: t.line, Col: t.col}, nil
	case tokString:
		p.next()
		return &StringExpr{Val: t.text, Line: t.line, Col: t.col}, nil
	case tokBool:
		p.next()
		return &BoolExpr{Val: t.text == "#t", Line: t.line, Col: t.col}, nil
	case tokChar:
		p.next()
		return &CharExpr{Val: rune(t.text[0]), Line: t.line, Col: t.col}, nil
	case tokFloat:
		p.next()
		f, _ := strconv.ParseFloat(t.text, 64)
		return &FloatExpr{Val: f, Line: t.line, Col: t.col}, nil
	case tokRational:
		p.next()
		slashIdx := strings.Index(t.text, "/")
		num, _ := strconv.ParseInt(t.text[:slashIdx], 10, 64)
		den, _ := strconv.ParseInt(t.text[slashIdx+1:], 10, 64)
		return &RationalExpr{Num: num, Den: den, Line: t.line, Col: t.col}, nil
	case tokQuote:
		p.next()
		inner, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		return &ListExpr{
			Elems: []Expr{&SymbolExpr{Name: "quote", Line: t.line, Col: t.col}, inner},
			Line:  t.line,
			Col:   t.col,
		}, nil
	case tokSyntaxQuote:
		p.next()
		inner, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		return &ListExpr{
			Elems: []Expr{&SymbolExpr{Name: "syntax", Line: t.line, Col: t.col}, inner},
			Line:  t.line,
			Col:   t.col,
		}, nil
	case tokQuasiquote:
		p.next()
		inner, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		return &ListExpr{
			Elems: []Expr{&SymbolExpr{Name: "quasiquote", Line: t.line, Col: t.col}, inner},
			Line:  t.line,
			Col:   t.col,
		}, nil
	case tokUnquote:
		p.next()
		inner, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		return &ListExpr{
			Elems: []Expr{&SymbolExpr{Name: "unquote", Line: t.line, Col: t.col}, inner},
			Line:  t.line,
			Col:   t.col,
		}, nil
	case tokUnquoteSplice:
		p.next()
		inner, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		return &ListExpr{
			Elems: []Expr{&SymbolExpr{Name: "unquote-splicing", Line: t.line, Col: t.col}, inner},
			Line:  t.line,
			Col:   t.col,
		}, nil
	case tokVecOpen:
		p.next()
		var elems []Expr
		for p.peek().kind != tokRParen {
			if p.peek().kind == tokEOF {
				return nil, fmt.Errorf("%d:%d: unexpected end of input in vector", t.line, t.col)
			}
			e, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			elems = append(elems, e)
		}
		p.next() // consume )
		return &VectorExpr{Elems: elems, Line: t.line, Col: t.col}, nil
	case tokLParen:
		p.next()
		var elems []Expr
		var dot Expr
		for p.peek().kind != tokRParen {
			if p.peek().kind == tokEOF {
				return nil, fmt.Errorf("%d:%d: unexpected end of input", t.line, t.col)
			}
			// Check for dotted pair: (a b . c)
			if pk := p.peek(); pk.kind == tokSymbol && pk.text == "." && len(elems) > 0 {
				p.next() // consume .
				var err error
				dot, err = p.parseExpr()
				if err != nil {
					return nil, err
				}
				if p.peek().kind != tokRParen {
					return nil, fmt.Errorf("%d:%d: expected ) after dotted pair", t.line, t.col)
				}
				break
			}
			e, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			elems = append(elems, e)
		}
		p.next() // consume )
		return &ListExpr{Elems: elems, Dot: dot, Line: t.line, Col: t.col}, nil
	case tokEOF:
		return nil, fmt.Errorf("unexpected end of input")
	default:
		p.next()
		return &SymbolExpr{Name: t.text, Line: t.line, Col: t.col}, nil
	}
}
