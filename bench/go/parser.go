package ming

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
)

// Token types
type tokenKind int

const (
	tokLParen tokenKind = iota
	tokRParen
	tokQuote
	tokNumber
	tokFloat
	tokRational
	tokString
	tokBool
	tokChar
	tokSymbol
	tokEOF
)

type token struct {
	kind tokenKind
	text string
	line int
	col  int
}

// Tokenizer
type tokenizer struct {
	input []rune
	pos   int
	line  int
	col   int
}

func newTokenizer(input string) *tokenizer {
	return &tokenizer{input: []rune(input), pos: 0, line: 1, col: 1}
}

func (t *tokenizer) peek() rune {
	if t.pos >= len(t.input) {
		return 0
	}
	return t.input[t.pos]
}

func (t *tokenizer) advance() rune {
	ch := t.input[t.pos]
	t.pos++
	if ch == '\n' {
		t.line++
		t.col = 1
	} else {
		t.col++
	}
	return ch
}

func (t *tokenizer) skipWhitespaceAndComments() {
	for t.pos < len(t.input) {
		ch := t.peek()
		if unicode.IsSpace(ch) {
			t.advance()
		} else if ch == ';' {
			for t.pos < len(t.input) && t.peek() != '\n' {
				t.advance()
			}
		} else {
			break
		}
	}
}

func isSymbolChar(ch rune) bool {
	if unicode.IsSpace(ch) {
		return false
	}
	switch ch {
	case '(', ')', '"', ';', '\'':
		return false
	}
	return ch > 0
}

func (t *tokenizer) nextToken() token {
	t.skipWhitespaceAndComments()
	if t.pos >= len(t.input) {
		return token{kind: tokEOF, line: t.line, col: t.col}
	}

	line, col := t.line, t.col
	ch := t.peek()

	switch {
	case ch == '(':
		t.advance()
		return token{kind: tokLParen, text: "(", line: line, col: col}
	case ch == ')':
		t.advance()
		return token{kind: tokRParen, text: ")", line: line, col: col}
	case ch == '\'':
		t.advance()
		return token{kind: tokQuote, text: "'", line: line, col: col}
	case ch == '#':
		t.advance()
		next := t.peek()
		if next == '\\' {
			// Character literal: #\x, #\space, #\newline
			t.advance() // consume backslash
			if t.pos >= len(t.input) {
				return token{kind: tokChar, text: " ", line: line, col: col}
			}
			// Read the character name
			first := t.advance()
			var name strings.Builder
			name.WriteRune(first)
			for t.pos < len(t.input) && isSymbolChar(t.peek()) {
				name.WriteRune(t.advance())
			}
			charName := name.String()
			switch strings.ToLower(charName) {
			case "space":
				return token{kind: tokChar, text: " ", line: line, col: col}
			case "newline":
				return token{kind: tokChar, text: "\n", line: line, col: col}
			case "tab":
				return token{kind: tokChar, text: "\t", line: line, col: col}
			default:
				// Single character
				if len([]rune(charName)) == 1 {
					return token{kind: tokChar, text: charName, line: line, col: col}
				}
				return token{kind: tokSymbol, text: "#\\" + charName, line: line, col: col}
			}
		}
		if next == 't' || next == 'f' {
			t.advance()
			// Make sure it's not part of a longer symbol
			if t.pos >= len(t.input) || !isSymbolChar(t.peek()) || t.peek() == '(' || t.peek() == ')' {
				val := "#" + string(next)
				return token{kind: tokBool, text: val, line: line, col: col}
			}
		}
		// Read rest as symbol
		var buf strings.Builder
		buf.WriteByte('#')
		buf.WriteRune(next)
		for t.pos < len(t.input) && isSymbolChar(t.peek()) {
			buf.WriteRune(t.advance())
		}
		return token{kind: tokSymbol, text: buf.String(), line: line, col: col}
	case ch == '"':
		t.advance() // opening quote
		var buf strings.Builder
		for t.pos < len(t.input) && t.peek() != '"' {
			c := t.advance()
			if c == '\\' && t.pos < len(t.input) {
				esc := t.advance()
				switch esc {
				case 'n':
					buf.WriteByte('\n')
				case 't':
					buf.WriteByte('\t')
				case '\\':
					buf.WriteByte('\\')
				case '"':
					buf.WriteByte('"')
				default:
					buf.WriteByte('\\')
					buf.WriteRune(esc)
				}
			} else {
				buf.WriteRune(c)
			}
		}
		if t.pos < len(t.input) {
			t.advance() // closing quote
		}
		return token{kind: tokString, text: buf.String(), line: line, col: col}
	default:
		// Number or symbol
		var buf strings.Builder
		for t.pos < len(t.input) && isSymbolChar(t.peek()) {
			buf.WriteRune(t.advance())
		}
		text := buf.String()
		// Check if it's a number
		if _, err := strconv.ParseInt(text, 10, 64); err == nil {
			return token{kind: tokNumber, text: text, line: line, col: col}
		}
		// Check if it's a float (e.g., 1.5, .5, 0.5)
		if _, err := strconv.ParseFloat(text, 64); err == nil && strings.ContainsAny(text, ".eE") {
			return token{kind: tokFloat, text: text, line: line, col: col}
		}
		// Check if it's a rational literal (e.g., 1/3, -6/4)
		if isRationalLiteral(text) {
			return token{kind: tokRational, text: text, line: line, col: col}
		}
		return token{kind: tokSymbol, text: text, line: line, col: col}
	}
}

func isRationalLiteral(text string) bool {
	slashIdx := strings.Index(text, "/")
	if slashIdx <= 0 || slashIdx == len(text)-1 {
		return false
	}
	numStr := text[:slashIdx]
	denStr := text[slashIdx+1:]
	if _, err := strconv.ParseInt(numStr, 10, 64); err != nil {
		return false
	}
	if _, err := strconv.ParseInt(denStr, 10, 64); err != nil {
		return false
	}
	return true
}

// AST nodes
type Expr interface {
	Line() int
	Col() int
}

type NumberExpr struct {
	Val      int64
	Ln, Cl   int
}
func (e *NumberExpr) Line() int { return e.Ln }
func (e *NumberExpr) Col() int  { return e.Cl }

type FloatExpr struct {
	Val    float64
	Ln, Cl int
}
func (e *FloatExpr) Line() int { return e.Ln }
func (e *FloatExpr) Col() int  { return e.Cl }

type RationalExpr struct {
	Num, Den int64
	Ln, Cl   int
}
func (e *RationalExpr) Line() int { return e.Ln }
func (e *RationalExpr) Col() int  { return e.Cl }

type BoolExpr struct {
	Val    bool
	Ln, Cl int
}
func (e *BoolExpr) Line() int { return e.Ln }
func (e *BoolExpr) Col() int  { return e.Cl }

type StringExpr struct {
	Val    string
	Ln, Cl int
}
func (e *StringExpr) Line() int { return e.Ln }
func (e *StringExpr) Col() int  { return e.Cl }

type CharExpr struct {
	Val    rune
	Ln, Cl int
}
func (e *CharExpr) Line() int { return e.Ln }
func (e *CharExpr) Col() int  { return e.Cl }

type SymbolExpr struct {
	Name   string
	Ln, Cl int
}
func (e *SymbolExpr) Line() int { return e.Ln }
func (e *SymbolExpr) Col() int  { return e.Cl }

type ListExpr struct {
	Items  []Expr
	Ln, Cl int
}
func (e *ListExpr) Line() int { return e.Ln }
func (e *ListExpr) Col() int  { return e.Cl }

// Parser
type parser struct {
	tok     *tokenizer
	current token
}

func newParser(input string) *parser {
	t := newTokenizer(input)
	p := &parser{tok: t}
	p.current = t.nextToken()
	return p
}

func (p *parser) next() token {
	tok := p.current
	p.current = p.tok.nextToken()
	return tok
}

func (p *parser) parseExpr() (Expr, error) {
	tok := p.current
	switch tok.kind {
	case tokEOF:
		return nil, fmt.Errorf("unexpected end of input")
	case tokNumber:
		p.next()
		val, _ := strconv.ParseInt(tok.text, 10, 64)
		return &NumberExpr{Val: val, Ln: tok.line, Cl: tok.col}, nil
	case tokFloat:
		p.next()
		val, _ := strconv.ParseFloat(tok.text, 64)
		return &FloatExpr{Val: val, Ln: tok.line, Cl: tok.col}, nil
	case tokRational:
		p.next()
		slashIdx := strings.Index(tok.text, "/")
		num, _ := strconv.ParseInt(tok.text[:slashIdx], 10, 64)
		den, _ := strconv.ParseInt(tok.text[slashIdx+1:], 10, 64)
		return &RationalExpr{Num: num, Den: den, Ln: tok.line, Cl: tok.col}, nil
	case tokBool:
		p.next()
		return &BoolExpr{Val: tok.text == "#t", Ln: tok.line, Cl: tok.col}, nil
	case tokString:
		p.next()
		return &StringExpr{Val: tok.text, Ln: tok.line, Cl: tok.col}, nil
	case tokChar:
		p.next()
		runes := []rune(tok.text)
		return &CharExpr{Val: runes[0], Ln: tok.line, Cl: tok.col}, nil
	case tokSymbol:
		p.next()
		return &SymbolExpr{Name: tok.text, Ln: tok.line, Cl: tok.col}, nil
	case tokQuote:
		p.next()
		inner, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		return &ListExpr{
			Items: []Expr{
				&SymbolExpr{Name: "quote", Ln: tok.line, Cl: tok.col},
				inner,
			},
			Ln: tok.line, Cl: tok.col,
		}, nil
	case tokLParen:
		p.next()
		var items []Expr
		for p.current.kind != tokRParen {
			if p.current.kind == tokEOF {
				return nil, fmt.Errorf("unexpected end of input, expected ')'")
			}
			item, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			items = append(items, item)
		}
		p.next() // consume ')'
		return &ListExpr{Items: items, Ln: tok.line, Cl: tok.col}, nil
	case tokRParen:
		return nil, fmt.Errorf("%d:%d: unexpected ')'", tok.line, tok.col)
	default:
		return nil, fmt.Errorf("unexpected token: %s", tok.text)
	}
}

func parse(input string) ([]Expr, error) {
	p := newParser(input)
	var exprs []Expr
	for p.current.kind != tokEOF {
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, expr)
	}
	return exprs, nil
}
