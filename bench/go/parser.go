package ming

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
)

type token struct {
	kind tokenKind
	val  string
	line int
	col  int
}

type tokenKind int

const (
	tokLParen tokenKind = iota
	tokRParen
	tokQuote
	tokAtom
	tokString
	tokDot
	tokEOF
)

func tokenize(input string) ([]token, error) {
	var tokens []token
	i := 0
	line := 1
	col := 1

	for i < len(input) {
		ch := input[i]

		// Skip whitespace
		if ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n' {
			if ch == '\n' {
				line++
				col = 1
			} else {
				col++
			}
			i++
			continue
		}

		// Skip line comments
		if ch == ';' {
			for i < len(input) && input[i] != '\n' {
				i++
			}
			continue
		}

		if ch == '(' {
			tokens = append(tokens, token{tokLParen, "(", line, col})
			i++
			col++
			continue
		}

		if ch == ')' {
			tokens = append(tokens, token{tokRParen, ")", line, col})
			i++
			col++
			continue
		}

		if ch == '\'' {
			tokens = append(tokens, token{tokQuote, "'", line, col})
			i++
			col++
			continue
		}

		// String literal
		if ch == '"' {
			startCol := col
			i++
			col++
			var buf strings.Builder
			for i < len(input) && input[i] != '"' {
				if input[i] == '\\' && i+1 < len(input) {
					i++
					col++
					switch input[i] {
					case 'n':
						buf.WriteByte('\n')
					case 't':
						buf.WriteByte('\t')
					case '"':
						buf.WriteByte('"')
					case '\\':
						buf.WriteByte('\\')
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
				return nil, fmt.Errorf("unterminated string at %d:%d", line, startCol)
			}
			i++ // skip closing quote
			col++
			tokens = append(tokens, token{tokString, buf.String(), line, startCol})
			continue
		}

		// Atom (number, symbol, boolean)
		if isAtomChar(ch) || ch == '#' {
			startCol := col
			start := i
			for i < len(input) && !isDelimiter(input[i]) {
				i++
				col++
			}
			word := input[start:i]
			// Check for dot
			if word == "." {
				tokens = append(tokens, token{tokDot, ".", line, startCol})
			} else {
				tokens = append(tokens, token{tokAtom, word, line, startCol})
			}
			continue
		}

		return nil, fmt.Errorf("unexpected character '%c' at %d:%d", ch, line, col)
	}

	tokens = append(tokens, token{tokEOF, "", line, col})
	return tokens, nil
}

func isDelimiter(ch byte) bool {
	return ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n' ||
		ch == '(' || ch == ')' || ch == '"' || ch == ';'
}

func isAtomChar(ch byte) bool {
	return !isDelimiter(ch) && ch != '\''
}

type parser struct {
	tokens []token
	pos    int
}

func (p *parser) peek() token {
	if p.pos >= len(p.tokens) {
		return token{tokEOF, "", 0, 0}
	}
	return p.tokens[p.pos]
}

func (p *parser) next() token {
	t := p.peek()
	p.pos++
	return t
}

func parse(input string) ([]*Value, error) {
	tokens, err := tokenize(input)
	if err != nil {
		return nil, err
	}

	p := &parser{tokens: tokens}
	var exprs []*Value
	for p.peek().kind != tokEOF {
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, expr)
	}
	return exprs, nil
}

func (p *parser) parseExpr() (*Value, error) {
	t := p.peek()
	switch t.kind {
	case tokLParen:
		return p.parseList()
	case tokQuote:
		p.next()
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		return pairVal(symVal("quote").withPos(t.line, t.col), pairVal(expr, nullVal())).withPos(t.line, t.col), nil
	case tokString:
		p.next()
		return strVal(t.val).withPos(t.line, t.col), nil
	case tokAtom:
		p.next()
		return parseAtom(t.val).withPos(t.line, t.col), nil
	default:
		return nil, fmt.Errorf("unexpected token %q at %d:%d", t.val, t.line, t.col)
	}
}

func (p *parser) parseList() (*Value, error) {
	open := p.next() // consume '('

	var elems []*Value
	for p.peek().kind != tokRParen && p.peek().kind != tokEOF {
		if p.peek().kind == tokDot {
			p.next() // consume '.'
			cdr, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			if p.peek().kind != tokRParen {
				return nil, fmt.Errorf("expected ')' after dot pair")
			}
			p.next() // consume ')'
			// Build the dotted pair
			result := cdr
			for i := len(elems) - 1; i >= 0; i-- {
				result = pairVal(elems[i], result).withPos(open.line, open.col)
			}
			return result, nil
		}
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		elems = append(elems, expr)
	}

	if p.peek().kind == tokEOF {
		return nil, fmt.Errorf("unterminated list")
	}
	p.next() // consume ')'

	// Build proper list
	result := nullVal()
	for i := len(elems) - 1; i >= 0; i-- {
		result = pairVal(elems[i], result).withPos(open.line, open.col)
	}
	return result, nil
}

func parseAtom(s string) *Value {
	if s == "#t" {
		return boolVal(true)
	}
	if s == "#f" {
		return boolVal(false)
	}
	// Try integer
	if n, err := strconv.ParseInt(s, 10, 64); err == nil {
		return intVal(n)
	}
	// Check for negative numbers with leading sign
	if len(s) > 1 && (s[0] == '-' || s[0] == '+') {
		if _, err := strconv.ParseInt(s, 10, 64); err == nil {
			n, _ := strconv.ParseInt(s, 10, 64)
			return intVal(n)
		}
	}
	return symVal(strings.ToLower(s))
}

func isSymbolStart(ch byte) bool {
	return unicode.IsLetter(rune(ch)) || strings.ContainsRune("!$%&*/:<=>?^_~+-", rune(ch))
}
