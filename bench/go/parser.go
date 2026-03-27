package ming

import (
	"strconv"
	"strings"
	"unicode"
)

type expr interface{}

type locatedExpr struct {
	form expr
	pos  SourcePos
}

type listExpr []locatedExpr
type symbolExpr string
type stringExpr string
type charExpr rune
type boolExpr bool
type numberExpr int

type tokenKind int

const (
	tokenLeftParen tokenKind = iota
	tokenRightParen
	tokenQuote
	tokenAtom
	tokenString
)

type token struct {
	kind  tokenKind
	value string
	pos   SourcePos
}

type parser struct {
	tokens []token
	pos    int
}

func parseProgram(input string) ([]locatedExpr, error) {
	tokens, err := tokenize(input)
	if err != nil {
		return nil, err
	}

	p := parser{tokens: tokens}
	var exprs []locatedExpr
	for p.hasNext() {
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, expr)
	}

	if len(exprs) == 0 {
		return nil, newEvalError(defaultSourcePos(), "empty input")
	}

	return exprs, nil
}

func tokenize(input string) ([]token, error) {
	var tokens []token
	line := 1
	col := 1

	advance := func(ch byte) {
		if ch == '\n' {
			line++
			col = 1
			return
		}
		col++
	}

	for i := 0; i < len(input); {
		r := rune(input[i])
		if unicode.IsSpace(r) {
			advance(input[i])
			i++
			continue
		}

		if input[i] == ';' {
			for i < len(input) && input[i] != '\n' {
				advance(input[i])
				i++
			}
			continue
		}

		pos := SourcePos{Line: line, Col: col}

		switch input[i] {
		case '(':
			tokens = append(tokens, token{kind: tokenLeftParen, value: "(", pos: pos})
			advance(input[i])
			i++
		case ')':
			tokens = append(tokens, token{kind: tokenRightParen, value: ")", pos: pos})
			advance(input[i])
			i++
		case '\'':
			tokens = append(tokens, token{kind: tokenQuote, value: "'", pos: pos})
			advance(input[i])
			i++
		case '"':
			value, next, nextPos, err := readStringToken(input, i, pos)
			if err != nil {
				return nil, err
			}
			tokens = append(tokens, token{kind: tokenString, value: value, pos: pos})
			i = next
			line = nextPos.Line
			col = nextPos.Col
		default:
			start := i
			for i < len(input) &&
				!unicode.IsSpace(rune(input[i])) &&
				input[i] != '(' &&
				input[i] != ')' &&
				input[i] != '\'' {
				advance(input[i])
				i++
			}
			tokens = append(tokens, token{kind: tokenAtom, value: input[start:i], pos: pos})
		}
	}

	return tokens, nil
}

func readStringToken(input string, start int, startPos SourcePos) (string, int, SourcePos, error) {
	var builder strings.Builder
	line := startPos.Line
	col := startPos.Col

	advance := func(ch byte) {
		if ch == '\n' {
			line++
			col = 1
			return
		}
		col++
	}

	advance('"')

	for i := start + 1; i < len(input); i++ {
		switch input[i] {
		case '"':
			advance(input[i])
			return builder.String(), i + 1, SourcePos{Line: line, Col: col}, nil
		case '\\':
			advance(input[i])
			i++
			if i >= len(input) {
				return "", 0, SourcePos{}, newEvalError(startPos, "unterminated string literal")
			}

			switch input[i] {
			case '"', '\\':
				builder.WriteByte(input[i])
			case 'n':
				builder.WriteByte('\n')
			case 't':
				builder.WriteByte('\t')
			case 'r':
				builder.WriteByte('\r')
			default:
				builder.WriteByte(input[i])
			}
			advance(input[i])
		default:
			builder.WriteByte(input[i])
			advance(input[i])
		}
	}

	return "", 0, SourcePos{}, newEvalError(startPos, "unterminated string literal")
}

func (p *parser) parseExpr() (locatedExpr, error) {
	if !p.hasNext() {
		return locatedExpr{}, newEvalError(defaultSourcePos(), "unexpected end of input")
	}

	tok := p.tokens[p.pos]
	p.pos++

	switch tok.kind {
	case tokenLeftParen:
		items, err := p.parseList(tok.pos)
		if err != nil {
			return locatedExpr{}, err
		}
		return locatedExpr{form: items, pos: tok.pos}, nil
	case tokenRightParen:
		return locatedExpr{}, newEvalError(tok.pos, "unexpected ')'")
	case tokenQuote:
		quoted, err := p.parseExpr()
		if err != nil {
			return locatedExpr{}, err
		}
		return locatedExpr{
			form: listExpr{
				{form: symbolExpr("quote"), pos: tok.pos},
				quoted,
			},
			pos: tok.pos,
		}, nil
	case tokenString:
		return locatedExpr{form: stringExpr(tok.value), pos: tok.pos}, nil
	case tokenAtom:
		return locatedExpr{form: parseAtom(tok.value), pos: tok.pos}, nil
	default:
		return locatedExpr{}, newEvalError(tok.pos, "unknown token: %q", tok.value)
	}
}

func (p *parser) parseList(startPos SourcePos) (listExpr, error) {
	var items []locatedExpr

	for {
		if !p.hasNext() {
			return nil, newEvalError(startPos, "unterminated list")
		}
		if p.tokens[p.pos].kind == tokenRightParen {
			p.pos++
			return listExpr(items), nil
		}

		item, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		items = append(items, item)
	}
}

func (p *parser) hasNext() bool {
	return p.pos < len(p.tokens)
}

func parseAtom(raw string) expr {
	switch raw {
	case "#t":
		return boolExpr(true)
	case "#f":
		return boolExpr(false)
	}

	if ch, ok := parseCharLiteral(raw); ok {
		return charExpr(ch)
	}

	if n, err := strconv.Atoi(raw); err == nil {
		return numberExpr(n)
	}

	return symbolExpr(raw)
}

func parseCharLiteral(raw string) (rune, bool) {
	if !strings.HasPrefix(raw, "#\\") {
		return 0, false
	}

	literal := raw[2:]
	switch literal {
	case "space":
		return ' ', true
	case "newline":
		return '\n', true
	}

	runes := []rune(literal)
	if len(runes) == 1 {
		return runes[0], true
	}

	return 0, false
}
