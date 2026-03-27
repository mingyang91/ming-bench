package ming

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
)

type expr interface{}

type listExpr []expr
type symbolExpr string
type stringExpr string
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
}

type parser struct {
	tokens []token
	pos    int
}

func parseProgram(input string) ([]expr, error) {
	tokens, err := tokenize(input)
	if err != nil {
		return nil, err
	}

	p := parser{tokens: tokens}
	var exprs []expr
	for p.hasNext() {
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, expr)
	}

	if len(exprs) == 0 {
		return nil, &EvalError{Message: "empty input"}
	}

	return exprs, nil
}

func tokenize(input string) ([]token, error) {
	var tokens []token

	for i := 0; i < len(input); {
		r := rune(input[i])
		if unicode.IsSpace(r) {
			i++
			continue
		}

		if input[i] == ';' {
			for i < len(input) && input[i] != '\n' {
				i++
			}
			continue
		}

		switch input[i] {
		case '(':
			tokens = append(tokens, token{kind: tokenLeftParen, value: "("})
			i++
		case ')':
			tokens = append(tokens, token{kind: tokenRightParen, value: ")"})
			i++
		case '\'':
			tokens = append(tokens, token{kind: tokenQuote, value: "'"})
			i++
		case '"':
			value, next, err := readStringToken(input, i)
			if err != nil {
				return nil, err
			}
			tokens = append(tokens, token{kind: tokenString, value: value})
			i = next
		default:
			start := i
			for i < len(input) && !unicode.IsSpace(rune(input[i])) && input[i] != '(' && input[i] != ')' {
				i++
			}
			tokens = append(tokens, token{kind: tokenAtom, value: input[start:i]})
		}
	}

	return tokens, nil
}

func readStringToken(input string, start int) (string, int, error) {
	var builder strings.Builder

	for i := start + 1; i < len(input); i++ {
		switch input[i] {
		case '"':
			return builder.String(), i + 1, nil
		case '\\':
			i++
			if i >= len(input) {
				return "", 0, &EvalError{Message: "unterminated string literal"}
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
		default:
			builder.WriteByte(input[i])
		}
	}

	return "", 0, &EvalError{Message: "unterminated string literal"}
}

func (p *parser) parseExpr() (expr, error) {
	if !p.hasNext() {
		return nil, &EvalError{Message: "unexpected end of input"}
	}

	tok := p.tokens[p.pos]
	p.pos++

	switch tok.kind {
	case tokenLeftParen:
		return p.parseList()
	case tokenRightParen:
		return nil, &EvalError{Message: "unexpected ')'"}
	case tokenQuote:
		quoted, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		return listExpr{symbolExpr("quote"), quoted}, nil
	case tokenString:
		return stringExpr(tok.value), nil
	case tokenAtom:
		return parseAtom(tok.value), nil
	default:
		return nil, &EvalError{Message: fmt.Sprintf("unknown token: %q", tok.value)}
	}
}

func (p *parser) parseList() (expr, error) {
	var items []expr

	for {
		if !p.hasNext() {
			return nil, &EvalError{Message: "unterminated list"}
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

	if n, err := strconv.Atoi(raw); err == nil {
		return numberExpr(n)
	}

	return symbolExpr(raw)
}
