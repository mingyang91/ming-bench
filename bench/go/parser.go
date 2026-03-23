package ming

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
)

// Token types
type TokenType int

const (
	TokenLParen TokenType = iota
	TokenRParen
	TokenNumber
	TokenBool
	TokenString
	TokenSymbol
	TokenEOF
)

type Token struct {
	Type  TokenType
	Value string
	Line  int
	Col   int
}

// Tokenize breaks input into tokens.
func Tokenize(input string) ([]Token, error) {
	var tokens []Token
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

		// Skip comments
		if ch == ';' {
			for i < len(input) && input[i] != '\n' {
				i++
			}
			continue
		}

		// Parentheses
		if ch == '(' {
			tokens = append(tokens, Token{Type: TokenLParen, Value: "(", Line: line, Col: col})
			i++
			col++
			continue
		}
		if ch == ')' {
			tokens = append(tokens, Token{Type: TokenRParen, Value: ")", Line: line, Col: col})
			i++
			col++
			continue
		}

		// String literal
		if ch == '"' {
			startCol := col
			i++
			col++
			var sb strings.Builder
			for i < len(input) && input[i] != '"' {
				if input[i] == '\\' && i+1 < len(input) {
					i++
					col++
					switch input[i] {
					case 'n':
						sb.WriteByte('\n')
					case 't':
						sb.WriteByte('\t')
					case '"':
						sb.WriteByte('"')
					case '\\':
						sb.WriteByte('\\')
					default:
						sb.WriteByte(input[i])
					}
				} else {
					if input[i] == '\n' {
						line++
						col = 0
					}
					sb.WriteByte(input[i])
				}
				i++
				col++
			}
			if i >= len(input) {
				return nil, fmt.Errorf("%d:%d: unterminated string", line, startCol)
			}
			i++ // skip closing quote
			col++
			tokens = append(tokens, Token{Type: TokenString, Value: sb.String(), Line: line, Col: startCol})
			continue
		}

		// Number or symbol starting with - or +
		if isSymbolChar(ch) || ch == '+' || ch == '-' {
			startCol := col
			start := i
			i++
			col++
			for i < len(input) && !isDelimiter(input[i]) {
				i++
				col++
			}
			word := input[start:i]

			// Check for booleans
			if word == "#t" || word == "#true" {
				tokens = append(tokens, Token{Type: TokenBool, Value: "#t", Line: line, Col: startCol})
				continue
			}
			if word == "#f" || word == "#false" {
				tokens = append(tokens, Token{Type: TokenBool, Value: "#f", Line: line, Col: startCol})
				continue
			}

			// Check for number
			if n, err := strconv.ParseInt(word, 10, 64); err == nil {
				_ = n
				tokens = append(tokens, Token{Type: TokenNumber, Value: word, Line: line, Col: startCol})
				continue
			}

			// It's a symbol
			tokens = append(tokens, Token{Type: TokenSymbol, Value: word, Line: line, Col: startCol})
			continue
		}

		return nil, fmt.Errorf("%d:%d: unexpected character '%c'", line, col, ch)
	}

	tokens = append(tokens, Token{Type: TokenEOF, Line: line, Col: col})
	return tokens, nil
}

func isDelimiter(ch byte) bool {
	return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' || ch == '(' || ch == ')' || ch == '"' || ch == ';'
}

func isSymbolChar(ch byte) bool {
	r := rune(ch)
	if unicode.IsLetter(r) || unicode.IsDigit(r) {
		return true
	}
	return strings.ContainsRune("!$%&*+-./:<=>?@^_~#", r)
}

// Expr is a parsed S-expression.
type Expr interface {
	Pos() (int, int) // line, col
}

type NumberExpr struct {
	Value    int64
	Line, Col int
}

func (e *NumberExpr) Pos() (int, int) { return e.Line, e.Col }

type BoolExpr struct {
	Value    bool
	Line, Col int
}

func (e *BoolExpr) Pos() (int, int) { return e.Line, e.Col }

type StringExpr struct {
	Value    string
	Line, Col int
}

func (e *StringExpr) Pos() (int, int) { return e.Line, e.Col }

type SymbolExpr struct {
	Name     string
	Line, Col int
}

func (e *SymbolExpr) Pos() (int, int) { return e.Line, e.Col }

type ListExpr struct {
	Elements  []Expr
	Line, Col int
}

func (e *ListExpr) Pos() (int, int) { return e.Line, e.Col }

// Parser holds parser state.
type Parser struct {
	tokens []Token
	pos    int
}

func NewParser(tokens []Token) *Parser {
	return &Parser{tokens: tokens, pos: 0}
}

func (p *Parser) peek() Token {
	return p.tokens[p.pos]
}

func (p *Parser) next() Token {
	t := p.tokens[p.pos]
	p.pos++
	return t
}

// ParseAll parses all expressions until EOF.
func (p *Parser) ParseAll() ([]Expr, error) {
	var exprs []Expr
	for p.peek().Type != TokenEOF {
		expr, err := p.ParseExpr()
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, expr)
	}
	return exprs, nil
}

// ParseExpr parses a single expression.
func (p *Parser) ParseExpr() (Expr, error) {
	tok := p.peek()

	switch tok.Type {
	case TokenNumber:
		p.next()
		n, _ := strconv.ParseInt(tok.Value, 10, 64)
		return &NumberExpr{Value: n, Line: tok.Line, Col: tok.Col}, nil

	case TokenBool:
		p.next()
		return &BoolExpr{Value: tok.Value == "#t", Line: tok.Line, Col: tok.Col}, nil

	case TokenString:
		p.next()
		return &StringExpr{Value: tok.Value, Line: tok.Line, Col: tok.Col}, nil

	case TokenSymbol:
		p.next()
		return &SymbolExpr{Name: tok.Value, Line: tok.Line, Col: tok.Col}, nil

	case TokenLParen:
		p.next() // consume '('
		var elements []Expr
		for p.peek().Type != TokenRParen {
			if p.peek().Type == TokenEOF {
				return nil, fmt.Errorf("%d:%d: unexpected end of input, expected ')'", tok.Line, tok.Col)
			}
			elem, err := p.ParseExpr()
			if err != nil {
				return nil, err
			}
			elements = append(elements, elem)
		}
		p.next() // consume ')'
		return &ListExpr{Elements: elements, Line: tok.Line, Col: tok.Col}, nil

	case TokenRParen:
		return nil, fmt.Errorf("%d:%d: unexpected ')'", tok.Line, tok.Col)

	case TokenEOF:
		return nil, fmt.Errorf("%d:%d: unexpected end of input", tok.Line, tok.Col)

	default:
		return nil, fmt.Errorf("%d:%d: unexpected token", tok.Line, tok.Col)
	}
}
