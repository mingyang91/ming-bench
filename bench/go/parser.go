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
	TokenQuote
	TokenQuasiquote
	TokenUnquote
	TokenUnquoteSplicing
	TokenChar
	TokenFloat
	TokenRational
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
		if ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n' || ch == '\f' || ch == '\v' {
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

		// Quote shorthand
		if ch == '\'' {
			tokens = append(tokens, Token{Type: TokenQuote, Value: "'", Line: line, Col: col})
			i++
			col++
			continue
		}

		// Quasiquote shorthand
		if ch == '`' {
			tokens = append(tokens, Token{Type: TokenQuasiquote, Value: "`", Line: line, Col: col})
			i++
			col++
			continue
		}

		// Unquote / unquote-splicing
		if ch == ',' {
			if i+1 < len(input) && input[i+1] == '@' {
				tokens = append(tokens, Token{Type: TokenUnquoteSplicing, Value: ",@", Line: line, Col: col})
				i += 2
				col += 2
			} else {
				tokens = append(tokens, Token{Type: TokenUnquote, Value: ",", Line: line, Col: col})
				i++
				col++
			}
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

		// Syntax quote shorthand #'
		if ch == '#' && i+1 < len(input) && input[i+1] == '\'' {
			tokens = append(tokens, Token{Type: TokenSymbol, Value: "#'", Line: line, Col: col})
			i += 2
			col += 2
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

			// Check for character literals #\x, #\space, #\newline, #\tab
			if strings.HasPrefix(word, `#\`) {
				charName := word[2:]
				var r rune
				switch charName {
				case "space":
					r = ' '
				case "newline":
					r = '\n'
				case "tab":
					r = '\t'
				default:
					runes := []rune(charName)
					if len(runes) != 1 {
						return nil, fmt.Errorf("%d:%d: invalid character literal '%s'", line, startCol, word)
					}
					r = runes[0]
				}
				tokens = append(tokens, Token{Type: TokenChar, Value: string(r), Line: line, Col: startCol})
				continue
			}

			// Check for number
			if n, err := strconv.ParseInt(word, 10, 64); err == nil {
				_ = n
				tokens = append(tokens, Token{Type: TokenNumber, Value: word, Line: line, Col: startCol})
				continue
			}

			// Check for float literal (e.g., 1.5, .5, -0.3)
			if _, err := strconv.ParseFloat(word, 64); err == nil && strings.ContainsAny(word, ".eE") {
				tokens = append(tokens, Token{Type: TokenFloat, Value: word, Line: line, Col: startCol})
				continue
			}

			// Check for rational literal (e.g., 1/3, -1/3)
			if slashIdx := strings.Index(word, "/"); slashIdx > 0 && slashIdx < len(word)-1 {
				numPart := word[:slashIdx]
				denPart := word[slashIdx+1:]
				if _, err1 := strconv.ParseInt(numPart, 10, 64); err1 == nil {
					if _, err2 := strconv.ParseInt(denPart, 10, 64); err2 == nil {
						tokens = append(tokens, Token{Type: TokenRational, Value: word, Line: line, Col: startCol})
						continue
					}
				}
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

type FloatExpr struct {
	Value     float64
	Line, Col int
}

func (e *FloatExpr) Pos() (int, int) { return e.Line, e.Col }

type RationalExpr struct {
	Num, Den  int64
	Line, Col int
}

func (e *RationalExpr) Pos() (int, int) { return e.Line, e.Col }

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

type CharExpr struct {
	Value    rune
	Line, Col int
}

func (e *CharExpr) Pos() (int, int) { return e.Line, e.Col }

type SymbolExpr struct {
	Name     string
	Line, Col int
}

func (e *SymbolExpr) Pos() (int, int) { return e.Line, e.Col }

type ListExpr struct {
	Elements  []Expr
	Dot       Expr // non-nil for improper lists: (a b . c)
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

	case TokenFloat:
		p.next()
		f, _ := strconv.ParseFloat(tok.Value, 64)
		return &FloatExpr{Value: f, Line: tok.Line, Col: tok.Col}, nil

	case TokenRational:
		p.next()
		slashIdx := strings.Index(tok.Value, "/")
		num, _ := strconv.ParseInt(tok.Value[:slashIdx], 10, 64)
		den, _ := strconv.ParseInt(tok.Value[slashIdx+1:], 10, 64)
		return &RationalExpr{Num: num, Den: den, Line: tok.Line, Col: tok.Col}, nil

	case TokenBool:
		p.next()
		return &BoolExpr{Value: tok.Value == "#t", Line: tok.Line, Col: tok.Col}, nil

	case TokenString:
		p.next()
		return &StringExpr{Value: tok.Value, Line: tok.Line, Col: tok.Col}, nil

	case TokenChar:
		p.next()
		runes := []rune(tok.Value)
		return &CharExpr{Value: runes[0], Line: tok.Line, Col: tok.Col}, nil

	case TokenSymbol:
		p.next()
		if tok.Value == "#'" {
			inner, err := p.ParseExpr()
			if err != nil {
				return nil, err
			}
			return &ListExpr{
				Elements: []Expr{
					&SymbolExpr{Name: "syntax", Line: tok.Line, Col: tok.Col},
					inner,
				},
				Line: tok.Line,
				Col:  tok.Col,
			}, nil
		}
		return &SymbolExpr{Name: tok.Value, Line: tok.Line, Col: tok.Col}, nil

	case TokenQuote:
		p.next()
		inner, err := p.ParseExpr()
		if err != nil {
			return nil, err
		}
		return &ListExpr{
			Elements: []Expr{
				&SymbolExpr{Name: "quote", Line: tok.Line, Col: tok.Col},
				inner,
			},
			Line: tok.Line,
			Col:  tok.Col,
		}, nil

	case TokenQuasiquote:
		p.next()
		inner, err := p.ParseExpr()
		if err != nil {
			return nil, err
		}
		return &ListExpr{
			Elements: []Expr{
				&SymbolExpr{Name: "quasiquote", Line: tok.Line, Col: tok.Col},
				inner,
			},
			Line: tok.Line,
			Col:  tok.Col,
		}, nil

	case TokenUnquote:
		p.next()
		inner, err := p.ParseExpr()
		if err != nil {
			return nil, err
		}
		return &ListExpr{
			Elements: []Expr{
				&SymbolExpr{Name: "unquote", Line: tok.Line, Col: tok.Col},
				inner,
			},
			Line: tok.Line,
			Col:  tok.Col,
		}, nil

	case TokenUnquoteSplicing:
		p.next()
		inner, err := p.ParseExpr()
		if err != nil {
			return nil, err
		}
		return &ListExpr{
			Elements: []Expr{
				&SymbolExpr{Name: "unquote-splicing", Line: tok.Line, Col: tok.Col},
				inner,
			},
			Line: tok.Line,
			Col:  tok.Col,
		}, nil

	case TokenLParen:
		p.next() // consume '('
		var elements []Expr
		var dot Expr
		for p.peek().Type != TokenRParen {
			if p.peek().Type == TokenEOF {
				return nil, fmt.Errorf("%d:%d: unexpected end of input, expected ')'", tok.Line, tok.Col)
			}
			// Check for dot notation: (a b . c)
			if p.peek().Type == TokenSymbol && p.peek().Value == "." && len(elements) > 0 {
				p.next() // consume '.'
				var err error
				dot, err = p.ParseExpr()
				if err != nil {
					return nil, err
				}
				if p.peek().Type != TokenRParen {
					return nil, fmt.Errorf("%d:%d: expected ')' after dotted pair", tok.Line, tok.Col)
				}
				break
			}
			elem, err := p.ParseExpr()
			if err != nil {
				return nil, err
			}
			elements = append(elements, elem)
		}
		p.next() // consume ')'
		return &ListExpr{Elements: elements, Dot: dot, Line: tok.Line, Col: tok.Col}, nil

	case TokenRParen:
		return nil, fmt.Errorf("%d:%d: unexpected ')'", tok.Line, tok.Col)

	case TokenEOF:
		return nil, fmt.Errorf("%d:%d: unexpected end of input", tok.Line, tok.Col)

	default:
		return nil, fmt.Errorf("%d:%d: unexpected token", tok.Line, tok.Col)
	}
}
