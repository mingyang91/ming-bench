package ming

import "fmt"

// Expr represents a parsed Scheme expression.
type Expr struct {
	// Atom types
	Type    ExprType
	IntVal  int64
	BoolVal bool
	StrVal  string
	// List (for compound expressions)
	List []*Expr
	// Source position
	Line int
	Col  int
}

type ExprType int

const (
	ExprInt ExprType = iota
	ExprBool
	ExprString
	ExprSymbol
	ExprList
)

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

func (p *Parser) advance() Token {
	tok := p.tokens[p.pos]
	p.pos++
	return tok
}

func (p *Parser) ParseAll() ([]*Expr, error) {
	var exprs []*Expr
	for p.peek().Type != TokenEOF {
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, expr)
	}
	return exprs, nil
}

func (p *Parser) parseExpr() (*Expr, error) {
	tok := p.peek()
	switch tok.Type {
	case TokenNumber:
		p.advance()
		n := parseInt(tok.Val)
		return &Expr{Type: ExprInt, IntVal: n, Line: tok.Line, Col: tok.Col}, nil
	case TokenBool:
		p.advance()
		return &Expr{Type: ExprBool, BoolVal: tok.Val == "#t", Line: tok.Line, Col: tok.Col}, nil
	case TokenString:
		p.advance()
		return &Expr{Type: ExprString, StrVal: tok.Val, Line: tok.Line, Col: tok.Col}, nil
	case TokenSymbol:
		p.advance()
		return &Expr{Type: ExprSymbol, StrVal: tok.Val, Line: tok.Line, Col: tok.Col}, nil
	case TokenQuote:
		p.advance()
		inner, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		return &Expr{
			Type: ExprList,
			List: []*Expr{
				{Type: ExprSymbol, StrVal: "quote", Line: tok.Line, Col: tok.Col},
				inner,
			},
			Line: tok.Line,
			Col:  tok.Col,
		}, nil
	case TokenLParen:
		return p.parseList()
	case TokenRParen:
		return nil, fmt.Errorf("%d:%d: unexpected ')'", tok.Line, tok.Col)
	case TokenEOF:
		return nil, fmt.Errorf("unexpected end of input")
	default:
		return nil, fmt.Errorf("%d:%d: unexpected token", tok.Line, tok.Col)
	}
}

func (p *Parser) parseList() (*Expr, error) {
	open := p.advance() // consume '('
	var elems []*Expr
	for p.peek().Type != TokenRParen {
		if p.peek().Type == TokenEOF {
			return nil, fmt.Errorf("%d:%d: unterminated list", open.Line, open.Col)
		}
		elem, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		elems = append(elems, elem)
	}
	p.advance() // consume ')'
	return &Expr{Type: ExprList, List: elems, Line: open.Line, Col: open.Col}, nil
}

func parseInt(s string) int64 {
	var neg bool
	start := 0
	if s[0] == '-' {
		neg = true
		start = 1
	} else if s[0] == '+' {
		start = 1
	}
	var n int64
	for i := start; i < len(s); i++ {
		n = n*10 + int64(s[i]-'0')
	}
	if neg {
		n = -n
	}
	return n
}
