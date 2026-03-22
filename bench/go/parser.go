package ming

import (
	"fmt"
	"strconv"
)

// Expr represents a parsed Scheme expression.
type Expr struct {
	Type     ExprType
	IntVal   int64
	BoolVal  bool
	StrVal   string
	FloatVal float64
	Num      int64
	Den      int64
	List     []*Expr
	Line     int
	Col      int
}

type ExprType int

const (
	ExprInteger ExprType = iota
	ExprBoolean
	ExprString
	ExprSymbol
	ExprList
	ExprChar
	ExprFloat
	ExprRational
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

func (p *Parser) Parse() (*Expr, error) {
	tok := p.peek()
	if tok.Type == TokenEOF {
		return nil, nil
	}
	return p.parseExpr()
}

func (p *Parser) parseExpr() (*Expr, error) {
	tok := p.peek()
	switch tok.Type {
	case TokenNumber:
		p.advance()
		if isRationalStr(tok.Val) {
			num, den := parseRational(tok.Val)
			return &Expr{Type: ExprRational, Num: num, Den: den, Line: tok.Line, Col: tok.Col}, nil
		}
		if isFloatStr(tok.Val) {
			f := parseFloatStr(tok.Val)
			return &Expr{Type: ExprFloat, FloatVal: f, Line: tok.Line, Col: tok.Col}, nil
		}
		n, err := strconv.ParseInt(tok.Val, 10, 64)
		if err != nil {
			return nil, fmt.Errorf("%d:%d: invalid number: %s", tok.Line, tok.Col, tok.Val)
		}
		return &Expr{Type: ExprInteger, IntVal: n, Line: tok.Line, Col: tok.Col}, nil
	case TokenBoolean:
		p.advance()
		return &Expr{Type: ExprBoolean, BoolVal: tok.Val == "#t", Line: tok.Line, Col: tok.Col}, nil
	case TokenString:
		p.advance()
		return &Expr{Type: ExprString, StrVal: tok.Val, Line: tok.Line, Col: tok.Col}, nil
	case TokenSymbol:
		p.advance()
		return &Expr{Type: ExprSymbol, StrVal: tok.Val, Line: tok.Line, Col: tok.Col}, nil
	case TokenChar:
		p.advance()
		runes := []rune(tok.Val)
		return &Expr{Type: ExprChar, IntVal: int64(runes[0]), Line: tok.Line, Col: tok.Col}, nil
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
			Line: tok.Line, Col: tok.Col,
		}, nil
	case TokenLParen:
		return p.parseList()
	case TokenEOF:
		return nil, fmt.Errorf("%d:%d: unexpected end of input", tok.Line, tok.Col)
	default:
		return nil, fmt.Errorf("%d:%d: unexpected token: %s", tok.Line, tok.Col, tok.Val)
	}
}

func (p *Parser) parseList() (*Expr, error) {
	open := p.advance() // consume '('
	var elems []*Expr
	for p.peek().Type != TokenRParen {
		if p.peek().Type == TokenEOF {
			return nil, fmt.Errorf("%d:%d: unterminated list", open.Line, open.Col)
		}
		e, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		elems = append(elems, e)
	}
	p.advance() // consume ')'
	return &Expr{Type: ExprList, List: elems, Line: open.Line, Col: open.Col}, nil
}

func ParseAll(input string) ([]*Expr, error) {
	tokens, err := Tokenize(input)
	if err != nil {
		return nil, err
	}
	p := NewParser(tokens)
	var exprs []*Expr
	for {
		expr, err := p.Parse()
		if err != nil {
			return nil, err
		}
		if expr == nil {
			break
		}
		exprs = append(exprs, expr)
	}
	return exprs, nil
}
