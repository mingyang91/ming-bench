package ming

import "fmt"

// Expr represents a parsed Scheme expression.
type Expr struct {
	Type    ExprType
	IntVal  int64
	BoolVal bool
	StrVal  string
	// For list expressions (function calls, special forms)
	Elements []*Expr
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
)

type Parser struct {
	tokenizer *Tokenizer
	current   Token
	hasPeek   bool
}

func NewParser(input string) *Parser {
	return &Parser{tokenizer: NewTokenizer(input)}
}

func (p *Parser) peek() (Token, error) {
	if !p.hasPeek {
		tok, err := p.tokenizer.NextToken()
		if err != nil {
			return Token{}, err
		}
		p.current = tok
		p.hasPeek = true
	}
	return p.current, nil
}

func (p *Parser) next() (Token, error) {
	tok, err := p.peek()
	if err != nil {
		return Token{}, err
	}
	p.hasPeek = false
	return tok, nil
}

func (p *Parser) ParseExpr() (*Expr, error) {
	tok, err := p.next()
	if err != nil {
		return nil, err
	}

	switch tok.Type {
	case TokenInteger:
		return &Expr{Type: ExprInteger, IntVal: tok.IntVal, Line: tok.Line, Col: tok.Col}, nil
	case TokenBoolean:
		return &Expr{Type: ExprBoolean, BoolVal: tok.StrVal == "t", Line: tok.Line, Col: tok.Col}, nil
	case TokenString:
		return &Expr{Type: ExprString, StrVal: tok.StrVal, Line: tok.Line, Col: tok.Col}, nil
	case TokenSymbol:
		return &Expr{Type: ExprSymbol, StrVal: tok.StrVal, Line: tok.Line, Col: tok.Col}, nil
	case TokenLParen:
		return p.parseList(tok.Line, tok.Col)
	case TokenRParen:
		return nil, fmt.Errorf("%d:%d: unexpected ')'", tok.Line, tok.Col)
	case TokenEOF:
		return nil, fmt.Errorf("unexpected end of input")
	}
	return nil, fmt.Errorf("%d:%d: unexpected token", tok.Line, tok.Col)
}

func (p *Parser) parseList(line, col int) (*Expr, error) {
	var elements []*Expr
	for {
		tok, err := p.peek()
		if err != nil {
			return nil, err
		}
		if tok.Type == TokenRParen {
			p.next()
			return &Expr{Type: ExprList, Elements: elements, Line: line, Col: col}, nil
		}
		if tok.Type == TokenEOF {
			return nil, fmt.Errorf("%d:%d: unterminated list", line, col)
		}
		expr, err := p.ParseExpr()
		if err != nil {
			return nil, err
		}
		elements = append(elements, expr)
	}
}

// ParseAll parses all expressions from the input.
func (p *Parser) ParseAll() ([]*Expr, error) {
	var exprs []*Expr
	for {
		tok, err := p.peek()
		if err != nil {
			return nil, err
		}
		if tok.Type == TokenEOF {
			return exprs, nil
		}
		expr, err := p.ParseExpr()
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, expr)
	}
}
