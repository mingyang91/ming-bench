package ming

import (
	"fmt"
	"strings"
	"unicode"
)

type TokenType int

const (
	TokenLParen TokenType = iota
	TokenRParen
	TokenNumber
	TokenBoolean
	TokenString
	TokenSymbol
	TokenQuote
	TokenEOF
)

type Token struct {
	Type TokenType
	Val  string
	Line int
	Col  int
}

type Tokenizer struct {
	input []rune
	pos   int
	line  int
	col   int
}

func NewTokenizer(input string) *Tokenizer {
	return &Tokenizer{input: []rune(input), pos: 0, line: 1, col: 1}
}

func (t *Tokenizer) peek() rune {
	if t.pos >= len(t.input) {
		return 0
	}
	return t.input[t.pos]
}

func (t *Tokenizer) advance() rune {
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

func (t *Tokenizer) skipWhitespaceAndComments() {
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
	if unicode.IsLetter(ch) || unicode.IsDigit(ch) {
		return true
	}
	return strings.ContainsRune("!$%&*+-./:<=>?@^_~", ch)
}

func (t *Tokenizer) NextToken() (Token, error) {
	t.skipWhitespaceAndComments()
	if t.pos >= len(t.input) {
		return Token{Type: TokenEOF, Line: t.line, Col: t.col}, nil
	}

	line, col := t.line, t.col
	ch := t.peek()

	switch {
	case ch == '(':
		t.advance()
		return Token{Type: TokenLParen, Val: "(", Line: line, Col: col}, nil
	case ch == ')':
		t.advance()
		return Token{Type: TokenRParen, Val: ")", Line: line, Col: col}, nil
	case ch == '\'':
		t.advance()
		return Token{Type: TokenQuote, Val: "'", Line: line, Col: col}, nil
	case ch == '#':
		t.advance()
		if t.pos >= len(t.input) {
			return Token{}, fmt.Errorf("%d:%d: unexpected end after #", line, col)
		}
		next := t.advance()
		if next == 't' {
			return Token{Type: TokenBoolean, Val: "#t", Line: line, Col: col}, nil
		} else if next == 'f' {
			return Token{Type: TokenBoolean, Val: "#f", Line: line, Col: col}, nil
		}
		return Token{}, fmt.Errorf("%d:%d: unexpected character after #: %c", line, col, next)
	case ch == '"':
		return t.readString(line, col)
	default:
		// Number or symbol
		return t.readAtom(line, col)
	}
}

func (t *Tokenizer) readString(line, col int) (Token, error) {
	t.advance() // opening quote
	var buf strings.Builder
	for t.pos < len(t.input) {
		ch := t.advance()
		if ch == '\\' {
			if t.pos >= len(t.input) {
				return Token{}, fmt.Errorf("%d:%d: unterminated string escape", line, col)
			}
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
		} else if ch == '"' {
			return Token{Type: TokenString, Val: buf.String(), Line: line, Col: col}, nil
		} else {
			buf.WriteRune(ch)
		}
	}
	return Token{}, fmt.Errorf("%d:%d: unterminated string", line, col)
}

func (t *Tokenizer) readAtom(line, col int) (Token, error) {
	var buf strings.Builder
	for t.pos < len(t.input) && isSymbolChar(t.peek()) {
		buf.WriteRune(t.advance())
	}
	val := buf.String()
	if val == "" {
		ch := t.advance()
		return Token{}, fmt.Errorf("%d:%d: unexpected character: %c", line, col, ch)
	}
	// Check if it's a number
	if isNumber(val) {
		return Token{Type: TokenNumber, Val: val, Line: line, Col: col}, nil
	}
	return Token{Type: TokenSymbol, Val: val, Line: line, Col: col}, nil
}

func isNumber(s string) bool {
	if len(s) == 0 {
		return false
	}
	start := 0
	if s[0] == '-' || s[0] == '+' {
		if len(s) == 1 {
			return false
		}
		start = 1
	}
	for i := start; i < len(s); i++ {
		if s[i] < '0' || s[i] > '9' {
			return false
		}
	}
	return true
}

func Tokenize(input string) ([]Token, error) {
	t := NewTokenizer(input)
	var tokens []Token
	for {
		tok, err := t.NextToken()
		if err != nil {
			return nil, err
		}
		tokens = append(tokens, tok)
		if tok.Type == TokenEOF {
			break
		}
	}
	return tokens, nil
}
