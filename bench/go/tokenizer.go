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
	TokenString
	TokenBool
	TokenSymbol
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
	if unicode.IsSpace(ch) {
		return false
	}
	switch ch {
	case '(', ')', '"', ';', 0:
		return false
	}
	return true
}

func (t *Tokenizer) Tokenize() ([]Token, error) {
	var tokens []Token
	for {
		t.skipWhitespaceAndComments()
		if t.pos >= len(t.input) {
			tokens = append(tokens, Token{Type: TokenEOF, Line: t.line, Col: t.col})
			return tokens, nil
		}

		ch := t.peek()
		line, col := t.line, t.col

		switch {
		case ch == '(':
			t.advance()
			tokens = append(tokens, Token{Type: TokenLParen, Val: "(", Line: line, Col: col})
		case ch == ')':
			t.advance()
			tokens = append(tokens, Token{Type: TokenRParen, Val: ")", Line: line, Col: col})
		case ch == '"':
			s, err := t.readString()
			if err != nil {
				return nil, err
			}
			tokens = append(tokens, Token{Type: TokenString, Val: s, Line: line, Col: col})
		case ch == '#':
			t.advance()
			if t.pos >= len(t.input) {
				return nil, fmt.Errorf("%d:%d: unexpected end after #", line, col)
			}
			next := t.advance()
			switch next {
			case 't':
				tokens = append(tokens, Token{Type: TokenBool, Val: "#t", Line: line, Col: col})
			case 'f':
				tokens = append(tokens, Token{Type: TokenBool, Val: "#f", Line: line, Col: col})
			default:
				return nil, fmt.Errorf("%d:%d: unexpected character after #: %c", line, col, next)
			}
		default:
			// Number or symbol
			var buf strings.Builder
			for t.pos < len(t.input) && isSymbolChar(t.peek()) {
				buf.WriteRune(t.advance())
			}
			word := buf.String()
			if isNumber(word) {
				tokens = append(tokens, Token{Type: TokenNumber, Val: word, Line: line, Col: col})
			} else {
				tokens = append(tokens, Token{Type: TokenSymbol, Val: word, Line: line, Col: col})
			}
		}
	}
}

func (t *Tokenizer) readString() (string, error) {
	t.advance() // skip opening "
	var buf strings.Builder
	for t.pos < len(t.input) {
		ch := t.advance()
		if ch == '"' {
			return buf.String(), nil
		}
		if ch == '\\' {
			if t.pos >= len(t.input) {
				return "", fmt.Errorf("%d:%d: unexpected end in string escape", t.line, t.col)
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
		} else {
			buf.WriteRune(ch)
		}
	}
	return "", fmt.Errorf("%d:%d: unterminated string", t.line, t.col)
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
