package ming

import "fmt"

type TokenType int

const (
	TokenLParen TokenType = iota
	TokenRParen
	TokenInteger
	TokenBoolean
	TokenString
	TokenSymbol
	TokenQuote
	TokenChar
	TokenEOF
)

type Token struct {
	Type   TokenType
	IntVal int64
	StrVal string
	Line   int
	Col    int
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
		if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
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
	if ch <= ' ' || ch == '(' || ch == ')' || ch == '"' || ch == ';' || ch == 0 {
		return false
	}
	return true
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
		return Token{Type: TokenLParen, Line: line, Col: col}, nil
	case ch == ')':
		t.advance()
		return Token{Type: TokenRParen, Line: line, Col: col}, nil
	case ch == '\'':
		t.advance()
		return Token{Type: TokenQuote, Line: line, Col: col}, nil
	case ch == '"':
		return t.readString(line, col)
	case ch == '#':
		return t.readHash(line, col)
	default:
		return t.readSymbolOrNumber(line, col)
	}
}

func (t *Tokenizer) readString(line, col int) (Token, error) {
	t.advance() // skip opening "
	var s []rune
	for t.pos < len(t.input) {
		ch := t.advance()
		if ch == '"' {
			return Token{Type: TokenString, StrVal: string(s), Line: line, Col: col}, nil
		}
		if ch == '\\' {
			if t.pos >= len(t.input) {
				return Token{}, fmt.Errorf("%d:%d: unterminated string escape", line, col)
			}
			esc := t.advance()
			switch esc {
			case 'n':
				s = append(s, '\n')
			case 't':
				s = append(s, '\t')
			case '"':
				s = append(s, '"')
			case '\\':
				s = append(s, '\\')
			default:
				s = append(s, '\\', esc)
			}
		} else {
			s = append(s, ch)
		}
	}
	return Token{}, fmt.Errorf("%d:%d: unterminated string", line, col)
}

func (t *Tokenizer) readHash(line, col int) (Token, error) {
	t.advance() // skip #
	if t.pos >= len(t.input) {
		return Token{}, fmt.Errorf("%d:%d: unexpected end after #", line, col)
	}
	ch := t.advance()
	switch ch {
	case 't':
		return Token{Type: TokenBoolean, StrVal: "t", Line: line, Col: col}, nil
	case 'f':
		return Token{Type: TokenBoolean, StrVal: "f", Line: line, Col: col}, nil
	case '\\':
		if t.pos >= len(t.input) {
			return Token{}, fmt.Errorf("%d:%d: unexpected end after #\\", line, col)
		}
		// Read character name or single char
		var name []rune
		for t.pos < len(t.input) && isSymbolChar(t.peek()) {
			name = append(name, t.advance())
		}
		if len(name) == 0 {
			// Could be space or other non-symbol char
			c := t.advance()
			return Token{Type: TokenChar, StrVal: string(c), Line: line, Col: col}, nil
		}
		if len(name) == 1 {
			return Token{Type: TokenChar, StrVal: string(name), Line: line, Col: col}, nil
		}
		// Named characters
		switch string(name) {
		case "space":
			return Token{Type: TokenChar, StrVal: " ", Line: line, Col: col}, nil
		case "newline":
			return Token{Type: TokenChar, StrVal: "\n", Line: line, Col: col}, nil
		case "tab":
			return Token{Type: TokenChar, StrVal: "\t", Line: line, Col: col}, nil
		default:
			return Token{}, fmt.Errorf("%d:%d: unknown character name #\\%s", line, col, string(name))
		}
	default:
		return Token{}, fmt.Errorf("%d:%d: unknown hash literal #%c", line, col, ch)
	}
}

func (t *Tokenizer) readSymbolOrNumber(line, col int) (Token, error) {
	var s []rune
	for t.pos < len(t.input) && isSymbolChar(t.peek()) {
		s = append(s, t.advance())
	}
	str := string(s)

	// Try to parse as integer
	n, ok := parseInt(str)
	if ok {
		return Token{Type: TokenInteger, IntVal: n, Line: line, Col: col}, nil
	}

	return Token{Type: TokenSymbol, StrVal: str, Line: line, Col: col}, nil
}

func parseInt(s string) (int64, bool) {
	if len(s) == 0 {
		return 0, false
	}
	start := 0
	neg := false
	if s[0] == '-' || s[0] == '+' {
		if len(s) == 1 {
			return 0, false
		}
		neg = s[0] == '-'
		start = 1
	}
	var n int64
	for i := start; i < len(s); i++ {
		if s[i] < '0' || s[i] > '9' {
			return 0, false
		}
		n = n*10 + int64(s[i]-'0')
	}
	if neg {
		n = -n
	}
	return n, true
}
