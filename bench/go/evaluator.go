package ming

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
)

// Value represents a Scheme value.
type Value interface {
	String() string
}

type IntVal struct{ Val int64 }
type BoolVal struct{ Val bool }
type StringVal struct{ Val string }

func (v *IntVal) String() string {
	return strconv.FormatInt(v.Val, 10)
}

func (v *BoolVal) String() string {
	if v.Val {
		return "#t"
	}
	return "#f"
}

func (v *StringVal) String() string {
	return fmt.Sprintf("%q", v.Val)
}

// tokenize splits input into tokens.
func tokenize(input string) []string {
	var tokens []string
	i := 0
	for i < len(input) {
		ch := input[i]
		if unicode.IsSpace(rune(ch)) {
			i++
		} else if ch == ';' {
			// line comment
			for i < len(input) && input[i] != '\n' {
				i++
			}
		} else if ch == '(' || ch == ')' {
			tokens = append(tokens, string(ch))
			i++
		} else if ch == '#' {
			if i+1 < len(input) && (input[i+1] == 't' || input[i+1] == 'f') {
				tokens = append(tokens, input[i:i+2])
				i += 2
			} else {
				tokens = append(tokens, string(ch))
				i++
			}
		} else if ch == '"' {
			// string literal
			j := i + 1
			for j < len(input) && input[j] != '"' {
				if input[j] == '\\' {
					j++
				}
				j++
			}
			if j < len(input) {
				j++ // closing quote
			}
			tokens = append(tokens, input[i:j])
			i = j
		} else {
			j := i
			for j < len(input) && !unicode.IsSpace(rune(input[j])) && input[j] != '(' && input[j] != ')' && input[j] != '"' && input[j] != ';' {
				j++
			}
			tokens = append(tokens, input[i:j])
			i = j
		}
	}
	return tokens
}

// Expr represents a parsed S-expression.
type Expr interface{}

type AtomExpr struct{ Token string }
type ListExpr struct{ Items []Expr }

// parse parses tokens into an Expr, returning the expr and remaining tokens.
func parse(tokens []string) (Expr, []string, error) {
	if len(tokens) == 0 {
		return nil, nil, &EvalError{Message: "unexpected EOF"}
	}
	tok := tokens[0]
	rest := tokens[1:]
	if tok == "(" {
		var items []Expr
		for len(rest) > 0 && rest[0] != ")" {
			var item Expr
			var err error
			item, rest, err = parse(rest)
			if err != nil {
				return nil, nil, err
			}
			items = append(items, item)
		}
		if len(rest) == 0 {
			return nil, nil, &EvalError{Message: "missing closing paren"}
		}
		rest = rest[1:] // skip ")"
		return &ListExpr{Items: items}, rest, nil
	} else if tok == ")" {
		return nil, nil, &EvalError{Message: "unexpected )"}
	}
	return &AtomExpr{Token: tok}, rest, nil
}

// eval evaluates an Expr and returns a Value.
func eval(expr Expr) (Value, error) {
	switch e := expr.(type) {
	case *AtomExpr:
		return evalAtom(e.Token)
	case *ListExpr:
		return evalList(e)
	}
	return nil, &EvalError{Message: "unknown expression"}
}

func evalAtom(token string) (Value, error) {
	if token == "#t" {
		return &BoolVal{Val: true}, nil
	}
	if token == "#f" {
		return &BoolVal{Val: false}, nil
	}
	if len(token) > 0 && token[0] == '"' {
		// string literal - unescape
		s, err := strconv.Unquote(token)
		if err != nil {
			return nil, &EvalError{Message: "bad string: " + token}
		}
		return &StringVal{Val: s}, nil
	}
	// try integer
	n, err := strconv.ParseInt(token, 10, 64)
	if err == nil {
		return &IntVal{Val: n}, nil
	}
	return nil, &EvalError{Message: "unbound variable: " + token}
}

func evalList(list *ListExpr) (Value, error) {
	if len(list.Items) == 0 {
		return nil, &EvalError{Message: "empty application"}
	}
	// check for special forms
	if atom, ok := list.Items[0].(*AtomExpr); ok {
		switch atom.Token {
		case "and":
			return evalAnd(list.Items[1:])
		case "or":
			return evalOr(list.Items[1:])
		}
	}

	// evaluate operator
	op, err := eval(list.Items[0])
	if err != nil {
		return nil, err
	}
	// evaluate arguments
	args := make([]Value, len(list.Items)-1)
	for i, item := range list.Items[1:] {
		args[i], err = eval(item)
		if err != nil {
			return nil, err
		}
	}
	return apply(op, args)
}

func isTruthy(v Value) bool {
	if b, ok := v.(*BoolVal); ok {
		return b.Val
	}
	return true // everything except #f is truthy
}

func evalAnd(exprs []Expr) (Value, error) {
	var result Value = &BoolVal{Val: true}
	for _, e := range exprs {
		v, err := eval(e)
		if err != nil {
			return nil, err
		}
		if !isTruthy(v) {
			return v, nil
		}
		result = v
	}
	return result, nil
}

func evalOr(exprs []Expr) (Value, error) {
	var result Value = &BoolVal{Val: false}
	for _, e := range exprs {
		v, err := eval(e)
		if err != nil {
			return nil, err
		}
		if isTruthy(v) {
			return v, nil
		}
		result = v
	}
	return result, nil
}

func apply(op Value, args []Value) (Value, error) {
	// Built-in operator identification - for now op must be a symbol that we resolved
	// But at L01 we handle builtins by name in evalList. Let me restructure.
	return nil, &EvalError{Message: "not a procedure"}
}

func evalBuiltin(name string, args []Value) (Value, error) {
	switch name {
	case "+":
		var sum int64
		for _, a := range args {
			n, ok := a.(*IntVal)
			if !ok {
				return nil, &EvalError{Message: "expected number"}
			}
			sum += n.Val
		}
		return &IntVal{Val: sum}, nil
	case "-":
		if len(args) == 0 {
			return nil, &EvalError{Message: "- requires at least 1 argument"}
		}
		if len(args) == 1 {
			n, ok := args[0].(*IntVal)
			if !ok {
				return nil, &EvalError{Message: "expected number"}
			}
			return &IntVal{Val: -n.Val}, nil
		}
		result, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "expected number"}
		}
		val := result.Val
		for _, a := range args[1:] {
			n, ok := a.(*IntVal)
			if !ok {
				return nil, &EvalError{Message: "expected number"}
			}
			val -= n.Val
		}
		return &IntVal{Val: val}, nil
	case "*":
		var prod int64 = 1
		for _, a := range args {
			n, ok := a.(*IntVal)
			if !ok {
				return nil, &EvalError{Message: "expected number"}
			}
			prod *= n.Val
		}
		return &IntVal{Val: prod}, nil
	case "/":
		if len(args) < 2 {
			return nil, &EvalError{Message: "/ requires at least 2 arguments"}
		}
		result, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "expected number"}
		}
		val := result.Val
		for _, a := range args[1:] {
			n, ok := a.(*IntVal)
			if !ok {
				return nil, &EvalError{Message: "expected number"}
			}
			if n.Val == 0 {
				return nil, &EvalError{Message: "division by zero"}
			}
			val /= n.Val
		}
		return &IntVal{Val: val}, nil
	case "=":
		return compareInts(args, func(a, b int64) bool { return a == b })
	case "<":
		return compareInts(args, func(a, b int64) bool { return a < b })
	case ">":
		return compareInts(args, func(a, b int64) bool { return a > b })
	case "<=":
		return compareInts(args, func(a, b int64) bool { return a <= b })
	case ">=":
		return compareInts(args, func(a, b int64) bool { return a >= b })
	case "not":
		if len(args) != 1 {
			return nil, &EvalError{Message: "not requires 1 argument"}
		}
		return &BoolVal{Val: !isTruthy(args[0])}, nil
	}
	return nil, &EvalError{Message: "unknown procedure: " + name}
}

func compareInts(args []Value, cmp func(int64, int64) bool) (Value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "comparison requires at least 2 arguments"}
	}
	for i := 0; i < len(args)-1; i++ {
		a, ok := args[i].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "expected number"}
		}
		b, ok2 := args[i+1].(*IntVal)
		if !ok2 {
			return nil, &EvalError{Message: "expected number"}
		}
		if !cmp(a.Val, b.Val) {
			return &BoolVal{Val: false}, nil
		}
	}
	return &BoolVal{Val: true}, nil
}

// parseAll parses all expressions from input.
func parseAll(input string) ([]Expr, error) {
	tokens := tokenize(input)
	var exprs []Expr
	for len(tokens) > 0 {
		expr, rest, err := parse(tokens)
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, expr)
		tokens = rest
	}
	return exprs, nil
}

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	input = strings.TrimSpace(input)
	if input == "" {
		return "", nil
	}
	exprs, err := parseAll(input)
	if err != nil {
		return "", err
	}
	var result Value
	for _, expr := range exprs {
		result, err = evalExpr(expr)
		if err != nil {
			return "", err
		}
	}
	return result.String(), nil
}

// evalExpr evaluates an expression - main entry point for evaluation.
func evalExpr(expr Expr) (Value, error) {
	switch e := expr.(type) {
	case *AtomExpr:
		return evalAtom(e.Token)
	case *ListExpr:
		return evalListExpr(e)
	}
	return nil, &EvalError{Message: "unknown expression"}
}

func evalListExpr(list *ListExpr) (Value, error) {
	if len(list.Items) == 0 {
		return nil, &EvalError{Message: "empty application"}
	}
	// check for special forms first
	if atom, ok := list.Items[0].(*AtomExpr); ok {
		switch atom.Token {
		case "and":
			return evalAndExpr(list.Items[1:])
		case "or":
			return evalOrExpr(list.Items[1:])
		}
		// try as builtin
		args := make([]Value, len(list.Items)-1)
		var err error
		for i, item := range list.Items[1:] {
			args[i], err = evalExpr(item)
			if err != nil {
				return nil, err
			}
		}
		return evalBuiltin(atom.Token, args)
	}
	return nil, &EvalError{Message: "not a procedure"}
}

func evalAndExpr(exprs []Expr) (Value, error) {
	var result Value = &BoolVal{Val: true}
	for _, e := range exprs {
		v, err := evalExpr(e)
		if err != nil {
			return nil, err
		}
		if !isTruthy(v) {
			return v, nil
		}
		result = v
	}
	return result, nil
}

func evalOrExpr(exprs []Expr) (Value, error) {
	var result Value = &BoolVal{Val: false}
	for _, e := range exprs {
		v, err := evalExpr(e)
		if err != nil {
			return nil, err
		}
		if isTruthy(v) {
			return v, nil
		}
		result = v
	}
	return result, nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	r, err := EvalStr(input)
	return r, "", err
}
