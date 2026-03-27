package ming

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
)

// --------------- Values ---------------

// Value represents a Scheme value.
type Value interface {
	String() string
}

type IntVal struct{ Val int64 }
type BoolVal struct{ Val bool }
type StringVal struct{ Val string }
type SymbolVal struct{ Name string }
type PairVal struct{ Car, Cdr Value }
type NilVal struct{} // empty list
type VoidVal struct{}
type CharVal struct{ Val rune }

type LambdaVal struct {
	Params []string
	Body   []Expr
	Env    *Env
}

type BuiltinVal struct {
	Name string
	Fn   func([]Value) (Value, error)
}

func (v *IntVal) String() string    { return strconv.FormatInt(v.Val, 10) }
func (v *BoolVal) String() string {
	if v.Val {
		return "#t"
	}
	return "#f"
}
func (v *StringVal) String() string  { return fmt.Sprintf("%q", v.Val) }
func (v *SymbolVal) String() string  { return v.Name }
func (v *NilVal) String() string     { return "()" }
func (v *VoidVal) String() string    { return "" }
func (v *CharVal) String() string     { return fmt.Sprintf("#\\%c", v.Val) }
func (v *LambdaVal) String() string  { return "#<procedure>" }
func (v *BuiltinVal) String() string { return "#<builtin:" + v.Name + ">" }

func (v *PairVal) String() string {
	var buf strings.Builder
	buf.WriteByte('(')
	cur := Value(v)
	first := true
	for {
		p, ok := cur.(*PairVal)
		if !ok {
			break
		}
		if !first {
			buf.WriteByte(' ')
		}
		first = false
		buf.WriteString(p.Car.String())
		cur = p.Cdr
	}
	if _, ok := cur.(*NilVal); !ok {
		buf.WriteString(" . ")
		buf.WriteString(cur.String())
	}
	buf.WriteByte(')')
	return buf.String()
}

// --------------- Environment ---------------

type Env struct {
	bindings map[string]Value
	parent   *Env
}

func newEnv(parent *Env) *Env {
	return &Env{bindings: make(map[string]Value), parent: parent}
}

func (e *Env) get(name string) (Value, bool) {
	if v, ok := e.bindings[name]; ok {
		return v, true
	}
	if e.parent != nil {
		return e.parent.get(name)
	}
	return nil, false
}

func (e *Env) set(name string, val Value) {
	e.bindings[name] = val
}

// --------------- Tokenizer ---------------

type Token struct {
	Val  string
	Line int
	Col  int
}

func tokenize(input string) []Token {
	var tokens []Token
	i := 0
	line := 1
	col := 1
	for i < len(input) {
		ch := input[i]
		if ch == '\n' {
			i++
			line++
			col = 1
		} else if unicode.IsSpace(rune(ch)) {
			i++
			col++
		} else if ch == ';' {
			for i < len(input) && input[i] != '\n' {
				i++
				col++
			}
		} else if ch == '\'' {
			tokens = append(tokens, Token{"'", line, col})
			i++
			col++
		} else if ch == '(' || ch == ')' {
			tokens = append(tokens, Token{string(ch), line, col})
			i++
			col++
		} else if ch == '#' {
			startCol := col
			if i+1 < len(input) && input[i+1] == '\\' {
				// Character literal: #\a, #\space, #\newline, etc.
				j := i + 2
				c := col + 2
				// Read the character name or single char
				for j < len(input) && !unicode.IsSpace(rune(input[j])) && input[j] != '(' && input[j] != ')' && input[j] != '"' && input[j] != ';' {
					j++
					c++
				}
				tokens = append(tokens, Token{input[i:j], line, startCol})
				i = j
				col = c
			} else if i+1 < len(input) && (input[i+1] == 't' || input[i+1] == 'f') {
				tokens = append(tokens, Token{input[i : i+2], line, startCol})
				i += 2
				col += 2
			} else {
				tokens = append(tokens, Token{string(ch), line, startCol})
				i++
				col++
			}
		} else if ch == '"' {
			startCol := col
			j := i + 1
			c := col + 1
			for j < len(input) && input[j] != '"' {
				if input[j] == '\\' {
					j++
					c++
				}
				j++
				c++
			}
			if j < len(input) {
				j++
				c++
			}
			tokens = append(tokens, Token{input[i:j], line, startCol})
			i = j
			col = c
		} else {
			startCol := col
			j := i
			for j < len(input) && !unicode.IsSpace(rune(input[j])) && input[j] != '(' && input[j] != ')' && input[j] != '"' && input[j] != ';' && input[j] != '\'' {
				j++
				col++
			}
			tokens = append(tokens, Token{input[i:j], line, startCol})
			i = j
		}
	}
	return tokens
}

// --------------- Parser ---------------

type Expr interface {
	Pos() (int, int)
}

type AtomExpr struct {
	Token    string
	Line, Col int
}

type ListExpr struct {
	Items     []Expr
	Line, Col int
}

func (e *AtomExpr) Pos() (int, int) { return e.Line, e.Col }
func (e *ListExpr) Pos() (int, int) { return e.Line, e.Col }

func parse(tokens []Token) (Expr, []Token, error) {
	if len(tokens) == 0 {
		return nil, nil, &EvalError{Message: "unexpected EOF"}
	}
	tok := tokens[0]
	rest := tokens[1:]
	if tok.Val == "'" {
		// 'x => (quote x)
		inner, rest2, err := parse(rest)
		if err != nil {
			return nil, nil, err
		}
		return &ListExpr{Items: []Expr{&AtomExpr{Token: "quote", Line: tok.Line, Col: tok.Col}, inner}, Line: tok.Line, Col: tok.Col}, rest2, nil
	}
	if tok.Val == "(" {
		var items []Expr
		for len(rest) > 0 && rest[0].Val != ")" {
			var item Expr
			var err error
			item, rest, err = parse(rest)
			if err != nil {
				return nil, nil, err
			}
			items = append(items, item)
		}
		if len(rest) == 0 {
			return nil, nil, &EvalError{Message: "missing closing paren", Line: tok.Line, Col: tok.Col}
		}
		rest = rest[1:]
		return &ListExpr{Items: items, Line: tok.Line, Col: tok.Col}, rest, nil
	} else if tok.Val == ")" {
		return nil, nil, &EvalError{Message: "unexpected )", Line: tok.Line, Col: tok.Col}
	}
	return &AtomExpr{Token: tok.Val, Line: tok.Line, Col: tok.Col}, rest, nil
}

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

// --------------- Evaluator ---------------

func isTruthy(v Value) bool {
	if b, ok := v.(*BoolVal); ok {
		return b.Val
	}
	return true
}

func errAt(expr Expr, msg string) error {
	l, c := expr.Pos()
	return &EvalError{Message: msg, Line: l, Col: c}
}

func evalInEnv(expr Expr, env *Env) (Value, error) {
	switch e := expr.(type) {
	case *AtomExpr:
		return evalAtomInEnv(e, env)
	case *ListExpr:
		return evalListInEnv(e, env)
	}
	return nil, errAt(expr, "unknown expression")
}

func evalAtomInEnv(atom *AtomExpr, env *Env) (Value, error) {
	token := atom.Token
	if token == "#t" {
		return &BoolVal{Val: true}, nil
	}
	if token == "#f" {
		return &BoolVal{Val: false}, nil
	}
	if len(token) >= 2 && token[0] == '#' && token[1] == '\\' {
		name := token[2:]
		switch name {
		case "space":
			return &CharVal{Val: ' '}, nil
		case "newline":
			return &CharVal{Val: '\n'}, nil
		case "tab":
			return &CharVal{Val: '\t'}, nil
		default:
			if len(name) == 1 {
				return &CharVal{Val: rune(name[0])}, nil
			}
			return nil, errAt(atom, "unknown character name: "+name)
		}
	}
	if len(token) > 0 && token[0] == '"' {
		s, err := strconv.Unquote(token)
		if err != nil {
			return nil, errAt(atom, "bad string: "+token)
		}
		return &StringVal{Val: s}, nil
	}
	if n, err := strconv.ParseInt(token, 10, 64); err == nil {
		return &IntVal{Val: n}, nil
	}
	// variable lookup
	if v, ok := env.get(token); ok {
		return v, nil
	}
	return nil, errAt(atom, "unbound variable: "+token)
}

func evalListInEnv(list *ListExpr, env *Env) (Value, error) {
	if len(list.Items) == 0 {
		return nil, errAt(list, "empty application")
	}

	// Check for special forms
	if atom, ok := list.Items[0].(*AtomExpr); ok {
		switch atom.Token {
		case "quote":
			if len(list.Items) != 2 {
				return nil, errAt(list, "quote requires 1 argument")
			}
			return exprToValue(list.Items[1])
		case "if":
			return evalIf(list, env)
		case "define":
			return evalDefine(list, env)
		case "lambda":
			return evalLambda(list, env)
		case "and":
			return evalAnd(list.Items[1:], env)
		case "or":
			return evalOr(list.Items[1:], env)
		case "let":
			return evalLet(list, env)
		case "begin":
			return evalBegin(list.Items[1:], env)
		case "cond":
			return evalCond(list.Items[1:], env)
		}
	}

	// Evaluate operator
	op, err := evalInEnv(list.Items[0], env)
	if err != nil {
		return nil, err
	}
	// Evaluate arguments
	args := make([]Value, len(list.Items)-1)
	for i, item := range list.Items[1:] {
		args[i], err = evalInEnv(item, env)
		if err != nil {
			return nil, err
		}
	}
	return applyProcAt(op, args, list)
}

func exprToValue(expr Expr) (Value, error) {
	switch e := expr.(type) {
	case *AtomExpr:
		if e.Token == "#t" {
			return &BoolVal{Val: true}, nil
		}
		if e.Token == "#f" {
			return &BoolVal{Val: false}, nil
		}
		if len(e.Token) > 0 && e.Token[0] == '"' {
			s, err := strconv.Unquote(e.Token)
			if err != nil {
				return nil, &EvalError{Message: "bad string: " + e.Token}
			}
			return &StringVal{Val: s}, nil
		}
		if n, err := strconv.ParseInt(e.Token, 10, 64); err == nil {
			return &IntVal{Val: n}, nil
		}
		return &SymbolVal{Name: e.Token}, nil
	case *ListExpr:
		if len(e.Items) == 0 {
			return &NilVal{}, nil
		}
		// Build list from items
		var result Value = &NilVal{}
		for i := len(e.Items) - 1; i >= 0; i-- {
			car, err := exprToValue(e.Items[i])
			if err != nil {
				return nil, err
			}
			result = &PairVal{Car: car, Cdr: result}
		}
		return result, nil
	}
	return nil, &EvalError{Message: "unknown expression in quote"}
}

func evalIf(list *ListExpr, env *Env) (Value, error) {
	args := list.Items[1:]
	if len(args) < 2 || len(args) > 3 {
		return nil, errAt(list, "if requires 2 or 3 arguments")
	}
	cond, err := evalInEnv(args[0], env)
	if err != nil {
		return nil, err
	}
	if isTruthy(cond) {
		return evalInEnv(args[1], env)
	}
	if len(args) == 3 {
		return evalInEnv(args[2], env)
	}
	return &VoidVal{}, nil
}

func evalDefine(list *ListExpr, env *Env) (Value, error) {
	args := list.Items[1:]
	if len(args) < 2 {
		return nil, errAt(list, "define requires at least 2 arguments")
	}
	// (define (f params...) body...)
	if plist, ok := args[0].(*ListExpr); ok {
		if len(plist.Items) == 0 {
			return nil, errAt(list, "define: empty name list")
		}
		nameAtom, ok := plist.Items[0].(*AtomExpr)
		if !ok {
			return nil, errAt(list, "define: expected symbol")
		}
		params := make([]string, len(plist.Items)-1)
		for i, item := range plist.Items[1:] {
			p, ok := item.(*AtomExpr)
			if !ok {
				return nil, errAt(list, "define: expected parameter name")
			}
			params[i] = p.Token
		}
		lambda := &LambdaVal{Params: params, Body: args[1:], Env: env}
		env.set(nameAtom.Token, lambda)
		return &VoidVal{}, nil
	}
	// (define x expr)
	nameAtom, ok := args[0].(*AtomExpr)
	if !ok {
		return nil, errAt(list, "define: expected symbol")
	}
	val, err := evalInEnv(args[1], env)
	if err != nil {
		return nil, err
	}
	env.set(nameAtom.Token, val)
	return &VoidVal{}, nil
}

func evalLambda(list *ListExpr, env *Env) (Value, error) {
	args := list.Items[1:]
	if len(args) < 2 {
		return nil, errAt(list, "lambda requires params and body")
	}
	paramList, ok := args[0].(*ListExpr)
	if !ok {
		return nil, errAt(list, "lambda: expected parameter list")
	}
	params := make([]string, len(paramList.Items))
	for i, item := range paramList.Items {
		p, ok := item.(*AtomExpr)
		if !ok {
			return nil, errAt(list, "lambda: expected parameter name")
		}
		params[i] = p.Token
	}
	return &LambdaVal{Params: params, Body: args[1:], Env: env}, nil
}

func evalAnd(exprs []Expr, env *Env) (Value, error) {
	var result Value = &BoolVal{Val: true}
	for _, e := range exprs {
		v, err := evalInEnv(e, env)
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

func evalOr(exprs []Expr, env *Env) (Value, error) {
	var result Value = &BoolVal{Val: false}
	for _, e := range exprs {
		v, err := evalInEnv(e, env)
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

func evalLet(list *ListExpr, env *Env) (Value, error) {
	args := list.Items[1:]
	if len(args) < 2 {
		return nil, errAt(list, "let requires bindings and body")
	}
	// Named let: (let name ((var init) ...) body...)
	offset := 0
	var loopName string
	if atom, ok := args[0].(*AtomExpr); ok {
		loopName = atom.Token
		offset = 1
		if len(args) < 3 {
			return nil, errAt(list, "named let requires bindings and body")
		}
	}
	bindList, ok := args[offset].(*ListExpr)
	if !ok {
		return nil, errAt(list, "let: expected binding list")
	}
	names := make([]string, len(bindList.Items))
	vals := make([]Value, len(bindList.Items))
	for i, item := range bindList.Items {
		pair, ok := item.(*ListExpr)
		if !ok || len(pair.Items) != 2 {
			return nil, errAt(list, "let: bad binding")
		}
		nameAtom, ok := pair.Items[0].(*AtomExpr)
		if !ok {
			return nil, errAt(list, "let: expected symbol in binding")
		}
		names[i] = nameAtom.Token
		v, err := evalInEnv(pair.Items[1], env)
		if err != nil {
			return nil, err
		}
		vals[i] = v
	}
	letEnv := newEnv(env)
	for i, name := range names {
		letEnv.set(name, vals[i])
	}
	body := args[offset+1:]
	if loopName != "" {
		// Named let: bind name to a lambda that recurses
		lambda := &LambdaVal{Params: names, Body: body, Env: letEnv}
		letEnv.set(loopName, lambda)
	}
	var result Value
	var err error
	for _, bodyExpr := range body {
		result, err = evalInEnv(bodyExpr, letEnv)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalBegin(args []Expr, env *Env) (Value, error) {
	var result Value = &VoidVal{}
	var err error
	for _, e := range args {
		result, err = evalInEnv(e, env)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalCond(clauses []Expr, env *Env) (Value, error) {
	for _, clause := range clauses {
		cl, ok := clause.(*ListExpr)
		if !ok || len(cl.Items) == 0 {
			return nil, &EvalError{Message: "cond: bad clause"}
		}
		// Check for else
		if atom, ok := cl.Items[0].(*AtomExpr); ok && atom.Token == "else" {
			var result Value = &VoidVal{}
			var err error
			for _, e := range cl.Items[1:] {
				result, err = evalInEnv(e, env)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
		test, err := evalInEnv(cl.Items[0], env)
		if err != nil {
			return nil, err
		}
		if isTruthy(test) {
			var result Value = test
			for _, e := range cl.Items[1:] {
				result, err = evalInEnv(e, env)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
	}
	return &VoidVal{}, nil
}

// --------------- Procedure application ---------------

func applyProcAt(op Value, args []Value, callSite Expr) (Value, error) {
	switch fn := op.(type) {
	case *BuiltinVal:
		val, err := fn.Fn(args)
		if err != nil {
			// Add position to builtin errors if they don't have one
			if ee, ok := err.(*EvalError); ok && ee.Line == 0 {
				l, c := callSite.Pos()
				ee.Line = l
				ee.Col = c
			}
			return nil, err
		}
		return val, nil
	case *LambdaVal:
		if len(args) != len(fn.Params) {
			return nil, errAt(callSite, fmt.Sprintf("expected %d arguments, got %d", len(fn.Params), len(args)))
		}
		callEnv := newEnv(fn.Env)
		for i, p := range fn.Params {
			callEnv.set(p, args[i])
		}
		var result Value
		var err error
		for _, bodyExpr := range fn.Body {
			result, err = evalInEnv(bodyExpr, callEnv)
			if err != nil {
				return nil, err
			}
		}
		return result, nil
	}
	return nil, errAt(callSite, "not a procedure")
}

// --------------- Builtins ---------------

func makeBuiltinEnv(outBuf *strings.Builder) *Env {
	env := newEnv(nil)

	addBuiltin := func(name string, fn func([]Value) (Value, error)) {
		env.set(name, &BuiltinVal{Name: name, Fn: fn})
	}

	addBuiltin("+", func(args []Value) (Value, error) {
		var sum int64
		for _, a := range args {
			n, ok := a.(*IntVal)
			if !ok {
				return nil, &EvalError{Message: "expected number"}
			}
			sum += n.Val
		}
		return &IntVal{Val: sum}, nil
	})

	addBuiltin("-", func(args []Value) (Value, error) {
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
		first, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "expected number"}
		}
		val := first.Val
		for _, a := range args[1:] {
			n, ok := a.(*IntVal)
			if !ok {
				return nil, &EvalError{Message: "expected number"}
			}
			val -= n.Val
		}
		return &IntVal{Val: val}, nil
	})

	addBuiltin("*", func(args []Value) (Value, error) {
		var prod int64 = 1
		for _, a := range args {
			n, ok := a.(*IntVal)
			if !ok {
				return nil, &EvalError{Message: "expected number"}
			}
			prod *= n.Val
		}
		return &IntVal{Val: prod}, nil
	})

	addBuiltin("/", func(args []Value) (Value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: "/ requires at least 2 arguments"}
		}
		first, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "expected number"}
		}
		val := first.Val
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
	})

	cmpBuiltin := func(name string, cmp func(int64, int64) bool) {
		addBuiltin(name, func(args []Value) (Value, error) {
			if len(args) < 2 {
				return nil, &EvalError{Message: name + " requires at least 2 arguments"}
			}
			for i := 0; i < len(args)-1; i++ {
				a, ok := args[i].(*IntVal)
				if !ok {
					return nil, &EvalError{Message: "expected number"}
				}
				b, ok := args[i+1].(*IntVal)
				if !ok {
					return nil, &EvalError{Message: "expected number"}
				}
				if !cmp(a.Val, b.Val) {
					return &BoolVal{Val: false}, nil
				}
			}
			return &BoolVal{Val: true}, nil
		})
	}

	cmpBuiltin("=", func(a, b int64) bool { return a == b })
	cmpBuiltin("<", func(a, b int64) bool { return a < b })
	cmpBuiltin(">", func(a, b int64) bool { return a > b })
	cmpBuiltin("<=", func(a, b int64) bool { return a <= b })
	cmpBuiltin(">=", func(a, b int64) bool { return a >= b })

	addBuiltin("not", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "not requires 1 argument"}
		}
		return &BoolVal{Val: !isTruthy(args[0])}, nil
	})

	addBuiltin("cons", func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "cons requires 2 arguments"}
		}
		return &PairVal{Car: args[0], Cdr: args[1]}, nil
	})

	addBuiltin("car", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "car requires 1 argument"}
		}
		p, ok := args[0].(*PairVal)
		if !ok {
			return nil, &EvalError{Message: "car: not a pair"}
		}
		return p.Car, nil
	})

	addBuiltin("cdr", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "cdr requires 1 argument"}
		}
		p, ok := args[0].(*PairVal)
		if !ok {
			return nil, &EvalError{Message: "cdr: not a pair"}
		}
		return p.Cdr, nil
	})

	addBuiltin("null?", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "null? requires 1 argument"}
		}
		_, ok := args[0].(*NilVal)
		return &BoolVal{Val: ok}, nil
	})

	addBuiltin("list", func(args []Value) (Value, error) {
		var result Value = &NilVal{}
		for i := len(args) - 1; i >= 0; i-- {
			result = &PairVal{Car: args[i], Cdr: result}
		}
		return result, nil
	})

	addBuiltin("length", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "length requires 1 argument"}
		}
		var count int64
		cur := args[0]
		for {
			if _, ok := cur.(*NilVal); ok {
				break
			}
			p, ok := cur.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "length: not a proper list"}
			}
			count++
			cur = p.Cdr
		}
		return &IntVal{Val: count}, nil
	})

	addBuiltin("append", func(args []Value) (Value, error) {
		if len(args) == 0 {
			return &NilVal{}, nil
		}
		// Collect all but last into a flat slice, then append last as tail
		result := args[len(args)-1]
		for i := len(args) - 2; i >= 0; i-- {
			var elems []Value
			cur := args[i]
			for {
				if _, ok := cur.(*NilVal); ok {
					break
				}
				p, ok := cur.(*PairVal)
				if !ok {
					return nil, &EvalError{Message: "append: not a proper list"}
				}
				elems = append(elems, p.Car)
				cur = p.Cdr
			}
			for j := len(elems) - 1; j >= 0; j-- {
				result = &PairVal{Car: elems[j], Cdr: result}
			}
		}
		return result, nil
	})

	// Type predicates
	addBuiltin("number?", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "number? requires 1 argument"}
		}
		_, ok := args[0].(*IntVal)
		return &BoolVal{Val: ok}, nil
	})

	addBuiltin("string?", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "string? requires 1 argument"}
		}
		_, ok := args[0].(*StringVal)
		return &BoolVal{Val: ok}, nil
	})

	addBuiltin("boolean?", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "boolean? requires 1 argument"}
		}
		_, ok := args[0].(*BoolVal)
		return &BoolVal{Val: ok}, nil
	})

	addBuiltin("pair?", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "pair? requires 1 argument"}
		}
		_, ok := args[0].(*PairVal)
		return &BoolVal{Val: ok}, nil
	})

	addBuiltin("symbol?", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "symbol? requires 1 argument"}
		}
		_, ok := args[0].(*SymbolVal)
		return &BoolVal{Val: ok}, nil
	})

	addBuiltin("char?", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "char? requires 1 argument"}
		}
		_, ok := args[0].(*CharVal)
		return &BoolVal{Val: ok}, nil
	})

	// L05: display, write, newline
	addBuiltin("display", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "display requires 1 argument"}
		}
		if outBuf != nil {
			outBuf.WriteString(displayValue(args[0]))
		}
		return &VoidVal{}, nil
	})

	addBuiltin("write", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "write requires 1 argument"}
		}
		if outBuf != nil {
			outBuf.WriteString(args[0].String())
		}
		return &VoidVal{}, nil
	})

	addBuiltin("newline", func(args []Value) (Value, error) {
		if len(args) != 0 {
			return nil, &EvalError{Message: "newline requires 0 arguments"}
		}
		if outBuf != nil {
			outBuf.WriteByte('\n')
		}
		return &VoidVal{}, nil
	})

	// L05: string operations
	addBuiltin("string-append", func(args []Value) (Value, error) {
		var buf strings.Builder
		for _, a := range args {
			s, ok := a.(*StringVal)
			if !ok {
				return nil, &EvalError{Message: "string-append: expected string"}
			}
			buf.WriteString(s.Val)
		}
		return &StringVal{Val: buf.String()}, nil
	})

	addBuiltin("string-length", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "string-length requires 1 argument"}
		}
		s, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string-length: expected string"}
		}
		return &IntVal{Val: int64(len(s.Val))}, nil
	})

	addBuiltin("substring", func(args []Value) (Value, error) {
		if len(args) != 3 {
			return nil, &EvalError{Message: "substring requires 3 arguments"}
		}
		s, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "substring: expected string"}
		}
		start, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "substring: expected number"}
		}
		end, ok := args[2].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "substring: expected number"}
		}
		return &StringVal{Val: s.Val[start.Val:end.Val]}, nil
	})

	addBuiltin("string->number", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "string->number requires 1 argument"}
		}
		s, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string->number: expected string"}
		}
		n, err := strconv.ParseInt(s.Val, 10, 64)
		if err != nil {
			return &BoolVal{Val: false}, nil
		}
		return &IntVal{Val: n}, nil
	})

	addBuiltin("number->string", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "number->string requires 1 argument"}
		}
		n, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "number->string: expected number"}
		}
		return &StringVal{Val: strconv.FormatInt(n.Val, 10)}, nil
	})

	addBuiltin("symbol->string", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "symbol->string requires 1 argument"}
		}
		s, ok := args[0].(*SymbolVal)
		if !ok {
			return nil, &EvalError{Message: "symbol->string: expected symbol"}
		}
		return &StringVal{Val: s.Name}, nil
	})

	addBuiltin("string->symbol", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "string->symbol requires 1 argument"}
		}
		s, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string->symbol: expected string"}
		}
		return &SymbolVal{Name: s.Val}, nil
	})

	// L06: string-copy, string-set!
	addBuiltin("string-copy", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "string-copy requires 1 argument"}
		}
		s, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string-copy: expected string"}
		}
		return &StringVal{Val: s.Val}, nil
	})

	addBuiltin("string-set!", func(args []Value) (Value, error) {
		if len(args) != 3 {
			return nil, &EvalError{Message: "string-set! requires 3 arguments"}
		}
		s, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string-set!: expected string"}
		}
		idx, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "string-set!: expected number"}
		}
		ch, ok := args[2].(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "string-set!: expected char"}
		}
		i := int(idx.Val)
		if i < 0 || i >= len(s.Val) {
			return nil, &EvalError{Message: "string-set!: index out of range"}
		}
		bs := []byte(s.Val)
		bs[i] = byte(ch.Val)
		s.Val = string(bs)
		return &VoidVal{}, nil
	})

	addBuiltin("string-ref", func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "string-ref requires 2 arguments"}
		}
		s, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string-ref: expected string"}
		}
		idx, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "string-ref: expected number"}
		}
		if idx.Val < 0 || idx.Val >= int64(len(s.Val)) {
			return nil, &EvalError{Message: "string-ref: index out of range"}
		}
		return &CharVal{Val: rune(s.Val[idx.Val])}, nil
	})

	return env
}

// displayValue formats a value for `display` (no quotes on strings).
func displayValue(v Value) string {
	switch val := v.(type) {
	case *StringVal:
		return val.Val
	case *CharVal:
		return string(val.Val)
	case *PairVal:
		var buf strings.Builder
		buf.WriteByte('(')
		cur := Value(val)
		first := true
		for {
			p, ok := cur.(*PairVal)
			if !ok {
				break
			}
			if !first {
				buf.WriteByte(' ')
			}
			first = false
			buf.WriteString(displayValue(p.Car))
			cur = p.Cdr
		}
		if _, ok := cur.(*NilVal); !ok {
			buf.WriteString(" . ")
			buf.WriteString(displayValue(cur))
		}
		buf.WriteByte(')')
		return buf.String()
	default:
		return v.String()
	}
}

// --------------- Public API ---------------

func EvalStr(input string) (string, error) {
	input = strings.TrimSpace(input)
	if input == "" {
		return "", nil
	}
	exprs, err := parseAll(input)
	if err != nil {
		return "", err
	}
	env := makeBuiltinEnv(nil)
	var result Value
	for _, expr := range exprs {
		result, err = evalInEnv(expr, env)
		if err != nil {
			return "", err
		}
	}
	if _, ok := result.(*VoidVal); ok {
		return "", nil
	}
	return result.String(), nil
}

func EvalStrWithOutput(input string) (result string, output string, err error) {
	input = strings.TrimSpace(input)
	if input == "" {
		return "", "", nil
	}
	exprs, parseErr := parseAll(input)
	if parseErr != nil {
		return "", "", parseErr
	}
	var outBuf strings.Builder
	env := makeBuiltinEnv(&outBuf)
	var res Value
	for _, expr := range exprs {
		res, err = evalInEnv(expr, env)
		if err != nil {
			return "", "", err
		}
	}
	var r string
	if _, ok := res.(*VoidVal); !ok {
		r = res.String()
	}
	return r, outBuf.String(), nil
}
