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

func tokenize(input string) []string {
	var tokens []string
	i := 0
	for i < len(input) {
		ch := input[i]
		if unicode.IsSpace(rune(ch)) {
			i++
		} else if ch == ';' {
			for i < len(input) && input[i] != '\n' {
				i++
			}
		} else if ch == '\'' {
			tokens = append(tokens, "'")
			i++
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
			j := i + 1
			for j < len(input) && input[j] != '"' {
				if input[j] == '\\' {
					j++
				}
				j++
			}
			if j < len(input) {
				j++
			}
			tokens = append(tokens, input[i:j])
			i = j
		} else {
			j := i
			for j < len(input) && !unicode.IsSpace(rune(input[j])) && input[j] != '(' && input[j] != ')' && input[j] != '"' && input[j] != ';' && input[j] != '\'' {
				j++
			}
			tokens = append(tokens, input[i:j])
			i = j
		}
	}
	return tokens
}

// --------------- Parser ---------------

type Expr interface{}

type AtomExpr struct{ Token string }
type ListExpr struct{ Items []Expr }

func parse(tokens []string) (Expr, []string, error) {
	if len(tokens) == 0 {
		return nil, nil, &EvalError{Message: "unexpected EOF"}
	}
	tok := tokens[0]
	rest := tokens[1:]
	if tok == "'" {
		// 'x => (quote x)
		inner, rest2, err := parse(rest)
		if err != nil {
			return nil, nil, err
		}
		return &ListExpr{Items: []Expr{&AtomExpr{Token: "quote"}, inner}}, rest2, nil
	}
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
		rest = rest[1:]
		return &ListExpr{Items: items}, rest, nil
	} else if tok == ")" {
		return nil, nil, &EvalError{Message: "unexpected )"}
	}
	return &AtomExpr{Token: tok}, rest, nil
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

func evalInEnv(expr Expr, env *Env) (Value, error) {
	switch e := expr.(type) {
	case *AtomExpr:
		return evalAtomInEnv(e.Token, env)
	case *ListExpr:
		return evalListInEnv(e, env)
	}
	return nil, &EvalError{Message: "unknown expression"}
}

func evalAtomInEnv(token string, env *Env) (Value, error) {
	if token == "#t" {
		return &BoolVal{Val: true}, nil
	}
	if token == "#f" {
		return &BoolVal{Val: false}, nil
	}
	if len(token) > 0 && token[0] == '"' {
		s, err := strconv.Unquote(token)
		if err != nil {
			return nil, &EvalError{Message: "bad string: " + token}
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
	return nil, &EvalError{Message: "unbound variable: " + token}
}

func evalListInEnv(list *ListExpr, env *Env) (Value, error) {
	if len(list.Items) == 0 {
		return nil, &EvalError{Message: "empty application"}
	}

	// Check for special forms
	if atom, ok := list.Items[0].(*AtomExpr); ok {
		switch atom.Token {
		case "quote":
			if len(list.Items) != 2 {
				return nil, &EvalError{Message: "quote requires 1 argument"}
			}
			return exprToValue(list.Items[1])
		case "if":
			return evalIf(list.Items[1:], env)
		case "define":
			return evalDefine(list.Items[1:], env)
		case "lambda":
			return evalLambda(list.Items[1:], env)
		case "and":
			return evalAnd(list.Items[1:], env)
		case "or":
			return evalOr(list.Items[1:], env)
		case "let":
			return evalLet(list.Items[1:], env)
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
	return applyProc(op, args)
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

func evalIf(args []Expr, env *Env) (Value, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, &EvalError{Message: "if requires 2 or 3 arguments"}
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

func evalDefine(args []Expr, env *Env) (Value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "define requires at least 2 arguments"}
	}
	// (define (f params...) body...)
	if list, ok := args[0].(*ListExpr); ok {
		if len(list.Items) == 0 {
			return nil, &EvalError{Message: "define: empty name list"}
		}
		nameAtom, ok := list.Items[0].(*AtomExpr)
		if !ok {
			return nil, &EvalError{Message: "define: expected symbol"}
		}
		params := make([]string, len(list.Items)-1)
		for i, item := range list.Items[1:] {
			p, ok := item.(*AtomExpr)
			if !ok {
				return nil, &EvalError{Message: "define: expected parameter name"}
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
		return nil, &EvalError{Message: "define: expected symbol"}
	}
	val, err := evalInEnv(args[1], env)
	if err != nil {
		return nil, err
	}
	env.set(nameAtom.Token, val)
	return &VoidVal{}, nil
}

func evalLambda(args []Expr, env *Env) (Value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "lambda requires params and body"}
	}
	paramList, ok := args[0].(*ListExpr)
	if !ok {
		return nil, &EvalError{Message: "lambda: expected parameter list"}
	}
	params := make([]string, len(paramList.Items))
	for i, item := range paramList.Items {
		p, ok := item.(*AtomExpr)
		if !ok {
			return nil, &EvalError{Message: "lambda: expected parameter name"}
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

func evalLet(args []Expr, env *Env) (Value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "let requires bindings and body"}
	}
	// Named let: (let name ((var init) ...) body...)
	offset := 0
	var loopName string
	if atom, ok := args[0].(*AtomExpr); ok {
		loopName = atom.Token
		offset = 1
		if len(args) < 3 {
			return nil, &EvalError{Message: "named let requires bindings and body"}
		}
	}
	bindList, ok := args[offset].(*ListExpr)
	if !ok {
		return nil, &EvalError{Message: "let: expected binding list"}
	}
	names := make([]string, len(bindList.Items))
	vals := make([]Value, len(bindList.Items))
	for i, item := range bindList.Items {
		pair, ok := item.(*ListExpr)
		if !ok || len(pair.Items) != 2 {
			return nil, &EvalError{Message: "let: bad binding"}
		}
		nameAtom, ok := pair.Items[0].(*AtomExpr)
		if !ok {
			return nil, &EvalError{Message: "let: expected symbol in binding"}
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
		// The lambda's env is letEnv which contains itself
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

func applyProc(op Value, args []Value) (Value, error) {
	switch fn := op.(type) {
	case *BuiltinVal:
		return fn.Fn(args)
	case *LambdaVal:
		if len(args) != len(fn.Params) {
			return nil, &EvalError{Message: fmt.Sprintf("expected %d arguments, got %d", len(fn.Params), len(args))}
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
	return nil, &EvalError{Message: "not a procedure"}
}

// --------------- Builtins ---------------

func makeBuiltinEnv() *Env {
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

	return env
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
	env := makeBuiltinEnv()
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
	r, err := EvalStr(input)
	return r, "", err
}
