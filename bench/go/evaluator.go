package ming

import (
	"fmt"
	"math"
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
type StringVal struct {
	Val       string
	Immutable bool
}
type SymbolVal struct{ Name string }
type PairVal struct{ Car, Cdr Value }
type NilVal struct{} // empty list
type VoidVal struct{}
type CharVal struct{ Val rune }
type RatVal struct{ Num, Den int64 } // exact rational, Den > 0, gcd(|Num|,Den)==1
type FloatVal struct{ Val float64 }  // inexact

type LambdaVal struct {
	Params   []string
	Rest     string // rest parameter name (dot notation), empty if none
	Body     []Expr
	Env      *Env
}

type BuiltinVal struct {
	Name string
	Fn   func([]Value) (Value, error)
}

type CaseLambdaVal struct {
	Clauses []*LambdaVal
}

type VectorVal struct {
	Items []Value
}

type SyntaxRulesVal struct {
	Literals []string
	Rules    []syntaxRule
	DefEnv   *Env
}

type syntaxRule struct {
	Pattern  Expr
	Template Expr
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
func (v *RatVal) String() string {
	if v.Den == 1 {
		return strconv.FormatInt(v.Num, 10)
	}
	return fmt.Sprintf("%d/%d", v.Num, v.Den)
}
func (v *FloatVal) String() string {
	s := strconv.FormatFloat(v.Val, 'f', -1, 64)
	// Ensure there's a decimal point for whole floats
	if !strings.Contains(s, ".") {
		s += ".0"
	}
	return s
}
func (v *LambdaVal) String() string  { return "#<procedure>" }
func (v *BuiltinVal) String() string      { return "#<builtin:" + v.Name + ">" }
func (v *CaseLambdaVal) String() string    { return "#<procedure>" }
func (v *SyntaxRulesVal) String() string   { return "#<syntax>" }
func (v *VectorVal) String() string {
	var buf strings.Builder
	buf.WriteString("#(")
	for i, item := range v.Items {
		if i > 0 {
			buf.WriteByte(' ')
		}
		buf.WriteString(item.String())
	}
	buf.WriteByte(')')
	return buf.String()
}

// Record types
type RecordTypeDesc struct {
	Name   string
	Fields []string
}

type RecordVal struct {
	Type   *RecordTypeDesc
	Fields map[string]Value
}

func (v *RecordVal) String() string { return "#<record:" + v.Type.Name + ">" }

var gensymCounter int

func gensym(base string) string {
	gensymCounter++
	return fmt.Sprintf("##%s~%d", base, gensymCounter)
}

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

// --------------- Numeric helpers ---------------

func gcdInt(a, b int64) int64 {
	if a < 0 {
		a = -a
	}
	if b < 0 {
		b = -b
	}
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func makeRat(num, den int64) Value {
	if den == 0 {
		panic("zero denominator in makeRat")
	}
	if den < 0 {
		num, den = -num, -den
	}
	if num == 0 {
		return &IntVal{Val: 0}
	}
	g := gcdInt(num, den)
	num /= g
	den /= g
	if den == 1 {
		return &IntVal{Val: num}
	}
	return &RatVal{Num: num, Den: den}
}

// toFloat64 extracts a float64 from any numeric Value
func toFloat64(v Value) (float64, bool) {
	switch n := v.(type) {
	case *IntVal:
		return float64(n.Val), true
	case *RatVal:
		return float64(n.Num) / float64(n.Den), true
	case *FloatVal:
		return n.Val, true
	}
	return 0, false
}

// isExact returns true for IntVal and RatVal
func isExact(v Value) bool {
	switch v.(type) {
	case *IntVal, *RatVal:
		return true
	}
	return false
}

// isNumber returns true for all numeric types
func isNumber(v Value) bool {
	switch v.(type) {
	case *IntVal, *RatVal, *FloatVal:
		return true
	}
	return false
}

// numAdd adds two numeric values
func numAdd(a, b Value) Value {
	if isExact(a) && isExact(b) {
		an, ad := toRat(a)
		bn, bd := toRat(b)
		return makeRat(an*bd+bn*ad, ad*bd)
	}
	af, _ := toFloat64(a)
	bf, _ := toFloat64(b)
	return &FloatVal{Val: af + bf}
}

// numSub subtracts b from a
func numSub(a, b Value) Value {
	if isExact(a) && isExact(b) {
		an, ad := toRat(a)
		bn, bd := toRat(b)
		return makeRat(an*bd-bn*ad, ad*bd)
	}
	af, _ := toFloat64(a)
	bf, _ := toFloat64(b)
	return &FloatVal{Val: af - bf}
}

// numMul multiplies two numeric values
func numMul(a, b Value) Value {
	if isExact(a) && isExact(b) {
		an, ad := toRat(a)
		bn, bd := toRat(b)
		return makeRat(an*bn, ad*bd)
	}
	af, _ := toFloat64(a)
	bf, _ := toFloat64(b)
	return &FloatVal{Val: af * bf}
}

// numDiv divides a by b, returns (result, error)
func numDiv(a, b Value) (Value, error) {
	bf, _ := toFloat64(b)
	if bf == 0 {
		return nil, &EvalError{Message: "division by zero"}
	}
	if isExact(a) && isExact(b) {
		an, ad := toRat(a)
		bn, bd := toRat(b)
		return makeRat(an*bd, ad*bn), nil
	}
	af, _ := toFloat64(a)
	return &FloatVal{Val: af / bf}, nil
}

// numNeg negates a numeric value
func numNeg(v Value) Value {
	switch n := v.(type) {
	case *IntVal:
		return &IntVal{Val: -n.Val}
	case *RatVal:
		return &RatVal{Num: -n.Num, Den: n.Den}
	case *FloatVal:
		return &FloatVal{Val: -n.Val}
	}
	return v
}

// toRat converts exact numeric to num/den
func toRat(v Value) (int64, int64) {
	switch n := v.(type) {
	case *IntVal:
		return n.Val, 1
	case *RatVal:
		return n.Num, n.Den
	}
	return 0, 1
}

// numCmp compares two numbers, returns -1, 0, 1
func numCmp(a, b Value) int {
	if isExact(a) && isExact(b) {
		an, ad := toRat(a)
		bn, bd := toRat(b)
		lhs := an * bd
		rhs := bn * ad
		if lhs < rhs {
			return -1
		} else if lhs > rhs {
			return 1
		}
		return 0
	}
	af, _ := toFloat64(a)
	bf, _ := toFloat64(b)
	if af < bf {
		return -1
	} else if af > bf {
		return 1
	}
	return 0
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

func (e *Env) setExisting(name string, val Value) bool {
	if _, ok := e.bindings[name]; ok {
		e.bindings[name] = val
		return true
	}
	if e.parent != nil {
		return e.parent.setExisting(name, val)
	}
	return false
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
			if i+1 < len(input) && input[i+1] == '(' {
				tokens = append(tokens, Token{"#(", line, startCol})
				i += 2
				col += 2
			} else if i+1 < len(input) && input[i+1] == '\\' {
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
	if tok.Val == "#(" {
		// Vector literal: #(items...)
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
			return nil, nil, &EvalError{Message: "missing closing paren in vector literal", Line: tok.Line, Col: tok.Col}
		}
		rest = rest[1:]
		// Represent as (##vector-literal items...)
		allItems := make([]Expr, len(items)+1)
		allItems[0] = &AtomExpr{Token: "##vector-literal", Line: tok.Line, Col: tok.Col}
		copy(allItems[1:], items)
		return &ListExpr{Items: allItems, Line: tok.Line, Col: tok.Col}, rest, nil
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
		return &StringVal{Val: s, Immutable: true}, nil
	}
	if n, err := strconv.ParseInt(token, 10, 64); err == nil {
		return &IntVal{Val: n}, nil
	}
	// Rational literal: digits/digits
	if idx := strings.Index(token, "/"); idx > 0 && idx < len(token)-1 {
		numStr, denStr := token[:idx], token[idx+1:]
		if num, err := strconv.ParseInt(numStr, 10, 64); err == nil {
			if den, err := strconv.ParseInt(denStr, 10, 64); err == nil && den != 0 {
				return makeRat(num, den), nil
			}
		}
	}
	// Float literal
	if f, err := strconv.ParseFloat(token, 64); err == nil {
		return &FloatVal{Val: f}, nil
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
		case "set!":
			return evalSetBang(list, env)
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
		case "define-syntax":
			return evalDefineSyntax(list, env)
		case "define-record-type":
			return evalDefineRecordType(list, env)
		case "case-lambda":
			return evalCaseLambda(list, env)
		case "letrec":
			return evalLetrec(list, env)
		case "letrec*":
			return evalLetrecStar(list, env)
		case "case":
			return evalCase(list, env)
		case "do":
			return evalDo(list, env)
		case "##vector-literal":
			items := make([]Value, len(list.Items)-1)
			for i, item := range list.Items[1:] {
				v, err := evalInEnv(item, env)
				if err != nil {
					return nil, err
				}
				items[i] = v
			}
			return &VectorVal{Items: items}, nil
		}
	}

	// Check for macro expansion
	if atom, ok := list.Items[0].(*AtomExpr); ok {
		if v, found := env.get(atom.Token); found {
			if macro, isMacro := v.(*SyntaxRulesVal); isMacro {
				expanded, expandErr := expandMacro(macro, list, env)
				if expandErr != nil {
					return nil, expandErr
				}
				return evalInEnv(expanded, env)
			}
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
			return &StringVal{Val: s, Immutable: true}, nil
		}
		if n, err := strconv.ParseInt(e.Token, 10, 64); err == nil {
			return &IntVal{Val: n}, nil
		}
		return &SymbolVal{Name: e.Token}, nil
	case *ListExpr:
		if len(e.Items) == 0 {
			return &NilVal{}, nil
		}
		// Check for vector literal in quote context
		if atom, ok := e.Items[0].(*AtomExpr); ok && atom.Token == "##vector-literal" {
			items := make([]Value, len(e.Items)-1)
			for i, item := range e.Items[1:] {
				v, err := exprToValue(item)
				if err != nil {
					return nil, err
				}
				items[i] = v
			}
			return &VectorVal{Items: items}, nil
		}
		// Check for dot notation: (a b . c)
		dotIdx := -1
		for i, item := range e.Items {
			if a, ok := item.(*AtomExpr); ok && a.Token == "." {
				dotIdx = i
				break
			}
		}
		if dotIdx >= 0 {
			if dotIdx != len(e.Items)-2 {
				return nil, &EvalError{Message: "bad dot syntax"}
			}
			cdr, err := exprToValue(e.Items[len(e.Items)-1])
			if err != nil {
				return nil, err
			}
			for i := dotIdx - 1; i >= 0; i-- {
				car, err := exprToValue(e.Items[i])
				if err != nil {
					return nil, err
				}
				cdr = &PairVal{Car: car, Cdr: cdr}
			}
			return cdr, nil
		}
		// Build proper list from items
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
		params, rest, err := parseDotParams(plist.Items[1:], list)
		if err != nil {
			return nil, err
		}
		lambda := &LambdaVal{Params: params, Rest: rest, Body: args[1:], Env: env}
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

func evalSetBang(list *ListExpr, env *Env) (Value, error) {
	args := list.Items[1:]
	if len(args) != 2 {
		return nil, errAt(list, "set! requires exactly 2 arguments")
	}
	nameAtom, ok := args[0].(*AtomExpr)
	if !ok {
		return nil, errAt(list, "set!: expected symbol")
	}
	val, err := evalInEnv(args[1], env)
	if err != nil {
		return nil, err
	}
	if !env.setExisting(nameAtom.Token, val) {
		return nil, errAt(list, fmt.Sprintf("set!: unbound variable '%s'", nameAtom.Token))
	}
	return &VoidVal{}, nil
}

func parseDotParams(items []Expr, errExpr Expr) ([]string, string, error) {
	var params []string
	var rest string
	for i, item := range items {
		a, ok := item.(*AtomExpr)
		if !ok {
			return nil, "", errAt(errExpr, "expected parameter name")
		}
		if a.Token == "." {
			if i != len(items)-2 {
				return nil, "", errAt(errExpr, "malformed dot parameter list")
			}
			restAtom, ok := items[i+1].(*AtomExpr)
			if !ok {
				return nil, "", errAt(errExpr, "expected rest parameter name")
			}
			rest = restAtom.Token
			break
		}
		params = append(params, a.Token)
	}
	return params, rest, nil
}

func evalLambda(list *ListExpr, env *Env) (Value, error) {
	args := list.Items[1:]
	if len(args) < 2 {
		return nil, errAt(list, "lambda requires params and body")
	}
	// (lambda args body) — single symbol captures all args
	if atom, ok := args[0].(*AtomExpr); ok {
		return &LambdaVal{Rest: atom.Token, Body: args[1:], Env: env}, nil
	}
	paramList, ok := args[0].(*ListExpr)
	if !ok {
		return nil, errAt(list, "lambda: expected parameter list")
	}
	params, rest, err := parseDotParams(paramList.Items, list)
	if err != nil {
		return nil, err
	}
	return &LambdaVal{Params: params, Rest: rest, Body: args[1:], Env: env}, nil
}

func evalCaseLambda(list *ListExpr, env *Env) (Value, error) {
	clauses := list.Items[1:]
	if len(clauses) == 0 {
		return nil, errAt(list, "case-lambda requires at least one clause")
	}
	var lambdas []*LambdaVal
	for _, c := range clauses {
		cl, ok := c.(*ListExpr)
		if !ok || len(cl.Items) < 2 {
			return nil, errAt(list, "case-lambda: bad clause")
		}
		// Parse formals
		paramExpr := cl.Items[0]
		body := cl.Items[1:]
		if atom, ok := paramExpr.(*AtomExpr); ok {
			// (rest-arg body...) — variadic catching all args
			lambdas = append(lambdas, &LambdaVal{Rest: atom.Token, Body: body, Env: env})
			continue
		}
		paramList, ok := paramExpr.(*ListExpr)
		if !ok {
			return nil, errAt(list, "case-lambda: expected parameter list")
		}
		params, rest, err := parseDotParams(paramList.Items, list)
		if err != nil {
			return nil, err
		}
		lambdas = append(lambdas, &LambdaVal{Params: params, Rest: rest, Body: body, Env: env})
	}
	return &CaseLambdaVal{Clauses: lambdas}, nil
}

func applyCaseLambda(cl *CaseLambdaVal, args []Value) (Value, error) {
	for _, lam := range cl.Clauses {
		if lam.Rest != "" {
			if len(args) >= len(lam.Params) {
				return applyLambda(lam, args)
			}
		} else {
			if len(args) == len(lam.Params) {
				return applyLambda(lam, args)
			}
		}
	}
	return nil, &EvalError{Message: fmt.Sprintf("case-lambda: no matching clause for %d arguments", len(args))}
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

// --------------- L14: letrec, letrec*, case, do ---------------

func evalLetrec(list *ListExpr, env *Env) (Value, error) {
	args := list.Items[1:]
	if len(args) < 2 {
		return nil, errAt(list, "letrec requires bindings and body")
	}
	bindList, ok := args[0].(*ListExpr)
	if !ok {
		return nil, errAt(list, "letrec: expected binding list")
	}
	letEnv := newEnv(env)
	// First pass: bind all names to undefined
	names := make([]string, len(bindList.Items))
	initExprs := make([]Expr, len(bindList.Items))
	for i, item := range bindList.Items {
		pair, ok := item.(*ListExpr)
		if !ok || len(pair.Items) != 2 {
			return nil, errAt(list, "letrec: bad binding")
		}
		nameAtom, ok := pair.Items[0].(*AtomExpr)
		if !ok {
			return nil, errAt(list, "letrec: expected symbol in binding")
		}
		names[i] = nameAtom.Token
		initExprs[i] = pair.Items[1]
		letEnv.set(nameAtom.Token, &VoidVal{})
	}
	// Second pass: evaluate init expressions in letEnv (all names visible)
	for i, initExpr := range initExprs {
		v, err := evalInEnv(initExpr, letEnv)
		if err != nil {
			return nil, err
		}
		letEnv.set(names[i], v)
	}
	// Evaluate body
	var result Value
	var err error
	for _, bodyExpr := range args[1:] {
		result, err = evalInEnv(bodyExpr, letEnv)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalLetrecStar(list *ListExpr, env *Env) (Value, error) {
	args := list.Items[1:]
	if len(args) < 2 {
		return nil, errAt(list, "letrec* requires bindings and body")
	}
	bindList, ok := args[0].(*ListExpr)
	if !ok {
		return nil, errAt(list, "letrec*: expected binding list")
	}
	letEnv := newEnv(env)
	for _, item := range bindList.Items {
		pair, ok := item.(*ListExpr)
		if !ok || len(pair.Items) != 2 {
			return nil, errAt(list, "letrec*: bad binding")
		}
		nameAtom, ok := pair.Items[0].(*AtomExpr)
		if !ok {
			return nil, errAt(list, "letrec*: expected symbol in binding")
		}
		v, err := evalInEnv(pair.Items[1], letEnv)
		if err != nil {
			return nil, err
		}
		letEnv.set(nameAtom.Token, v)
	}
	var result Value
	var err error
	for _, bodyExpr := range args[1:] {
		result, err = evalInEnv(bodyExpr, letEnv)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalCase(list *ListExpr, env *Env) (Value, error) {
	if len(list.Items) < 3 {
		return nil, errAt(list, "case requires key and at least one clause")
	}
	key, err := evalInEnv(list.Items[1], env)
	if err != nil {
		return nil, err
	}
	for _, clauseExpr := range list.Items[2:] {
		clause, ok := clauseExpr.(*ListExpr)
		if !ok || len(clause.Items) < 2 {
			return nil, errAt(list, "case: bad clause")
		}
		// Check for else
		if atom, ok := clause.Items[0].(*AtomExpr); ok && atom.Token == "else" {
			var result Value = &VoidVal{}
			for _, e := range clause.Items[1:] {
				result, err = evalInEnv(e, env)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
		// Datum list
		datumList, ok := clause.Items[0].(*ListExpr)
		if !ok {
			return nil, errAt(list, "case: expected datum list")
		}
		matched := false
		for _, datumExpr := range datumList.Items {
			datum, err := exprToValue(datumExpr)
			if err != nil {
				return nil, err
			}
			if valuesEqv(key, datum) {
				matched = true
				break
			}
		}
		if matched {
			var result Value = &VoidVal{}
			for _, e := range clause.Items[1:] {
				result, err = evalInEnv(e, env)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
	}
	// No match, no else => void
	return &VoidVal{}, nil
}

func valuesEqv(a, b Value) bool {
	switch av := a.(type) {
	case *IntVal:
		bv, ok := b.(*IntVal)
		return ok && av.Val == bv.Val
	case *RatVal:
		bv, ok := b.(*RatVal)
		return ok && av.Num == bv.Num && av.Den == bv.Den
	case *FloatVal:
		bv, ok := b.(*FloatVal)
		return ok && av.Val == bv.Val
	case *BoolVal:
		bv, ok := b.(*BoolVal)
		return ok && av.Val == bv.Val
	case *SymbolVal:
		bv, ok := b.(*SymbolVal)
		return ok && av.Name == bv.Name
	case *CharVal:
		bv, ok := b.(*CharVal)
		return ok && av.Val == bv.Val
	case *NilVal:
		_, ok := b.(*NilVal)
		return ok
	default:
		return a == b
	}
}

func evalDo(list *ListExpr, env *Env) (Value, error) {
	// (do ((var init step) ...) (test expr ...) body ...)
	if len(list.Items) < 3 {
		return nil, errAt(list, "do requires variable list and test clause")
	}
	varList, ok := list.Items[1].(*ListExpr)
	if !ok {
		return nil, errAt(list, "do: expected variable list")
	}
	testClause, ok := list.Items[2].(*ListExpr)
	if !ok || len(testClause.Items) == 0 {
		return nil, errAt(list, "do: expected test clause")
	}
	bodyExprs := list.Items[3:]

	type doVar struct {
		name string
		step Expr // nil if no step
	}
	vars := make([]doVar, len(varList.Items))
	doEnv := newEnv(env)

	// Initialize variables
	for i, item := range varList.Items {
		vl, ok := item.(*ListExpr)
		if !ok || len(vl.Items) < 2 || len(vl.Items) > 3 {
			return nil, errAt(list, "do: bad variable spec")
		}
		nameAtom, ok := vl.Items[0].(*AtomExpr)
		if !ok {
			return nil, errAt(list, "do: expected variable name")
		}
		initVal, err := evalInEnv(vl.Items[1], env)
		if err != nil {
			return nil, err
		}
		vars[i] = doVar{name: nameAtom.Token}
		if len(vl.Items) == 3 {
			vars[i].step = vl.Items[2]
		}
		doEnv.set(nameAtom.Token, initVal)
	}

	// Iteration loop
	for {
		// Test
		testVal, err := evalInEnv(testClause.Items[0], doEnv)
		if err != nil {
			return nil, err
		}
		if isTruthy(testVal) {
			// Evaluate result expressions
			if len(testClause.Items) > 1 {
				var result Value
				for _, e := range testClause.Items[1:] {
					result, err = evalInEnv(e, doEnv)
					if err != nil {
						return nil, err
					}
				}
				return result, nil
			}
			return &VoidVal{}, nil
		}
		// Evaluate body
		for _, bodyExpr := range bodyExprs {
			_, err = evalInEnv(bodyExpr, doEnv)
			if err != nil {
				return nil, err
			}
		}
		// Compute step values (parallel update)
		newVals := make([]Value, len(vars))
		for i, v := range vars {
			if v.step != nil {
				newVals[i], err = evalInEnv(v.step, doEnv)
				if err != nil {
					return nil, err
				}
			} else {
				newVals[i], _ = doEnv.get(v.name)
			}
		}
		// Update all at once
		for i, v := range vars {
			doEnv.set(v.name, newVals[i])
		}
	}
}

// --------------- Macros (syntax-rules) ---------------

func evalDefineSyntax(list *ListExpr, env *Env) (Value, error) {
	if len(list.Items) != 3 {
		return nil, errAt(list, "define-syntax requires 2 arguments")
	}
	nameAtom, ok := list.Items[1].(*AtomExpr)
	if !ok {
		return nil, errAt(list, "define-syntax: expected symbol")
	}
	sr, ok := list.Items[2].(*ListExpr)
	if !ok || len(sr.Items) < 2 {
		return nil, errAt(list, "define-syntax: expected syntax-rules")
	}
	srHead, ok := sr.Items[0].(*AtomExpr)
	if !ok || srHead.Token != "syntax-rules" {
		return nil, errAt(list, "define-syntax: expected syntax-rules")
	}
	litList, ok := sr.Items[1].(*ListExpr)
	if !ok {
		return nil, errAt(list, "syntax-rules: expected literal list")
	}
	var literals []string
	for _, lit := range litList.Items {
		a, ok := lit.(*AtomExpr)
		if !ok {
			return nil, errAt(list, "syntax-rules: expected symbol in literals")
		}
		literals = append(literals, a.Token)
	}
	var rules []syntaxRule
	for _, ruleExpr := range sr.Items[2:] {
		rule, ok := ruleExpr.(*ListExpr)
		if !ok || len(rule.Items) != 2 {
			return nil, errAt(list, "syntax-rules: bad rule")
		}
		rules = append(rules, syntaxRule{Pattern: rule.Items[0], Template: rule.Items[1]})
	}
	env.set(nameAtom.Token, &SyntaxRulesVal{Literals: literals, Rules: rules, DefEnv: env})
	return &VoidVal{}, nil
}

// (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
func evalDefineRecordType(list *ListExpr, env *Env) (Value, error) {
	if len(list.Items) < 4 {
		return nil, errAt(list, "define-record-type: too few arguments")
	}

	// 1. Type name
	_, ok := list.Items[1].(*AtomExpr)
	if !ok {
		return nil, errAt(list, "define-record-type: expected type name")
	}
	typeName := list.Items[1].(*AtomExpr).Token

	// 2. Constructor: (constructor-name field ...)
	ctorList, ok := list.Items[2].(*ListExpr)
	if !ok || len(ctorList.Items) < 1 {
		return nil, errAt(list, "define-record-type: expected constructor")
	}
	ctorNameAtom, ok := ctorList.Items[0].(*AtomExpr)
	if !ok {
		return nil, errAt(list, "define-record-type: expected constructor name")
	}
	ctorName := ctorNameAtom.Token
	var ctorFields []string
	for _, item := range ctorList.Items[1:] {
		a, ok := item.(*AtomExpr)
		if !ok {
			return nil, errAt(list, "define-record-type: expected field name in constructor")
		}
		ctorFields = append(ctorFields, a.Token)
	}

	// 3. Predicate name
	predAtom, ok := list.Items[3].(*AtomExpr)
	if !ok {
		return nil, errAt(list, "define-record-type: expected predicate name")
	}
	predName := predAtom.Token

	// 4. Field specs: (field accessor) ...
	var allFields []string
	type fieldSpec struct {
		name     string
		accessor string
	}
	var specs []fieldSpec
	for _, item := range list.Items[4:] {
		fList, ok := item.(*ListExpr)
		if !ok || len(fList.Items) < 2 {
			return nil, errAt(list, "define-record-type: bad field spec")
		}
		fName, ok := fList.Items[0].(*AtomExpr)
		if !ok {
			return nil, errAt(list, "define-record-type: expected field name")
		}
		fAccessor, ok := fList.Items[1].(*AtomExpr)
		if !ok {
			return nil, errAt(list, "define-record-type: expected accessor name")
		}
		allFields = append(allFields, fName.Token)
		specs = append(specs, fieldSpec{name: fName.Token, accessor: fAccessor.Token})
	}

	// Create the type descriptor
	rtd := &RecordTypeDesc{Name: typeName, Fields: allFields}

	// Define constructor
	env.set(ctorName, &BuiltinVal{Name: ctorName, Fn: func(args []Value) (Value, error) {
		if len(args) != len(ctorFields) {
			return nil, &EvalError{Message: fmt.Sprintf("%s: expected %d arguments, got %d", ctorName, len(ctorFields), len(args))}
		}
		fields := make(map[string]Value)
		for i, f := range ctorFields {
			fields[f] = args[i]
		}
		return &RecordVal{Type: rtd, Fields: fields}, nil
	}})

	// Define predicate
	env.set(predName, &BuiltinVal{Name: predName, Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: predName + ": expected 1 argument"}
		}
		rec, ok := args[0].(*RecordVal)
		return &BoolVal{Val: ok && rec.Type == rtd}, nil
	}})

	// Define accessors
	for _, spec := range specs {
		fieldName := spec.name
		accName := spec.accessor
		env.set(accName, &BuiltinVal{Name: accName, Fn: func(args []Value) (Value, error) {
			if len(args) != 1 {
				return nil, &EvalError{Message: accName + ": expected 1 argument"}
			}
			rec, ok := args[0].(*RecordVal)
			if !ok || rec.Type != rtd {
				return nil, &EvalError{Message: accName + ": not a " + typeName}
			}
			return rec.Fields[fieldName], nil
		}})
	}

	return &VoidVal{}, nil
}

type patternBindings struct {
	singles map[string]Expr
	lists   map[string][]Expr
}

func expandMacro(macro *SyntaxRulesVal, form *ListExpr, env *Env) (Expr, error) {
	for _, rule := range macro.Rules {
		bindings := &patternBindings{
			singles: make(map[string]Expr),
			lists:   make(map[string][]Expr),
		}
		if matchPatternList(rule.Pattern, form, macro.Literals, bindings) {
			renames := make(map[string]string)
			expanded := expandTemplate(rule.Template, bindings, macro, env, renames)
			return expanded, nil
		}
	}
	return nil, errAt(form, "no matching syntax-rules pattern")
}

func matchPatternList(pattern Expr, input *ListExpr, literals []string, bindings *patternBindings) bool {
	patList, ok := pattern.(*ListExpr)
	if !ok {
		return false
	}
	// Skip first element (macro name)
	return matchElements(patList.Items[1:], input.Items[1:], literals, bindings)
}

func matchElements(patElems, inElems []Expr, literals []string, bindings *patternBindings) bool {
	pi := 0
	ii := 0
	for pi < len(patElems) {
		// Check if current element is followed by ...
		if pi+1 < len(patElems) {
			if a, ok := patElems[pi+1].(*AtomExpr); ok && a.Token == "..." {
				patVar, ok := patElems[pi].(*AtomExpr)
				if !ok {
					return false
				}
				remaining := len(patElems) - pi - 2
				available := len(inElems) - ii - remaining
				if available < 0 {
					return false
				}
				collected := make([]Expr, available)
				for j := 0; j < available; j++ {
					collected[j] = inElems[ii+j]
				}
				bindings.lists[patVar.Token] = collected
				ii += available
				pi += 2
				continue
			}
		}
		if ii >= len(inElems) {
			return false
		}
		patAtom, isAtom := patElems[pi].(*AtomExpr)
		if isAtom {
			isLiteral := false
			for _, lit := range literals {
				if patAtom.Token == lit {
					isLiteral = true
					break
				}
			}
			if isLiteral {
				inAtom, ok := inElems[ii].(*AtomExpr)
				if !ok || inAtom.Token != patAtom.Token {
					return false
				}
			} else if patAtom.Token == "_" {
				// wildcard, matches anything
			} else {
				bindings.singles[patAtom.Token] = inElems[ii]
			}
		} else if patList, ok := patElems[pi].(*ListExpr); ok {
			inList, ok := inElems[ii].(*ListExpr)
			if !ok {
				return false
			}
			if !matchElements(patList.Items, inList.Items, literals, bindings) {
				return false
			}
		} else {
			return false
		}
		pi++
		ii++
	}
	return ii == len(inElems)
}

func expandTemplate(tmpl Expr, bindings *patternBindings, macro *SyntaxRulesVal, env *Env, renames map[string]string) Expr {
	switch t := tmpl.(type) {
	case *AtomExpr:
		if _, ok := bindings.singles[t.Token]; ok {
			return bindings.singles[t.Token]
		}
		if _, ok := bindings.lists[t.Token]; ok {
			return t
		}
		if isSelfEvaluating(t.Token) || isSpecialForm(t.Token) {
			return t
		}
		if newName, ok := renames[t.Token]; ok {
			return &AtomExpr{Token: newName, Line: t.Line, Col: t.Col}
		}
		gs := gensym(t.Token)
		renames[t.Token] = gs
		if val, ok := macro.DefEnv.get(t.Token); ok {
			env.set(gs, val)
		}
		return &AtomExpr{Token: gs, Line: t.Line, Col: t.Col}
	case *ListExpr:
		var items []Expr
		for i := 0; i < len(t.Items); i++ {
			if i+1 < len(t.Items) {
				if a, ok := t.Items[i+1].(*AtomExpr); ok && a.Token == "..." {
					vars := collectEllipsisVars(t.Items[i], bindings)
					if len(vars) > 0 {
						count := len(bindings.lists[vars[0]])
						for j := 0; j < count; j++ {
							iterBindings := &patternBindings{
								singles: make(map[string]Expr),
								lists:   bindings.lists,
							}
							for k, v := range bindings.singles {
								iterBindings.singles[k] = v
							}
							for _, v := range vars {
								if j < len(bindings.lists[v]) {
									iterBindings.singles[v] = bindings.lists[v][j]
								}
							}
							expanded := expandTemplate(t.Items[i], iterBindings, macro, env, renames)
							items = append(items, expanded)
						}
					}
					i++ // skip ...
					continue
				}
			}
			items = append(items, expandTemplate(t.Items[i], bindings, macro, env, renames))
		}
		return &ListExpr{Items: items, Line: t.Line, Col: t.Col}
	}
	return tmpl
}

func collectEllipsisVars(tmpl Expr, bindings *patternBindings) []string {
	var vars []string
	switch t := tmpl.(type) {
	case *AtomExpr:
		if _, ok := bindings.lists[t.Token]; ok {
			vars = append(vars, t.Token)
		}
	case *ListExpr:
		for _, item := range t.Items {
			vars = append(vars, collectEllipsisVars(item, bindings)...)
		}
	}
	return vars
}

func isSpecialForm(name string) bool {
	switch name {
	case "if", "let", "begin", "set!", "define", "lambda", "and", "or", "cond", "quote", "define-syntax", "syntax-rules", "define-record-type", "letrec", "letrec*", "case", "do", "case-lambda":
		return true
	}
	return false
}

func isSelfEvaluating(token string) bool {
	if token == "#t" || token == "#f" {
		return true
	}
	if _, err := strconv.ParseInt(token, 10, 64); err == nil {
		return true
	}
	if len(token) > 0 && token[0] == '"' {
		return true
	}
	if len(token) >= 2 && token[0] == '#' && token[1] == '\\' {
		return true
	}
	return false
}

// --------------- Value comparison helpers ---------------

func valuesEqual(a, b Value) bool {
	switch av := a.(type) {
	case *IntVal:
		if bv, ok := b.(*IntVal); ok {
			return av.Val == bv.Val
		}
		if isNumber(b) {
			return numCmp(a, b) == 0
		}
		return false
	case *RatVal:
		if bv, ok := b.(*RatVal); ok {
			return av.Num == bv.Num && av.Den == bv.Den
		}
		if isNumber(b) {
			return numCmp(a, b) == 0
		}
		return false
	case *FloatVal:
		if bv, ok := b.(*FloatVal); ok {
			return av.Val == bv.Val
		}
		if isNumber(b) {
			return numCmp(a, b) == 0
		}
		return false
	case *BoolVal:
		bv, ok := b.(*BoolVal)
		return ok && av.Val == bv.Val
	case *StringVal:
		bv, ok := b.(*StringVal)
		return ok && av.Val == bv.Val
	case *SymbolVal:
		bv, ok := b.(*SymbolVal)
		return ok && av.Name == bv.Name
	case *CharVal:
		bv, ok := b.(*CharVal)
		return ok && av.Val == bv.Val
	case *NilVal:
		_, ok := b.(*NilVal)
		return ok
	case *PairVal:
		bv, ok := b.(*PairVal)
		if !ok {
			return false
		}
		return valuesEqual(av.Car, bv.Car) && valuesEqual(av.Cdr, bv.Cdr)
	case *VectorVal:
		bv, ok := b.(*VectorVal)
		if !ok || len(av.Items) != len(bv.Items) {
			return false
		}
		for i := range av.Items {
			if !valuesEqual(av.Items[i], bv.Items[i]) {
				return false
			}
		}
		return true
	default:
		return a == b
	}
}

func valuesEq(a, b Value) bool {
	switch av := a.(type) {
	case *SymbolVal:
		bv, ok := b.(*SymbolVal)
		return ok && av.Name == bv.Name
	case *BoolVal:
		bv, ok := b.(*BoolVal)
		return ok && av.Val == bv.Val
	case *IntVal:
		bv, ok := b.(*IntVal)
		return ok && av.Val == bv.Val
	case *RatVal:
		bv, ok := b.(*RatVal)
		return ok && av.Num == bv.Num && av.Den == bv.Den
	case *FloatVal:
		bv, ok := b.(*FloatVal)
		return ok && av.Val == bv.Val
	case *NilVal:
		_, ok := b.(*NilVal)
		return ok
	case *CharVal:
		bv, ok := b.(*CharVal)
		return ok && av.Val == bv.Val
	default:
		return a == b
	}
}

func applyProcSimple(proc Value, args []Value) (Value, error) {
	switch fn := proc.(type) {
	case *BuiltinVal:
		return fn.Fn(args)
	case *LambdaVal:
		return applyLambda(fn, args)
	case *CaseLambdaVal:
		return applyCaseLambda(fn, args)
	}
	return nil, &EvalError{Message: "not a procedure"}
}

// --------------- Procedure application ---------------

func applyLambda(fn *LambdaVal, args []Value) (Value, error) {
	if fn.Rest != "" {
		if len(args) < len(fn.Params) {
			return nil, &EvalError{Message: fmt.Sprintf("expected at least %d arguments, got %d", len(fn.Params), len(args))}
		}
	} else {
		if len(args) != len(fn.Params) {
			return nil, &EvalError{Message: fmt.Sprintf("expected %d arguments, got %d", len(fn.Params), len(args))}
		}
	}
	callEnv := newEnv(fn.Env)
	for i, p := range fn.Params {
		callEnv.set(p, args[i])
	}
	if fn.Rest != "" {
		var restList Value = &NilVal{}
		for i := len(args) - 1; i >= len(fn.Params); i-- {
			restList = &PairVal{Car: args[i], Cdr: restList}
		}
		callEnv.set(fn.Rest, restList)
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
		val, err := applyLambda(fn, args)
		if err != nil {
			if ee, ok := err.(*EvalError); ok && ee.Line == 0 {
				l, c := callSite.Pos()
				ee.Line = l
				ee.Col = c
			}
			return nil, err
		}
		return val, nil
	case *CaseLambdaVal:
		val, err := applyCaseLambda(fn, args)
		if err != nil {
			if ee, ok := err.(*EvalError); ok && ee.Line == 0 {
				l, c := callSite.Pos()
				ee.Line = l
				ee.Col = c
			}
			return nil, err
		}
		return val, nil
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
		var result Value = &IntVal{Val: 0}
		for _, a := range args {
			if !isNumber(a) {
				return nil, &EvalError{Message: "expected number"}
			}
			result = numAdd(result, a)
		}
		return result, nil
	})

	addBuiltin("-", func(args []Value) (Value, error) {
		if len(args) == 0 {
			return nil, &EvalError{Message: "- requires at least 1 argument"}
		}
		if !isNumber(args[0]) {
			return nil, &EvalError{Message: "expected number"}
		}
		if len(args) == 1 {
			return numNeg(args[0]), nil
		}
		result := args[0]
		for _, a := range args[1:] {
			if !isNumber(a) {
				return nil, &EvalError{Message: "expected number"}
			}
			result = numSub(result, a)
		}
		return result, nil
	})

	addBuiltin("*", func(args []Value) (Value, error) {
		var result Value = &IntVal{Val: 1}
		for _, a := range args {
			if !isNumber(a) {
				return nil, &EvalError{Message: "expected number"}
			}
			result = numMul(result, a)
		}
		return result, nil
	})

	addBuiltin("/", func(args []Value) (Value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: "/ requires at least 2 arguments"}
		}
		if !isNumber(args[0]) {
			return nil, &EvalError{Message: "expected number"}
		}
		result := args[0]
		for _, a := range args[1:] {
			if !isNumber(a) {
				return nil, &EvalError{Message: "expected number"}
			}
			var err error
			result, err = numDiv(result, a)
			if err != nil {
				return nil, err
			}
		}
		return result, nil
	})

	numCmpBuiltin := func(name string, test func(int) bool) {
		addBuiltin(name, func(args []Value) (Value, error) {
			if len(args) < 2 {
				return nil, &EvalError{Message: name + " requires at least 2 arguments"}
			}
			for i := 0; i < len(args)-1; i++ {
				if !isNumber(args[i]) || !isNumber(args[i+1]) {
					return nil, &EvalError{Message: "expected number"}
				}
				if !test(numCmp(args[i], args[i+1])) {
					return &BoolVal{Val: false}, nil
				}
			}
			return &BoolVal{Val: true}, nil
		})
	}

	numCmpBuiltin("=", func(c int) bool { return c == 0 })
	numCmpBuiltin("<", func(c int) bool { return c < 0 })
	numCmpBuiltin(">", func(c int) bool { return c > 0 })
	numCmpBuiltin("<=", func(c int) bool { return c <= 0 })
	numCmpBuiltin(">=", func(c int) bool { return c >= 0 })

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
		return &BoolVal{Val: isNumber(args[0])}, nil
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
		if s.Immutable {
			return nil, &EvalError{Message: "string-set!: strings are immutable"}
		}
		idx, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "string-set!: expected integer index"}
		}
		c, ok := args[2].(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "string-set!: expected char"}
		}
		runes := []rune(s.Val)
		if idx.Val < 0 || idx.Val >= int64(len(runes)) {
			return nil, &EvalError{Message: "string-set!: index out of range"}
		}
		runes[idx.Val] = c.Val
		s.Val = string(runes)
		return &VoidVal{}, nil
	})

	// L15: string->list, list->string, char->integer, integer->char
	addBuiltin("string->list", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "string->list requires 1 argument"}
		}
		s, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string->list: expected string"}
		}
		runes := []rune(s.Val)
		var result Value = &NilVal{}
		for i := len(runes) - 1; i >= 0; i-- {
			result = &PairVal{Car: &CharVal{Val: runes[i]}, Cdr: result}
		}
		return result, nil
	})

	addBuiltin("list->string", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "list->string requires 1 argument"}
		}
		var runes []rune
		cur := args[0]
		for {
			if _, ok := cur.(*NilVal); ok {
				break
			}
			p, ok := cur.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "list->string: expected proper list"}
			}
			ch, ok := p.Car.(*CharVal)
			if !ok {
				return nil, &EvalError{Message: "list->string: expected list of characters"}
			}
			runes = append(runes, ch.Val)
			cur = p.Cdr
		}
		return &StringVal{Val: string(runes)}, nil
	})

	addBuiltin("char->integer", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "char->integer requires 1 argument"}
		}
		ch, ok := args[0].(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "char->integer: expected char"}
		}
		return &IntVal{Val: int64(ch.Val)}, nil
	})

	addBuiltin("integer->char", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "integer->char requires 1 argument"}
		}
		n, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "integer->char: expected integer"}
		}
		return &CharVal{Val: rune(n.Val)}, nil
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

	// L09: numeric utilities
	addBuiltin("abs", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "abs requires 1 argument"}
		}
		n, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "abs: expected number"}
		}
		v := n.Val
		if v < 0 {
			v = -v
		}
		return &IntVal{Val: v}, nil
	})

	addBuiltin("modulo", func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "modulo requires 2 arguments"}
		}
		a, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "modulo: expected number"}
		}
		b, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "modulo: expected number"}
		}
		if b.Val == 0 {
			return nil, &EvalError{Message: "modulo: division by zero"}
		}
		r := a.Val % b.Val
		// modulo takes sign of divisor
		if r != 0 && (r > 0) != (b.Val > 0) {
			r += b.Val
		}
		return &IntVal{Val: r}, nil
	})

	addBuiltin("remainder", func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "remainder requires 2 arguments"}
		}
		a, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "remainder: expected number"}
		}
		b, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "remainder: expected number"}
		}
		if b.Val == 0 {
			return nil, &EvalError{Message: "remainder: division by zero"}
		}
		return &IntVal{Val: a.Val % b.Val}, nil
	})

	addBuiltin("quotient", func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "quotient requires 2 arguments"}
		}
		a, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "quotient: expected number"}
		}
		b, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "quotient: expected number"}
		}
		if b.Val == 0 {
			return nil, &EvalError{Message: "quotient: division by zero"}
		}
		return &IntVal{Val: a.Val / b.Val}, nil
	})

	addBuiltin("min", func(args []Value) (Value, error) {
		if len(args) == 0 {
			return nil, &EvalError{Message: "min requires at least 1 argument"}
		}
		best, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "min: expected number"}
		}
		for _, a := range args[1:] {
			n, ok := a.(*IntVal)
			if !ok {
				return nil, &EvalError{Message: "min: expected number"}
			}
			if n.Val < best.Val {
				best = n
			}
		}
		return best, nil
	})

	addBuiltin("max", func(args []Value) (Value, error) {
		if len(args) == 0 {
			return nil, &EvalError{Message: "max requires at least 1 argument"}
		}
		best, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "max: expected number"}
		}
		for _, a := range args[1:] {
			n, ok := a.(*IntVal)
			if !ok {
				return nil, &EvalError{Message: "max: expected number"}
			}
			if n.Val > best.Val {
				best = n
			}
		}
		return best, nil
	})

	addBuiltin("expt", func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "expt requires 2 arguments"}
		}
		base, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "expt: expected number"}
		}
		exp, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "expt: expected number"}
		}
		var result int64 = 1
		e := exp.Val
		b := base.Val
		for e > 0 {
			result *= b
			e--
		}
		return &IntVal{Val: result}, nil
	})

	addBuiltin("zero?", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "zero? requires 1 argument"}
		}
		n, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "zero?: expected number"}
		}
		return &BoolVal{Val: n.Val == 0}, nil
	})

	addBuiltin("positive?", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "positive? requires 1 argument"}
		}
		n, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "positive?: expected number"}
		}
		return &BoolVal{Val: n.Val > 0}, nil
	})

	addBuiltin("negative?", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "negative? requires 1 argument"}
		}
		n, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "negative?: expected number"}
		}
		return &BoolVal{Val: n.Val < 0}, nil
	})

	addBuiltin("odd?", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "odd? requires 1 argument"}
		}
		n, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "odd?: expected number"}
		}
		return &BoolVal{Val: n.Val%2 != 0}, nil
	})

	addBuiltin("even?", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "even? requires 1 argument"}
		}
		n, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "even?: expected number"}
		}
		return &BoolVal{Val: n.Val%2 == 0}, nil
	})

	// L09: list utilities
	addBuiltin("list-ref", func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "list-ref requires 2 arguments"}
		}
		idx, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "list-ref: expected number"}
		}
		cur := args[0]
		for i := int64(0); i < idx.Val; i++ {
			p, ok := cur.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "list-ref: index out of range"}
			}
			cur = p.Cdr
		}
		p, ok := cur.(*PairVal)
		if !ok {
			return nil, &EvalError{Message: "list-ref: index out of range"}
		}
		return p.Car, nil
	})

	addBuiltin("list-tail", func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "list-tail requires 2 arguments"}
		}
		idx, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "list-tail: expected number"}
		}
		cur := args[0]
		for i := int64(0); i < idx.Val; i++ {
			p, ok := cur.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "list-tail: index out of range"}
			}
			cur = p.Cdr
		}
		return cur, nil
	})

	addBuiltin("list?", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "list? requires 1 argument"}
		}
		cur := args[0]
		for {
			if _, ok := cur.(*NilVal); ok {
				return &BoolVal{Val: true}, nil
			}
			p, ok := cur.(*PairVal)
			if !ok {
				return &BoolVal{Val: false}, nil
			}
			cur = p.Cdr
		}
	})

	addBuiltin("assoc", func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "assoc requires 2 arguments"}
		}
		key := args[0]
		cur := args[1]
		for {
			if _, ok := cur.(*NilVal); ok {
				return &BoolVal{Val: false}, nil
			}
			p, ok := cur.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "assoc: not a proper list"}
			}
			entry, ok := p.Car.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "assoc: entry is not a pair"}
			}
			if valuesEqual(key, entry.Car) {
				return entry, nil
			}
			cur = p.Cdr
		}
	})

	addBuiltin("equal?", func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "equal? requires 2 arguments"}
		}
		return &BoolVal{Val: valuesEqual(args[0], args[1])}, nil
	})

	addBuiltin("eq?", func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "eq? requires 2 arguments"}
		}
		return &BoolVal{Val: valuesEq(args[0], args[1])}, nil
	})

	addBuiltin("eqv?", func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "eqv? requires 2 arguments"}
		}
		return &BoolVal{Val: valuesEqv(args[0], args[1])}, nil
	})

	// L14: vector operations
	addBuiltin("vector", func(args []Value) (Value, error) {
		items := make([]Value, len(args))
		copy(items, args)
		return &VectorVal{Items: items}, nil
	})

	addBuiltin("make-vector", func(args []Value) (Value, error) {
		if len(args) < 1 || len(args) > 2 {
			return nil, &EvalError{Message: "make-vector requires 1 or 2 arguments"}
		}
		n, ok := args[0].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "make-vector: expected integer"}
		}
		var fill Value = &IntVal{Val: 0}
		if len(args) == 2 {
			fill = args[1]
		}
		items := make([]Value, n.Val)
		for i := range items {
			items[i] = fill
		}
		return &VectorVal{Items: items}, nil
	})

	addBuiltin("vector-ref", func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "vector-ref requires 2 arguments"}
		}
		v, ok := args[0].(*VectorVal)
		if !ok {
			return nil, &EvalError{Message: "vector-ref: expected vector"}
		}
		idx, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "vector-ref: expected integer index"}
		}
		if idx.Val < 0 || idx.Val >= int64(len(v.Items)) {
			return nil, &EvalError{Message: "vector-ref: index out of range"}
		}
		return v.Items[idx.Val], nil
	})

	addBuiltin("vector-set!", func(args []Value) (Value, error) {
		if len(args) != 3 {
			return nil, &EvalError{Message: "vector-set! requires 3 arguments"}
		}
		v, ok := args[0].(*VectorVal)
		if !ok {
			return nil, &EvalError{Message: "vector-set!: expected vector"}
		}
		idx, ok := args[1].(*IntVal)
		if !ok {
			return nil, &EvalError{Message: "vector-set!: expected integer index"}
		}
		if idx.Val < 0 || idx.Val >= int64(len(v.Items)) {
			return nil, &EvalError{Message: "vector-set!: index out of range"}
		}
		v.Items[idx.Val] = args[2]
		return &VoidVal{}, nil
	})

	addBuiltin("vector-length", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "vector-length requires 1 argument"}
		}
		v, ok := args[0].(*VectorVal)
		if !ok {
			return nil, &EvalError{Message: "vector-length: expected vector"}
		}
		return &IntVal{Val: int64(len(v.Items))}, nil
	})

	addBuiltin("vector?", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "vector? requires 1 argument"}
		}
		_, ok := args[0].(*VectorVal)
		return &BoolVal{Val: ok}, nil
	})

	addBuiltin("vector->list", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "vector->list requires 1 argument"}
		}
		v, ok := args[0].(*VectorVal)
		if !ok {
			return nil, &EvalError{Message: "vector->list: expected vector"}
		}
		var result Value = &NilVal{}
		for i := len(v.Items) - 1; i >= 0; i-- {
			result = &PairVal{Car: v.Items[i], Cdr: result}
		}
		return result, nil
	})

	addBuiltin("list->vector", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "list->vector requires 1 argument"}
		}
		var items []Value
		cur := args[0]
		for {
			if _, ok := cur.(*NilVal); ok {
				break
			}
			p, ok := cur.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "list->vector: not a proper list"}
			}
			items = append(items, p.Car)
			cur = p.Cdr
		}
		return &VectorVal{Items: items}, nil
	})

	// L09: built-in map (multi-list)
	addBuiltin("map", func(args []Value) (Value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: "map requires at least 2 arguments"}
		}
		proc := args[0]
		lists := args[1:]
		var result []Value
		for {
			// Check if any list is exhausted
			callArgs := make([]Value, len(lists))
			done := false
			for i, lst := range lists {
				if _, ok := lst.(*NilVal); ok {
					done = true
					break
				}
				p, ok := lst.(*PairVal)
				if !ok {
					return nil, &EvalError{Message: "map: not a proper list"}
				}
				callArgs[i] = p.Car
				lists[i] = p.Cdr
			}
			if done {
				break
			}
			val, err := applyProcSimple(proc, callArgs)
			if err != nil {
				return nil, err
			}
			result = append(result, val)
		}
		var out Value = &NilVal{}
		for i := len(result) - 1; i >= 0; i-- {
			out = &PairVal{Car: result[i], Cdr: out}
		}
		return out, nil
	})

	// L09: character utilities
	addBuiltin("char-alphabetic?", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "char-alphabetic? requires 1 argument"}
		}
		c, ok := args[0].(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "char-alphabetic?: expected char"}
		}
		return &BoolVal{Val: unicode.IsLetter(c.Val)}, nil
	})

	addBuiltin("char-numeric?", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "char-numeric? requires 1 argument"}
		}
		c, ok := args[0].(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "char-numeric?: expected char"}
		}
		return &BoolVal{Val: unicode.IsDigit(c.Val)}, nil
	})

	addBuiltin("char-upcase", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "char-upcase requires 1 argument"}
		}
		c, ok := args[0].(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "char-upcase: expected char"}
		}
		return &CharVal{Val: unicode.ToUpper(c.Val)}, nil
	})

	addBuiltin("char-downcase", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "char-downcase requires 1 argument"}
		}
		c, ok := args[0].(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "char-downcase: expected char"}
		}
		return &CharVal{Val: unicode.ToLower(c.Val)}, nil
	})

	addBuiltin("char=?", func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "char=? requires 2 arguments"}
		}
		a, ok := args[0].(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "char=?: expected char"}
		}
		b, ok := args[1].(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "char=?: expected char"}
		}
		return &BoolVal{Val: a.Val == b.Val}, nil
	})

	addBuiltin("char<?", func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "char<? requires 2 arguments"}
		}
		a, ok := args[0].(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "char<?: expected char"}
		}
		b, ok := args[1].(*CharVal)
		if !ok {
			return nil, &EvalError{Message: "char<?: expected char"}
		}
		return &BoolVal{Val: a.Val < b.Val}, nil
	})

	// L09: string comparison/case utilities
	addBuiltin("string=?", func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "string=? requires 2 arguments"}
		}
		a, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string=?: expected string"}
		}
		b, ok := args[1].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string=?: expected string"}
		}
		return &BoolVal{Val: a.Val == b.Val}, nil
	})

	addBuiltin("string<?", func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "string<? requires 2 arguments"}
		}
		a, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string<?: expected string"}
		}
		b, ok := args[1].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string<?: expected string"}
		}
		return &BoolVal{Val: a.Val < b.Val}, nil
	})

	addBuiltin("string-ci=?", func(args []Value) (Value, error) {
		if len(args) != 2 {
			return nil, &EvalError{Message: "string-ci=? requires 2 arguments"}
		}
		a, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string-ci=?: expected string"}
		}
		b, ok := args[1].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string-ci=?: expected string"}
		}
		return &BoolVal{Val: strings.EqualFold(a.Val, b.Val)}, nil
	})

	addBuiltin("string-upcase", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "string-upcase requires 1 argument"}
		}
		s, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string-upcase: expected string"}
		}
		return &StringVal{Val: strings.ToUpper(s.Val)}, nil
	})

	addBuiltin("string-downcase", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "string-downcase requires 1 argument"}
		}
		s, ok := args[0].(*StringVal)
		if !ok {
			return nil, &EvalError{Message: "string-downcase: expected string"}
		}
		return &StringVal{Val: strings.ToLower(s.Val)}, nil
	})

	// L08: apply
	addBuiltin("apply", func(args []Value) (Value, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: "apply requires at least 2 arguments"}
		}
		proc := args[0]
		// Last argument must be a list; prefix args are prepended
		last := args[len(args)-1]
		var finalArgs []Value
		// Collect prefix args (between proc and last)
		for _, a := range args[1 : len(args)-1] {
			finalArgs = append(finalArgs, a)
		}
		// Unpack last argument (must be a proper list)
		cur := last
		for {
			if _, ok := cur.(*NilVal); ok {
				break
			}
			p, ok := cur.(*PairVal)
			if !ok {
				return nil, &EvalError{Message: "apply: last argument must be a list"}
			}
			finalArgs = append(finalArgs, p.Car)
			cur = p.Cdr
		}
		// Apply using applyLambda or builtin call
		switch fn := proc.(type) {
		case *BuiltinVal:
			return fn.Fn(finalArgs)
		case *LambdaVal:
			return applyLambda(fn, finalArgs)
		case *CaseLambdaVal:
			return applyCaseLambda(fn, finalArgs)
		}
		return nil, &EvalError{Message: "apply: not a procedure"}
	})

	// L11: exact arithmetic & rationals
	addBuiltin("exact?", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "exact? requires 1 argument"}
		}
		return &BoolVal{Val: isExact(args[0])}, nil
	})

	addBuiltin("inexact?", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "inexact? requires 1 argument"}
		}
		_, ok := args[0].(*FloatVal)
		return &BoolVal{Val: ok}, nil
	})

	addBuiltin("integer?", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "integer? requires 1 argument"}
		}
		switch n := args[0].(type) {
		case *IntVal:
			return &BoolVal{Val: true}, nil
		case *RatVal:
			return &BoolVal{Val: n.Den == 1}, nil
		case *FloatVal:
			return &BoolVal{Val: n.Val == math.Trunc(n.Val) && !math.IsInf(n.Val, 0) && !math.IsNaN(n.Val)}, nil
		}
		return &BoolVal{Val: false}, nil
	})

	addBuiltin("rational?", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "rational? requires 1 argument"}
		}
		return &BoolVal{Val: isExact(args[0])}, nil
	})

	addBuiltin("exact->inexact", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "exact->inexact requires 1 argument"}
		}
		f, ok := toFloat64(args[0])
		if !ok {
			return nil, &EvalError{Message: "exact->inexact: expected number"}
		}
		return &FloatVal{Val: f}, nil
	})

	addBuiltin("inexact->exact", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "inexact->exact requires 1 argument"}
		}
		switch n := args[0].(type) {
		case *IntVal:
			return n, nil
		case *RatVal:
			return n, nil
		case *FloatVal:
			// Convert float to exact rational
			if n.Val == math.Trunc(n.Val) && !math.IsInf(n.Val, 0) && !math.IsNaN(n.Val) {
				return &IntVal{Val: int64(n.Val)}, nil
			}
			// Use continued fraction / multiply out denominator approach
			// For 0.5 -> 1/2, etc.
			num, den := floatToRat(n.Val)
			return makeRat(num, den), nil
		}
		return nil, &EvalError{Message: "inexact->exact: expected number"}
	})

	addBuiltin("numerator", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "numerator requires 1 argument"}
		}
		switch n := args[0].(type) {
		case *IntVal:
			return n, nil
		case *RatVal:
			return &IntVal{Val: n.Num}, nil
		}
		return nil, &EvalError{Message: "numerator: expected rational"}
	})

	addBuiltin("denominator", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "denominator requires 1 argument"}
		}
		switch args[0].(type) {
		case *IntVal:
			return &IntVal{Val: 1}, nil
		case *RatVal:
			r := args[0].(*RatVal)
			return &IntVal{Val: r.Den}, nil
		}
		return nil, &EvalError{Message: "denominator: expected rational"}
	})

	// L13: case-lambda & procedure?
	addBuiltin("procedure?", func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "procedure? requires 1 argument"}
		}
		switch args[0].(type) {
		case *LambdaVal, *BuiltinVal, *CaseLambdaVal:
			return &BoolVal{Val: true}, nil
		}
		return &BoolVal{Val: false}, nil
	})

	return env
}

// floatToRat converts a float64 to an exact rational num/den
func floatToRat(f float64) (int64, int64) {
	if f == 0 {
		return 0, 1
	}
	neg := false
	if f < 0 {
		neg = true
		f = -f
	}
	// Multiply by powers of 2 to find exact representation
	den := int64(1)
	for f != math.Trunc(f) && den < 1<<53 {
		f *= 2
		den *= 2
	}
	num := int64(f)
	if neg {
		num = -num
	}
	return num, den
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
	case *VectorVal:
		var buf strings.Builder
		buf.WriteString("#(")
		for i, item := range val.Items {
			if i > 0 {
				buf.WriteByte(' ')
			}
			buf.WriteString(displayValue(item))
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
