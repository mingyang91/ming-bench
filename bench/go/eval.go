package ming

import (
	"fmt"
	"math/big"
	"strconv"
	"strings"
	"unicode"
)

// Env represents a Scheme environment (scope).
type Env struct {
	bindings map[string]*Value
	parent   *Env
	output   *strings.Builder // shared output buffer for display/write/newline
}

func NewEnv(parent *Env) *Env {
	e := &Env{bindings: make(map[string]*Value), parent: parent}
	if parent != nil {
		e.output = parent.output
	}
	return e
}

// GetOutput returns the shared output buffer, walking up to the root.
func (e *Env) GetOutput() *strings.Builder {
	if e.output != nil {
		return e.output
	}
	if e.parent != nil {
		return e.parent.GetOutput()
	}
	return nil
}

func (e *Env) Get(name string) (*Value, bool) {
	if v, ok := e.bindings[name]; ok {
		return v, true
	}
	if e.parent != nil {
		return e.parent.Get(name)
	}
	return nil, false
}

func (e *Env) Set(name string, val *Value) {
	e.bindings[name] = val
}

// SetExisting mutates an existing binding, walking up the scope chain.
// Returns false if the variable is not bound in any enclosing scope.
func (e *Env) SetExisting(name string, val *Value) bool {
	if _, ok := e.bindings[name]; ok {
		e.bindings[name] = val
		return true
	}
	if e.parent != nil {
		return e.parent.SetExisting(name, val)
	}
	return false
}

// tailCall creates a trampoline marker for tail call optimization.
func tailCall(expr *Expr, env *Env) *Value {
	return &Value{Type: TypeTailCall, TailExpr: expr, TailEnv: env}
}

// Eval evaluates an expression in the given environment, with trampoline for TCO.
func Eval(expr *Expr, env *Env) (*Value, error) {
	for {
		v, err := evalInner(expr, env)
		if err != nil {
			return nil, err
		}
		if v.Type == TypeTailCall {
			expr = v.TailExpr
			env = v.TailEnv
			continue
		}
		return v, nil
	}
}

func evalInner(expr *Expr, env *Env) (*Value, error) {
	switch expr.Type {
	case ExprInt:
		return IntValue(expr.IntVal), nil
	case ExprBool:
		return BoolValue(expr.BoolVal), nil
	case ExprString:
		return StringValue(expr.StrVal), nil
	case ExprChar:
		return CharValue(rune(expr.IntVal)), nil
	case ExprRational:
		return RationalValue(expr.Num, expr.Denom), nil
	case ExprFloat:
		return FloatValue(expr.FloatVal), nil
	case ExprSymbol:
		v, ok := env.Get(expr.StrVal)
		if !ok {
			return nil, fmt.Errorf("%d:%d: unbound variable: %s", expr.Line, expr.Col, expr.StrVal)
		}
		return v, nil
	case ExprList:
		return evalList(expr, env)
	default:
		return nil, fmt.Errorf("%d:%d: unknown expression type", expr.Line, expr.Col)
	}
}

func evalList(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) == 0 {
		return nil, fmt.Errorf("%d:%d: empty application", expr.Line, expr.Col)
	}

	head := expr.List[0]

	// Special forms
	if head.Type == ExprSymbol {
		switch head.StrVal {
		case "and":
			return evalAnd(expr, env)
		case "or":
			return evalOr(expr, env)
		case "define":
			return evalDefine(expr, env)
		case "if":
			return evalIf(expr, env)
		case "quote":
			return evalQuote(expr)
		case "lambda":
			return evalLambda(expr, env)
		case "let":
			return evalLet(expr, env)
		case "let*":
			return evalLetStar(expr, env)
		case "begin":
			return evalBegin(expr, env)
		case "cond":
			return evalCond(expr, env)
		case "set!":
			return evalSetBang(expr, env)
		case "define-syntax":
			return evalDefineSyntax(expr, env)
		case "define-record-type":
			return evalDefineRecordType(expr, env)
		case "case-lambda":
			return evalCaseLambda(expr, env)
		case "letrec":
			return evalLetrec(expr, env)
		case "letrec*":
			return evalLetrecStar(expr, env)
		case "case":
			return evalCase(expr, env)
		case "do":
			return evalDo(expr, env)
		}

		// Check if head is a macro
		if v, ok := env.Get(head.StrVal); ok && v.Type == TypeSyntax {
			expanded, err := expandMacro(v.Syntax, expr, env)
			if err != nil {
				return nil, err
			}
			// Inject definition-site bindings for gensym'd references
			evalEnv := NewEnv(env)
			injectDefSiteBindings(expanded, v.Syntax.DefEnv, evalEnv)
			return tailCall(expanded, evalEnv), nil
		}
	}

	// Evaluate operator
	op, err := Eval(head, env)
	if err != nil {
		return nil, err
	}

	// Evaluate arguments
	args := make([]*Value, 0, len(expr.List)-1)
	for _, a := range expr.List[1:] {
		v, err := Eval(a, env)
		if err != nil {
			return nil, err
		}
		args = append(args, v)
	}

	// Dispatch builtins
	if op.Type == TypeSymbol && len(op.StrVal) > 10 && op.StrVal[:10] == "__builtin:" {
		name := op.StrVal[10:]
		// I/O builtins need env access for output buffer
		switch name {
		case "display":
			return builtinDisplay(args, expr, env)
		case "write":
			return builtinWrite(args, expr, env)
		case "newline":
			return builtinNewline(args, expr, env)
		case "apply":
			return builtinApply(args, expr, env)
		case "map":
			return builtinMap(args, expr, env)
		case "for-each":
			return builtinForEach(args, expr, env)
		}
		if fn, ok := builtinRegistry[name]; ok {
			return fn(args, expr)
		}
		// Record type builtins
		if strings.HasPrefix(name, "record-ctor:") {
			return dispatchRecordCtor(name[12:], args, expr)
		}
		if strings.HasPrefix(name, "record-pred:") {
			tag := name[12:]
			if len(args) != 1 {
				return nil, fmt.Errorf("%d:%d: record predicate: expected 1 argument", expr.Line, expr.Col)
			}
			return BoolValue(args[0].Type == TypeRecord && args[0].RecordTag == tag), nil
		}
		if strings.HasPrefix(name, "record-acc:") {
			rest := name[11:]
			sepIdx := strings.Index(rest, ":")
			tag := rest[:sepIdx]
			field := rest[sepIdx+1:]
			if len(args) != 1 {
				return nil, fmt.Errorf("%d:%d: record accessor: expected 1 argument", expr.Line, expr.Col)
			}
			if args[0].Type != TypeRecord || args[0].RecordTag != tag {
				return nil, fmt.Errorf("%d:%d: record accessor: not a %s", expr.Line, expr.Col, tag)
			}
			v, ok := args[0].RecordFields[field]
			if !ok {
				return nil, fmt.Errorf("%d:%d: record accessor: no field %s", expr.Line, expr.Col, field)
			}
			return v, nil
		}
	}

	// Dispatch lambda calls
	if op.Type == TypeLambda {
		if op.Clauses != nil {
			return callCaseLambda(op, args, expr)
		}
		return callLambda(op, args, expr)
	}

	return nil, fmt.Errorf("%d:%d: not a procedure", head.Line, head.Col)
}

func callLambda(op *Value, args []*Value, expr *Expr) (*Value, error) {
	if op.RestParam != "" {
		if len(args) < len(op.Params) {
			return nil, fmt.Errorf("%d:%d: wrong number of arguments: expected at least %d, got %d", expr.Line, expr.Col, len(op.Params), len(args))
		}
	} else {
		if len(args) != len(op.Params) {
			return nil, fmt.Errorf("%d:%d: wrong number of arguments: expected %d, got %d", expr.Line, expr.Col, len(op.Params), len(args))
		}
	}
	callEnv := NewEnv(op.ClosureEnv)
	for i, param := range op.Params {
		callEnv.Set(param, args[i])
	}
	if op.RestParam != "" {
		// Collect remaining args into a list
		rest := Nil
		for i := len(args) - 1; i >= len(op.Params); i-- {
			rest = &Value{Type: TypePair, Car: args[i], Cdr: rest}
		}
		callEnv.Set(op.RestParam, rest)
	}
	// Evaluate all but the last body expression normally
	for _, bodyExpr := range op.Body[:len(op.Body)-1] {
		_, err := Eval(bodyExpr, callEnv)
		if err != nil {
			return nil, err
		}
	}
	// Return tail call for the last body expression
	return tailCall(op.Body[len(op.Body)-1], callEnv), nil
}

func evalAnd(expr *Expr, env *Env) (*Value, error) {
	args := expr.List[1:]
	if len(args) == 0 {
		return BoolValue(true), nil
	}
	for _, e := range args[:len(args)-1] {
		v, err := Eval(e, env)
		if err != nil {
			return nil, err
		}
		if !v.IsTruthy() {
			return v, nil
		}
	}
	return tailCall(args[len(args)-1], env), nil
}

func evalOr(expr *Expr, env *Env) (*Value, error) {
	args := expr.List[1:]
	if len(args) == 0 {
		return BoolValue(false), nil
	}
	for _, e := range args[:len(args)-1] {
		v, err := Eval(e, env)
		if err != nil {
			return nil, err
		}
		if v.IsTruthy() {
			return v, nil
		}
	}
	return tailCall(args[len(args)-1], env), nil
}

// BuiltinFunc is a builtin function type.
type BuiltinFunc func(args []*Value, expr *Expr) (*Value, error)

// MakeDefaultEnv creates an environment with L01 builtins.
func MakeDefaultEnv() *Env {
	env := NewEnv(nil)

	builtins := map[string]BuiltinFunc{
		"+":  builtinAdd,
		"-":  builtinSub,
		"*":  builtinMul,
		"/":  builtinDiv,
		"<":  builtinLt,
		">":  builtinGt,
		"=":  builtinEq,
		"<=": builtinLe,
		">=": builtinGe,
		"not":      builtinNot,
		"cons":     builtinCons,
		"car":      builtinCar,
		"cdr":      builtinCdr,
		"null?":    builtinNullQ,
		"list":     builtinList,
		"length":   builtinLength,
		"append":   builtinAppend,
		"number?":  builtinNumberQ,
		"string?":  builtinStringQ,
		"boolean?": builtinBooleanQ,
		"pair?":    builtinPairQ,
		"symbol?":  builtinSymbolQ,
		// L05 builtins
		"string-append":   builtinStringAppend,
		"string-length":   builtinStringLength,
		"substring":       builtinSubstring,
		"string->number":  builtinStringToNumber,
		"number->string":  builtinNumberToString,
		"symbol->string":  builtinSymbolToString,
		"string->symbol":  builtinStringToSymbol,
		"string-ref":      builtinStringRef,
		"char?":           builtinCharQ,
		"string-copy":     builtinStringCopy,
		"string-set!":     builtinStringSet,
		// L15 builtins
		"string->list":    builtinStringToList,
		"list->string":    builtinListToString,
		"char->integer":   builtinCharToInteger,
		"integer->char":   builtinIntegerToChar,
		// L09 builtins
		"abs":               builtinAbs,
		"modulo":            builtinModulo,
		"remainder":         builtinRemainder,
		"quotient":          builtinQuotient,
		"min":               builtinMin,
		"max":               builtinMax,
		"expt":              builtinExpt,
		"zero?":             builtinZeroQ,
		"positive?":         builtinPositiveQ,
		"negative?":         builtinNegativeQ,
		"odd?":              builtinOddQ,
		"even?":             builtinEvenQ,
		"list-ref":          builtinListRef,
		"list-tail":         builtinListTail,
		"list?":             builtinListQ,
		"assoc":             builtinAssoc,
		"map":               nil, // special-cased
		"eq?":               builtinEqQ,
		"equal?":            builtinEqualQ,
		"char-alphabetic?":  builtinCharAlphabeticQ,
		"char-numeric?":     builtinCharNumericQ,
		"char-upcase":       builtinCharUpcase,
		"char-downcase":     builtinCharDowncase,
		"char=?":            builtinCharEqQ,
		"char<?":            builtinCharLtQ,
		"string=?":          builtinStringEqQ,
		"string<?":          builtinStringLtQ,
		"string-ci=?":       builtinStringCiEqQ,
		"string-upcase":     builtinStringUpcase,
		"string-downcase":   builtinStringDowncase,
		// I/O builtins (dispatch is special-cased in evalList, but need env registration)
		"display": nil,
		"write":   nil,
		"newline": nil,
		"apply":   nil,
		// L11 builtins
		"exact?":           builtinExactQ,
		"inexact?":         builtinInexactQ,
		"exact->inexact":   builtinExactToInexact,
		"inexact->exact":   builtinInexactToExact,
		"numerator":        builtinNumerator,
		"denominator":      builtinDenominator,
		"integer?":         builtinIntegerQ,
		"rational?":        builtinRationalQ,
		"procedure?":       builtinProcedureQ,
		// L14 builtins
		"eqv?":           builtinEqvQ,
		"vector":         builtinVector,
		"make-vector":    builtinMakeVector,
		"vector-ref":     builtinVectorRef,
		"vector-set!":    builtinVectorSet,
		"vector-length":  builtinVectorLength,
		"vector?":        builtinVectorQ,
		"vector->list":   builtinVectorToList,
		"list->vector":   builtinListToVector,
		// L17 builtins
		"set-car!":  builtinSetCar,
		"set-cdr!":  builtinSetCdr,
		"caar":      builtinCaar,
		"cadr":      builtinCadr,
		"cdar":      builtinCdar,
		"cddr":      builtinCddr,
		"caddr":     builtinCaddr,
		"cadddr":    builtinCadddr,
		"for-each":  nil, // special-cased
		"reverse":   builtinReverse,
		"memq":      builtinMemq,
		"memv":      builtinMemv,
		"assq":      builtinAssq,
		"member":    builtinMember,
		"assv":        builtinAssv,
		"gcd":         builtinGcd,
		"lcm":         builtinLcm,
		"truncate":    builtinTruncate,
		"round":       builtinRound,
		"make-string": builtinMakeString,
		"string":      builtinString,
		"string>?":    builtinStringGtQ,
		"string<=?":   builtinStringLeQ,
		"string>=?":   builtinStringGeQ,
	}

	for name, fn := range builtins {
		env.Set(name, &Value{Type: TypeSymbol, StrVal: "__builtin:" + name})
		_ = fn // stored in dispatch below
	}

	// Store builtins in a global map for dispatch
	for name, fn := range builtins {
		builtinRegistry[name] = fn
	}

	return env
}

var builtinRegistry = map[string]BuiltinFunc{}

func init() {
	// Will be populated by MakeDefaultEnv
}

// injectDefSiteBindings walks an expanded macro expression and, for any gensym'd
// symbol (##prefix.N), resolves the original name from the definition-site environment
// and binds it in evalEnv. This preserves definition-site binding semantics.
func injectDefSiteBindings(expr *Expr, defEnv *Env, evalEnv *Env) {
	if expr.Type == ExprSymbol {
		name := expr.StrVal
		if len(name) > 2 && name[0] == '#' && name[1] == '#' {
			// Extract original name: ##name.N -> name
			orig := name[2:]
			for i := len(orig) - 1; i >= 0; i-- {
				if orig[i] == '.' {
					orig = orig[:i]
					break
				}
			}
			if v, ok := defEnv.Get(orig); ok {
				evalEnv.Set(name, v)
			}
		}
		return
	}
	if expr.Type == ExprList {
		for _, sub := range expr.List {
			injectDefSiteBindings(sub, defEnv, evalEnv)
		}
	}
}

func requireInts(args []*Value, expr *Expr, name string) error {
	for _, a := range args {
		if a.Type != TypeInt {
			return fmt.Errorf("%d:%d: %s: expected number, got %s", expr.Line, expr.Col, name, a.String())
		}
	}
	return nil
}

func requireNums(args []*Value, expr *Expr, name string) error {
	for _, a := range args {
		if !a.IsNumeric() {
			return fmt.Errorf("%d:%d: %s: expected number, got %s", expr.Line, expr.Col, name, a.String())
		}
	}
	return nil
}

func anyInexact(args []*Value) bool {
	for _, a := range args {
		if a.Type == TypeFloat {
			return true
		}
	}
	return false
}

func addRat(an, ad, bn, bd int64) *Value {
	num := an*bd + bn*ad
	denom := ad * bd
	return RationalValue(num, denom)
}

func subRat(an, ad, bn, bd int64) *Value {
	num := an*bd - bn*ad
	denom := ad * bd
	return RationalValue(num, denom)
}

func mulRat(an, ad, bn, bd int64) *Value {
	return RationalValue(an*bn, ad*bd)
}

func divRat(an, ad, bn, bd int64) *Value {
	return RationalValue(an*bd, ad*bn)
}

func builtinAdd(args []*Value, expr *Expr) (*Value, error) {
	if err := requireNums(args, expr, "+"); err != nil {
		return nil, err
	}
	if anyInexact(args) {
		var sum float64
		for _, a := range args {
			sum += a.ToFloat64()
		}
		return FloatValue(sum), nil
	}
	result := IntValue(0)
	for _, a := range args {
		rn, rd := result.ToRat()
		an, ad := a.ToRat()
		result = addRat(rn, rd, an, ad)
	}
	return result, nil
}

func builtinSub(args []*Value, expr *Expr) (*Value, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("%d:%d: -: need at least 1 argument", expr.Line, expr.Col)
	}
	if err := requireNums(args, expr, "-"); err != nil {
		return nil, err
	}
	if anyInexact(args) {
		if len(args) == 1 {
			return FloatValue(-args[0].ToFloat64()), nil
		}
		result := args[0].ToFloat64()
		for _, a := range args[1:] {
			result -= a.ToFloat64()
		}
		return FloatValue(result), nil
	}
	if len(args) == 1 {
		n, d := args[0].ToRat()
		return RationalValue(-n, d), nil
	}
	result := args[0]
	for _, a := range args[1:] {
		rn, rd := result.ToRat()
		an, ad := a.ToRat()
		result = subRat(rn, rd, an, ad)
	}
	return result, nil
}

func builtinMul(args []*Value, expr *Expr) (*Value, error) {
	if err := requireNums(args, expr, "*"); err != nil {
		return nil, err
	}
	if anyInexact(args) {
		result := 1.0
		for _, a := range args {
			result *= a.ToFloat64()
		}
		return FloatValue(result), nil
	}
	result := IntValue(1)
	for _, a := range args {
		rn, rd := result.ToRat()
		an, ad := a.ToRat()
		result = mulRat(rn, rd, an, ad)
	}
	return result, nil
}

func builtinDiv(args []*Value, expr *Expr) (*Value, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("%d:%d: /: need at least 2 arguments", expr.Line, expr.Col)
	}
	if err := requireNums(args, expr, "/"); err != nil {
		return nil, err
	}
	if anyInexact(args) {
		result := args[0].ToFloat64()
		for _, a := range args[1:] {
			d := a.ToFloat64()
			if d == 0 {
				return nil, fmt.Errorf("%d:%d: /: division by zero", expr.Line, expr.Col)
			}
			result /= d
		}
		return FloatValue(result), nil
	}
	result := args[0]
	for _, a := range args[1:] {
		an, ad := a.ToRat()
		if an == 0 {
			return nil, fmt.Errorf("%d:%d: /: division by zero", expr.Line, expr.Col)
		}
		rn, rd := result.ToRat()
		result = divRat(rn, rd, an, ad)
	}
	return result, nil
}

func numCmp(a, b *Value) float64 {
	// If both exact, compare via cross-multiplication to avoid float imprecision
	if a.IsExact() && b.IsExact() {
		an, ad := a.ToRat()
		bn, bd := b.ToRat()
		// an/ad vs bn/bd => an*bd vs bn*ad
		lhs := an * bd
		rhs := bn * ad
		return float64(lhs - rhs)
	}
	return a.ToFloat64() - b.ToFloat64()
}

func builtinLt(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: <: expected 2 arguments", expr.Line, expr.Col)
	}
	if err := requireNums(args, expr, "<"); err != nil {
		return nil, err
	}
	return BoolValue(numCmp(args[0], args[1]) < 0), nil
}

func builtinGt(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: >: expected 2 arguments", expr.Line, expr.Col)
	}
	if err := requireNums(args, expr, ">"); err != nil {
		return nil, err
	}
	return BoolValue(numCmp(args[0], args[1]) > 0), nil
}

func builtinEq(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: =: expected 2 arguments", expr.Line, expr.Col)
	}
	if err := requireNums(args, expr, "="); err != nil {
		return nil, err
	}
	// Cross-tower: convert both to float for comparison if either is inexact
	if anyInexact(args) {
		return BoolValue(args[0].ToFloat64() == args[1].ToFloat64()), nil
	}
	return BoolValue(numCmp(args[0], args[1]) == 0), nil
}

func builtinLe(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: <=: expected 2 arguments", expr.Line, expr.Col)
	}
	if err := requireNums(args, expr, "<="); err != nil {
		return nil, err
	}
	return BoolValue(numCmp(args[0], args[1]) <= 0), nil
}

func builtinGe(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: >=: expected 2 arguments", expr.Line, expr.Col)
	}
	if err := requireNums(args, expr, ">="); err != nil {
		return nil, err
	}
	return BoolValue(numCmp(args[0], args[1]) >= 0), nil
}

func builtinNot(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: not: expected 1 argument", expr.Line, expr.Col)
	}
	return BoolValue(!args[0].IsTruthy()), nil
}

func evalDefine(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) < 3 {
		return nil, fmt.Errorf("%d:%d: define: too few arguments", expr.Line, expr.Col)
	}
	target := expr.List[1]
	if target.Type == ExprSymbol {
		// (define x val)
		val, err := Eval(expr.List[2], env)
		if err != nil {
			return nil, err
		}
		env.Set(target.StrVal, val)
		return Void, nil
	}
	if target.Type == ExprList && len(target.List) >= 1 && target.List[0].Type == ExprSymbol {
		// (define (f params...) body...)
		name := target.List[0].StrVal
		paramListExpr := &Expr{Type: ExprList, List: target.List[1:], Line: target.Line, Col: target.Col}
		params, restParam, err := parseParams(paramListExpr)
		if err != nil {
			return nil, err
		}
		lambda := &Value{
			Type:       TypeLambda,
			Params:     params,
			RestParam:  restParam,
			Body:       expr.List[2:],
			ClosureEnv: env,
		}
		env.Set(name, lambda)
		return Void, nil
	}
	return nil, fmt.Errorf("%d:%d: define: bad syntax", expr.Line, expr.Col)
}

func evalIf(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) < 3 || len(expr.List) > 4 {
		return nil, fmt.Errorf("%d:%d: if: expected 2 or 3 arguments", expr.Line, expr.Col)
	}
	cond, err := Eval(expr.List[1], env)
	if err != nil {
		return nil, err
	}
	if cond.IsTruthy() {
		return tailCall(expr.List[2], env), nil
	}
	if len(expr.List) == 4 {
		return tailCall(expr.List[3], env), nil
	}
	return Void, nil
}

func evalQuote(expr *Expr) (*Value, error) {
	if len(expr.List) != 2 {
		return nil, fmt.Errorf("%d:%d: quote: expected 1 argument", expr.Line, expr.Col)
	}
	return exprToValue(expr.List[1]), nil
}

func exprToValue(expr *Expr) *Value {
	switch expr.Type {
	case ExprInt:
		return IntValue(expr.IntVal)
	case ExprBool:
		return BoolValue(expr.BoolVal)
	case ExprString:
		return StringValue(expr.StrVal)
	case ExprSymbol:
		return SymbolValue(expr.StrVal)
	case ExprChar:
		return CharValue(rune(expr.IntVal))
	case ExprRational:
		return RationalValue(expr.Num, expr.Denom)
	case ExprFloat:
		return FloatValue(expr.FloatVal)
	case ExprList:
		if len(expr.List) == 0 {
			return Nil
		}
		// Build a proper list from the elements
		result := Nil
		for i := len(expr.List) - 1; i >= 0; i-- {
			result = &Value{Type: TypePair, Car: exprToValue(expr.List[i]), Cdr: result}
		}
		return result
	default:
		return Void
	}
}

func evalLet(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) < 3 {
		return nil, fmt.Errorf("%d:%d: let: too few arguments", expr.Line, expr.Col)
	}
	bindingExpr := expr.List[1]
	body := expr.List[2:]

	// Named let: (let name ((var init) ...) body...)
	if bindingExpr.Type == ExprSymbol {
		if len(expr.List) < 4 {
			return nil, fmt.Errorf("%d:%d: let: too few arguments", expr.Line, expr.Col)
		}
		loopName := bindingExpr.StrVal
		bindingExpr = expr.List[2]
		body = expr.List[3:]

		params := make([]string, 0, len(bindingExpr.List))
		initVals := make([]*Value, 0, len(bindingExpr.List))
		for _, b := range bindingExpr.List {
			if b.Type != ExprList || len(b.List) != 2 || b.List[0].Type != ExprSymbol {
				return nil, fmt.Errorf("%d:%d: let: bad binding", b.Line, b.Col)
			}
			params = append(params, b.List[0].StrVal)
			v, err := Eval(b.List[1], env)
			if err != nil {
				return nil, err
			}
			initVals = append(initVals, v)
		}
		// Create lambda for the loop
		letEnv := NewEnv(env)
		lambda := &Value{
			Type:       TypeLambda,
			Params:     params,
			Body:       body,
			ClosureEnv: letEnv,
		}
		letEnv.Set(loopName, lambda)
		// Call with initial values
		callEnv := NewEnv(letEnv)
		for i, p := range params {
			callEnv.Set(p, initVals[i])
		}
		for _, bodyExpr := range body[:len(body)-1] {
			_, err := Eval(bodyExpr, callEnv)
			if err != nil {
				return nil, err
			}
		}
		return tailCall(body[len(body)-1], callEnv), nil
	}

	if bindingExpr.Type != ExprList {
		return nil, fmt.Errorf("%d:%d: let: bindings must be a list", bindingExpr.Line, bindingExpr.Col)
	}
	letEnv := NewEnv(env)
	for _, b := range bindingExpr.List {
		if b.Type != ExprList || len(b.List) != 2 || b.List[0].Type != ExprSymbol {
			return nil, fmt.Errorf("%d:%d: let: bad binding", b.Line, b.Col)
		}
		v, err := Eval(b.List[1], env)
		if err != nil {
			return nil, err
		}
		letEnv.Set(b.List[0].StrVal, v)
	}
	for _, bodyExpr := range body[:len(body)-1] {
		_, err := Eval(bodyExpr, letEnv)
		if err != nil {
			return nil, err
		}
	}
	return tailCall(body[len(body)-1], letEnv), nil
}

func evalLetStar(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) < 3 {
		return nil, fmt.Errorf("%d:%d: let*: too few arguments", expr.Line, expr.Col)
	}
	bindingExpr := expr.List[1]
	body := expr.List[2:]
	if bindingExpr.Type != ExprList {
		return nil, fmt.Errorf("%d:%d: let*: bindings must be a list", bindingExpr.Line, bindingExpr.Col)
	}
	letEnv := NewEnv(env)
	for _, b := range bindingExpr.List {
		if b.Type != ExprList || len(b.List) != 2 || b.List[0].Type != ExprSymbol {
			return nil, fmt.Errorf("%d:%d: let*: bad binding", b.Line, b.Col)
		}
		v, err := Eval(b.List[1], letEnv)
		if err != nil {
			return nil, err
		}
		letEnv.Set(b.List[0].StrVal, v)
	}
	for _, bodyExpr := range body[:len(body)-1] {
		_, err := Eval(bodyExpr, letEnv)
		if err != nil {
			return nil, err
		}
	}
	return tailCall(body[len(body)-1], letEnv), nil
}

func evalBegin(expr *Expr, env *Env) (*Value, error) {
	body := expr.List[1:]
	if len(body) == 0 {
		return Void, nil
	}
	for _, e := range body[:len(body)-1] {
		_, err := Eval(e, env)
		if err != nil {
			return nil, err
		}
	}
	return tailCall(body[len(body)-1], env), nil
}

func evalCond(expr *Expr, env *Env) (*Value, error) {
	for _, clause := range expr.List[1:] {
		if clause.Type != ExprList || len(clause.List) < 1 {
			return nil, fmt.Errorf("%d:%d: cond: bad clause", clause.Line, clause.Col)
		}
		test := clause.List[0]
		if test.Type == ExprSymbol && test.StrVal == "else" {
			body := clause.List[1:]
			for _, bodyExpr := range body[:len(body)-1] {
				_, err := Eval(bodyExpr, env)
				if err != nil {
					return nil, err
				}
			}
			return tailCall(body[len(body)-1], env), nil
		}
		cond, err := Eval(test, env)
		if err != nil {
			return nil, err
		}
		if cond.IsTruthy() {
			if len(clause.List) == 1 {
				// Single-expression clause: ((expr)) returns expr's value
				return cond, nil
			}
			body := clause.List[1:]
			for _, bodyExpr := range body[:len(body)-1] {
				_, err = Eval(bodyExpr, env)
				if err != nil {
					return nil, err
				}
			}
			return tailCall(body[len(body)-1], env), nil
		}
	}
	return Void, nil
}

func builtinCons(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: cons: expected 2 arguments", expr.Line, expr.Col)
	}
	return &Value{Type: TypePair, Car: args[0], Cdr: args[1]}, nil
}

func builtinCar(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypePair {
		return nil, fmt.Errorf("%d:%d: car: expected a pair", expr.Line, expr.Col)
	}
	return args[0].Car, nil
}

func builtinCdr(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypePair {
		return nil, fmt.Errorf("%d:%d: cdr: expected a pair", expr.Line, expr.Col)
	}
	return args[0].Cdr, nil
}

func builtinSetCar(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 || args[0].Type != TypePair {
		return nil, fmt.Errorf("%d:%d: set-car!: expected a pair and a value", expr.Line, expr.Col)
	}
	args[0].Car = args[1]
	return Void, nil
}

func builtinSetCdr(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 || args[0].Type != TypePair {
		return nil, fmt.Errorf("%d:%d: set-cdr!: expected a pair and a value", expr.Line, expr.Col)
	}
	args[0].Cdr = args[1]
	return Void, nil
}

func builtinCaar(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypePair || args[0].Car.Type != TypePair {
		return nil, fmt.Errorf("%d:%d: caar: expected a pair", expr.Line, expr.Col)
	}
	return args[0].Car.Car, nil
}

func builtinCadr(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypePair || args[0].Cdr.Type != TypePair {
		return nil, fmt.Errorf("%d:%d: cadr: expected a pair", expr.Line, expr.Col)
	}
	return args[0].Cdr.Car, nil
}

func builtinCdar(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypePair || args[0].Car.Type != TypePair {
		return nil, fmt.Errorf("%d:%d: cdar: expected a pair", expr.Line, expr.Col)
	}
	return args[0].Car.Cdr, nil
}

func builtinCddr(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypePair || args[0].Cdr.Type != TypePair {
		return nil, fmt.Errorf("%d:%d: cddr: expected a pair", expr.Line, expr.Col)
	}
	return args[0].Cdr.Cdr, nil
}

func builtinCaddr(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypePair || args[0].Cdr.Type != TypePair || args[0].Cdr.Cdr.Type != TypePair {
		return nil, fmt.Errorf("%d:%d: caddr: expected a pair", expr.Line, expr.Col)
	}
	return args[0].Cdr.Cdr.Car, nil
}

func builtinCadddr(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypePair || args[0].Cdr.Type != TypePair || args[0].Cdr.Cdr.Type != TypePair || args[0].Cdr.Cdr.Cdr.Type != TypePair {
		return nil, fmt.Errorf("%d:%d: cadddr: expected a pair", expr.Line, expr.Col)
	}
	return args[0].Cdr.Cdr.Cdr.Car, nil
}

func builtinReverse(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: reverse: expected 1 argument", expr.Line, expr.Col)
	}
	result := Nil
	v := args[0]
	for v.Type == TypePair {
		result = &Value{Type: TypePair, Car: v.Car, Cdr: result}
		v = v.Cdr
	}
	return result, nil
}

func builtinMemq(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: memq: expected 2 arguments", expr.Line, expr.Col)
	}
	obj := args[0]
	lst := args[1]
	for lst.Type == TypePair {
		if eqIdentity(obj, lst.Car) {
			return lst, nil
		}
		lst = lst.Cdr
	}
	return BoolValue(false), nil
}

func builtinMemv(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: memv: expected 2 arguments", expr.Line, expr.Col)
	}
	obj := args[0]
	lst := args[1]
	for lst.Type == TypePair {
		if eqvCompare(obj, lst.Car) {
			return lst, nil
		}
		lst = lst.Cdr
	}
	return BoolValue(false), nil
}

func builtinAssq(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: assq: expected 2 arguments", expr.Line, expr.Col)
	}
	key := args[0]
	lst := args[1]
	for lst.Type == TypePair {
		if lst.Car.Type == TypePair && eqIdentity(key, lst.Car.Car) {
			return lst.Car, nil
		}
		lst = lst.Cdr
	}
	return BoolValue(false), nil
}

func builtinAssv(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: assv: expected 2 arguments", expr.Line, expr.Col)
	}
	key := args[0]
	lst := args[1]
	for lst.Type == TypePair {
		if lst.Car.Type == TypePair && eqvCompare(key, lst.Car.Car) {
			return lst.Car, nil
		}
		lst = lst.Cdr
	}
	return BoolValue(false), nil
}

func builtinMember(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: member: expected 2 arguments", expr.Line, expr.Col)
	}
	obj := args[0]
	lst := args[1]
	for lst.Type == TypePair {
		if valuesEqual(obj, lst.Car) {
			return lst, nil
		}
		lst = lst.Cdr
	}
	return BoolValue(false), nil
}

// eqIdentity checks eq? identity (pointer equality for pairs, value equality for atoms).
func eqIdentity(a, b *Value) bool {
	if a == b {
		return true
	}
	if a.Type != b.Type {
		return false
	}
	switch a.Type {
	case TypeInt:
		return a.IntVal == b.IntVal
	case TypeBool:
		return a.BoolVal == b.BoolVal
	case TypeSymbol:
		return a.StrVal == b.StrVal
	case TypeChar:
		return a.IntVal == b.IntVal
	case TypeNil:
		return true
	default:
		return false
	}
}

func builtinNullQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: null?: expected 1 argument", expr.Line, expr.Col)
	}
	return BoolValue(args[0].Type == TypeNil), nil
}

func builtinList(args []*Value, expr *Expr) (*Value, error) {
	result := Nil
	for i := len(args) - 1; i >= 0; i-- {
		result = &Value{Type: TypePair, Car: args[i], Cdr: result}
	}
	return result, nil
}

func builtinLength(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: length: expected 1 argument", expr.Line, expr.Col)
	}
	var count int64
	v := args[0]
	for v.Type == TypePair {
		count++
		v = v.Cdr
	}
	return IntValue(count), nil
}

func builtinAppend(args []*Value, expr *Expr) (*Value, error) {
	if len(args) == 0 {
		return Nil, nil
	}
	if len(args) == 1 {
		return args[0], nil
	}
	// Append all lists together
	result := args[len(args)-1]
	for i := len(args) - 2; i >= 0; i-- {
		lst := args[i]
		if lst.Type == TypeNil {
			continue
		}
		// Collect elements of lst
		var elems []*Value
		for lst.Type == TypePair {
			elems = append(elems, lst.Car)
			lst = lst.Cdr
		}
		for j := len(elems) - 1; j >= 0; j-- {
			result = &Value{Type: TypePair, Car: elems[j], Cdr: result}
		}
	}
	return result, nil
}

func builtinNumberQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: number?: expected 1 argument", expr.Line, expr.Col)
	}
	return BoolValue(args[0].IsNumeric()), nil
}

func builtinStringQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: string?: expected 1 argument", expr.Line, expr.Col)
	}
	return BoolValue(args[0].Type == TypeString), nil
}

func builtinBooleanQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: boolean?: expected 1 argument", expr.Line, expr.Col)
	}
	return BoolValue(args[0].Type == TypeBool), nil
}

func builtinPairQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: pair?: expected 1 argument", expr.Line, expr.Col)
	}
	return BoolValue(args[0].Type == TypePair), nil
}

func builtinSymbolQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: symbol?: expected 1 argument", expr.Line, expr.Col)
	}
	return BoolValue(args[0].Type == TypeSymbol), nil
}

// L05 builtins

func builtinDisplay(args []*Value, expr *Expr, env *Env) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: display: expected 1 argument", expr.Line, expr.Col)
	}
	if buf := env.GetOutput(); buf != nil {
		buf.WriteString(args[0].DisplayString())
	}
	return Void, nil
}

func builtinWrite(args []*Value, expr *Expr, env *Env) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: write: expected 1 argument", expr.Line, expr.Col)
	}
	if buf := env.GetOutput(); buf != nil {
		buf.WriteString(args[0].String())
	}
	return Void, nil
}

func builtinNewline(args []*Value, expr *Expr, env *Env) (*Value, error) {
	if len(args) != 0 {
		return nil, fmt.Errorf("%d:%d: newline: expected 0 arguments", expr.Line, expr.Col)
	}
	if buf := env.GetOutput(); buf != nil {
		buf.WriteString("\n")
	}
	return Void, nil
}

func builtinApply(args []*Value, expr *Expr, env *Env) (*Value, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("%d:%d: apply: expected at least 2 arguments", expr.Line, expr.Col)
	}
	fn := args[0]
	// Last arg must be a list; prefix args are prepended
	lastArg := args[len(args)-1]
	// Collect prefix args
	var callArgs []*Value
	for _, a := range args[1 : len(args)-1] {
		callArgs = append(callArgs, a)
	}
	// Flatten the last argument (a list) into callArgs
	lst := lastArg
	for lst.Type == TypePair {
		callArgs = append(callArgs, lst.Car)
		lst = lst.Cdr
	}

	// Dispatch based on function type
	if fn.Type == TypeLambda {
		if fn.Clauses != nil {
			return callCaseLambda(fn, callArgs, expr)
		}
		return callLambda(fn, callArgs, expr)
	}
	if fn.Type == TypeSymbol && len(fn.StrVal) > 10 && fn.StrVal[:10] == "__builtin:" {
		name := fn.StrVal[10:]
		switch name {
		case "display":
			return builtinDisplay(callArgs, expr, env)
		case "write":
			return builtinWrite(callArgs, expr, env)
		case "newline":
			return builtinNewline(callArgs, expr, env)
		case "apply":
			return builtinApply(callArgs, expr, env)
		case "map":
			return builtinMap(callArgs, expr, env)
		case "for-each":
			return builtinForEach(callArgs, expr, env)
		}
		if bfn, ok := builtinRegistry[name]; ok {
			return bfn(callArgs, expr)
		}
	}
	return nil, fmt.Errorf("%d:%d: apply: first argument is not a procedure", expr.Line, expr.Col)
}

func builtinStringAppend(args []*Value, expr *Expr) (*Value, error) {
	var sb strings.Builder
	for _, a := range args {
		if a.Type != TypeString {
			return nil, fmt.Errorf("%d:%d: string-append: expected string", expr.Line, expr.Col)
		}
		sb.WriteString(a.StrContent())
	}
	return StringValue(sb.String()), nil
}

func builtinStringLength(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypeString {
		return nil, fmt.Errorf("%d:%d: string-length: expected 1 string argument", expr.Line, expr.Col)
	}
	return IntValue(int64(len([]rune(args[0].StrContent())))), nil
}

func builtinSubstring(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 3 || args[0].Type != TypeString || args[1].Type != TypeInt || args[2].Type != TypeInt {
		return nil, fmt.Errorf("%d:%d: substring: expected string, int, int", expr.Line, expr.Col)
	}
	runes := []rune(args[0].StrContent())
	start := int(args[1].IntVal)
	end := int(args[2].IntVal)
	if start < 0 || end < start || end > len(runes) {
		return nil, fmt.Errorf("%d:%d: substring: index out of range", expr.Line, expr.Col)
	}
	return StringValue(string(runes[start:end])), nil
}

func builtinStringToNumber(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypeString {
		return nil, fmt.Errorf("%d:%d: string->number: expected 1 string argument", expr.Line, expr.Col)
	}
	n, err := strconv.ParseInt(args[0].StrContent(), 10, 64)
	if err != nil {
		return BoolValue(false), nil
	}
	return IntValue(n), nil
}

func builtinNumberToString(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || !args[0].IsNumeric() {
		return nil, fmt.Errorf("%d:%d: number->string: expected 1 number argument", expr.Line, expr.Col)
	}
	return StringValue(args[0].String()), nil
}

func builtinSymbolToString(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypeSymbol {
		return nil, fmt.Errorf("%d:%d: symbol->string: expected 1 symbol argument", expr.Line, expr.Col)
	}
	return StringValue(args[0].StrContent()), nil
}

func builtinStringToSymbol(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypeString {
		return nil, fmt.Errorf("%d:%d: string->symbol: expected 1 string argument", expr.Line, expr.Col)
	}
	return SymbolValue(args[0].StrContent()), nil
}

func builtinStringRef(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 || args[0].Type != TypeString || args[1].Type != TypeInt {
		return nil, fmt.Errorf("%d:%d: string-ref: expected string and int", expr.Line, expr.Col)
	}
	runes := []rune(args[0].StrContent())
	idx := int(args[1].IntVal)
	if idx < 0 || idx >= len(runes) {
		return nil, fmt.Errorf("%d:%d: string-ref: index out of range", expr.Line, expr.Col)
	}
	return CharValue(runes[idx]), nil
}

func builtinCharQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: char?: expected 1 argument", expr.Line, expr.Col)
	}
	return BoolValue(args[0].Type == TypeChar), nil
}

// L06 builtins

func builtinStringCopy(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypeString {
		return nil, fmt.Errorf("%d:%d: string-copy: expected 1 string argument", expr.Line, expr.Col)
	}
	return MutableStringValue(args[0].StrContent()), nil
}

func builtinStringSet(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 3 || args[0].Type != TypeString || args[1].Type != TypeInt || args[2].Type != TypeChar {
		return nil, fmt.Errorf("%d:%d: string-set!: expected string, index, char", expr.Line, expr.Col)
	}
	// L15: only mutable strings (from string-copy) can be modified
	if args[0].Runes == nil {
		return nil, fmt.Errorf("%d:%d: string-set!: strings are immutable", expr.Line, expr.Col)
	}
	idx := args[1].IntVal
	if idx < 0 || idx >= int64(len(args[0].Runes)) {
		return nil, fmt.Errorf("%d:%d: string-set!: index out of range", expr.Line, expr.Col)
	}
	args[0].Runes[idx] = rune(args[2].IntVal)
	return Void, nil
}

// L15 builtins

func builtinStringToList(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypeString {
		return nil, fmt.Errorf("%d:%d: string->list: expected 1 string argument", expr.Line, expr.Col)
	}
	runes := []rune(args[0].StrContent())
	result := Nil
	for i := len(runes) - 1; i >= 0; i-- {
		result = &Value{Type: TypePair, Car: CharValue(runes[i]), Cdr: result}
	}
	return result, nil
}

func builtinListToString(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: list->string: expected 1 argument", expr.Line, expr.Col)
	}
	var runes []rune
	cur := args[0]
	for cur.Type == TypePair {
		if cur.Car.Type != TypeChar {
			return nil, fmt.Errorf("%d:%d: list->string: expected list of characters", expr.Line, expr.Col)
		}
		runes = append(runes, rune(cur.Car.IntVal))
		cur = cur.Cdr
	}
	if cur.Type != TypeNil {
		return nil, fmt.Errorf("%d:%d: list->string: expected proper list", expr.Line, expr.Col)
	}
	return StringValue(string(runes)), nil
}

func builtinCharToInteger(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypeChar {
		return nil, fmt.Errorf("%d:%d: char->integer: expected 1 character argument", expr.Line, expr.Col)
	}
	return IntValue(args[0].IntVal), nil
}

func builtinIntegerToChar(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypeInt {
		return nil, fmt.Errorf("%d:%d: integer->char: expected 1 integer argument", expr.Line, expr.Col)
	}
	return CharValue(rune(args[0].IntVal)), nil
}

// L09 builtins

func builtinAbs(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypeInt {
		return nil, fmt.Errorf("%d:%d: abs: expected 1 number argument", expr.Line, expr.Col)
	}
	n := args[0].IntVal
	if n < 0 {
		n = -n
	}
	return IntValue(n), nil
}

func builtinModulo(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: modulo: expected 2 arguments", expr.Line, expr.Col)
	}
	if err := requireInts(args, expr, "modulo"); err != nil {
		return nil, err
	}
	if args[1].IntVal == 0 {
		return nil, fmt.Errorf("%d:%d: modulo: division by zero", expr.Line, expr.Col)
	}
	a, b := args[0].IntVal, args[1].IntVal
	r := a % b
	// modulo takes the sign of the divisor
	if r != 0 && (r > 0) != (b > 0) {
		r += b
	}
	return IntValue(r), nil
}

func builtinRemainder(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: remainder: expected 2 arguments", expr.Line, expr.Col)
	}
	if err := requireInts(args, expr, "remainder"); err != nil {
		return nil, err
	}
	if args[1].IntVal == 0 {
		return nil, fmt.Errorf("%d:%d: remainder: division by zero", expr.Line, expr.Col)
	}
	// Go's % already takes the sign of the dividend
	return IntValue(args[0].IntVal % args[1].IntVal), nil
}

func builtinQuotient(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: quotient: expected 2 arguments", expr.Line, expr.Col)
	}
	if err := requireInts(args, expr, "quotient"); err != nil {
		return nil, err
	}
	if args[1].IntVal == 0 {
		return nil, fmt.Errorf("%d:%d: quotient: division by zero", expr.Line, expr.Col)
	}
	// Go's / truncates toward zero, which is what quotient does
	return IntValue(args[0].IntVal / args[1].IntVal), nil
}

func builtinMin(args []*Value, expr *Expr) (*Value, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("%d:%d: min: expected at least 1 argument", expr.Line, expr.Col)
	}
	if err := requireInts(args, expr, "min"); err != nil {
		return nil, err
	}
	m := args[0].IntVal
	for _, a := range args[1:] {
		if a.IntVal < m {
			m = a.IntVal
		}
	}
	return IntValue(m), nil
}

func builtinMax(args []*Value, expr *Expr) (*Value, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("%d:%d: max: expected at least 1 argument", expr.Line, expr.Col)
	}
	if err := requireInts(args, expr, "max"); err != nil {
		return nil, err
	}
	m := args[0].IntVal
	for _, a := range args[1:] {
		if a.IntVal > m {
			m = a.IntVal
		}
	}
	return IntValue(m), nil
}

func builtinExpt(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: expt: expected 2 arguments", expr.Line, expr.Col)
	}
	if err := requireInts(args, expr, "expt"); err != nil {
		return nil, err
	}
	base, exp := args[0].IntVal, args[1].IntVal
	result := int64(1)
	for i := int64(0); i < exp; i++ {
		result *= base
	}
	return IntValue(result), nil
}

func builtinZeroQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypeInt {
		return nil, fmt.Errorf("%d:%d: zero?: expected 1 number argument", expr.Line, expr.Col)
	}
	return BoolValue(args[0].IntVal == 0), nil
}

func builtinPositiveQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypeInt {
		return nil, fmt.Errorf("%d:%d: positive?: expected 1 number argument", expr.Line, expr.Col)
	}
	return BoolValue(args[0].IntVal > 0), nil
}

func builtinNegativeQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypeInt {
		return nil, fmt.Errorf("%d:%d: negative?: expected 1 number argument", expr.Line, expr.Col)
	}
	return BoolValue(args[0].IntVal < 0), nil
}

func builtinOddQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypeInt {
		return nil, fmt.Errorf("%d:%d: odd?: expected 1 number argument", expr.Line, expr.Col)
	}
	return BoolValue(args[0].IntVal%2 != 0), nil
}

func builtinEvenQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypeInt {
		return nil, fmt.Errorf("%d:%d: even?: expected 1 number argument", expr.Line, expr.Col)
	}
	return BoolValue(args[0].IntVal%2 == 0), nil
}

func builtinExactQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: exact?: expected 1 argument", expr.Line, expr.Col)
	}
	return BoolValue(args[0].IsExact()), nil
}

func builtinInexactQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: inexact?: expected 1 argument", expr.Line, expr.Col)
	}
	return BoolValue(args[0].Type == TypeFloat), nil
}

func builtinExactToInexact(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || !args[0].IsNumeric() {
		return nil, fmt.Errorf("%d:%d: exact->inexact: expected 1 number argument", expr.Line, expr.Col)
	}
	return FloatValue(args[0].ToFloat64()), nil
}

func builtinInexactToExact(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || !args[0].IsNumeric() {
		return nil, fmt.Errorf("%d:%d: inexact->exact: expected 1 number argument", expr.Line, expr.Col)
	}
	if args[0].IsExact() {
		return args[0], nil
	}
	f := args[0].FloatVal
	// Check if it's an integer
	if f == float64(int64(f)) {
		return IntValue(int64(f)), nil
	}
	// Convert float to rational using math/big
	r := new(big.Rat).SetFloat64(f)
	num := r.Num().Int64()
	denom := r.Denom().Int64()
	return RationalValue(num, denom), nil
}

func builtinNumerator(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || !args[0].IsNumeric() {
		return nil, fmt.Errorf("%d:%d: numerator: expected 1 number argument", expr.Line, expr.Col)
	}
	switch args[0].Type {
	case TypeInt:
		return IntValue(args[0].IntVal), nil
	case TypeRational:
		return IntValue(args[0].Num), nil
	default:
		return nil, fmt.Errorf("%d:%d: numerator: expected exact number", expr.Line, expr.Col)
	}
}

func builtinDenominator(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || !args[0].IsNumeric() {
		return nil, fmt.Errorf("%d:%d: denominator: expected 1 number argument", expr.Line, expr.Col)
	}
	switch args[0].Type {
	case TypeInt:
		return IntValue(1), nil
	case TypeRational:
		return IntValue(args[0].Denom), nil
	default:
		return nil, fmt.Errorf("%d:%d: denominator: expected exact number", expr.Line, expr.Col)
	}
}

func builtinIntegerQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: integer?: expected 1 argument", expr.Line, expr.Col)
	}
	return BoolValue(args[0].Type == TypeInt), nil
}

func builtinRationalQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: rational?: expected 1 argument", expr.Line, expr.Col)
	}
	return BoolValue(args[0].Type == TypeInt || args[0].Type == TypeRational), nil
}

func builtinProcedureQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: procedure?: expected 1 argument", expr.Line, expr.Col)
	}
	a := args[0]
	if a.Type == TypeLambda {
		return BoolValue(true), nil
	}
	if a.Type == TypeSymbol && len(a.StrVal) > 10 && a.StrVal[:10] == "__builtin:" {
		return BoolValue(true), nil
	}
	return BoolValue(false), nil
}

func builtinListRef(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 || args[1].Type != TypeInt {
		return nil, fmt.Errorf("%d:%d: list-ref: expected list and int", expr.Line, expr.Col)
	}
	idx := int(args[1].IntVal)
	v := args[0]
	for i := 0; i < idx; i++ {
		if v.Type != TypePair {
			return nil, fmt.Errorf("%d:%d: list-ref: index out of range", expr.Line, expr.Col)
		}
		v = v.Cdr
	}
	if v.Type != TypePair {
		return nil, fmt.Errorf("%d:%d: list-ref: index out of range", expr.Line, expr.Col)
	}
	return v.Car, nil
}

func builtinListTail(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 || args[1].Type != TypeInt {
		return nil, fmt.Errorf("%d:%d: list-tail: expected list and int", expr.Line, expr.Col)
	}
	idx := int(args[1].IntVal)
	v := args[0]
	for i := 0; i < idx; i++ {
		if v.Type != TypePair {
			return nil, fmt.Errorf("%d:%d: list-tail: index out of range", expr.Line, expr.Col)
		}
		v = v.Cdr
	}
	return v, nil
}

func builtinListQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: list?: expected 1 argument", expr.Line, expr.Col)
	}
	// Tortoise-and-hare cycle detection
	slow := args[0]
	fast := args[0]
	for fast.Type == TypePair {
		fast = fast.Cdr
		if fast.Type != TypePair {
			break
		}
		fast = fast.Cdr
		slow = slow.Cdr
		if slow == fast {
			return BoolValue(false), nil // cycle detected
		}
	}
	return BoolValue(fast.Type == TypeNil), nil
}

type equalPair struct{ a, b *Value }

func valuesEqual(a, b *Value) bool {
	return valuesEqualSeen(a, b, make(map[equalPair]bool))
}

func valuesEqualSeen(a, b *Value, seen map[equalPair]bool) bool {
	if a == b {
		return true
	}
	if a.Type != b.Type {
		return false
	}
	switch a.Type {
	case TypeInt:
		return a.IntVal == b.IntVal
	case TypeBool:
		return a.BoolVal == b.BoolVal
	case TypeString:
		return a.StrContent() == b.StrContent()
	case TypeSymbol:
		return a.StrVal == b.StrVal
	case TypeChar:
		return a.IntVal == b.IntVal
	case TypeNil:
		return true
	case TypePair:
		key := equalPair{a, b}
		if seen[key] {
			return true // assume equal for cycles
		}
		seen[key] = true
		return valuesEqualSeen(a.Car, b.Car, seen) && valuesEqualSeen(a.Cdr, b.Cdr, seen)
	case TypeVector:
		if len(a.VecElems) != len(b.VecElems) {
			return false
		}
		for i := range a.VecElems {
			if !valuesEqualSeen(a.VecElems[i], b.VecElems[i], seen) {
				return false
			}
		}
		return true
	default:
		return a == b
	}
}

func builtinAssoc(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: assoc: expected 2 arguments", expr.Line, expr.Col)
	}
	key := args[0]
	lst := args[1]
	for lst.Type == TypePair {
		if lst.Car.Type == TypePair && valuesEqual(lst.Car.Car, key) {
			return lst.Car, nil
		}
		lst = lst.Cdr
	}
	return BoolValue(false), nil
}

func builtinMap(args []*Value, expr *Expr, env *Env) (*Value, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("%d:%d: map: expected at least 2 arguments", expr.Line, expr.Col)
	}
	fn := args[0]
	lists := args[1:]

	var result []*Value
	for {
		// Check if any list is exhausted
		allPair := true
		for _, l := range lists {
			if l.Type != TypePair {
				allPair = false
				break
			}
		}
		if !allPair {
			break
		}
		// Collect cars
		callArgs := make([]*Value, len(lists))
		for i, l := range lists {
			callArgs[i] = l.Car
		}
		// Call function
		var val *Value
		var err error
		if fn.Type == TypeLambda {
			if fn.Clauses != nil {
				val, err = callCaseLambda(fn, callArgs, expr)
			} else {
				val, err = callLambda(fn, callArgs, expr)
			}
		} else if fn.Type == TypeSymbol && len(fn.StrVal) > 10 && fn.StrVal[:10] == "__builtin:" {
			name := fn.StrVal[10:]
			if bfn, ok := builtinRegistry[name]; ok && bfn != nil {
				val, err = bfn(callArgs, expr)
			} else {
				// Try special-cased builtins
				switch name {
				case "display":
					val, err = builtinDisplay(callArgs, expr, env)
				case "write":
					val, err = builtinWrite(callArgs, expr, env)
				default:
					return nil, fmt.Errorf("%d:%d: map: not a procedure", expr.Line, expr.Col)
				}
			}
		} else {
			return nil, fmt.Errorf("%d:%d: map: first argument is not a procedure", expr.Line, expr.Col)
		}
		if err != nil {
			return nil, err
		}
		// Resolve trampoline tail calls
		for val.Type == TypeTailCall {
			val, err = evalInner(val.TailExpr, val.TailEnv)
			if err != nil {
				return nil, err
			}
		}
		result = append(result, val)
		// Advance all lists
		for i := range lists {
			lists[i] = lists[i].Cdr
		}
	}
	// Build result list
	out := Nil
	for i := len(result) - 1; i >= 0; i-- {
		out = &Value{Type: TypePair, Car: result[i], Cdr: out}
	}
	return out, nil
}

func builtinForEach(args []*Value, expr *Expr, env *Env) (*Value, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("%d:%d: for-each: expected at least 2 arguments", expr.Line, expr.Col)
	}
	fn := args[0]
	lists := args[1:]

	for {
		allPair := true
		for _, l := range lists {
			if l.Type != TypePair {
				allPair = false
				break
			}
		}
		if !allPair {
			break
		}
		callArgs := make([]*Value, len(lists))
		for i, l := range lists {
			callArgs[i] = l.Car
		}
		var val *Value
		var err error
		if fn.Type == TypeLambda {
			if fn.Clauses != nil {
				val, err = callCaseLambda(fn, callArgs, expr)
			} else {
				val, err = callLambda(fn, callArgs, expr)
			}
		} else if fn.Type == TypeSymbol && len(fn.StrVal) > 10 && fn.StrVal[:10] == "__builtin:" {
			name := fn.StrVal[10:]
			if bfn, ok := builtinRegistry[name]; ok && bfn != nil {
				val, err = bfn(callArgs, expr)
			} else {
				switch name {
				case "display":
					val, err = builtinDisplay(callArgs, expr, env)
				case "write":
					val, err = builtinWrite(callArgs, expr, env)
				default:
					return nil, fmt.Errorf("%d:%d: for-each: not a procedure", expr.Line, expr.Col)
				}
			}
		} else {
			return nil, fmt.Errorf("%d:%d: for-each: first argument is not a procedure", expr.Line, expr.Col)
		}
		if err != nil {
			return nil, err
		}
		for val != nil && val.Type == TypeTailCall {
			val, err = evalInner(val.TailExpr, val.TailEnv)
			if err != nil {
				return nil, err
			}
		}
		for i := range lists {
			lists[i] = lists[i].Cdr
		}
	}
	return Void, nil
}

func builtinEqQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: eq?: expected 2 arguments", expr.Line, expr.Col)
	}
	a, b := args[0], args[1]
	if a.Type != b.Type {
		return BoolValue(false), nil
	}
	switch a.Type {
	case TypeSymbol:
		return BoolValue(a.StrVal == b.StrVal), nil
	case TypeBool:
		return BoolValue(a.BoolVal == b.BoolVal), nil
	case TypeInt:
		return BoolValue(a.IntVal == b.IntVal), nil
	case TypeChar:
		return BoolValue(a.IntVal == b.IntVal), nil
	case TypeNil:
		return BoolValue(true), nil
	default:
		return BoolValue(a == b), nil
	}
}

func builtinEqualQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: equal?: expected 2 arguments", expr.Line, expr.Col)
	}
	return BoolValue(valuesEqual(args[0], args[1])), nil
}

func builtinCharAlphabeticQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypeChar {
		return nil, fmt.Errorf("%d:%d: char-alphabetic?: expected 1 char argument", expr.Line, expr.Col)
	}
	return BoolValue(unicode.IsLetter(rune(args[0].IntVal))), nil
}

func builtinCharNumericQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypeChar {
		return nil, fmt.Errorf("%d:%d: char-numeric?: expected 1 char argument", expr.Line, expr.Col)
	}
	return BoolValue(unicode.IsDigit(rune(args[0].IntVal))), nil
}

func builtinCharUpcase(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypeChar {
		return nil, fmt.Errorf("%d:%d: char-upcase: expected 1 char argument", expr.Line, expr.Col)
	}
	return CharValue(unicode.ToUpper(rune(args[0].IntVal))), nil
}

func builtinCharDowncase(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypeChar {
		return nil, fmt.Errorf("%d:%d: char-downcase: expected 1 char argument", expr.Line, expr.Col)
	}
	return CharValue(unicode.ToLower(rune(args[0].IntVal))), nil
}

func builtinCharEqQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 || args[0].Type != TypeChar || args[1].Type != TypeChar {
		return nil, fmt.Errorf("%d:%d: char=?: expected 2 char arguments", expr.Line, expr.Col)
	}
	return BoolValue(args[0].IntVal == args[1].IntVal), nil
}

func builtinCharLtQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 || args[0].Type != TypeChar || args[1].Type != TypeChar {
		return nil, fmt.Errorf("%d:%d: char<?: expected 2 char arguments", expr.Line, expr.Col)
	}
	return BoolValue(args[0].IntVal < args[1].IntVal), nil
}

func builtinStringEqQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 || args[0].Type != TypeString || args[1].Type != TypeString {
		return nil, fmt.Errorf("%d:%d: string=?: expected 2 string arguments", expr.Line, expr.Col)
	}
	return BoolValue(args[0].StrContent() == args[1].StrContent()), nil
}

func builtinStringLtQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 || args[0].Type != TypeString || args[1].Type != TypeString {
		return nil, fmt.Errorf("%d:%d: string<?: expected 2 string arguments", expr.Line, expr.Col)
	}
	return BoolValue(args[0].StrContent() < args[1].StrContent()), nil
}

func builtinStringCiEqQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 || args[0].Type != TypeString || args[1].Type != TypeString {
		return nil, fmt.Errorf("%d:%d: string-ci=?: expected 2 string arguments", expr.Line, expr.Col)
	}
	return BoolValue(strings.EqualFold(args[0].StrContent(), args[1].StrContent())), nil
}

func builtinStringUpcase(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypeString {
		return nil, fmt.Errorf("%d:%d: string-upcase: expected 1 string argument", expr.Line, expr.Col)
	}
	return StringValue(strings.ToUpper(args[0].StrContent())), nil
}

func builtinStringDowncase(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypeString {
		return nil, fmt.Errorf("%d:%d: string-downcase: expected 1 string argument", expr.Line, expr.Col)
	}
	return StringValue(strings.ToLower(args[0].StrContent())), nil
}

func builtinGcd(args []*Value, expr *Expr) (*Value, error) {
	if len(args) == 0 {
		return IntValue(0), nil
	}
	result := args[0].IntVal
	if result < 0 {
		result = -result
	}
	for _, a := range args[1:] {
		b := a.IntVal
		if b < 0 {
			b = -b
		}
		result = gcd(result, b)
	}
	return IntValue(result), nil
}

func builtinLcm(args []*Value, expr *Expr) (*Value, error) {
	if len(args) == 0 {
		return IntValue(1), nil
	}
	result := args[0].IntVal
	if result < 0 {
		result = -result
	}
	for _, a := range args[1:] {
		b := a.IntVal
		if b < 0 {
			b = -b
		}
		if result == 0 && b == 0 {
			result = 0
		} else {
			result = result / gcd(result, b) * b
		}
	}
	return IntValue(result), nil
}

func builtinTruncate(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: truncate: expected 1 argument", expr.Line, expr.Col)
	}
	switch args[0].Type {
	case TypeInt:
		return args[0], nil
	case TypeFloat:
		return IntValue(int64(args[0].FloatVal)), nil
	case TypeRational:
		return IntValue(args[0].Num / args[0].Denom), nil
	default:
		return nil, fmt.Errorf("%d:%d: truncate: expected number", expr.Line, expr.Col)
	}
}

func builtinRound(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: round: expected 1 argument", expr.Line, expr.Col)
	}
	switch args[0].Type {
	case TypeInt:
		return args[0], nil
	case TypeFloat:
		f := args[0].FloatVal
		// Banker's rounding (round half to even)
		rounded := int64(f + 0.5)
		if f-float64(int64(f)) == 0.5 && rounded%2 != 0 {
			rounded--
		}
		return IntValue(rounded), nil
	case TypeRational:
		return IntValue(args[0].Num / args[0].Denom), nil
	default:
		return nil, fmt.Errorf("%d:%d: round: expected number", expr.Line, expr.Col)
	}
}

func builtinMakeString(args []*Value, expr *Expr) (*Value, error) {
	if len(args) < 1 || len(args) > 2 || args[0].Type != TypeInt {
		return nil, fmt.Errorf("%d:%d: make-string: expected int [char]", expr.Line, expr.Col)
	}
	n := int(args[0].IntVal)
	ch := ' '
	if len(args) == 2 && args[1].Type == TypeChar {
		ch = rune(args[1].IntVal)
	}
	runes := make([]rune, n)
	for i := range runes {
		runes[i] = ch
	}
	return MutableStringValue(string(runes)), nil
}

func builtinString(args []*Value, expr *Expr) (*Value, error) {
	runes := make([]rune, len(args))
	for i, a := range args {
		if a.Type != TypeChar {
			return nil, fmt.Errorf("%d:%d: string: expected char argument", expr.Line, expr.Col)
		}
		runes[i] = rune(a.IntVal)
	}
	return StringValue(string(runes)), nil
}

func builtinStringGtQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 || args[0].Type != TypeString || args[1].Type != TypeString {
		return nil, fmt.Errorf("%d:%d: string>?: expected 2 string arguments", expr.Line, expr.Col)
	}
	return BoolValue(args[0].StrContent() > args[1].StrContent()), nil
}

func builtinStringLeQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 || args[0].Type != TypeString || args[1].Type != TypeString {
		return nil, fmt.Errorf("%d:%d: string<=?: expected 2 string arguments", expr.Line, expr.Col)
	}
	return BoolValue(args[0].StrContent() <= args[1].StrContent()), nil
}

func builtinStringGeQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 || args[0].Type != TypeString || args[1].Type != TypeString {
		return nil, fmt.Errorf("%d:%d: string>=?: expected 2 string arguments", expr.Line, expr.Col)
	}
	return BoolValue(args[0].StrContent() >= args[1].StrContent()), nil
}

func evalSetBang(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) != 3 {
		return nil, fmt.Errorf("%d:%d: set!: expected 2 arguments", expr.Line, expr.Col)
	}
	target := expr.List[1]
	if target.Type != ExprSymbol {
		return nil, fmt.Errorf("%d:%d: set!: first argument must be a symbol", target.Line, target.Col)
	}
	val, err := Eval(expr.List[2], env)
	if err != nil {
		return nil, err
	}
	if !env.SetExisting(target.StrVal, val) {
		return nil, fmt.Errorf("%d:%d: set!: unbound variable: %s", target.Line, target.Col, target.StrVal)
	}
	return Void, nil
}

func parseParams(paramList *Expr) (params []string, restParam string, err error) {
	for i, p := range paramList.List {
		if p.Type != ExprSymbol {
			return nil, "", fmt.Errorf("%d:%d: parameter must be a symbol", p.Line, p.Col)
		}
		if p.StrVal == "." {
			// dot notation: everything before is fixed params, next is rest param
			if i+1 >= len(paramList.List) || i+2 != len(paramList.List) {
				return nil, "", fmt.Errorf("%d:%d: bad dot notation in parameters", p.Line, p.Col)
			}
			rest := paramList.List[i+1]
			if rest.Type != ExprSymbol {
				return nil, "", fmt.Errorf("%d:%d: rest parameter must be a symbol", rest.Line, rest.Col)
			}
			return params, rest.StrVal, nil
		}
		params = append(params, p.StrVal)
	}
	return params, "", nil
}

func evalLambda(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) < 3 {
		return nil, fmt.Errorf("%d:%d: lambda: too few arguments", expr.Line, expr.Col)
	}
	paramList := expr.List[1]
	if paramList.Type == ExprSymbol {
		// (lambda args body...) — all args collected as rest
		return &Value{
			Type:       TypeLambda,
			RestParam:  paramList.StrVal,
			Body:       expr.List[2:],
			ClosureEnv: env,
		}, nil
	}
	if paramList.Type != ExprList {
		return nil, fmt.Errorf("%d:%d: lambda: parameters must be a list", paramList.Line, paramList.Col)
	}
	params, restParam, err := parseParams(paramList)
	if err != nil {
		return nil, err
	}
	return &Value{
		Type:       TypeLambda,
		Params:     params,
		RestParam:  restParam,
		Body:       expr.List[2:],
		ClosureEnv: env,
	}, nil
}

func evalCaseLambda(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) < 2 {
		return nil, fmt.Errorf("%d:%d: case-lambda: no clauses", expr.Line, expr.Col)
	}
	var clauses []*Value
	for _, clauseExpr := range expr.List[1:] {
		if clauseExpr.Type != ExprList || len(clauseExpr.List) < 2 {
			return nil, fmt.Errorf("%d:%d: case-lambda: bad clause", clauseExpr.Line, clauseExpr.Col)
		}
		paramList := clauseExpr.List[0]
		body := clauseExpr.List[1:]
		if paramList.Type == ExprSymbol {
			// (args body...) — rest-only
			clauses = append(clauses, &Value{
				Type:       TypeLambda,
				RestParam:  paramList.StrVal,
				Body:       body,
				ClosureEnv: env,
			})
			continue
		}
		if paramList.Type != ExprList {
			return nil, fmt.Errorf("%d:%d: case-lambda: bad parameter list", paramList.Line, paramList.Col)
		}
		params, restParam, err := parseParams(paramList)
		if err != nil {
			return nil, err
		}
		clauses = append(clauses, &Value{
			Type:       TypeLambda,
			Params:     params,
			RestParam:  restParam,
			Body:       body,
			ClosureEnv: env,
		})
	}
	return &Value{Type: TypeLambda, Clauses: clauses, ClosureEnv: env}, nil
}

func callCaseLambda(op *Value, args []*Value, expr *Expr) (*Value, error) {
	for _, clause := range op.Clauses {
		if clause.RestParam != "" && len(clause.Params) == 0 {
			// rest-only: matches any arity
			return callLambda(clause, args, expr)
		}
		if clause.RestParam != "" {
			if len(args) >= len(clause.Params) {
				return callLambda(clause, args, expr)
			}
		} else {
			if len(args) == len(clause.Params) {
				return callLambda(clause, args, expr)
			}
		}
	}
	return nil, fmt.Errorf("%d:%d: case-lambda: no matching clause for %d arguments", expr.Line, expr.Col, len(args))
}

// evalDefineRecordType handles (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
func evalDefineRecordType(expr *Expr, env *Env) (*Value, error) {
	// (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
	if len(expr.List) < 4 {
		return nil, fmt.Errorf("%d:%d: define-record-type: bad syntax", expr.Line, expr.Col)
	}

	// 1. Type name
	typeName := expr.List[1].StrVal

	// 2. Constructor: (make-xxx field1 field2 ...)
	ctorExpr := expr.List[2]
	if ctorExpr.Type != ExprList || len(ctorExpr.List) < 1 {
		return nil, fmt.Errorf("%d:%d: define-record-type: bad constructor", expr.Line, expr.Col)
	}
	ctorName := ctorExpr.List[0].StrVal
	ctorFields := make([]string, len(ctorExpr.List)-1)
	for i, f := range ctorExpr.List[1:] {
		ctorFields[i] = f.StrVal
	}

	// 3. Predicate name
	predName := expr.List[3].StrVal

	// 4. Field accessors: (field-name accessor-name) ...
	type fieldSpec struct {
		fieldName    string
		accessorName string
	}
	var fields []fieldSpec
	for _, fExpr := range expr.List[4:] {
		if fExpr.Type != ExprList || len(fExpr.List) < 2 {
			return nil, fmt.Errorf("%d:%d: define-record-type: bad field spec", expr.Line, expr.Col)
		}
		fields = append(fields, fieldSpec{
			fieldName:    fExpr.List[0].StrVal,
			accessorName: fExpr.List[1].StrVal,
		})
	}

	// Define constructor
	tag := typeName
	ctorFieldsCopy := make([]string, len(ctorFields))
	copy(ctorFieldsCopy, ctorFields)
	env.Set(ctorName, SymbolValue("__builtin:record-ctor:"+tag+":"+strings.Join(ctorFieldsCopy, ",")))

	// Predicate
	env.Set(predName, SymbolValue("__builtin:record-pred:"+tag))

	// Accessors
	for _, f := range fields {
		env.Set(f.accessorName, SymbolValue("__builtin:record-acc:"+tag+":"+f.fieldName))
	}

	return Void, nil
}

func dispatchRecordCtor(info string, args []*Value, expr *Expr) (*Value, error) {
	sepIdx := strings.Index(info, ":")
	tag := info[:sepIdx]
	fieldStr := info[sepIdx+1:]
	var fieldNames []string
	if fieldStr != "" {
		fieldNames = strings.Split(fieldStr, ",")
	}
	if len(args) != len(fieldNames) {
		return nil, fmt.Errorf("%d:%d: record constructor: expected %d arguments, got %d", expr.Line, expr.Col, len(fieldNames), len(args))
	}
	fields := make(map[string]*Value, len(fieldNames))
	for i, name := range fieldNames {
		fields[name] = args[i]
	}
	return &Value{Type: TypeRecord, RecordTag: tag, RecordFields: fields}, nil
}

// --- L14: letrec ---

func evalLetrec(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) < 3 {
		return nil, fmt.Errorf("%d:%d: letrec: too few arguments", expr.Line, expr.Col)
	}
	bindingExpr := expr.List[1]
	body := expr.List[2:]
	if bindingExpr.Type != ExprList {
		return nil, fmt.Errorf("%d:%d: letrec: bindings must be a list", bindingExpr.Line, bindingExpr.Col)
	}
	letEnv := NewEnv(env)
	// First, bind all variables to undefined (Void)
	for _, b := range bindingExpr.List {
		if b.Type != ExprList || len(b.List) != 2 || b.List[0].Type != ExprSymbol {
			return nil, fmt.Errorf("%d:%d: letrec: bad binding", b.Line, b.Col)
		}
		letEnv.Set(b.List[0].StrVal, Void)
	}
	// Then evaluate init expressions in the letrec env (all mutually visible)
	for _, b := range bindingExpr.List {
		v, err := Eval(b.List[1], letEnv)
		if err != nil {
			return nil, err
		}
		letEnv.Set(b.List[0].StrVal, v)
	}
	for _, bodyExpr := range body[:len(body)-1] {
		_, err := Eval(bodyExpr, letEnv)
		if err != nil {
			return nil, err
		}
	}
	return tailCall(body[len(body)-1], letEnv), nil
}

func evalLetrecStar(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) < 3 {
		return nil, fmt.Errorf("%d:%d: letrec*: too few arguments", expr.Line, expr.Col)
	}
	bindingExpr := expr.List[1]
	body := expr.List[2:]
	if bindingExpr.Type != ExprList {
		return nil, fmt.Errorf("%d:%d: letrec*: bindings must be a list", bindingExpr.Line, bindingExpr.Col)
	}
	letEnv := NewEnv(env)
	// Bind sequentially — each init can see previous bindings
	for _, b := range bindingExpr.List {
		if b.Type != ExprList || len(b.List) != 2 || b.List[0].Type != ExprSymbol {
			return nil, fmt.Errorf("%d:%d: letrec*: bad binding", b.Line, b.Col)
		}
		v, err := Eval(b.List[1], letEnv)
		if err != nil {
			return nil, err
		}
		letEnv.Set(b.List[0].StrVal, v)
	}
	for _, bodyExpr := range body[:len(body)-1] {
		_, err := Eval(bodyExpr, letEnv)
		if err != nil {
			return nil, err
		}
	}
	return tailCall(body[len(body)-1], letEnv), nil
}

// --- L14: case ---

func eqvCompare(a, b *Value) bool {
	if a.Type != b.Type {
		return false
	}
	switch a.Type {
	case TypeSymbol:
		return a.StrVal == b.StrVal
	case TypeBool:
		return a.BoolVal == b.BoolVal
	case TypeInt:
		return a.IntVal == b.IntVal
	case TypeChar:
		return a.IntVal == b.IntVal
	case TypeNil:
		return true
	default:
		return a == b
	}
}

func evalCase(expr *Expr, env *Env) (*Value, error) {
	if len(expr.List) < 3 {
		return nil, fmt.Errorf("%d:%d: case: too few arguments", expr.Line, expr.Col)
	}
	key, err := Eval(expr.List[1], env)
	if err != nil {
		return nil, err
	}
	for _, clause := range expr.List[2:] {
		if clause.Type != ExprList || len(clause.List) < 2 {
			return nil, fmt.Errorf("%d:%d: case: bad clause", clause.Line, clause.Col)
		}
		datums := clause.List[0]
		// Check for else
		if datums.Type == ExprSymbol && datums.StrVal == "else" {
			var result *Value
			for _, bodyExpr := range clause.List[1:] {
				result, err = Eval(bodyExpr, env)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
		// datums should be a list of values
		if datums.Type == ExprList {
			for _, d := range datums.List {
				dv := exprToValue(d)
				if eqvCompare(key, dv) {
					var result *Value
					for _, bodyExpr := range clause.List[1:] {
						result, err = Eval(bodyExpr, env)
						if err != nil {
							return nil, err
						}
					}
					return result, nil
				}
			}
		}
	}
	return Void, nil
}

// --- L14: do ---

func evalDo(expr *Expr, env *Env) (*Value, error) {
	// (do ((var init step) ...) (test expr ...) body ...)
	if len(expr.List) < 3 {
		return nil, fmt.Errorf("%d:%d: do: too few arguments", expr.Line, expr.Col)
	}
	varSpecs := expr.List[1]
	testClause := expr.List[2]
	body := expr.List[3:]

	if varSpecs.Type != ExprList {
		return nil, fmt.Errorf("%d:%d: do: variable specs must be a list", varSpecs.Line, varSpecs.Col)
	}
	if testClause.Type != ExprList || len(testClause.List) < 1 {
		return nil, fmt.Errorf("%d:%d: do: bad test clause", testClause.Line, testClause.Col)
	}

	type doVar struct {
		name    string
		stepExpr *Expr // nil if no step
	}
	var vars []doVar

	doEnv := NewEnv(env)
	// Initialize variables
	for _, spec := range varSpecs.List {
		if spec.Type != ExprList || len(spec.List) < 2 || spec.List[0].Type != ExprSymbol {
			return nil, fmt.Errorf("%d:%d: do: bad variable spec", spec.Line, spec.Col)
		}
		name := spec.List[0].StrVal
		initVal, err := Eval(spec.List[1], env)
		if err != nil {
			return nil, err
		}
		doEnv.Set(name, initVal)
		var step *Expr
		if len(spec.List) >= 3 {
			step = spec.List[2]
		}
		vars = append(vars, doVar{name: name, stepExpr: step})
	}

	for {
		// Test
		testVal, err := Eval(testClause.List[0], doEnv)
		if err != nil {
			return nil, err
		}
		if testVal.IsTruthy() {
			// Evaluate result expressions
			if len(testClause.List) > 1 {
				var result *Value
				for _, e := range testClause.List[1:] {
					result, err = Eval(e, doEnv)
					if err != nil {
						return nil, err
					}
				}
				return result, nil
			}
			return Void, nil
		}
		// Execute body
		for _, b := range body {
			_, err := Eval(b, doEnv)
			if err != nil {
				return nil, err
			}
		}
		// Parallel step: evaluate all steps using current values, then update
		newVals := make([]*Value, len(vars))
		for i, v := range vars {
			if v.stepExpr != nil {
				newVals[i], err = Eval(v.stepExpr, doEnv)
				if err != nil {
					return nil, err
				}
			}
		}
		for i, v := range vars {
			if v.stepExpr != nil {
				doEnv.Set(v.name, newVals[i])
			}
		}
	}
}

// --- L14: eqv? ---

func builtinEqvQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%d:%d: eqv?: expected 2 arguments", expr.Line, expr.Col)
	}
	return BoolValue(eqvCompare(args[0], args[1])), nil
}

// --- L14: vector builtins ---

func builtinVector(args []*Value, expr *Expr) (*Value, error) {
	elems := make([]*Value, len(args))
	copy(elems, args)
	return &Value{Type: TypeVector, VecElems: elems}, nil
}

func builtinMakeVector(args []*Value, expr *Expr) (*Value, error) {
	if len(args) < 1 || len(args) > 2 || args[0].Type != TypeInt {
		return nil, fmt.Errorf("%d:%d: make-vector: expected int [fill]", expr.Line, expr.Col)
	}
	n := int(args[0].IntVal)
	fill := IntValue(0)
	if len(args) == 2 {
		fill = args[1]
	}
	elems := make([]*Value, n)
	for i := range elems {
		elems[i] = fill
	}
	return &Value{Type: TypeVector, VecElems: elems}, nil
}

func builtinVectorRef(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 2 || args[0].Type != TypeVector || args[1].Type != TypeInt {
		return nil, fmt.Errorf("%d:%d: vector-ref: expected vector and int", expr.Line, expr.Col)
	}
	idx := int(args[1].IntVal)
	if idx < 0 || idx >= len(args[0].VecElems) {
		return nil, fmt.Errorf("%d:%d: vector-ref: index out of range", expr.Line, expr.Col)
	}
	return args[0].VecElems[idx], nil
}

func builtinVectorSet(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 3 || args[0].Type != TypeVector || args[1].Type != TypeInt {
		return nil, fmt.Errorf("%d:%d: vector-set!: expected vector, int, value", expr.Line, expr.Col)
	}
	idx := int(args[1].IntVal)
	if idx < 0 || idx >= len(args[0].VecElems) {
		return nil, fmt.Errorf("%d:%d: vector-set!: index out of range", expr.Line, expr.Col)
	}
	args[0].VecElems[idx] = args[2]
	return Void, nil
}

func builtinVectorLength(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypeVector {
		return nil, fmt.Errorf("%d:%d: vector-length: expected vector", expr.Line, expr.Col)
	}
	return IntValue(int64(len(args[0].VecElems))), nil
}

func builtinVectorQ(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: vector?: expected 1 argument", expr.Line, expr.Col)
	}
	return BoolValue(args[0].Type == TypeVector), nil
}

func builtinVectorToList(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 || args[0].Type != TypeVector {
		return nil, fmt.Errorf("%d:%d: vector->list: expected vector", expr.Line, expr.Col)
	}
	result := Nil
	for i := len(args[0].VecElems) - 1; i >= 0; i-- {
		result = &Value{Type: TypePair, Car: args[0].VecElems[i], Cdr: result}
	}
	return result, nil
}

func builtinListToVector(args []*Value, expr *Expr) (*Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%d:%d: list->vector: expected 1 argument", expr.Line, expr.Col)
	}
	var elems []*Value
	v := args[0]
	for v.Type == TypePair {
		elems = append(elems, v.Car)
		v = v.Cdr
	}
	return &Value{Type: TypeVector, VecElems: elems}, nil
}
