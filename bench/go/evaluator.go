package ming

import (
	"fmt"
	"math"
	"strconv"
	"strings"
	"unicode"
)

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	exprs, err := parse(input)
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}
	if len(exprs) == 0 {
		return "", &EvalError{Message: "empty input"}
	}

	env := defaultEnv(nil)
	result, err := evalTopLevel(exprs, env)
	if err != nil {
		return "", err
	}
	// void produces empty string
	if _, ok := result.(*VoidVal); ok {
		return "", nil
	}
	return result.String(), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	exprs, parseErr := parse(input)
	if parseErr != nil {
		return "", "", &EvalError{Message: parseErr.Error()}
	}
	if len(exprs) == 0 {
		return "", "", &EvalError{Message: "empty input"}
	}

	var buf strings.Builder
	env := defaultEnv(&buf)
	res, evalErr := evalTopLevel(exprs, env)
	if evalErr != nil {
		return "", "", evalErr
	}
	if _, ok := res.(*VoidVal); ok {
		return "", buf.String(), nil
	}
	return res.String(), buf.String(), nil
}

// tailCall is a sentinel value for tail call optimization (trampoline).
type tailCall struct {
	expr *Expr
	env  *Env
}

func (t *tailCall) String() string { return "" }

// contJump is panicked when a continuation is invoked, causing non-local transfer.
type contJump struct {
	cont  *ContinuationVal
	value Value
}

// contFrameStack tracks the current evaluation context for continuation capture.
var contFrameStack []contFrame

// dynamicWindStack tracks active dynamic-wind extents.
var dynamicWindStack []windRecord
var windIDCounter int

// eval evaluates an expression in the given environment using a trampoline for TCO.
func eval(expr *Expr, env *Env) (Value, error) {
	for {
		result, err := evalStep(expr, env)
		if err != nil {
			return nil, err
		}
		if tc, ok := result.(*tailCall); ok {
			expr = tc.expr
			env = tc.env
			continue
		}
		return result, nil
	}
}

// evalTopLevel evaluates top-level expressions with continuation support.
func evalTopLevel(exprs []*Expr, env *Env) (Value, error) {
	contFrameStack = contFrameStack[:0]
	dynamicWindStack = dynamicWindStack[:0]

	result, err, cj := protectedEval(func() (Value, error) {
		contFrameStack = append(contFrameStack, contFrame{Kind: frameTopLevel, Exprs: exprs, Env: env})
		defer func() {
			if len(contFrameStack) > 0 {
				contFrameStack = contFrameStack[:len(contFrameStack)-1]
			}
		}()
		var r Value
		for i, expr := range exprs {
			contFrameStack[len(contFrameStack)-1].Idx = i
			var err error
			r, err = eval(expr, env)
			if err != nil {
				return nil, err
			}
		}
		return r, nil
	})

	for cj != nil {
		cont := cj.cont
		val := cj.value
		result, err, cj = protectedEval(func() (Value, error) {
			return execContinuation(cont, val)
		})
	}

	return result, err
}

// protectedEval runs fn and catches contJump panics.
func protectedEval(fn func() (Value, error)) (result Value, err error, jump *contJump) {
	defer func() {
		if r := recover(); r != nil {
			if cj, ok := r.(*contJump); ok {
				jump = cj
				return
			}
			panic(r)
		}
	}()
	result, err = fn()
	return result, err, nil
}

// callWithContRecover calls fn and catches escape continuation panics for the given continuation.
func callWithContRecover(k *ContinuationVal, fn func() (Value, error)) (result Value, err error) {
	defer func() {
		if r := recover(); r != nil {
			if cj, ok := r.(*contJump); ok && cj.cont == k {
				// Escape continuation: return the value directly
				result = cj.value
				err = nil
				return
			}
			panic(r) // re-panic if not for us
		}
	}()
	result, err = fn()
	if err != nil {
		return nil, err
	}
	// Resolve tail calls from proc
	for {
		if tc, ok := result.(*tailCall); ok {
			result, err = evalStep(tc.expr, tc.env)
			if err != nil {
				return nil, err
			}
			continue
		}
		break
	}
	return result, err
}

// callccInjectValue, when non-nil, makes the next call/cc return this value directly.
var callccInjectValue *Value

// windDummyExpr is used for error context when calling wind thunks.
var windDummyExpr = &Expr{Kind: ExprList, List: []*Expr{{Kind: ExprSymbol, SVal: "dynamic-wind"}}}

// callThunk calls a zero-argument procedure and resolves tail calls.
func callThunk(thunk Value, callExpr *Expr) (Value, error) {
	if callExpr == nil {
		callExpr = windDummyExpr
	}
	result, err := applyProc(thunk, []Value{}, callExpr)
	if err != nil {
		return nil, err
	}
	for {
		tc, ok := result.(*tailCall)
		if !ok {
			return result, nil
		}
		result, err = evalStep(tc.expr, tc.env)
		if err != nil {
			return nil, err
		}
	}
}

// execContinuation re-evaluates from a captured continuation's frames.
func execContinuation(cont *ContinuationVal, value Value) (Value, error) {
	savedStack := contFrameStack
	contFrameStack = nil
	defer func() { contFrameStack = savedStack }()

	// Set inject value so the next call/cc returns it directly
	callccInjectValue = &value

	var result Value
	var err error

	// Frames are stored outermost-first. Process innermost-first.
	for fi := len(cont.Frames) - 1; fi >= 0; fi-- {
		frame := cont.Frames[fi]

		// Set up frame stack: current frame + all outer frames (outermost first)
		contFrameStack = contFrameStack[:0]
		for i := 0; i <= fi; i++ {
			contFrameStack = append(contFrameStack, cont.Frames[i])
		}

		isInnermost := fi == len(cont.Frames)-1

		startIdx := frame.Idx
		if !isInnermost {
			// Outer frames: skip the expression at idx (handled by inner frames)
			startIdx = frame.Idx + 1
		}
		for i := startIdx; i < len(frame.Exprs); i++ {
			contFrameStack[len(contFrameStack)-1].Idx = i
			result, err = eval(frame.Exprs[i], frame.Env)
			if err != nil {
				return nil, err
			}
		}
	}

	return result, nil
}

// evalStep performs one step of evaluation, returning tailCall for tail positions.
func evalStep(expr *Expr, env *Env) (Value, error) {
	switch expr.Kind {
	case ExprInt:
		return &IntVal{Val: expr.IVal}, nil
	case ExprFloat:
		return &FloatVal{Val: expr.FVal}, nil
	case ExprRational:
		return makeRational(expr.Num, expr.Denom), nil
	case ExprBool:
		return &BoolVal{Val: expr.BVal}, nil
	case ExprString:
		return &StringVal{Val: expr.SVal, Immutable: true}, nil
	case ExprChar:
		return &CharVal{Val: expr.RVal}, nil
	case ExprSymbol:
		v, ok := env.Get(expr.SVal)
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unbound variable: %s", expr.Line, expr.Col, expr.SVal)}
		}
		return v, nil
	case ExprList:
		return evalList(expr, env)
	}
	return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unknown expression", expr.Line, expr.Col)}
}

func evalList(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) == 0 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: empty application", expr.Line, expr.Col)}
	}

	head := expr.List[0]

	// special forms
	if head.Kind == ExprSymbol {
		switch head.SVal {
		case "and":
			return evalAnd(expr, env)
		case "or":
			return evalOr(expr, env)
		case "define":
			return evalDefine(expr, env)
		case "if":
			return evalIf(expr, env)
		case "quote":
			return evalQuote(expr, env)
		case "lambda":
			return evalLambda(expr, env)
		case "let":
			return evalLet(expr, env)
		case "begin":
			return evalBegin(expr, env)
		case "cond":
			return evalCond(expr, env)
		case "set!":
			return evalSet(expr, env)
		case "define-syntax":
			return evalDefineSyntax(expr, env)
		case "define-record-type":
			return evalDefineRecordType(expr, env)
		case "case-lambda":
			return evalCaseLambda(expr, env)
		case "let*":
			return evalLetStar(expr, env)
		case "letrec":
			return evalLetrec(expr, env)
		case "letrec*":
			return evalLetrecStar(expr, env)
		case "case":
			return evalCase(expr, env)
		case "do":
			return evalDo(expr, env)
		}

		// macro expansion: check if head symbol is bound to a SyntaxVal
		if v, ok := env.Get(head.SVal); ok {
			if sv, ok := v.(*SyntaxVal); ok {
				return expandMacro(sv, expr, env)
			}
		}
	}

	// evaluate operator
	op, err := eval(head, env)
	if err != nil {
		return nil, err
	}

	// evaluate arguments
	args := make([]Value, len(expr.List)-1)
	for i, a := range expr.List[1:] {
		args[i], err = eval(a, env)
		if err != nil {
			return nil, err
		}
	}

	// apply
	result, err := applyProc(op, args, expr)
	if err != nil {
		return nil, err
	}
	return result, nil
}

func evalAnd(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) == 1 {
		return &BoolVal{Val: true}, nil
	}
	elems := expr.List[1:]
	for _, e := range elems[:len(elems)-1] {
		result, err := eval(e, env)
		if err != nil {
			return nil, err
		}
		if !isTruthy(result) {
			return result, nil
		}
	}
	return &tailCall{expr: elems[len(elems)-1], env: env}, nil
}

func evalOr(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) == 1 {
		return &BoolVal{Val: false}, nil
	}
	elems := expr.List[1:]
	for _, e := range elems[:len(elems)-1] {
		result, err := eval(e, env)
		if err != nil {
			return nil, err
		}
		if isTruthy(result) {
			return result, nil
		}
	}
	return &tailCall{expr: elems[len(elems)-1], env: env}, nil
}

// BuiltinFunc is a built-in procedure.
type BuiltinFunc struct {
	Name string
	Fn   func(args []Value) (Value, error)
}

func (b *BuiltinFunc) String() string { return fmt.Sprintf("#<procedure %s>", b.Name) }

func defaultEnv(output *strings.Builder) *Env {
	env := NewEnv(nil)

	env.Set("+", &BuiltinFunc{Name: "+", Fn: builtinAdd})
	env.Set("-", &BuiltinFunc{Name: "-", Fn: builtinSub})
	env.Set("*", &BuiltinFunc{Name: "*", Fn: builtinMul})
	env.Set("/", &BuiltinFunc{Name: "/", Fn: builtinDiv})
	env.Set("<", &BuiltinFunc{Name: "<", Fn: builtinLt})
	env.Set(">", &BuiltinFunc{Name: ">", Fn: builtinGt})
	env.Set("=", &BuiltinFunc{Name: "=", Fn: builtinEq})
	env.Set("<=", &BuiltinFunc{Name: "<=", Fn: builtinLe})
	env.Set("not", &BuiltinFunc{Name: "not", Fn: builtinNot})

	// L03 builtins
	env.Set("cons", &BuiltinFunc{Name: "cons", Fn: builtinCons})
	env.Set("car", &BuiltinFunc{Name: "car", Fn: builtinCar})
	env.Set("cdr", &BuiltinFunc{Name: "cdr", Fn: builtinCdr})
	env.Set("null?", &BuiltinFunc{Name: "null?", Fn: builtinNullQ})
	env.Set("list", &BuiltinFunc{Name: "list", Fn: builtinList})
	env.Set("length", &BuiltinFunc{Name: "length", Fn: builtinLength})
	env.Set("number?", &BuiltinFunc{Name: "number?", Fn: builtinNumberQ})
	env.Set("string?", &BuiltinFunc{Name: "string?", Fn: builtinStringQ})
	env.Set("boolean?", &BuiltinFunc{Name: "boolean?", Fn: builtinBooleanQ})
	env.Set("pair?", &BuiltinFunc{Name: "pair?", Fn: builtinPairQ})
	env.Set("symbol?", &BuiltinFunc{Name: "symbol?", Fn: builtinSymbolQ})
	env.Set("append", &BuiltinFunc{Name: "append", Fn: builtinAppend})

	// L05 builtins — I/O
	env.Set("display", &BuiltinFunc{Name: "display", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("display: expected 1 argument, got %d", len(args))
		}
		if output != nil {
			output.WriteString(DisplayString(args[0]))
		}
		return &VoidVal{}, nil
	}})
	env.Set("write", &BuiltinFunc{Name: "write", Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("write: expected 1 argument, got %d", len(args))
		}
		if output != nil {
			output.WriteString(args[0].String())
		}
		return &VoidVal{}, nil
	}})
	env.Set("newline", &BuiltinFunc{Name: "newline", Fn: func(args []Value) (Value, error) {
		if len(args) != 0 {
			return nil, fmt.Errorf("newline: expected 0 arguments, got %d", len(args))
		}
		if output != nil {
			output.WriteByte('\n')
		}
		return &VoidVal{}, nil
	}})

	// L05 builtins — string operations
	env.Set("string-append", &BuiltinFunc{Name: "string-append", Fn: builtinStringAppend})
	env.Set("string-length", &BuiltinFunc{Name: "string-length", Fn: builtinStringLength})
	env.Set("substring", &BuiltinFunc{Name: "substring", Fn: builtinSubstring})
	env.Set("string->number", &BuiltinFunc{Name: "string->number", Fn: builtinStringToNumber})
	env.Set("number->string", &BuiltinFunc{Name: "number->string", Fn: builtinNumberToString})
	env.Set("symbol->string", &BuiltinFunc{Name: "symbol->string", Fn: builtinSymbolToString})
	env.Set("string->symbol", &BuiltinFunc{Name: "string->symbol", Fn: builtinStringToSymbol})
	env.Set("string-ref", &BuiltinFunc{Name: "string-ref", Fn: builtinStringRef})
	env.Set("char?", &BuiltinFunc{Name: "char?", Fn: builtinCharQ})

	// L06 builtins — mutable strings
	env.Set("string-copy", &BuiltinFunc{Name: "string-copy", Fn: builtinStringCopy})
	env.Set("string-set!", &BuiltinFunc{Name: "string-set!", Fn: builtinStringSet})

	// L15 builtins — string immutability + char/integer conversion
	env.Set("string->list", &BuiltinFunc{Name: "string->list", Fn: builtinStringToList})
	env.Set("list->string", &BuiltinFunc{Name: "list->string", Fn: builtinListToString})
	env.Set("char->integer", &BuiltinFunc{Name: "char->integer", Fn: builtinCharToInteger})
	env.Set("integer->char", &BuiltinFunc{Name: "integer->char", Fn: builtinIntegerToChar})

	// L08 builtins
	env.Set("apply", &ApplyVal{})

	// L09 builtins — numeric utilities
	env.Set("abs", &BuiltinFunc{Name: "abs", Fn: builtinAbs})
	env.Set("modulo", &BuiltinFunc{Name: "modulo", Fn: builtinModulo})
	env.Set("remainder", &BuiltinFunc{Name: "remainder", Fn: builtinRemainder})
	env.Set("quotient", &BuiltinFunc{Name: "quotient", Fn: builtinQuotient})
	env.Set("min", &BuiltinFunc{Name: "min", Fn: builtinMin})
	env.Set("max", &BuiltinFunc{Name: "max", Fn: builtinMax})
	env.Set("expt", &BuiltinFunc{Name: "expt", Fn: builtinExpt})
	env.Set("zero?", &BuiltinFunc{Name: "zero?", Fn: builtinZeroQ})
	env.Set("positive?", &BuiltinFunc{Name: "positive?", Fn: builtinPositiveQ})
	env.Set("negative?", &BuiltinFunc{Name: "negative?", Fn: builtinNegativeQ})
	env.Set("odd?", &BuiltinFunc{Name: "odd?", Fn: builtinOddQ})
	env.Set("even?", &BuiltinFunc{Name: "even?", Fn: builtinEvenQ})

	// L09 builtins — list utilities
	env.Set("list-ref", &BuiltinFunc{Name: "list-ref", Fn: builtinListRef})
	env.Set("list-tail", &BuiltinFunc{Name: "list-tail", Fn: builtinListTail})
	env.Set("list?", &BuiltinFunc{Name: "list?", Fn: builtinListQ})
	env.Set("assoc", &BuiltinFunc{Name: "assoc", Fn: builtinAssoc})
	env.Set("assq", &BuiltinFunc{Name: "assq", Fn: builtinAssq})
	env.Set("assv", &BuiltinFunc{Name: "assv", Fn: builtinAssv})
	env.Set("member", &BuiltinFunc{Name: "member", Fn: builtinMember})
	env.Set("memq", &BuiltinFunc{Name: "memq", Fn: builtinMemq})
	env.Set("memv", &BuiltinFunc{Name: "memv", Fn: builtinMemv})
	env.Set("equal?", &BuiltinFunc{Name: "equal?", Fn: builtinEqualQ})
	env.Set("eq?", &BuiltinFunc{Name: "eq?", Fn: builtinEqQ})
	env.Set("map", &MapVal{})

	// L09 builtins — char utilities
	env.Set("char-alphabetic?", &BuiltinFunc{Name: "char-alphabetic?", Fn: builtinCharAlphaQ})
	env.Set("char-numeric?", &BuiltinFunc{Name: "char-numeric?", Fn: builtinCharNumericQ})
	env.Set("char-upcase", &BuiltinFunc{Name: "char-upcase", Fn: builtinCharUpcase})
	env.Set("char-downcase", &BuiltinFunc{Name: "char-downcase", Fn: builtinCharDowncase})
	env.Set("char=?", &BuiltinFunc{Name: "char=?", Fn: builtinCharEqQ})
	env.Set("char<?", &BuiltinFunc{Name: "char<?", Fn: builtinCharLtQ})

	// L09 builtins — string utilities
	env.Set("string=?", &BuiltinFunc{Name: "string=?", Fn: builtinStringEqQ})
	env.Set("string<?", &BuiltinFunc{Name: "string<?", Fn: builtinStringLtQ})
	env.Set("string-ci=?", &BuiltinFunc{Name: "string-ci=?", Fn: builtinStringCiEqQ})
	env.Set("string-upcase", &BuiltinFunc{Name: "string-upcase", Fn: builtinStringUpcase})
	env.Set("string-downcase", &BuiltinFunc{Name: "string-downcase", Fn: builtinStringDowncase})
	env.Set(">=", &BuiltinFunc{Name: ">=", Fn: builtinGe})

	// L11 builtins — exact arithmetic & rationals
	env.Set("exact?", &BuiltinFunc{Name: "exact?", Fn: builtinExactQ})
	env.Set("inexact?", &BuiltinFunc{Name: "inexact?", Fn: builtinInexactQ})
	env.Set("exact->inexact", &BuiltinFunc{Name: "exact->inexact", Fn: builtinExactToInexact})
	env.Set("inexact->exact", &BuiltinFunc{Name: "inexact->exact", Fn: builtinInexactToExact})
	env.Set("numerator", &BuiltinFunc{Name: "numerator", Fn: builtinNumerator})
	env.Set("denominator", &BuiltinFunc{Name: "denominator", Fn: builtinDenominator})
	env.Set("integer?", &BuiltinFunc{Name: "integer?", Fn: builtinIntegerQ})
	env.Set("rational?", &BuiltinFunc{Name: "rational?", Fn: builtinRationalQ})
	env.Set("procedure?", &BuiltinFunc{Name: "procedure?", Fn: builtinProcedureQ})

	// L17 builtins — pair mutation
	env.Set("set-car!", &BuiltinFunc{Name: "set-car!", Fn: builtinSetCar})
	env.Set("set-cdr!", &BuiltinFunc{Name: "set-cdr!", Fn: builtinSetCdr})

	// L17 builtins — cxr compositions
	env.Set("caar", &BuiltinFunc{Name: "caar", Fn: func(args []Value) (Value, error) { return cxr(args, "caar", "aa") }})
	env.Set("cadr", &BuiltinFunc{Name: "cadr", Fn: func(args []Value) (Value, error) { return cxr(args, "cadr", "da") }})
	env.Set("cdar", &BuiltinFunc{Name: "cdar", Fn: func(args []Value) (Value, error) { return cxr(args, "cdar", "ad") }})
	env.Set("cddr", &BuiltinFunc{Name: "cddr", Fn: func(args []Value) (Value, error) { return cxr(args, "cddr", "dd") }})
	env.Set("caddr", &BuiltinFunc{Name: "caddr", Fn: func(args []Value) (Value, error) { return cxr(args, "caddr", "dda") }})
	env.Set("cdddr", &BuiltinFunc{Name: "cdddr", Fn: func(args []Value) (Value, error) { return cxr(args, "cdddr", "ddd") }})
	env.Set("cadddr", &BuiltinFunc{Name: "cadddr", Fn: func(args []Value) (Value, error) { return cxr(args, "cadddr", "ddda") }})
	env.Set("caddar", &BuiltinFunc{Name: "caddar", Fn: func(args []Value) (Value, error) { return cxr(args, "caddar", "adda") }})

	// L17 builtins — for-each
	env.Set("for-each", &BuiltinFunc{Name: "for-each", Fn: builtinForEach})

	// L14 builtins — vectors
	env.Set("vector", &BuiltinFunc{Name: "vector", Fn: builtinVector})
	env.Set("make-vector", &BuiltinFunc{Name: "make-vector", Fn: builtinMakeVector})
	env.Set("vector-ref", &BuiltinFunc{Name: "vector-ref", Fn: builtinVectorRef})
	env.Set("vector-set!", &BuiltinFunc{Name: "vector-set!", Fn: builtinVectorSet})
	env.Set("vector-length", &BuiltinFunc{Name: "vector-length", Fn: builtinVectorLength})
	env.Set("vector?", &BuiltinFunc{Name: "vector?", Fn: builtinVectorQ})
	env.Set("vector->list", &BuiltinFunc{Name: "vector->list", Fn: builtinVectorToList})
	env.Set("list->vector", &BuiltinFunc{Name: "list->vector", Fn: builtinListToVector})
	env.Set("eqv?", &BuiltinFunc{Name: "eqv?", Fn: builtinEqvQ})
	env.Set("gcd", &BuiltinFunc{Name: "gcd", Fn: builtinGcd})
	env.Set("lcm", &BuiltinFunc{Name: "lcm", Fn: builtinLcm})
	env.Set("truncate", &BuiltinFunc{Name: "truncate", Fn: builtinTruncate})
	env.Set("round", &BuiltinFunc{Name: "round", Fn: builtinRound})
	env.Set("make-string", &BuiltinFunc{Name: "make-string", Fn: builtinMakeString})
	env.Set("string", &BuiltinFunc{Name: "string", Fn: builtinStringConstructor})
	env.Set("string>?", &BuiltinFunc{Name: "string>?", Fn: builtinStringGtQ})
	env.Set("string<=?", &BuiltinFunc{Name: "string<=?", Fn: builtinStringLeQ})
	env.Set("string>=?", &BuiltinFunc{Name: "string>=?", Fn: builtinStringGeQ})
	env.Set("reverse", &BuiltinFunc{Name: "reverse", Fn: builtinReverse})
	env.Set("error", &BuiltinFunc{Name: "error", Fn: builtinError})

	// L18 builtins — first-class continuations
	env.Set("call/cc", &CallCCVal{})
	env.Set("call-with-current-continuation", &CallCCVal{})

	// L19 builtins — dynamic-wind
	env.Set("dynamic-wind", &DynamicWindVal{})

	return env
}

func requireInts(name string, args []Value) ([]int64, error) {
	nums := make([]int64, len(args))
	for i, a := range args {
		n, ok := a.(*IntVal)
		if !ok {
			return nil, fmt.Errorf("%s: expected number, got %s", name, a.String())
		}
		nums[i] = n.Val
	}
	return nums, nil
}

// hasInexact returns true if any argument is a FloatVal.
func hasInexact(args []Value) bool {
	for _, a := range args {
		if _, ok := a.(*FloatVal); ok {
			return true
		}
	}
	return false
}

// requireNumeric checks all args are numeric.
func requireNumeric(name string, args []Value) error {
	for _, a := range args {
		if !isNumeric(a) {
			return fmt.Errorf("%s: expected number, got %s", name, a.String())
		}
	}
	return nil
}

// addRat adds two rationals: a/b + c/d = (ad+bc)/bd
func addRat(an, ad, bn, bd int64) (int64, int64) {
	return an*bd + bn*ad, ad * bd
}

// subRat subtracts two rationals: a/b - c/d = (ad-bc)/bd
func subRat(an, ad, bn, bd int64) (int64, int64) {
	return an*bd - bn*ad, ad * bd
}

// mulRat multiplies two rationals: a/b * c/d = ac/bd
func mulRat(an, ad, bn, bd int64) (int64, int64) {
	return an * bn, ad * bd
}

// divRat divides two rationals: a/b / c/d = ad/bc
func divRat(an, ad, bn, bd int64) (int64, int64) {
	return an * bd, ad * bn
}

func builtinAdd(args []Value) (Value, error) {
	if err := requireNumeric("+", args); err != nil {
		return nil, err
	}
	if hasInexact(args) {
		var sum float64
		for _, a := range args {
			f, _ := toFloat64(a)
			sum += f
		}
		return &FloatVal{Val: sum}, nil
	}
	rn, rd := int64(0), int64(1)
	for _, a := range args {
		an, ad, _ := toRational(a)
		rn, rd = addRat(rn, rd, an, ad)
	}
	return makeRational(rn, rd), nil
}

func builtinSub(args []Value) (Value, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("-: need at least 1 argument")
	}
	if err := requireNumeric("-", args); err != nil {
		return nil, err
	}
	if hasInexact(args) {
		f0, _ := toFloat64(args[0])
		if len(args) == 1 {
			return &FloatVal{Val: -f0}, nil
		}
		for _, a := range args[1:] {
			f, _ := toFloat64(a)
			f0 -= f
		}
		return &FloatVal{Val: f0}, nil
	}
	an, ad, _ := toRational(args[0])
	if len(args) == 1 {
		return makeRational(-an, ad), nil
	}
	for _, a := range args[1:] {
		bn, bd, _ := toRational(a)
		an, ad = subRat(an, ad, bn, bd)
	}
	return makeRational(an, ad), nil
}

func builtinMul(args []Value) (Value, error) {
	if err := requireNumeric("*", args); err != nil {
		return nil, err
	}
	if hasInexact(args) {
		result := 1.0
		for _, a := range args {
			f, _ := toFloat64(a)
			result *= f
		}
		return &FloatVal{Val: result}, nil
	}
	rn, rd := int64(1), int64(1)
	for _, a := range args {
		an, ad, _ := toRational(a)
		rn, rd = mulRat(rn, rd, an, ad)
	}
	return makeRational(rn, rd), nil
}

func builtinDiv(args []Value) (Value, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("/: need at least 2 arguments")
	}
	if err := requireNumeric("/", args); err != nil {
		return nil, err
	}
	if hasInexact(args) {
		f0, _ := toFloat64(args[0])
		for _, a := range args[1:] {
			f, _ := toFloat64(a)
			if f == 0 {
				return nil, fmt.Errorf("/: division by zero")
			}
			f0 /= f
		}
		return &FloatVal{Val: f0}, nil
	}
	an, ad, _ := toRational(args[0])
	for _, a := range args[1:] {
		bn, bd, _ := toRational(a)
		if bn == 0 {
			return nil, fmt.Errorf("/: division by zero")
		}
		an, ad = divRat(an, ad, bn, bd)
	}
	return makeRational(an, ad), nil
}

func numericCompare(name string, args []Value) (float64, float64, error) {
	if len(args) != 2 {
		return 0, 0, fmt.Errorf("%s: expected 2 arguments", name)
	}
	if err := requireNumeric(name, args); err != nil {
		return 0, 0, err
	}
	a, _ := toFloat64(args[0])
	b, _ := toFloat64(args[1])
	return a, b, nil
}

func builtinLt(args []Value) (Value, error) {
	a, b, err := numericCompare("<", args)
	if err != nil {
		return nil, err
	}
	return &BoolVal{Val: a < b}, nil
}

func builtinGt(args []Value) (Value, error) {
	a, b, err := numericCompare(">", args)
	if err != nil {
		return nil, err
	}
	return &BoolVal{Val: a > b}, nil
}

func builtinEq(args []Value) (Value, error) {
	a, b, err := numericCompare("=", args)
	if err != nil {
		return nil, err
	}
	return &BoolVal{Val: a == b}, nil
}

func builtinLe(args []Value) (Value, error) {
	a, b, err := numericCompare("<=", args)
	if err != nil {
		return nil, err
	}
	return &BoolVal{Val: a <= b}, nil
}

func builtinNot(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("not: expected 1 argument")
	}
	return &BoolVal{Val: !isTruthy(args[0])}, nil
}

// applyProc applies a procedure (builtin or lambda) to arguments.
func applyProc(op Value, args []Value, callExpr *Expr) (Value, error) {
	switch fn := op.(type) {
	case *BuiltinFunc:
		result, err := fn.Fn(args)
		if err != nil {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s", callExpr.Line, callExpr.Col, err.Error())}
		}
		return result, nil
	case *LambdaVal:
		if fn.RestParam != "" {
			if len(args) < len(fn.Params) {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: expected at least %d arguments, got %d", callExpr.Line, callExpr.Col, len(fn.Params), len(args))}
			}
		} else {
			if len(args) != len(fn.Params) {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: expected %d arguments, got %d", callExpr.Line, callExpr.Col, len(fn.Params), len(args))}
			}
		}
		childEnv := NewEnv(fn.Env)
		for i, p := range fn.Params {
			childEnv.Set(p, args[i])
		}
		if fn.RestParam != "" {
			rest := Value(&NilVal{})
			for i := len(args) - 1; i >= len(fn.Params); i-- {
				rest = &PairVal{Car: args[i], Cdr: rest}
			}
			childEnv.Set(fn.RestParam, rest)
		}
		for _, bodyExpr := range fn.Body[:len(fn.Body)-1] {
			_, err := eval(bodyExpr, childEnv)
			if err != nil {
				return nil, err
			}
		}
		return &tailCall{expr: fn.Body[len(fn.Body)-1], env: childEnv}, nil
	case *CaseLambdaVal:
		// Find matching clause by arity
		for _, clause := range fn.Clauses {
			if clause.RestParam != "" {
				if len(args) >= len(clause.Params) {
					childEnv := NewEnv(fn.Env)
					for i, p := range clause.Params {
						childEnv.Set(p, args[i])
					}
					rest := Value(&NilVal{})
					for i := len(args) - 1; i >= len(clause.Params); i-- {
						rest = &PairVal{Car: args[i], Cdr: rest}
					}
					childEnv.Set(clause.RestParam, rest)
					for _, bodyExpr := range clause.Body[:len(clause.Body)-1] {
						_, err := eval(bodyExpr, childEnv)
						if err != nil {
							return nil, err
						}
					}
					return &tailCall{expr: clause.Body[len(clause.Body)-1], env: childEnv}, nil
				}
			} else {
				if len(args) == len(clause.Params) {
					childEnv := NewEnv(fn.Env)
					for i, p := range clause.Params {
						childEnv.Set(p, args[i])
					}
					for _, bodyExpr := range clause.Body[:len(clause.Body)-1] {
						_, err := eval(bodyExpr, childEnv)
						if err != nil {
							return nil, err
						}
					}
					return &tailCall{expr: clause.Body[len(clause.Body)-1], env: childEnv}, nil
				}
			}
		}
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case-lambda: no matching clause for %d arguments", callExpr.Line, callExpr.Col, len(args))}
	case *ApplyVal:
		// (apply proc arg1 ... argList)
		if len(args) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: apply: expected at least 2 arguments", callExpr.Line, callExpr.Col)}
		}
		proc := args[0]
		lastArg := args[len(args)-1]
		// Flatten last argument (must be a list) with prefix args
		var flatArgs []Value
		for _, a := range args[1 : len(args)-1] {
			flatArgs = append(flatArgs, a)
		}
		// Convert last arg (scheme list) to slice
		cur := lastArg
		for {
			switch v := cur.(type) {
			case *PairVal:
				flatArgs = append(flatArgs, v.Car)
				cur = v.Cdr
				continue
			case *NilVal:
				// done
			default:
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: apply: last argument must be a list", callExpr.Line, callExpr.Col)}
			}
			break
		}
		return applyProc(proc, flatArgs, callExpr)
	case *MapVal:
		// (map proc list1 list2 ...)
		if len(args) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: map: expected at least 2 arguments", callExpr.Line, callExpr.Col)}
		}
		proc := args[0]
		lists := args[1:]
		// Convert each list arg to a slice
		slices := make([][]Value, len(lists))
		listLen := -1
		for i, l := range lists {
			var elems []Value
			cur := l
			for {
				switch v := cur.(type) {
				case *PairVal:
					elems = append(elems, v.Car)
					cur = v.Cdr
					continue
				case *NilVal:
				default:
					return nil, &EvalError{Message: fmt.Sprintf("%d:%d: map: expected list", callExpr.Line, callExpr.Col)}
				}
				break
			}
			if listLen == -1 {
				listLen = len(elems)
			} else if len(elems) != listLen {
				return nil, &EvalError{Message: fmt.Sprintf("%d:%d: map: lists must have equal length", callExpr.Line, callExpr.Col)}
			}
			slices[i] = elems
		}
		if listLen <= 0 {
			return &NilVal{}, nil
		}
		// Apply proc to each set of elements
		results := make([]Value, listLen)
		for j := 0; j < listLen; j++ {
			callArgs := make([]Value, len(slices))
			for i := range slices {
				callArgs[i] = slices[i][j]
			}
			v, err := applyProc(proc, callArgs, callExpr)
			if err != nil {
				return nil, err
			}
			// Resolve any tail calls from applyProc
			for {
				if tc, ok := v.(*tailCall); ok {
					v, err = evalStep(tc.expr, tc.env)
					if err != nil {
						return nil, err
					}
					continue
				}
				break
			}
			results[j] = v
		}
		// Build result list
		result := Value(&NilVal{})
		for i := len(results) - 1; i >= 0; i-- {
			result = &PairVal{Car: results[i], Cdr: result}
		}
		return result, nil
	case *DynamicWindVal:
		if len(args) != 3 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: dynamic-wind: expected 3 arguments, got %d", callExpr.Line, callExpr.Col, len(args))}
		}
		inThunk, bodyThunk, outThunk := args[0], args[1], args[2]

		// Call in-thunk
		_, err := callThunk(inThunk, callExpr)
		if err != nil {
			return nil, err
		}

		// Push wind record
		windIDCounter++
		rec := windRecord{id: windIDCounter, inThunk: inThunk, outThunk: outThunk}
		dynamicWindStack = append(dynamicWindStack, rec)

		// Call body-thunk, ensuring out-thunk runs on any exit
		var bodyResult Value
		var bodyErr error
		var jumped *contJump

		func() {
			defer func() {
				if r := recover(); r != nil {
					if cj, ok := r.(*contJump); ok {
						jumped = cj
					} else {
						panic(r)
					}
				}
				// Pop wind record
				if len(dynamicWindStack) > 0 {
					dynamicWindStack = dynamicWindStack[:len(dynamicWindStack)-1]
				}
				// Run out-thunk (ignore errors from out-thunk)
				callThunk(outThunk, callExpr)
			}()
			bodyResult, bodyErr = callThunk(bodyThunk, callExpr)
		}()

		if jumped != nil {
			panic(jumped)
		}
		if bodyErr != nil {
			return nil, bodyErr
		}
		return bodyResult, nil
	case *CallCCVal:
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: call/cc: expected 1 argument, got %d", callExpr.Line, callExpr.Col, len(args))}
		}
		// If an inject value is set (re-evaluation from continuation), return it directly
		if callccInjectValue != nil {
			val := *callccInjectValue
			callccInjectValue = nil
			return val, nil
		}
		// Capture current continuation (snapshot the frame stack and wind stack)
		frames := make([]contFrame, len(contFrameStack))
		copy(frames, contFrameStack)
		winds := make([]windRecord, len(dynamicWindStack))
		copy(winds, dynamicWindStack)
		k := &ContinuationVal{Frames: frames, WindStack: winds}
		// Call proc(k) with escape continuation recovery
		result, err := callWithContRecover(k, func() (Value, error) {
			return applyProc(args[0], []Value{k}, callExpr)
		})
		return result, err
	case *ContinuationVal:
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: continuation: expected 1 argument, got %d", callExpr.Line, callExpr.Col, len(args))}
		}
		panic(&contJump{cont: fn, value: args[0]})
	default:
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not a procedure", callExpr.List[0].Line, callExpr.List[0].Col)}
	}
}

func evalDefine(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", expr.Line, expr.Col)}
	}
	target := expr.List[1]

	// (define (f params...) body...)
	if target.Kind == ExprList && len(target.List) > 0 {
		name := target.List[0]
		if name.Kind != ExprSymbol {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: expected symbol", name.Line, name.Col)}
		}
		params, rest, pErr := parseDottedParams(target.List[1:], "define")
		if pErr != nil {
			return nil, pErr
		}
		lam := &LambdaVal{Params: params, RestParam: rest, Body: expr.List[2:], Env: env}
		env.Set(name.SVal, lam)
		return &VoidVal{}, nil
	}

	// (define x val)
	if target.Kind != ExprSymbol {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: define: expected symbol", target.Line, target.Col)}
	}
	val, err := eval(expr.List[2], env)
	if err != nil {
		return nil, err
	}
	env.Set(target.SVal, val)
	return &VoidVal{}, nil
}

func evalIf(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) < 3 || len(expr.List) > 4 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: if: bad syntax", expr.Line, expr.Col)}
	}
	cond, err := eval(expr.List[1], env)
	if err != nil {
		return nil, err
	}
	if isTruthy(cond) {
		return &tailCall{expr: expr.List[2], env: env}, nil
	}
	if len(expr.List) == 4 {
		return &tailCall{expr: expr.List[3], env: env}, nil
	}
	return &VoidVal{}, nil
}

func evalQuote(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) != 2 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: quote: expected 1 argument", expr.Line, expr.Col)}
	}
	return exprToValue(expr.List[1]), nil
}

func exprToValue(e *Expr) Value {
	switch e.Kind {
	case ExprInt:
		return &IntVal{Val: e.IVal}
	case ExprBool:
		return &BoolVal{Val: e.BVal}
	case ExprString:
		return &StringVal{Val: e.SVal, Immutable: true}
	case ExprSymbol:
		return &SymbolVal{Name: e.SVal}
	case ExprList:
		if len(e.List) == 0 {
			return &NilVal{}
		}
		// Build proper list from elements
		result := Value(&NilVal{})
		for i := len(e.List) - 1; i >= 0; i-- {
			result = &PairVal{Car: exprToValue(e.List[i]), Cdr: result}
		}
		return result
	}
	return &VoidVal{}
}

func evalLet(expr *Expr, env *Env) (Value, error) {
	// (let ((var val) ...) body...)
	// or named let: (let name ((var val) ...) body...)
	if len(expr.List) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", expr.Line, expr.Col)}
	}

	nameIdx := 1
	var loopName string

	// named let?
	if expr.List[1].Kind == ExprSymbol {
		loopName = expr.List[1].SVal
		nameIdx = 2
		if len(expr.List) < 4 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", expr.Line, expr.Col)}
		}
	}

	bindingsExpr := expr.List[nameIdx]
	if bindingsExpr.Kind != ExprList {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: expected bindings list", bindingsExpr.Line, bindingsExpr.Col)}
	}

	params := make([]string, len(bindingsExpr.List))
	vals := make([]Value, len(bindingsExpr.List))
	for i, b := range bindingsExpr.List {
		if b.Kind != ExprList || len(b.List) != 2 || b.List[0].Kind != ExprSymbol {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let: bad binding", b.Line, b.Col)}
		}
		params[i] = b.List[0].SVal
		var err error
		vals[i], err = eval(b.List[1], env)
		if err != nil {
			return nil, err
		}
	}

	body := expr.List[nameIdx+1:]

	if loopName != "" {
		// named let: create a lambda and bind it, then call it
		lam := &LambdaVal{Params: params, Body: body, Env: env}
		childEnv := NewEnv(env)
		childEnv.Set(loopName, lam)
		lam.Env = childEnv
		for i, p := range params {
			childEnv.Set(p, vals[i])
		}
		contFrameStack = append(contFrameStack, contFrame{Kind: frameBody, Exprs: body, Env: childEnv})
		defer func() { contFrameStack = contFrameStack[:len(contFrameStack)-1] }()
		for i, bodyExpr := range body[:len(body)-1] {
			contFrameStack[len(contFrameStack)-1].Idx = i
			_, err := eval(bodyExpr, childEnv)
			if err != nil {
				return nil, err
			}
		}
		contFrameStack[len(contFrameStack)-1].Idx = len(body) - 1
		return &tailCall{expr: body[len(body)-1], env: childEnv}, nil
	}

	childEnv := NewEnv(env)
	for i, p := range params {
		childEnv.Set(p, vals[i])
	}
	contFrameStack = append(contFrameStack, contFrame{Kind: frameBody, Exprs: body, Env: childEnv})
	defer func() { contFrameStack = contFrameStack[:len(contFrameStack)-1] }()
	for i, bodyExpr := range body[:len(body)-1] {
		contFrameStack[len(contFrameStack)-1].Idx = i
		_, err := eval(bodyExpr, childEnv)
		if err != nil {
			return nil, err
		}
	}
	contFrameStack[len(contFrameStack)-1].Idx = len(body) - 1
	return &tailCall{expr: body[len(body)-1], env: childEnv}, nil
}

func evalLetStar(expr *Expr, env *Env) (Value, error) {
	// (let* ((var val) ...) body...)
	if len(expr.List) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let*: bad syntax", expr.Line, expr.Col)}
	}
	bindingsExpr := expr.List[1]
	if bindingsExpr.Kind != ExprList {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let*: expected bindings list", bindingsExpr.Line, bindingsExpr.Col)}
	}
	childEnv := NewEnv(env)
	for _, b := range bindingsExpr.List {
		if b.Kind != ExprList || len(b.List) != 2 || b.List[0].Kind != ExprSymbol {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: let*: bad binding", b.Line, b.Col)}
		}
		val, err := eval(b.List[1], childEnv)
		if err != nil {
			return nil, err
		}
		childEnv.Set(b.List[0].SVal, val)
	}
	body := expr.List[2:]
	contFrameStack = append(contFrameStack, contFrame{Kind: frameBody, Exprs: body, Env: childEnv})
	defer func() { contFrameStack = contFrameStack[:len(contFrameStack)-1] }()
	for i, bodyExpr := range body[:len(body)-1] {
		contFrameStack[len(contFrameStack)-1].Idx = i
		_, err := eval(bodyExpr, childEnv)
		if err != nil {
			return nil, err
		}
	}
	contFrameStack[len(contFrameStack)-1].Idx = len(body) - 1
	return &tailCall{expr: body[len(body)-1], env: childEnv}, nil
}

func evalBegin(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) < 2 {
		return &VoidVal{}, nil
	}
	body := expr.List[1:]
	contFrameStack = append(contFrameStack, contFrame{Kind: frameBody, Exprs: body, Env: env})
	defer func() { contFrameStack = contFrameStack[:len(contFrameStack)-1] }()
	for i, e := range body[:len(body)-1] {
		contFrameStack[len(contFrameStack)-1].Idx = i
		_, err := eval(e, env)
		if err != nil {
			return nil, err
		}
	}
	contFrameStack[len(contFrameStack)-1].Idx = len(body) - 1
	return &tailCall{expr: body[len(body)-1], env: env}, nil
}

func evalSet(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) != 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set!: bad syntax", expr.Line, expr.Col)}
	}
	target := expr.List[1]
	if target.Kind != ExprSymbol {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set!: expected symbol", target.Line, target.Col)}
	}
	val, err := eval(expr.List[2], env)
	if err != nil {
		return nil, err
	}
	if !env.SetMut(target.SVal, val) {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: set!: unbound variable: %s", target.Line, target.Col, target.SVal)}
	}
	return &VoidVal{}, nil
}

func evalCond(expr *Expr, env *Env) (Value, error) {
	for _, clause := range expr.List[1:] {
		if clause.Kind != ExprList || len(clause.List) < 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: cond: bad clause", clause.Line, clause.Col)}
		}
		// else clause
		if clause.List[0].Kind == ExprSymbol && clause.List[0].SVal == "else" {
			body := clause.List[1:]
			for _, e := range body[:len(body)-1] {
				_, err := eval(e, env)
				if err != nil {
					return nil, err
				}
			}
			return &tailCall{expr: body[len(body)-1], env: env}, nil
		}
		test, err := eval(clause.List[0], env)
		if err != nil {
			return nil, err
		}
		if isTruthy(test) {
			// Single-expression clause: return the test value
			if len(clause.List) == 1 {
				return test, nil
			}
			body := clause.List[1:]
			for _, e := range body[:len(body)-1] {
				_, err = eval(e, env)
				if err != nil {
					return nil, err
				}
			}
			return &tailCall{expr: body[len(body)-1], env: env}, nil
		}
	}
	return &VoidVal{}, nil
}

func evalLambda(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: bad syntax", expr.Line, expr.Col)}
	}
	paramExpr := expr.List[1]
	// Single symbol means all-variadic: (lambda args body)
	if paramExpr.Kind == ExprSymbol {
		return &LambdaVal{RestParam: paramExpr.SVal, Body: expr.List[2:], Env: env}, nil
	}
	if paramExpr.Kind != ExprList {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: lambda: expected parameter list", paramExpr.Line, paramExpr.Col)}
	}
	params, rest, err := parseDottedParams(paramExpr.List, "lambda")
	if err != nil {
		return nil, err
	}
	return &LambdaVal{Params: params, RestParam: rest, Body: expr.List[2:], Env: env}, nil
}

func evalCaseLambda(expr *Expr, env *Env) (Value, error) {
	// (case-lambda (params body...) ...)
	if len(expr.List) < 2 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case-lambda: bad syntax", expr.Line, expr.Col)}
	}
	var clauses []CaseLambdaClause
	for _, clauseExpr := range expr.List[1:] {
		if clauseExpr.Kind != ExprList || len(clauseExpr.List) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case-lambda: bad clause", clauseExpr.Line, clauseExpr.Col)}
		}
		paramExpr := clauseExpr.List[0]
		body := clauseExpr.List[1:]
		var params []string
		var rest string
		if paramExpr.Kind == ExprSymbol {
			// (args body...) — all variadic
			rest = paramExpr.SVal
		} else if paramExpr.Kind == ExprList {
			var err error
			params, rest, err = parseDottedParams(paramExpr.List, "case-lambda")
			if err != nil {
				return nil, err
			}
		} else if paramExpr.Kind != ExprList {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case-lambda: bad parameter list", paramExpr.Line, paramExpr.Col)}
		}
		clauses = append(clauses, CaseLambdaClause{Params: params, RestParam: rest, Body: body})
	}
	return &CaseLambdaVal{Clauses: clauses, Env: env}, nil
}

// parseDottedParams parses a parameter list that may contain dot notation.
// e.g. (x y . rest) -> params=["x","y"], rest="rest"
// e.g. (x y) -> params=["x","y"], rest=""
// e.g. (. rest) -> params=[], rest="rest"
func parseDottedParams(plist []*Expr, context string) ([]string, string, error) {
	dotIdx := -1
	for i, p := range plist {
		if p.Kind == ExprSymbol && p.SVal == "." {
			dotIdx = i
			break
		}
	}
	if dotIdx == -1 {
		// No dot, all fixed params
		params := make([]string, len(plist))
		for i, p := range plist {
			if p.Kind != ExprSymbol {
				return nil, "", &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected parameter name", p.Line, p.Col, context)}
			}
			params[i] = p.SVal
		}
		return params, "", nil
	}
	// dot found: must have exactly one symbol after it
	if dotIdx+2 != len(plist) || plist[dotIdx+1].Kind != ExprSymbol {
		return nil, "", &EvalError{Message: fmt.Sprintf("%d:%d: %s: bad dot syntax in parameters", plist[dotIdx].Line, plist[dotIdx].Col, context)}
	}
	params := make([]string, dotIdx)
	for i := 0; i < dotIdx; i++ {
		if plist[i].Kind != ExprSymbol {
			return nil, "", &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected parameter name", plist[i].Line, plist[i].Col, context)}
		}
		params[i] = plist[i].SVal
	}
	return params, plist[dotIdx+1].SVal, nil
}

// L03 builtins

func builtinCons(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("cons: expected 2 arguments, got %d", len(args))
	}
	return &PairVal{Car: args[0], Cdr: args[1]}, nil
}

func builtinCar(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("car: expected 1 argument, got %d", len(args))
	}
	p, ok := args[0].(*PairVal)
	if !ok {
		return nil, fmt.Errorf("car: expected pair, got %s", args[0].String())
	}
	return p.Car, nil
}

func builtinCdr(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("cdr: expected 1 argument, got %d", len(args))
	}
	p, ok := args[0].(*PairVal)
	if !ok {
		return nil, fmt.Errorf("cdr: expected pair, got %s", args[0].String())
	}
	return p.Cdr, nil
}

func builtinNullQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("null?: expected 1 argument, got %d", len(args))
	}
	_, isNil := args[0].(*NilVal)
	return &BoolVal{Val: isNil}, nil
}

func builtinList(args []Value) (Value, error) {
	result := Value(&NilVal{})
	for i := len(args) - 1; i >= 0; i-- {
		result = &PairVal{Car: args[i], Cdr: result}
	}
	return result, nil
}

func builtinLength(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("length: expected 1 argument, got %d", len(args))
	}
	count := int64(0)
	cur := args[0]
	for {
		switch v := cur.(type) {
		case *NilVal:
			return &IntVal{Val: count}, nil
		case *PairVal:
			count++
			cur = v.Cdr
		default:
			return nil, fmt.Errorf("length: expected list")
		}
	}
}

func builtinAppend(args []Value) (Value, error) {
	if len(args) == 0 {
		return &NilVal{}, nil
	}
	if len(args) == 1 {
		return args[0], nil
	}
	// Collect all elements from all lists except the last, then attach the last
	var elems []Value
	for i := 0; i < len(args)-1; i++ {
		cur := args[i]
		for {
			switch v := cur.(type) {
			case *NilVal:
				goto nextList
			case *PairVal:
				elems = append(elems, v.Car)
				cur = v.Cdr
			default:
				return nil, fmt.Errorf("append: expected list")
			}
		}
	nextList:
	}
	result := args[len(args)-1]
	for i := len(elems) - 1; i >= 0; i-- {
		result = &PairVal{Car: elems[i], Cdr: result}
	}
	return result, nil
}

func builtinNumberQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("number?: expected 1 argument")
	}
	return &BoolVal{Val: isNumeric(args[0])}, nil
}

func builtinStringQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("string?: expected 1 argument")
	}
	_, ok := args[0].(*StringVal)
	return &BoolVal{Val: ok}, nil
}

func builtinBooleanQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("boolean?: expected 1 argument")
	}
	_, ok := args[0].(*BoolVal)
	return &BoolVal{Val: ok}, nil
}

func builtinPairQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("pair?: expected 1 argument")
	}
	_, ok := args[0].(*PairVal)
	return &BoolVal{Val: ok}, nil
}

func builtinSymbolQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("symbol?: expected 1 argument")
	}
	_, ok := args[0].(*SymbolVal)
	return &BoolVal{Val: ok}, nil
}

// L05 builtins

func builtinStringAppend(args []Value) (Value, error) {
	var buf strings.Builder
	for _, a := range args {
		s, ok := a.(*StringVal)
		if !ok {
			return nil, fmt.Errorf("string-append: expected string, got %s", a.String())
		}
		buf.WriteString(s.Val)
	}
	return &StringVal{Val: buf.String()}, nil
}

func builtinStringLength(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("string-length: expected 1 argument, got %d", len(args))
	}
	s, ok := args[0].(*StringVal)
	if !ok {
		return nil, fmt.Errorf("string-length: expected string, got %s", args[0].String())
	}
	return &IntVal{Val: int64(len([]rune(s.Val)))}, nil
}

func builtinSubstring(args []Value) (Value, error) {
	if len(args) != 3 {
		return nil, fmt.Errorf("substring: expected 3 arguments, got %d", len(args))
	}
	s, ok := args[0].(*StringVal)
	if !ok {
		return nil, fmt.Errorf("substring: expected string, got %s", args[0].String())
	}
	start, ok := args[1].(*IntVal)
	if !ok {
		return nil, fmt.Errorf("substring: expected number, got %s", args[1].String())
	}
	end, ok := args[2].(*IntVal)
	if !ok {
		return nil, fmt.Errorf("substring: expected number, got %s", args[2].String())
	}
	runes := []rune(s.Val)
	if start.Val < 0 || end.Val < start.Val || int(end.Val) > len(runes) {
		return nil, fmt.Errorf("substring: index out of range")
	}
	return &StringVal{Val: string(runes[start.Val:end.Val])}, nil
}

func builtinStringToNumber(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("string->number: expected 1 argument, got %d", len(args))
	}
	s, ok := args[0].(*StringVal)
	if !ok {
		return nil, fmt.Errorf("string->number: expected string, got %s", args[0].String())
	}
	n, err := strconv.ParseInt(s.Val, 10, 64)
	if err != nil {
		return &BoolVal{Val: false}, nil
	}
	return &IntVal{Val: n}, nil
}

func builtinNumberToString(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("number->string: expected 1 argument, got %d", len(args))
	}
	n, ok := args[0].(*IntVal)
	if !ok {
		return nil, fmt.Errorf("number->string: expected number, got %s", args[0].String())
	}
	return &StringVal{Val: strconv.FormatInt(n.Val, 10)}, nil
}

func builtinSymbolToString(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("symbol->string: expected 1 argument, got %d", len(args))
	}
	s, ok := args[0].(*SymbolVal)
	if !ok {
		return nil, fmt.Errorf("symbol->string: expected symbol, got %s", args[0].String())
	}
	return &StringVal{Val: s.Name}, nil
}

func builtinStringToSymbol(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("string->symbol: expected 1 argument, got %d", len(args))
	}
	s, ok := args[0].(*StringVal)
	if !ok {
		return nil, fmt.Errorf("string->symbol: expected string, got %s", args[0].String())
	}
	return &SymbolVal{Name: s.Val}, nil
}

func builtinStringRef(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("string-ref: expected 2 arguments, got %d", len(args))
	}
	s, ok := args[0].(*StringVal)
	if !ok {
		return nil, fmt.Errorf("string-ref: expected string, got %s", args[0].String())
	}
	idx, ok := args[1].(*IntVal)
	if !ok {
		return nil, fmt.Errorf("string-ref: expected number, got %s", args[1].String())
	}
	runes := []rune(s.Val)
	if idx.Val < 0 || int(idx.Val) >= len(runes) {
		return nil, fmt.Errorf("string-ref: index out of range")
	}
	return &CharVal{Val: runes[idx.Val]}, nil
}

func builtinCharQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("char?: expected 1 argument, got %d", len(args))
	}
	_, ok := args[0].(*CharVal)
	return &BoolVal{Val: ok}, nil
}

// L06 builtins

func builtinStringCopy(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("string-copy: expected 1 argument, got %d", len(args))
	}
	s, ok := args[0].(*StringVal)
	if !ok {
		return nil, fmt.Errorf("string-copy: expected string, got %s", args[0].String())
	}
	return &StringVal{Val: s.Val}, nil
}

func builtinStringSet(args []Value) (Value, error) {
	if len(args) != 3 {
		return nil, fmt.Errorf("string-set!: expected 3 arguments, got %d", len(args))
	}
	s, ok := args[0].(*StringVal)
	if !ok {
		return nil, fmt.Errorf("string-set!: expected string, got %s", args[0].String())
	}
	if s.Immutable {
		return nil, fmt.Errorf("string-set!: strings are immutable")
	}
	idx, ok := args[1].(*IntVal)
	if !ok {
		return nil, fmt.Errorf("string-set!: expected integer index, got %s", args[1].String())
	}
	c, ok := args[2].(*CharVal)
	if !ok {
		return nil, fmt.Errorf("string-set!: expected character, got %s", args[2].String())
	}
	runes := []rune(s.Val)
	if idx.Val < 0 || idx.Val >= int64(len(runes)) {
		return nil, fmt.Errorf("string-set!: index %d out of range for string of length %d", idx.Val, len(runes))
	}
	runes[idx.Val] = c.Val
	s.Val = string(runes)
	return &VoidVal{}, nil
}

// L09 builtins

func builtinAbs(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("abs: expected 1 argument, got %d", len(args))
	}
	n, ok := args[0].(*IntVal)
	if !ok {
		return nil, fmt.Errorf("abs: expected number, got %s", args[0].String())
	}
	if n.Val < 0 {
		return &IntVal{Val: -n.Val}, nil
	}
	return &IntVal{Val: n.Val}, nil
}

func builtinModulo(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("modulo: expected 2 arguments, got %d", len(args))
	}
	nums, err := requireInts("modulo", args)
	if err != nil {
		return nil, err
	}
	if nums[1] == 0 {
		return nil, fmt.Errorf("modulo: division by zero")
	}
	// Go's % gives remainder (sign of dividend). Modulo takes sign of divisor.
	r := nums[0] % nums[1]
	if r != 0 && (r > 0) != (nums[1] > 0) {
		r += nums[1]
	}
	return &IntVal{Val: r}, nil
}

func builtinRemainder(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("remainder: expected 2 arguments, got %d", len(args))
	}
	nums, err := requireInts("remainder", args)
	if err != nil {
		return nil, err
	}
	if nums[1] == 0 {
		return nil, fmt.Errorf("remainder: division by zero")
	}
	return &IntVal{Val: nums[0] % nums[1]}, nil
}

func builtinQuotient(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("quotient: expected 2 arguments, got %d", len(args))
	}
	nums, err := requireInts("quotient", args)
	if err != nil {
		return nil, err
	}
	if nums[1] == 0 {
		return nil, fmt.Errorf("quotient: division by zero")
	}
	// Truncated toward zero (Go default behavior)
	return &IntVal{Val: nums[0] / nums[1]}, nil
}

func builtinMin(args []Value) (Value, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("min: expected at least 1 argument")
	}
	nums, err := requireInts("min", args)
	if err != nil {
		return nil, err
	}
	result := nums[0]
	for _, n := range nums[1:] {
		if n < result {
			result = n
		}
	}
	return &IntVal{Val: result}, nil
}

func builtinMax(args []Value) (Value, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("max: expected at least 1 argument")
	}
	nums, err := requireInts("max", args)
	if err != nil {
		return nil, err
	}
	result := nums[0]
	for _, n := range nums[1:] {
		if n > result {
			result = n
		}
	}
	return &IntVal{Val: result}, nil
}

func builtinExpt(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("expt: expected 2 arguments, got %d", len(args))
	}
	nums, err := requireInts("expt", args)
	if err != nil {
		return nil, err
	}
	base, exp := nums[0], nums[1]
	if exp < 0 {
		return &IntVal{Val: 0}, nil
	}
	result := int64(math.Pow(float64(base), float64(exp)))
	return &IntVal{Val: result}, nil
}

func builtinZeroQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("zero?: expected 1 argument, got %d", len(args))
	}
	n, ok := args[0].(*IntVal)
	if !ok {
		return nil, fmt.Errorf("zero?: expected number, got %s", args[0].String())
	}
	return &BoolVal{Val: n.Val == 0}, nil
}

func builtinPositiveQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("positive?: expected 1 argument, got %d", len(args))
	}
	n, ok := args[0].(*IntVal)
	if !ok {
		return nil, fmt.Errorf("positive?: expected number, got %s", args[0].String())
	}
	return &BoolVal{Val: n.Val > 0}, nil
}

func builtinNegativeQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("negative?: expected 1 argument, got %d", len(args))
	}
	n, ok := args[0].(*IntVal)
	if !ok {
		return nil, fmt.Errorf("negative?: expected number, got %s", args[0].String())
	}
	return &BoolVal{Val: n.Val < 0}, nil
}

func builtinOddQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("odd?: expected 1 argument, got %d", len(args))
	}
	n, ok := args[0].(*IntVal)
	if !ok {
		return nil, fmt.Errorf("odd?: expected number, got %s", args[0].String())
	}
	return &BoolVal{Val: n.Val%2 != 0}, nil
}

func builtinEvenQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("even?: expected 1 argument, got %d", len(args))
	}
	n, ok := args[0].(*IntVal)
	if !ok {
		return nil, fmt.Errorf("even?: expected number, got %s", args[0].String())
	}
	return &BoolVal{Val: n.Val%2 == 0}, nil
}

func builtinListRef(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("list-ref: expected 2 arguments, got %d", len(args))
	}
	idx, ok := args[1].(*IntVal)
	if !ok {
		return nil, fmt.Errorf("list-ref: expected number, got %s", args[1].String())
	}
	cur := args[0]
	for i := int64(0); i < idx.Val; i++ {
		p, ok := cur.(*PairVal)
		if !ok {
			return nil, fmt.Errorf("list-ref: index out of range")
		}
		cur = p.Cdr
	}
	p, ok := cur.(*PairVal)
	if !ok {
		return nil, fmt.Errorf("list-ref: index out of range")
	}
	return p.Car, nil
}

func builtinListTail(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("list-tail: expected 2 arguments, got %d", len(args))
	}
	idx, ok := args[1].(*IntVal)
	if !ok {
		return nil, fmt.Errorf("list-tail: expected number, got %s", args[1].String())
	}
	cur := args[0]
	for i := int64(0); i < idx.Val; i++ {
		p, ok := cur.(*PairVal)
		if !ok {
			return nil, fmt.Errorf("list-tail: index out of range")
		}
		cur = p.Cdr
	}
	return cur, nil
}

func builtinListQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("list?: expected 1 argument, got %d", len(args))
	}
	// Tortoise-and-hare cycle detection
	slow := args[0]
	fast := args[0]
	for {
		// Advance fast by 2
		fp, ok := fast.(*PairVal)
		if !ok {
			_, isNil := fast.(*NilVal)
			return &BoolVal{Val: isNil}, nil
		}
		fast = fp.Cdr
		fp2, ok := fast.(*PairVal)
		if !ok {
			_, isNil := fast.(*NilVal)
			return &BoolVal{Val: isNil}, nil
		}
		fast = fp2.Cdr
		// Advance slow by 1
		slow = slow.(*PairVal).Cdr
		// If they meet, it's circular
		if slow == fast {
			return &BoolVal{Val: false}, nil
		}
	}
}

type equalPair struct{ a, b Value }

func schemeEqual(a, b Value) bool {
	seen := make(map[equalPair]bool)
	return schemeEqualRec(a, b, seen)
}

func schemeEqualRec(a, b Value, seen map[equalPair]bool) bool {
	switch av := a.(type) {
	case *IntVal:
		if bv, ok := b.(*IntVal); ok {
			return av.Val == bv.Val
		}
	case *BoolVal:
		if bv, ok := b.(*BoolVal); ok {
			return av.Val == bv.Val
		}
	case *StringVal:
		if bv, ok := b.(*StringVal); ok {
			return av.Val == bv.Val
		}
	case *SymbolVal:
		if bv, ok := b.(*SymbolVal); ok {
			return av.Name == bv.Name
		}
	case *CharVal:
		if bv, ok := b.(*CharVal); ok {
			return av.Val == bv.Val
		}
	case *NilVal:
		_, ok := b.(*NilVal)
		return ok
	case *PairVal:
		if bv, ok := b.(*PairVal); ok {
			key := equalPair{a, b}
			if seen[key] {
				return true // already comparing these, assume equal
			}
			seen[key] = true
			return schemeEqualRec(av.Car, bv.Car, seen) && schemeEqualRec(av.Cdr, bv.Cdr, seen)
		}
	case *VectorVal:
		if bv, ok := b.(*VectorVal); ok {
			if len(av.Elems) != len(bv.Elems) {
				return false
			}
			for i := range av.Elems {
				if !schemeEqualRec(av.Elems[i], bv.Elems[i], seen) {
					return false
				}
			}
			return true
		}
	case *RationalVal:
		if bv, ok := b.(*RationalVal); ok {
			return av.Num == bv.Num && av.Denom == bv.Denom
		}
	case *FloatVal:
		if bv, ok := b.(*FloatVal); ok {
			return av.Val == bv.Val
		}
	}
	return a == b
}

func builtinEqualQ(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("equal?: expected 2 arguments, got %d", len(args))
	}
	return &BoolVal{Val: schemeEqual(args[0], args[1])}, nil
}

func builtinEqQ(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("eq?: expected 2 arguments, got %d", len(args))
	}
	a, b := args[0], args[1]
	switch av := a.(type) {
	case *IntVal:
		if bv, ok := b.(*IntVal); ok {
			return &BoolVal{Val: av.Val == bv.Val}, nil
		}
	case *BoolVal:
		if bv, ok := b.(*BoolVal); ok {
			return &BoolVal{Val: av.Val == bv.Val}, nil
		}
	case *SymbolVal:
		if bv, ok := b.(*SymbolVal); ok {
			return &BoolVal{Val: av.Name == bv.Name}, nil
		}
	case *CharVal:
		if bv, ok := b.(*CharVal); ok {
			return &BoolVal{Val: av.Val == bv.Val}, nil
		}
	case *NilVal:
		_, ok := b.(*NilVal)
		return &BoolVal{Val: ok}, nil
	case *VoidVal:
		_, ok := b.(*VoidVal)
		return &BoolVal{Val: ok}, nil
	}
	return &BoolVal{Val: a == b}, nil
}

func builtinAssoc(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("assoc: expected 2 arguments, got %d", len(args))
	}
	key := args[0]
	cur := args[1]
	for {
		switch v := cur.(type) {
		case *NilVal:
			return &BoolVal{Val: false}, nil
		case *PairVal:
			pair, ok := v.Car.(*PairVal)
			if !ok {
				return nil, fmt.Errorf("assoc: expected list of pairs")
			}
			if schemeEqual(pair.Car, key) {
				return pair, nil
			}
			cur = v.Cdr
		default:
			return nil, fmt.Errorf("assoc: expected list")
		}
	}
}

// schemeEq tests eq? identity (pointer equality for compound, value equality for atoms).
func schemeEq(a, b Value) bool {
	switch av := a.(type) {
	case *IntVal:
		if bv, ok := b.(*IntVal); ok {
			return av.Val == bv.Val
		}
	case *BoolVal:
		if bv, ok := b.(*BoolVal); ok {
			return av.Val == bv.Val
		}
	case *SymbolVal:
		if bv, ok := b.(*SymbolVal); ok {
			return av.Name == bv.Name
		}
	case *CharVal:
		if bv, ok := b.(*CharVal); ok {
			return av.Val == bv.Val
		}
	case *NilVal:
		_, ok := b.(*NilVal)
		return ok
	case *VoidVal:
		_, ok := b.(*VoidVal)
		return ok
	}
	return a == b
}

func builtinAssq(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("assq: expected 2 arguments, got %d", len(args))
	}
	key := args[0]
	cur := args[1]
	for {
		switch v := cur.(type) {
		case *NilVal:
			return &BoolVal{Val: false}, nil
		case *PairVal:
			pair, ok := v.Car.(*PairVal)
			if !ok {
				return nil, fmt.Errorf("assq: expected list of pairs")
			}
			if schemeEq(pair.Car, key) {
				return pair, nil
			}
			cur = v.Cdr
		default:
			return nil, fmt.Errorf("assq: expected list")
		}
	}
}

func builtinAssv(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("assv: expected 2 arguments, got %d", len(args))
	}
	key := args[0]
	cur := args[1]
	for {
		switch v := cur.(type) {
		case *NilVal:
			return &BoolVal{Val: false}, nil
		case *PairVal:
			pair, ok := v.Car.(*PairVal)
			if !ok {
				return nil, fmt.Errorf("assv: expected list of pairs")
			}
			if schemeEqv(pair.Car, key) {
				return pair, nil
			}
			cur = v.Cdr
		default:
			return nil, fmt.Errorf("assv: expected list")
		}
	}
}

func builtinMember(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("member: expected 2 arguments, got %d", len(args))
	}
	key := args[0]
	cur := args[1]
	for {
		switch v := cur.(type) {
		case *NilVal:
			return &BoolVal{Val: false}, nil
		case *PairVal:
			if schemeEqual(v.Car, key) {
				return v, nil
			}
			cur = v.Cdr
		default:
			return nil, fmt.Errorf("member: expected list")
		}
	}
}

func builtinMemq(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("memq: expected 2 arguments, got %d", len(args))
	}
	key := args[0]
	cur := args[1]
	for {
		switch v := cur.(type) {
		case *NilVal:
			return &BoolVal{Val: false}, nil
		case *PairVal:
			if schemeEq(v.Car, key) {
				return v, nil
			}
			cur = v.Cdr
		default:
			return nil, fmt.Errorf("memq: expected list")
		}
	}
}

func builtinMemv(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("memv: expected 2 arguments, got %d", len(args))
	}
	key := args[0]
	cur := args[1]
	for {
		switch v := cur.(type) {
		case *NilVal:
			return &BoolVal{Val: false}, nil
		case *PairVal:
			if schemeEqv(v.Car, key) {
				return v, nil
			}
			cur = v.Cdr
		default:
			return nil, fmt.Errorf("memv: expected list")
		}
	}
}

// Char builtins

func builtinCharAlphaQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("char-alphabetic?: expected 1 argument, got %d", len(args))
	}
	c, ok := args[0].(*CharVal)
	if !ok {
		return nil, fmt.Errorf("char-alphabetic?: expected char, got %s", args[0].String())
	}
	return &BoolVal{Val: unicode.IsLetter(c.Val)}, nil
}

func builtinCharNumericQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("char-numeric?: expected 1 argument, got %d", len(args))
	}
	c, ok := args[0].(*CharVal)
	if !ok {
		return nil, fmt.Errorf("char-numeric?: expected char, got %s", args[0].String())
	}
	return &BoolVal{Val: unicode.IsDigit(c.Val)}, nil
}

func builtinCharUpcase(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("char-upcase: expected 1 argument, got %d", len(args))
	}
	c, ok := args[0].(*CharVal)
	if !ok {
		return nil, fmt.Errorf("char-upcase: expected char, got %s", args[0].String())
	}
	return &CharVal{Val: unicode.ToUpper(c.Val)}, nil
}

func builtinCharDowncase(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("char-downcase: expected 1 argument, got %d", len(args))
	}
	c, ok := args[0].(*CharVal)
	if !ok {
		return nil, fmt.Errorf("char-downcase: expected char, got %s", args[0].String())
	}
	return &CharVal{Val: unicode.ToLower(c.Val)}, nil
}

func builtinCharEqQ(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("char=?: expected 2 arguments, got %d", len(args))
	}
	a, ok := args[0].(*CharVal)
	if !ok {
		return nil, fmt.Errorf("char=?: expected char, got %s", args[0].String())
	}
	b, ok := args[1].(*CharVal)
	if !ok {
		return nil, fmt.Errorf("char=?: expected char, got %s", args[1].String())
	}
	return &BoolVal{Val: a.Val == b.Val}, nil
}

func builtinCharLtQ(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("char<?: expected 2 arguments, got %d", len(args))
	}
	a, ok := args[0].(*CharVal)
	if !ok {
		return nil, fmt.Errorf("char<?: expected char, got %s", args[0].String())
	}
	b, ok := args[1].(*CharVal)
	if !ok {
		return nil, fmt.Errorf("char<?: expected char, got %s", args[1].String())
	}
	return &BoolVal{Val: a.Val < b.Val}, nil
}

// String comparison builtins

func builtinStringEqQ(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("string=?: expected 2 arguments, got %d", len(args))
	}
	a, ok := args[0].(*StringVal)
	if !ok {
		return nil, fmt.Errorf("string=?: expected string, got %s", args[0].String())
	}
	b, ok := args[1].(*StringVal)
	if !ok {
		return nil, fmt.Errorf("string=?: expected string, got %s", args[1].String())
	}
	return &BoolVal{Val: a.Val == b.Val}, nil
}

func builtinStringLtQ(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("string<?: expected 2 arguments, got %d", len(args))
	}
	a, ok := args[0].(*StringVal)
	if !ok {
		return nil, fmt.Errorf("string<?: expected string, got %s", args[0].String())
	}
	b, ok := args[1].(*StringVal)
	if !ok {
		return nil, fmt.Errorf("string<?: expected string, got %s", args[1].String())
	}
	return &BoolVal{Val: a.Val < b.Val}, nil
}

func builtinStringCiEqQ(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("string-ci=?: expected 2 arguments, got %d", len(args))
	}
	a, ok := args[0].(*StringVal)
	if !ok {
		return nil, fmt.Errorf("string-ci=?: expected string, got %s", args[0].String())
	}
	b, ok := args[1].(*StringVal)
	if !ok {
		return nil, fmt.Errorf("string-ci=?: expected string, got %s", args[1].String())
	}
	return &BoolVal{Val: strings.EqualFold(a.Val, b.Val)}, nil
}

func builtinStringUpcase(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("string-upcase: expected 1 argument, got %d", len(args))
	}
	s, ok := args[0].(*StringVal)
	if !ok {
		return nil, fmt.Errorf("string-upcase: expected string, got %s", args[0].String())
	}
	return &StringVal{Val: strings.ToUpper(s.Val)}, nil
}

func builtinStringDowncase(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("string-downcase: expected 1 argument, got %d", len(args))
	}
	s, ok := args[0].(*StringVal)
	if !ok {
		return nil, fmt.Errorf("string-downcase: expected string, got %s", args[0].String())
	}
	return &StringVal{Val: strings.ToLower(s.Val)}, nil
}

func builtinGe(args []Value) (Value, error) {
	a, b, err := numericCompare(">=", args)
	if err != nil {
		return nil, err
	}
	return &BoolVal{Val: a >= b}, nil
}

// L11 builtins

func builtinExactQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("exact?: expected 1 argument, got %d", len(args))
	}
	return &BoolVal{Val: isExact(args[0])}, nil
}

func builtinInexactQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("inexact?: expected 1 argument, got %d", len(args))
	}
	_, ok := args[0].(*FloatVal)
	return &BoolVal{Val: ok}, nil
}

func builtinExactToInexact(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("exact->inexact: expected 1 argument, got %d", len(args))
	}
	f, ok := toFloat64(args[0])
	if !ok {
		return nil, fmt.Errorf("exact->inexact: expected number, got %s", args[0].String())
	}
	return &FloatVal{Val: f}, nil
}

func builtinInexactToExact(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("inexact->exact: expected 1 argument, got %d", len(args))
	}
	switch n := args[0].(type) {
	case *IntVal:
		return n, nil
	case *RationalVal:
		return n, nil
	case *FloatVal:
		// Convert float to rational using continued fraction approximation
		// For simple cases like 0.5 -> 1/2
		num, denom := float64ToRational(n.Val)
		return makeRational(num, denom), nil
	default:
		return nil, fmt.Errorf("inexact->exact: expected number, got %s", args[0].String())
	}
}

// float64ToRational converts a float64 to a rational approximation.
func float64ToRational(f float64) (int64, int64) {
	if f == 0 {
		return 0, 1
	}
	sign := int64(1)
	if f < 0 {
		sign = -1
		f = -f
	}
	// Use the standard approach: multiply by power of 2 to get integer ratio
	// For common fractions, check if f*denom is close to integer
	for denom := int64(1); denom <= 1000000; denom++ {
		num := f * float64(denom)
		rounded := math.Round(num)
		if math.Abs(num-rounded) < 1e-9 {
			return sign * int64(rounded), denom
		}
	}
	// fallback
	return sign * int64(math.Round(f*1000000)), 1000000
}

func builtinNumerator(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("numerator: expected 1 argument, got %d", len(args))
	}
	switch n := args[0].(type) {
	case *IntVal:
		return n, nil
	case *RationalVal:
		return &IntVal{Val: n.Num}, nil
	default:
		return nil, fmt.Errorf("numerator: expected exact number, got %s", args[0].String())
	}
}

func builtinDenominator(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("denominator: expected 1 argument, got %d", len(args))
	}
	switch args[0].(type) {
	case *IntVal:
		return &IntVal{Val: 1}, nil
	case *RationalVal:
		return &IntVal{Val: args[0].(*RationalVal).Denom}, nil
	default:
		return nil, fmt.Errorf("denominator: expected exact number, got %s", args[0].String())
	}
}

func builtinIntegerQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("integer?: expected 1 argument, got %d", len(args))
	}
	switch n := args[0].(type) {
	case *IntVal:
		return &BoolVal{Val: true}, nil
	case *RationalVal:
		// 4/2 simplifies to IntVal, so a RationalVal always has denom != 1
		_ = n
		return &BoolVal{Val: false}, nil
	case *FloatVal:
		return &BoolVal{Val: n.Val == math.Floor(n.Val)}, nil
	default:
		return &BoolVal{Val: false}, nil
	}
}

func builtinRationalQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("rational?: expected 1 argument, got %d", len(args))
	}
	return &BoolVal{Val: isExact(args[0])}, nil
}

// evalDefineRecordType implements R7RS define-record-type.
// (define-record-type <name> (<constructor> <field-name> ...) <predicate> (<field> <accessor>) ...)
func evalDefineRecordType(expr *Expr, env *Env) (Value, error) {
	args := expr.List[1:]
	if len(args) < 3 {
		return nil, &EvalError{Message: "define-record-type: bad syntax"}
	}

	// 1. Type name (symbol like <point>)
	if args[0].Kind != ExprSymbol {
		return nil, &EvalError{Message: "define-record-type: expected type name"}
	}
	typeName := args[0].SVal

	// 2. Constructor spec: (<constructor-name> <field-name> ...)
	if args[1].Kind != ExprList || len(args[1].List) < 1 {
		return nil, &EvalError{Message: "define-record-type: expected constructor spec"}
	}
	ctorSpec := args[1].List
	if ctorSpec[0].Kind != ExprSymbol {
		return nil, &EvalError{Message: "define-record-type: expected constructor name"}
	}
	ctorName := ctorSpec[0].SVal
	var ctorFields []string
	for _, f := range ctorSpec[1:] {
		if f.Kind != ExprSymbol {
			return nil, &EvalError{Message: "define-record-type: expected field name in constructor"}
		}
		ctorFields = append(ctorFields, f.SVal)
	}

	// 3. Predicate name
	if args[2].Kind != ExprSymbol {
		return nil, &EvalError{Message: "define-record-type: expected predicate name"}
	}
	predName := args[2].SVal

	// 4. Field specs: (<field-name> <accessor-name>) ...
	type fieldSpec struct {
		name     string
		accessor string
	}
	var fields []fieldSpec
	for _, fspec := range args[3:] {
		if fspec.Kind != ExprList || len(fspec.List) < 2 {
			return nil, &EvalError{Message: "define-record-type: bad field spec"}
		}
		if fspec.List[0].Kind != ExprSymbol || fspec.List[1].Kind != ExprSymbol {
			return nil, &EvalError{Message: "define-record-type: expected symbols in field spec"}
		}
		fields = append(fields, fieldSpec{name: fspec.List[0].SVal, accessor: fspec.List[1].SVal})
	}

	// Build field name -> index mapping
	allFieldNames := make([]string, len(fields))
	fieldIndex := make(map[string]int, len(fields))
	for i, f := range fields {
		allFieldNames[i] = f.name
		fieldIndex[f.name] = i
	}

	// Build constructor field order -> record field index
	ctorIndices := make([]int, len(ctorFields))
	for i, cf := range ctorFields {
		idx, ok := fieldIndex[cf]
		if !ok {
			return nil, &EvalError{Message: fmt.Sprintf("define-record-type: constructor field %s not in field specs", cf)}
		}
		ctorIndices[i] = idx
	}

	rt := &RecordType{Name: typeName, Fields: allFieldNames}

	// Define constructor
	env.Set(ctorName, &BuiltinFunc{Name: ctorName, Fn: func(args []Value) (Value, error) {
		if len(args) != len(ctorFields) {
			return nil, fmt.Errorf("%s: expected %d arguments, got %d", ctorName, len(ctorFields), len(args))
		}
		fvals := make([]Value, len(allFieldNames))
		for i, idx := range ctorIndices {
			fvals[idx] = args[i]
		}
		return &RecordVal{Type: rt, Fields: fvals}, nil
	}})

	// Define predicate
	env.Set(predName, &BuiltinFunc{Name: predName, Fn: func(args []Value) (Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("%s: expected 1 argument, got %d", predName, len(args))
		}
		rv, ok := args[0].(*RecordVal)
		return &BoolVal{Val: ok && rv.Type == rt}, nil
	}})

	// Define accessors
	for _, f := range fields {
		idx := fieldIndex[f.name]
		accName := f.accessor
		env.Set(accName, &BuiltinFunc{Name: accName, Fn: func(args []Value) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("%s: expected 1 argument, got %d", accName, len(args))
			}
			rv, ok := args[0].(*RecordVal)
			if !ok || rv.Type != rt {
				return nil, fmt.Errorf("%s: expected %s, got %s", accName, typeName, args[0].String())
			}
			return rv.Fields[idx], nil
		}})
	}

	return &VoidVal{}, nil
}

func builtinProcedureQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("procedure?: expected 1 argument, got %d", len(args))
	}
	switch args[0].(type) {
	case *LambdaVal, *CaseLambdaVal, *BuiltinFunc, *ApplyVal, *MapVal, *CallCCVal, *ContinuationVal, *DynamicWindVal:
		return &BoolVal{Val: true}, nil
	}
	return &BoolVal{Val: false}, nil
}

// L14 special forms

func evalLetrec(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec: bad syntax", expr.Line, expr.Col)}
	}
	bindingsExpr := expr.List[1]
	if bindingsExpr.Kind != ExprList {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec: expected bindings list", bindingsExpr.Line, bindingsExpr.Col)}
	}
	childEnv := NewEnv(env)
	// First, bind all variables to undefined (using VoidVal as placeholder)
	names := make([]string, len(bindingsExpr.List))
	for i, b := range bindingsExpr.List {
		if b.Kind != ExprList || len(b.List) != 2 || b.List[0].Kind != ExprSymbol {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec: bad binding", b.Line, b.Col)}
		}
		names[i] = b.List[0].SVal
		childEnv.Set(names[i], &VoidVal{})
	}
	// Then evaluate init expressions in the child env and assign
	for i, b := range bindingsExpr.List {
		val, err := eval(b.List[1], childEnv)
		if err != nil {
			return nil, err
		}
		childEnv.Set(names[i], val)
	}
	// Evaluate body
	body := expr.List[2:]
	contFrameStack = append(contFrameStack, contFrame{Kind: frameBody, Exprs: body, Env: childEnv})
	defer func() { contFrameStack = contFrameStack[:len(contFrameStack)-1] }()
	for i, bodyExpr := range body[:len(body)-1] {
		contFrameStack[len(contFrameStack)-1].Idx = i
		_, err := eval(bodyExpr, childEnv)
		if err != nil {
			return nil, err
		}
	}
	contFrameStack[len(contFrameStack)-1].Idx = len(body) - 1
	return &tailCall{expr: body[len(body)-1], env: childEnv}, nil
}

func evalLetrecStar(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec*: bad syntax", expr.Line, expr.Col)}
	}
	bindingsExpr := expr.List[1]
	if bindingsExpr.Kind != ExprList {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec*: expected bindings list", bindingsExpr.Line, bindingsExpr.Col)}
	}
	childEnv := NewEnv(env)
	// Evaluate bindings sequentially, each visible to the next
	for _, b := range bindingsExpr.List {
		if b.Kind != ExprList || len(b.List) != 2 || b.List[0].Kind != ExprSymbol {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: letrec*: bad binding", b.Line, b.Col)}
		}
		val, err := eval(b.List[1], childEnv)
		if err != nil {
			return nil, err
		}
		childEnv.Set(b.List[0].SVal, val)
	}
	body := expr.List[2:]
	contFrameStack = append(contFrameStack, contFrame{Kind: frameBody, Exprs: body, Env: childEnv})
	defer func() { contFrameStack = contFrameStack[:len(contFrameStack)-1] }()
	for i, bodyExpr := range body[:len(body)-1] {
		contFrameStack[len(contFrameStack)-1].Idx = i
		contFrameStack[len(contFrameStack)-1].Idx = i
		_, err := eval(bodyExpr, childEnv)
		if err != nil {
			return nil, err
		}
	}
	contFrameStack[len(contFrameStack)-1].Idx = len(body) - 1
	return &tailCall{expr: body[len(body)-1], env: childEnv}, nil
}

func evalCase(expr *Expr, env *Env) (Value, error) {
	// (case <key> (<datum> ...) <expr> ...) ...)
	if len(expr.List) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case: bad syntax", expr.Line, expr.Col)}
	}
	key, err := eval(expr.List[1], env)
	if err != nil {
		return nil, err
	}
	for _, clause := range expr.List[2:] {
		if clause.Kind != ExprList || len(clause.List) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case: bad clause", clause.Line, clause.Col)}
		}
		// else clause
		if clause.List[0].Kind == ExprSymbol && clause.List[0].SVal == "else" {
			body := clause.List[1:]
			for _, e := range body[:len(body)-1] {
				_, err = eval(e, env)
				if err != nil {
					return nil, err
				}
			}
			return &tailCall{expr: body[len(body)-1], env: env}, nil
		}
		// datum list
		if clause.List[0].Kind != ExprList {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: case: expected datum list", clause.List[0].Line, clause.List[0].Col)}
		}
		for _, datum := range clause.List[0].List {
			dv := exprToValue(datum)
			if schemeEqv(key, dv) {
				body := clause.List[1:]
				for _, e := range body[:len(body)-1] {
					_, err = eval(e, env)
					if err != nil {
						return nil, err
					}
				}
				return &tailCall{expr: body[len(body)-1], env: env}, nil
			}
		}
	}
	return &VoidVal{}, nil
}

// schemeEqv implements eqv? semantics for case dispatch.
func schemeEqv(a, b Value) bool {
	switch av := a.(type) {
	case *IntVal:
		if bv, ok := b.(*IntVal); ok {
			return av.Val == bv.Val
		}
	case *BoolVal:
		if bv, ok := b.(*BoolVal); ok {
			return av.Val == bv.Val
		}
	case *SymbolVal:
		if bv, ok := b.(*SymbolVal); ok {
			return av.Name == bv.Name
		}
	case *CharVal:
		if bv, ok := b.(*CharVal); ok {
			return av.Val == bv.Val
		}
	case *NilVal:
		_, ok := b.(*NilVal)
		return ok
	case *StringVal:
		if bv, ok := b.(*StringVal); ok {
			return av.Val == bv.Val
		}
	case *FloatVal:
		if bv, ok := b.(*FloatVal); ok {
			return av.Val == bv.Val
		}
	case *RationalVal:
		if bv, ok := b.(*RationalVal); ok {
			return av.Num == bv.Num && av.Denom == bv.Denom
		}
	}
	return a == b
}

func evalDo(expr *Expr, env *Env) (Value, error) {
	// (do ((var init step) ...) (test expr ...) body ...)
	if len(expr.List) < 3 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: do: bad syntax", expr.Line, expr.Col)}
	}
	varsExpr := expr.List[1]
	testExpr := expr.List[2]
	body := expr.List[3:]

	if varsExpr.Kind != ExprList {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: do: expected variable list", varsExpr.Line, varsExpr.Col)}
	}
	if testExpr.Kind != ExprList || len(testExpr.List) < 1 {
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: do: expected test clause", testExpr.Line, testExpr.Col)}
	}

	type doVar struct {
		name    string
		stepExpr *Expr // nil if no step
	}
	vars := make([]doVar, len(varsExpr.List))
	doEnv := NewEnv(env)

	// Initialize variables
	for i, v := range varsExpr.List {
		if v.Kind != ExprList || len(v.List) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: do: bad variable spec", v.Line, v.Col)}
		}
		if v.List[0].Kind != ExprSymbol {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: do: expected variable name", v.List[0].Line, v.List[0].Col)}
		}
		vars[i].name = v.List[0].SVal
		initVal, err := eval(v.List[1], env)
		if err != nil {
			return nil, err
		}
		doEnv.Set(vars[i].name, initVal)
		if len(v.List) >= 3 {
			vars[i].stepExpr = v.List[2]
		}
	}

	// Iteration loop
	for {
		// Evaluate test
		testVal, err := eval(testExpr.List[0], doEnv)
		if err != nil {
			return nil, err
		}
		if isTruthy(testVal) {
			// Test is true: evaluate result expressions
			if len(testExpr.List) == 1 {
				return &VoidVal{}, nil
			}
			var result Value
			for _, e := range testExpr.List[1:] {
				result, err = eval(e, doEnv)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
		// Evaluate body
		for _, b := range body {
			_, err = eval(b, doEnv)
			if err != nil {
				return nil, err
			}
		}
		// Evaluate step expressions with PREVIOUS values (parallel update)
		newVals := make([]Value, len(vars))
		for i, v := range vars {
			if v.stepExpr != nil {
				newVals[i], err = eval(v.stepExpr, doEnv)
				if err != nil {
					return nil, err
				}
			}
		}
		// Update variables
		for i, v := range vars {
			if v.stepExpr != nil {
				doEnv.Set(v.name, newVals[i])
			}
		}
	}
}

// L14 vector builtins

func builtinVector(args []Value) (Value, error) {
	elems := make([]Value, len(args))
	copy(elems, args)
	return &VectorVal{Elems: elems}, nil
}

func builtinMakeVector(args []Value) (Value, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, fmt.Errorf("make-vector: expected 1-2 arguments, got %d", len(args))
	}
	n, ok := args[0].(*IntVal)
	if !ok {
		return nil, fmt.Errorf("make-vector: expected integer, got %s", args[0].String())
	}
	fill := Value(&IntVal{Val: 0})
	if len(args) == 2 {
		fill = args[1]
	}
	elems := make([]Value, n.Val)
	for i := range elems {
		elems[i] = fill
	}
	return &VectorVal{Elems: elems}, nil
}

func builtinVectorRef(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("vector-ref: expected 2 arguments, got %d", len(args))
	}
	v, ok := args[0].(*VectorVal)
	if !ok {
		return nil, fmt.Errorf("vector-ref: expected vector, got %s", args[0].String())
	}
	idx, ok := args[1].(*IntVal)
	if !ok {
		return nil, fmt.Errorf("vector-ref: expected integer, got %s", args[1].String())
	}
	if idx.Val < 0 || int(idx.Val) >= len(v.Elems) {
		return nil, fmt.Errorf("vector-ref: index %d out of range for vector of length %d", idx.Val, len(v.Elems))
	}
	return v.Elems[idx.Val], nil
}

func builtinVectorSet(args []Value) (Value, error) {
	if len(args) != 3 {
		return nil, fmt.Errorf("vector-set!: expected 3 arguments, got %d", len(args))
	}
	v, ok := args[0].(*VectorVal)
	if !ok {
		return nil, fmt.Errorf("vector-set!: expected vector, got %s", args[0].String())
	}
	idx, ok := args[1].(*IntVal)
	if !ok {
		return nil, fmt.Errorf("vector-set!: expected integer, got %s", args[1].String())
	}
	if idx.Val < 0 || int(idx.Val) >= len(v.Elems) {
		return nil, fmt.Errorf("vector-set!: index %d out of range for vector of length %d", idx.Val, len(v.Elems))
	}
	v.Elems[idx.Val] = args[2]
	return &VoidVal{}, nil
}

func builtinVectorLength(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("vector-length: expected 1 argument, got %d", len(args))
	}
	v, ok := args[0].(*VectorVal)
	if !ok {
		return nil, fmt.Errorf("vector-length: expected vector, got %s", args[0].String())
	}
	return &IntVal{Val: int64(len(v.Elems))}, nil
}

func builtinVectorQ(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("vector?: expected 1 argument, got %d", len(args))
	}
	_, ok := args[0].(*VectorVal)
	return &BoolVal{Val: ok}, nil
}

func builtinVectorToList(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("vector->list: expected 1 argument, got %d", len(args))
	}
	v, ok := args[0].(*VectorVal)
	if !ok {
		return nil, fmt.Errorf("vector->list: expected vector, got %s", args[0].String())
	}
	result := Value(&NilVal{})
	for i := len(v.Elems) - 1; i >= 0; i-- {
		result = &PairVal{Car: v.Elems[i], Cdr: result}
	}
	return result, nil
}

func builtinListToVector(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("list->vector: expected 1 argument, got %d", len(args))
	}
	var elems []Value
	cur := args[0]
	for {
		switch v := cur.(type) {
		case *NilVal:
			return &VectorVal{Elems: elems}, nil
		case *PairVal:
			elems = append(elems, v.Car)
			cur = v.Cdr
		default:
			return nil, fmt.Errorf("list->vector: expected list")
		}
	}
}

func builtinEqvQ(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("eqv?: expected 2 arguments, got %d", len(args))
	}
	return &BoolVal{Val: schemeEqv(args[0], args[1])}, nil
}

func builtinReverse(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("reverse: expected 1 argument, got %d", len(args))
	}
	result := Value(&NilVal{})
	cur := args[0]
	for {
		switch v := cur.(type) {
		case *NilVal:
			return result, nil
		case *PairVal:
			result = &PairVal{Car: v.Car, Cdr: result}
			cur = v.Cdr
		default:
			return nil, fmt.Errorf("reverse: expected list")
		}
	}
}

func builtinError(args []Value) (Value, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("error: expected at least 1 argument")
	}
	var parts []string
	for _, a := range args {
		switch v := a.(type) {
		case *StringVal:
			parts = append(parts, v.Val)
		case *BoolVal:
			if !v.Val {
				continue
			}
			parts = append(parts, v.String())
		default:
			parts = append(parts, a.String())
		}
	}
	return nil, fmt.Errorf("%s", strings.Join(parts, ""))
}

// L15 builtins — string immutability

func builtinStringToList(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("string->list: expected 1 argument, got %d", len(args))
	}
	s, ok := args[0].(*StringVal)
	if !ok {
		return nil, fmt.Errorf("string->list: expected string, got %s", args[0].String())
	}
	var result Value = &NilVal{}
	runes := []rune(s.Val)
	for i := len(runes) - 1; i >= 0; i-- {
		result = &PairVal{Car: &CharVal{Val: runes[i]}, Cdr: result}
	}
	return result, nil
}

func builtinListToString(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("list->string: expected 1 argument, got %d", len(args))
	}
	var runes []rune
	cur := args[0]
	for {
		if _, ok := cur.(*NilVal); ok {
			break
		}
		p, ok := cur.(*PairVal)
		if !ok {
			return nil, fmt.Errorf("list->string: expected proper list of characters")
		}
		ch, ok := p.Car.(*CharVal)
		if !ok {
			return nil, fmt.Errorf("list->string: expected character, got %s", p.Car.String())
		}
		runes = append(runes, ch.Val)
		cur = p.Cdr
	}
	return &StringVal{Val: string(runes)}, nil
}

func builtinCharToInteger(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("char->integer: expected 1 argument, got %d", len(args))
	}
	ch, ok := args[0].(*CharVal)
	if !ok {
		return nil, fmt.Errorf("char->integer: expected char, got %s", args[0].String())
	}
	return &IntVal{Val: int64(ch.Val)}, nil
}

func builtinIntegerToChar(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("integer->char: expected 1 argument, got %d", len(args))
	}
	n, ok := args[0].(*IntVal)
	if !ok {
		return nil, fmt.Errorf("integer->char: expected integer, got %s", args[0].String())
	}
	return &CharVal{Val: rune(n.Val)}, nil
}

// --- L17: Pair mutation ---

func builtinSetCar(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("set-car!: expected 2 arguments, got %d", len(args))
	}
	p, ok := args[0].(*PairVal)
	if !ok {
		return nil, fmt.Errorf("set-car!: expected pair, got %s", args[0].String())
	}
	p.Car = args[1]
	return &VoidVal{}, nil
}

func builtinSetCdr(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("set-cdr!: expected 2 arguments, got %d", len(args))
	}
	p, ok := args[0].(*PairVal)
	if !ok {
		return nil, fmt.Errorf("set-cdr!: expected pair, got %s", args[0].String())
	}
	p.Cdr = args[1]
	return &VoidVal{}, nil
}

// cxr implements compositions of car/cdr.
// ops is a string like "da" for cadr: read right-to-left, 'a'=car, 'd'=cdr.
func cxr(args []Value, name, ops string) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("%s: expected 1 argument, got %d", name, len(args))
	}
	v := args[0]
	for i := 0; i < len(ops); i++ {
		p, ok := v.(*PairVal)
		if !ok {
			return nil, fmt.Errorf("%s: expected pair, got %s", name, v.String())
		}
		if ops[i] == 'a' {
			v = p.Car
		} else {
			v = p.Cdr
		}
	}
	return v, nil
}

// builtinForEach implements (for-each proc list1 ...).
func builtinForEach(args []Value) (Value, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("for-each: expected at least 2 arguments, got %d", len(args))
	}
	proc := args[0]
	lists := args[1:]
	for {
		// Check if any list is exhausted
		anyDone := false
		for _, l := range lists {
			if _, ok := l.(*NilVal); ok {
				anyDone = true
				break
			}
		}
		if anyDone {
			break
		}
		// Collect heads and advance
		callArgs := make([]Value, len(lists))
		for i := range lists {
			p, ok := lists[i].(*PairVal)
			if !ok {
				return nil, fmt.Errorf("for-each: expected list")
			}
			callArgs[i] = p.Car
			lists[i] = p.Cdr
		}
		_, err := callProc(proc, callArgs)
		if err != nil {
			return nil, err
		}
	}
	return &VoidVal{}, nil
}

// callProc calls a procedure without needing a callExpr (for use from builtins).
func callProc(proc Value, args []Value) (Value, error) {
	switch fn := proc.(type) {
	case *BuiltinFunc:
		return fn.Fn(args)
	case *LambdaVal:
		if fn.RestParam != "" {
			if len(args) < len(fn.Params) {
				return nil, fmt.Errorf("expected at least %d arguments, got %d", len(fn.Params), len(args))
			}
		} else {
			if len(args) != len(fn.Params) {
				return nil, fmt.Errorf("expected %d arguments, got %d", len(fn.Params), len(args))
			}
		}
		childEnv := NewEnv(fn.Env)
		for i, p := range fn.Params {
			childEnv.Set(p, args[i])
		}
		if fn.RestParam != "" {
			rest := Value(&NilVal{})
			for i := len(args) - 1; i >= len(fn.Params); i-- {
				rest = &PairVal{Car: args[i], Cdr: rest}
			}
			childEnv.Set(fn.RestParam, rest)
		}
		var result Value
		var err error
		for _, bodyExpr := range fn.Body {
			result, err = eval(bodyExpr, childEnv)
			if err != nil {
				return nil, err
			}
		}
		return result, nil
	case *CaseLambdaVal:
		for _, clause := range fn.Clauses {
			if clause.RestParam != "" {
				if len(args) < len(clause.Params) {
					continue
				}
			} else {
				if len(args) != len(clause.Params) {
					continue
				}
			}
			childEnv := NewEnv(fn.Env)
			for i, p := range clause.Params {
				childEnv.Set(p, args[i])
			}
			if clause.RestParam != "" {
				rest := Value(&NilVal{})
				for i := len(args) - 1; i >= len(clause.Params); i-- {
					rest = &PairVal{Car: args[i], Cdr: rest}
				}
				childEnv.Set(clause.RestParam, rest)
			}
			var result Value
			var err error
			for _, bodyExpr := range clause.Body {
				result, err = eval(bodyExpr, childEnv)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
		return nil, fmt.Errorf("no matching clause for %d arguments", len(args))
	default:
		return nil, fmt.Errorf("not a procedure: %s", proc.String())
	}
}

func builtinGcd(args []Value) (Value, error) {
	if len(args) == 0 {
		return &IntVal{Val: 0}, nil
	}
	result := int64(0)
	for _, a := range args {
		n, ok := a.(*IntVal)
		if !ok {
			return nil, fmt.Errorf("gcd: expected integer, got %s", a.String())
		}
		result = gcd(result, n.Val)
	}
	if result < 0 {
		result = -result
	}
	return &IntVal{Val: result}, nil
}

func builtinLcm(args []Value) (Value, error) {
	if len(args) == 0 {
		return &IntVal{Val: 1}, nil
	}
	result := int64(1)
	for _, a := range args {
		n, ok := a.(*IntVal)
		if !ok {
			return nil, fmt.Errorf("lcm: expected integer, got %s", a.String())
		}
		v := n.Val
		if v < 0 {
			v = -v
		}
		if v == 0 {
			return &IntVal{Val: 0}, nil
		}
		result = result / gcd(result, v) * v
	}
	return &IntVal{Val: result}, nil
}

func builtinTruncate(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("truncate: expected 1 argument, got %d", len(args))
	}
	switch n := args[0].(type) {
	case *IntVal:
		return n, nil
	case *FloatVal:
		return &FloatVal{Val: math.Trunc(n.Val)}, nil
	case *RationalVal:
		return &IntVal{Val: n.Num / n.Denom}, nil
	default:
		return nil, fmt.Errorf("truncate: expected number, got %s", args[0].String())
	}
}

func builtinRound(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("round: expected 1 argument, got %d", len(args))
	}
	switch n := args[0].(type) {
	case *IntVal:
		return n, nil
	case *FloatVal:
		return &FloatVal{Val: math.RoundToEven(n.Val)}, nil
	case *RationalVal:
		return &IntVal{Val: int64(math.RoundToEven(float64(n.Num) / float64(n.Denom)))}, nil
	default:
		return nil, fmt.Errorf("round: expected number, got %s", args[0].String())
	}
}

func builtinMakeString(args []Value) (Value, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, fmt.Errorf("make-string: expected 1-2 arguments, got %d", len(args))
	}
	n, ok := args[0].(*IntVal)
	if !ok {
		return nil, fmt.Errorf("make-string: expected integer, got %s", args[0].String())
	}
	ch := rune(' ')
	if len(args) == 2 {
		c, ok := args[1].(*CharVal)
		if !ok {
			return nil, fmt.Errorf("make-string: expected char, got %s", args[1].String())
		}
		ch = c.Val
	}
	return &StringVal{Val: strings.Repeat(string(ch), int(n.Val))}, nil
}

func builtinStringConstructor(args []Value) (Value, error) {
	var buf strings.Builder
	for _, a := range args {
		c, ok := a.(*CharVal)
		if !ok {
			return nil, fmt.Errorf("string: expected char, got %s", a.String())
		}
		buf.WriteRune(c.Val)
	}
	return &StringVal{Val: buf.String()}, nil
}

func builtinStringGtQ(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("string>?: expected 2 arguments, got %d", len(args))
	}
	a, ok1 := args[0].(*StringVal)
	b, ok2 := args[1].(*StringVal)
	if !ok1 || !ok2 {
		return nil, fmt.Errorf("string>?: expected strings")
	}
	return &BoolVal{Val: a.Val > b.Val}, nil
}

func builtinStringLeQ(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("string<=?: expected 2 arguments, got %d", len(args))
	}
	a, ok1 := args[0].(*StringVal)
	b, ok2 := args[1].(*StringVal)
	if !ok1 || !ok2 {
		return nil, fmt.Errorf("string<=?: expected strings")
	}
	return &BoolVal{Val: a.Val <= b.Val}, nil
}

func builtinStringGeQ(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("string>=?: expected 2 arguments, got %d", len(args))
	}
	a, ok1 := args[0].(*StringVal)
	b, ok2 := args[1].(*StringVal)
	if !ok1 || !ok2 {
		return nil, fmt.Errorf("string>=?: expected strings")
	}
	return &BoolVal{Val: a.Val >= b.Val}, nil
}
