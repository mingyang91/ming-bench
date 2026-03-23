package ming

import (
	"fmt"
)

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	tokens, err := Tokenize(input)
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}

	parser := NewParser(tokens)
	exprs, err := parser.ParseAll()
	if err != nil {
		return "", &EvalError{Message: err.Error()}
	}

	if len(exprs) == 0 {
		return "", &EvalError{Message: "no expressions"}
	}

	var result SchemeValue
	for _, expr := range exprs {
		result, err = Eval(expr)
		if err != nil {
			return "", err
		}
	}

	return result.String(), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	r, err := EvalStr(input)
	return r, "", err
}

// Eval evaluates an expression and returns a SchemeValue.
func Eval(expr Expr) (SchemeValue, error) {
	switch e := expr.(type) {
	case *NumberExpr:
		return &SchemeInt{Value: e.Value}, nil

	case *BoolExpr:
		return &SchemeBool{Value: e.Value}, nil

	case *StringExpr:
		return &SchemeString{Value: e.Value}, nil

	case *SymbolExpr:
		if b, ok := builtins[e.Name]; ok {
			return b, nil
		}
		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: unbound variable '%s'", line, col, e.Name)}

	case *ListExpr:
		if len(e.Elements) == 0 {
			line, col := e.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: empty application", line, col)}
		}

		// Check for special forms
		if sym, ok := e.Elements[0].(*SymbolExpr); ok {
			switch sym.Name {
			case "and":
				return evalAnd(e.Elements[1:])
			case "or":
				return evalOr(e.Elements[1:])
			}
		}

		// Evaluate operator
		op, err := Eval(e.Elements[0])
		if err != nil {
			return nil, err
		}

		// Evaluate arguments
		args := make([]SchemeValue, len(e.Elements)-1)
		for i, arg := range e.Elements[1:] {
			args[i], err = Eval(arg)
			if err != nil {
				return nil, err
			}
		}

		// Apply built-in procedures
		if builtin, ok := op.(*BuiltinProc); ok {
			return builtin.Fn(args, e)
		}

		line, col := e.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not a procedure", line, col)}

	default:
		return nil, &EvalError{Message: "unknown expression type"}
	}
}

// BuiltinProc is a built-in procedure.
type BuiltinProc struct {
	Name string
	Fn   func(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error)
}

func (b *BuiltinProc) String() string {
	return fmt.Sprintf("#<procedure %s>", b.Name)
}

// isTruthy returns true for all values except #f.
func isTruthy(v SchemeValue) bool {
	if b, ok := v.(*SchemeBool); ok {
		return b.Value
	}
	return true
}

func evalAnd(exprs []Expr) (SchemeValue, error) {
	var result SchemeValue = &SchemeBool{Value: true}
	for _, expr := range exprs {
		var err error
		result, err = Eval(expr)
		if err != nil {
			return nil, err
		}
		if !isTruthy(result) {
			return result, nil
		}
	}
	return result, nil
}

func evalOr(exprs []Expr) (SchemeValue, error) {
	var result SchemeValue = &SchemeBool{Value: false}
	for _, expr := range exprs {
		var err error
		result, err = Eval(expr)
		if err != nil {
			return nil, err
		}
		if isTruthy(result) {
			return result, nil
		}
	}
	return result, nil
}

// requireInts extracts int64 values from args, returning an error if any aren't integers.
func requireInts(args []SchemeValue, name string, callExpr *ListExpr) ([]int64, error) {
	nums := make([]int64, len(args))
	for i, a := range args {
		n, ok := a.(*SchemeInt)
		if !ok {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: %s: expected number, got %s", line, col, name, a.String())}
		}
		nums[i] = n.Value
	}
	return nums, nil
}

// builtins is the map of built-in procedures.
var builtins = map[string]*BuiltinProc{}

func init() {
	builtins["+"] = &BuiltinProc{Name: "+", Fn: builtinAdd}
	builtins["-"] = &BuiltinProc{Name: "-", Fn: builtinSub}
	builtins["*"] = &BuiltinProc{Name: "*", Fn: builtinMul}
	builtins["/"] = &BuiltinProc{Name: "/", Fn: builtinDiv}
	builtins["<"] = &BuiltinProc{Name: "<", Fn: builtinLT}
	builtins[">"] = &BuiltinProc{Name: ">", Fn: builtinGT}
	builtins["="] = &BuiltinProc{Name: "=", Fn: builtinEq}
	builtins["<="] = &BuiltinProc{Name: "<=", Fn: builtinLE}
	builtins[">="] = &BuiltinProc{Name: ">=", Fn: builtinGE}
	builtins["not"] = &BuiltinProc{Name: "not", Fn: builtinNot}
}

func builtinAdd(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	nums, err := requireInts(args, "+", callExpr)
	if err != nil {
		return nil, err
	}
	var sum int64
	for _, n := range nums {
		sum += n
	}
	return &SchemeInt{Value: sum}, nil
}

func builtinSub(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) == 0 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: -: requires at least 1 argument", line, col)}
	}
	nums, err := requireInts(args, "-", callExpr)
	if err != nil {
		return nil, err
	}
	if len(nums) == 1 {
		return &SchemeInt{Value: -nums[0]}, nil
	}
	result := nums[0]
	for _, n := range nums[1:] {
		result -= n
	}
	return &SchemeInt{Value: result}, nil
}

func builtinMul(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	nums, err := requireInts(args, "*", callExpr)
	if err != nil {
		return nil, err
	}
	var product int64 = 1
	for _, n := range nums {
		product *= n
	}
	return &SchemeInt{Value: product}, nil
}

func builtinDiv(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) < 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: /: requires at least 2 arguments", line, col)}
	}
	nums, err := requireInts(args, "/", callExpr)
	if err != nil {
		return nil, err
	}
	result := nums[0]
	for _, n := range nums[1:] {
		if n == 0 {
			line, col := callExpr.Pos()
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: /: division by zero", line, col)}
		}
		result /= n
	}
	return &SchemeInt{Value: result}, nil
}

func builtinLT(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	nums, err := requireInts(args, "<", callExpr)
	if err != nil {
		return nil, err
	}
	if len(nums) < 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: <: requires at least 2 arguments", line, col)}
	}
	for i := 0; i < len(nums)-1; i++ {
		if !(nums[i] < nums[i+1]) {
			return &SchemeBool{Value: false}, nil
		}
	}
	return &SchemeBool{Value: true}, nil
}

func builtinGT(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	nums, err := requireInts(args, ">", callExpr)
	if err != nil {
		return nil, err
	}
	if len(nums) < 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: >: requires at least 2 arguments", line, col)}
	}
	for i := 0; i < len(nums)-1; i++ {
		if !(nums[i] > nums[i+1]) {
			return &SchemeBool{Value: false}, nil
		}
	}
	return &SchemeBool{Value: true}, nil
}

func builtinEq(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	nums, err := requireInts(args, "=", callExpr)
	if err != nil {
		return nil, err
	}
	if len(nums) < 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: =: requires at least 2 arguments", line, col)}
	}
	for i := 0; i < len(nums)-1; i++ {
		if nums[i] != nums[i+1] {
			return &SchemeBool{Value: false}, nil
		}
	}
	return &SchemeBool{Value: true}, nil
}

func builtinLE(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	nums, err := requireInts(args, "<=", callExpr)
	if err != nil {
		return nil, err
	}
	if len(nums) < 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: <=: requires at least 2 arguments", line, col)}
	}
	for i := 0; i < len(nums)-1; i++ {
		if !(nums[i] <= nums[i+1]) {
			return &SchemeBool{Value: false}, nil
		}
	}
	return &SchemeBool{Value: true}, nil
}

func builtinGE(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	nums, err := requireInts(args, ">=", callExpr)
	if err != nil {
		return nil, err
	}
	if len(nums) < 2 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: >=: requires at least 2 arguments", line, col)}
	}
	for i := 0; i < len(nums)-1; i++ {
		if !(nums[i] >= nums[i+1]) {
			return &SchemeBool{Value: false}, nil
		}
	}
	return &SchemeBool{Value: true}, nil
}

func builtinNot(args []SchemeValue, callExpr *ListExpr) (SchemeValue, error) {
	if len(args) != 1 {
		line, col := callExpr.Pos()
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not: requires exactly 1 argument", line, col)}
	}
	return &SchemeBool{Value: !isTruthy(args[0])}, nil
}
