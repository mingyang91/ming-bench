package ming

import (
	"fmt"
	"strconv"
)

type value interface {
	schemeString() string
	isTruthy() bool
}

type numberValue int
type boolValue bool
type stringValue string

func (n numberValue) schemeString() string {
	return strconv.Itoa(int(n))
}

func (numberValue) isTruthy() bool {
	return true
}

func (b boolValue) schemeString() string {
	if b {
		return "#t"
	}
	return "#f"
}

func (b boolValue) isTruthy() bool {
	return bool(b)
}

func (s stringValue) schemeString() string {
	return strconv.Quote(string(s))
}

func (stringValue) isTruthy() bool {
	return true
}

func evalInput(input string) (result string, output string, err error) {
	exprs, err := parseProgram(input)
	if err != nil {
		return "", "", err
	}

	var last value
	for _, expr := range exprs {
		last, err = evalExpr(expr)
		if err != nil {
			return "", "", err
		}
	}

	return last.schemeString(), "", nil
}

func evalExpr(e expr) (value, error) {
	switch expr := e.(type) {
	case numberExpr:
		return numberValue(expr), nil
	case boolExpr:
		return boolValue(expr), nil
	case stringExpr:
		return stringValue(expr), nil
	case symbolExpr:
		return nil, &EvalError{Message: fmt.Sprintf("unbound variable: %s", string(expr))}
	case listExpr:
		return evalList(expr)
	default:
		return nil, &EvalError{Message: "unknown expression"}
	}
}

func evalList(items listExpr) (value, error) {
	if len(items) == 0 {
		return nil, &EvalError{Message: "cannot evaluate empty list"}
	}

	operator, ok := items[0].(symbolExpr)
	if !ok {
		return nil, &EvalError{Message: "first list element is not a procedure name"}
	}

	switch string(operator) {
	case "and":
		return evalAnd(items[1:])
	case "or":
		return evalOr(items[1:])
	}

	args := make([]value, 0, len(items)-1)
	for _, item := range items[1:] {
		arg, err := evalExpr(item)
		if err != nil {
			return nil, err
		}
		args = append(args, arg)
	}

	switch string(operator) {
	case "+":
		return evalAdd(args)
	case "-":
		return evalSub(args)
	case "*":
		return evalMul(args)
	case "/":
		return evalDiv(args)
	case "<":
		return evalCompare(args, "<", func(a, b int) bool { return a < b })
	case ">":
		return evalCompare(args, ">", func(a, b int) bool { return a > b })
	case "=":
		return evalCompare(args, "=", func(a, b int) bool { return a == b })
	case "<=":
		return evalCompare(args, "<=", func(a, b int) bool { return a <= b })
	case "not":
		return evalNot(args)
	default:
		return nil, &EvalError{Message: fmt.Sprintf("unknown procedure: %s", string(operator))}
	}
}

func evalAnd(items []expr) (value, error) {
	result := value(boolValue(true))
	for _, item := range items {
		next, err := evalExpr(item)
		if err != nil {
			return nil, err
		}
		result = next
		if !next.isTruthy() {
			return next, nil
		}
	}
	return result, nil
}

func evalOr(items []expr) (value, error) {
	for _, item := range items {
		next, err := evalExpr(item)
		if err != nil {
			return nil, err
		}
		if next.isTruthy() {
			return next, nil
		}
	}
	return boolValue(false), nil
}

func evalAdd(args []value) (value, error) {
	sum := 0
	for _, arg := range args {
		n, err := expectNumber(arg)
		if err != nil {
			return nil, err
		}
		sum += n
	}
	return numberValue(sum), nil
}

func evalSub(args []value) (value, error) {
	if len(args) == 0 {
		return nil, &EvalError{Message: "'-' expects at least 1 argument"}
	}

	first, err := expectNumber(args[0])
	if err != nil {
		return nil, err
	}
	if len(args) == 1 {
		return numberValue(-first), nil
	}

	result := first
	for _, arg := range args[1:] {
		n, err := expectNumber(arg)
		if err != nil {
			return nil, err
		}
		result -= n
	}
	return numberValue(result), nil
}

func evalMul(args []value) (value, error) {
	product := 1
	for _, arg := range args {
		n, err := expectNumber(arg)
		if err != nil {
			return nil, err
		}
		product *= n
	}
	return numberValue(product), nil
}

func evalDiv(args []value) (value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "'/' expects at least 2 arguments"}
	}

	first, err := expectNumber(args[0])
	if err != nil {
		return nil, err
	}

	result := first
	for _, arg := range args[1:] {
		n, err := expectNumber(arg)
		if err != nil {
			return nil, err
		}
		if n == 0 {
			return nil, &EvalError{Message: "division by zero"}
		}
		result /= n
	}
	return numberValue(result), nil
}

func evalCompare(args []value, name string, pred func(int, int) bool) (value, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: fmt.Sprintf("'%s' expects at least 2 arguments", name)}
	}

	prev, err := expectNumber(args[0])
	if err != nil {
		return nil, err
	}

	for _, arg := range args[1:] {
		next, err := expectNumber(arg)
		if err != nil {
			return nil, err
		}
		if !pred(prev, next) {
			return boolValue(false), nil
		}
		prev = next
	}

	return boolValue(true), nil
}

func evalNot(args []value) (value, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "'not' expects exactly 1 argument"}
	}
	return boolValue(!args[0].isTruthy()), nil
}

func expectNumber(v value) (int, error) {
	n, ok := v.(numberValue)
	if !ok {
		return 0, &EvalError{Message: fmt.Sprintf("expected number, got %s", v.schemeString())}
	}
	return int(n), nil
}
