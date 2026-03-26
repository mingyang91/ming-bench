package ming

import (
	"math"
	"math/big"
	"strconv"
	"strings"
)

type rationalExpr struct {
	value *big.Rat
}

type inexactExpr struct {
	value float64
}

type numberValue struct {
	exact     *big.Rat
	inexact   float64
	isInexact bool
}

func parseNumberLiteral(text string) (expr, bool) {
	if n, err := strconv.Atoi(text); err == nil {
		return intExpr(n), true
	}
	if value, ok := parseRationalLiteral(text); ok {
		return exprFromRat(value), true
	}
	if value, ok := parseInexactLiteral(text); ok {
		return newInexactExpr(value), true
	}
	return nil, false
}

func parseRationalLiteral(text string) (*big.Rat, bool) {
	if strings.Count(text, "/") != 1 {
		return nil, false
	}

	parts := strings.SplitN(text, "/", 2)
	if parts[0] == "" || parts[1] == "" {
		return nil, false
	}

	numerator, ok := new(big.Int).SetString(parts[0], 10)
	if !ok {
		return nil, false
	}
	denominator, ok := new(big.Int).SetString(parts[1], 10)
	if !ok || denominator.Sign() == 0 {
		return nil, false
	}

	return new(big.Rat).SetFrac(numerator, denominator), true
}

func parseInexactLiteral(text string) (float64, bool) {
	if !strings.ContainsAny(text, ".eE") {
		return 0, false
	}

	value, err := strconv.ParseFloat(text, 64)
	if err != nil {
		return 0, false
	}
	return value, true
}

func exactFromDecimalString(text string) (*big.Rat, bool) {
	if text == "" {
		return nil, false
	}

	sign := 1
	switch text[0] {
	case '+':
		text = text[1:]
	case '-':
		sign = -1
		text = text[1:]
	}

	if text == "" {
		return nil, false
	}

	exponent := 0
	if idx := strings.IndexAny(text, "eE"); idx >= 0 {
		var err error
		exponent, err = strconv.Atoi(text[idx+1:])
		if err != nil {
			return nil, false
		}
		text = text[:idx]
		if text == "" {
			return nil, false
		}
	}

	whole := text
	fraction := ""
	if idx := strings.IndexByte(text, '.'); idx >= 0 {
		whole = text[:idx]
		fraction = text[idx+1:]
		if strings.Contains(text[idx+1:], ".") {
			return nil, false
		}
	}

	if whole == "" {
		whole = "0"
	}
	if !allDecimalDigits(whole) || !allDecimalDigits(fraction) {
		return nil, false
	}

	digits := strings.TrimLeft(whole+fraction, "0")
	if digits == "" {
		return new(big.Rat), true
	}

	numerator, ok := new(big.Int).SetString(digits, 10)
	if !ok {
		return nil, false
	}
	if sign < 0 {
		numerator.Neg(numerator)
	}

	denominator := powerOfTen(len(fraction))
	if exponent > 0 {
		numerator.Mul(numerator, powerOfTen(exponent))
	} else if exponent < 0 {
		denominator.Mul(denominator, powerOfTen(-exponent))
	}

	return new(big.Rat).SetFrac(numerator, denominator), true
}

func allDecimalDigits(text string) bool {
	for _, r := range text {
		if r < '0' || r > '9' {
			return false
		}
	}
	return true
}

func powerOfTen(exp int) *big.Int {
	result := big.NewInt(1)
	ten := big.NewInt(10)
	for i := 0; i < exp; i++ {
		result.Mul(result, ten)
	}
	return result
}

func newInexactExpr(value float64) inexactExpr {
	if value == 0 {
		value = 0
	}
	return inexactExpr{value: value}
}

func copyRat(value *big.Rat) *big.Rat {
	if value == nil {
		return new(big.Rat)
	}
	return new(big.Rat).Set(value)
}

func exprFromRat(value *big.Rat) expr {
	rat := copyRat(value)
	if n, ok := ratToInt(rat); ok {
		return intExpr(n)
	}
	return rationalExpr{value: rat}
}

func ratToInt(value *big.Rat) (int, bool) {
	if value == nil || !value.IsInt() {
		return 0, false
	}

	numerator := value.Num()
	if !numerator.IsInt64() {
		return 0, false
	}

	n := numerator.Int64()
	if strconv.IntSize == 32 && (n < -1<<31 || n > 1<<31-1) {
		return 0, false
	}

	return int(n), true
}

func numberFromExpr(value expr) (numberValue, bool) {
	switch v := value.(type) {
	case intExpr:
		return numberValue{exact: big.NewRat(int64(v), 1)}, true
	case rationalExpr:
		return numberValue{exact: copyRat(v.value)}, true
	case inexactExpr:
		return numberValue{inexact: v.value, isInexact: true}, true
	default:
		return numberValue{}, false
	}
}

func numericArgs(values []expr) ([]numberValue, error) {
	args := make([]numberValue, 0, len(values))
	for _, value := range values {
		number, ok := numberFromExpr(value)
		if !ok {
			return nil, &EvalError{Message: "expected number"}
		}
		args = append(args, number)
	}
	return args, nil
}

func numericPair(args []expr, name string) (numberValue, numberValue, error) {
	numbers, err := numericArgs(args)
	if err != nil {
		return numberValue{}, numberValue{}, err
	}
	if len(numbers) != 2 {
		return numberValue{}, numberValue{}, &EvalError{Message: name + " expects exactly 2 arguments"}
	}
	return numbers[0], numbers[1], nil
}

func unaryNumberArg(args []expr, name string) (numberValue, error) {
	if len(args) != 1 {
		return numberValue{}, &EvalError{Message: name + " expects exactly 1 argument"}
	}

	number, ok := numberFromExpr(args[0])
	if !ok {
		return numberValue{}, &EvalError{Message: name + " expects a number"}
	}
	return number, nil
}

func exactIntegerValue(value expr) (int, bool) {
	switch v := value.(type) {
	case intExpr:
		return int(v), true
	case rationalExpr:
		return ratToInt(v.value)
	default:
		return 0, false
	}
}

func exactIntegerPair(args []expr, name string) (int, int, error) {
	if len(args) != 2 {
		return 0, 0, &EvalError{Message: name + " expects exactly 2 arguments"}
	}

	first, ok := exactIntegerValue(args[0])
	if !ok {
		return 0, 0, &EvalError{Message: name + " expects integer arguments"}
	}
	second, ok := exactIntegerValue(args[1])
	if !ok {
		return 0, 0, &EvalError{Message: name + " expects integer arguments"}
	}

	return first, second, nil
}

func unaryExactIntegerArg(args []expr, name string) (int, error) {
	if len(args) != 1 {
		return 0, &EvalError{Message: name + " expects exactly 1 argument"}
	}

	value, ok := exactIntegerValue(args[0])
	if !ok {
		return 0, &EvalError{Message: name + " expects an integer"}
	}
	return value, nil
}

func isNumber(value expr) bool {
	_, ok := numberFromExpr(value)
	return ok
}

func isExactNumber(value expr) bool {
	switch value.(type) {
	case intExpr, rationalExpr:
		return true
	default:
		return false
	}
}

func isInexactNumber(value expr) bool {
	_, ok := value.(inexactExpr)
	return ok
}

func isIntegerNumber(value expr) bool {
	switch v := value.(type) {
	case intExpr:
		return true
	case rationalExpr:
		return v.value != nil && v.value.IsInt()
	case inexactExpr:
		return !math.IsNaN(v.value) && !math.IsInf(v.value, 0) && math.Trunc(v.value) == v.value
	default:
		return false
	}
}

func isRationalNumber(value expr) bool {
	switch v := value.(type) {
	case intExpr, rationalExpr:
		return true
	case inexactExpr:
		return !math.IsNaN(v.value) && !math.IsInf(v.value, 0)
	default:
		return false
	}
}

func numberToFloat(value numberValue) float64 {
	if value.isInexact {
		return value.inexact
	}
	f, _ := value.exact.Float64()
	return f
}

func numberSign(value numberValue) int {
	if value.isInexact {
		switch {
		case value.inexact < 0:
			return -1
		case value.inexact > 0:
			return 1
		default:
			return 0
		}
	}
	return value.exact.Sign()
}

func numberIsZero(value numberValue) bool {
	return numberSign(value) == 0
}

func numbersContainInexact(values []numberValue) bool {
	for _, value := range values {
		if value.isInexact {
			return true
		}
	}
	return false
}

func compareNumbers(left, right numberValue) int {
	if left.isInexact || right.isInexact {
		leftFloat := numberToFloat(left)
		rightFloat := numberToFloat(right)
		switch {
		case leftFloat < rightFloat:
			return -1
		case leftFloat > rightFloat:
			return 1
		default:
			return 0
		}
	}
	return left.exact.Cmp(right.exact)
}

func numericEqualExpr(left, right expr) (bool, bool) {
	leftNumber, ok := numberFromExpr(left)
	if !ok {
		return false, false
	}

	rightNumber, ok := numberFromExpr(right)
	if !ok {
		return false, false
	}

	return compareNumbers(leftNumber, rightNumber) == 0, true
}

func renderNumber(value expr) (string, bool) {
	switch v := value.(type) {
	case intExpr:
		return strconv.Itoa(int(v)), true
	case rationalExpr:
		if v.value == nil {
			return "0", true
		}
		if v.value.IsInt() {
			return v.value.Num().String(), true
		}
		return v.value.RatString(), true
	case inexactExpr:
		return renderInexact(v.value), true
	default:
		return "", false
	}
}

func renderInexact(value float64) string {
	if math.IsNaN(value) || math.IsInf(value, 0) {
		return strconv.FormatFloat(value, 'g', -1, 64)
	}
	if value == 0 {
		value = 0
	}

	text := strconv.FormatFloat(value, 'g', -1, 64)
	if !strings.ContainsAny(strings.ToLower(text), ".e") {
		text += ".0"
	}
	return text
}

func convertExactToInexact(value expr) (expr, error) {
	switch v := value.(type) {
	case inexactExpr:
		return v, nil
	case intExpr:
		return newInexactExpr(float64(v)), nil
	case rationalExpr:
		f, _ := v.value.Float64()
		return newInexactExpr(f), nil
	default:
		return nil, &EvalError{Message: "exact->inexact expects a number"}
	}
}

func convertInexactToExact(value expr) (expr, error) {
	switch v := value.(type) {
	case intExpr, rationalExpr:
		return v, nil
	case inexactExpr:
		if math.IsNaN(v.value) || math.IsInf(v.value, 0) {
			return nil, &EvalError{Message: "inexact->exact cannot convert non-finite numbers"}
		}
		rat, ok := exactFromDecimalString(renderInexact(v.value))
		if !ok {
			return nil, &EvalError{Message: "inexact->exact expects a finite decimal number"}
		}
		return exprFromRat(rat), nil
	default:
		return nil, &EvalError{Message: "inexact->exact expects a number"}
	}
}

func builtinExact(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "exact? expects exactly 1 argument"}
	}
	return boolExpr(isExactNumber(args[0])), nil
}

func builtinInexact(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "inexact? expects exactly 1 argument"}
	}
	return boolExpr(isInexactNumber(args[0])), nil
}

func builtinInteger(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "integer? expects exactly 1 argument"}
	}
	return boolExpr(isIntegerNumber(args[0])), nil
}

func builtinRational(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "rational? expects exactly 1 argument"}
	}
	return boolExpr(isRationalNumber(args[0])), nil
}

func builtinExactToInexact(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "exact->inexact expects exactly 1 argument"}
	}
	return convertExactToInexact(args[0])
}

func builtinInexactToExact(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "inexact->exact expects exactly 1 argument"}
	}
	return convertInexactToExact(args[0])
}

func builtinNumerator(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "numerator expects exactly 1 argument"}
	}

	switch v := args[0].(type) {
	case intExpr:
		return v, nil
	case rationalExpr:
		return exprFromRat(new(big.Rat).SetInt(v.value.Num())), nil
	default:
		return nil, &EvalError{Message: "numerator expects an exact rational"}
	}
}

func builtinDenominator(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "denominator expects exactly 1 argument"}
	}

	switch v := args[0].(type) {
	case intExpr:
		return intExpr(1), nil
	case rationalExpr:
		return exprFromRat(new(big.Rat).SetInt(v.value.Denom())), nil
	default:
		return nil, &EvalError{Message: "denominator expects an exact rational"}
	}
}
