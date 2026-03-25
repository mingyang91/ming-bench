package ming

import (
	"fmt"
	"strconv"
	"strings"
	"unicode/utf8"
)

type Env struct {
	bindings map[string]*Value
	parent   *Env
	output   *strings.Builder
}

func NewEnv(parent *Env) *Env {
	return &Env{bindings: make(map[string]*Value), parent: parent}
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

func (e *Env) Update(name string, val *Value) bool {
	if _, ok := e.bindings[name]; ok {
		e.bindings[name] = val
		return true
	}
	if e.parent != nil {
		return e.parent.Update(name, val)
	}
	return false
}

func (e *Env) GetOutput() *strings.Builder {
	if e.output != nil {
		return e.output
	}
	if e.parent != nil {
		return e.parent.GetOutput()
	}
	return nil
}

func makeGlobalEnv() *Env {
	env := NewEnv(nil)

	// Arithmetic
	env.Set("+", &Value{Type: TypeSymbol, StrVal: "builtin:+"})
	env.Set("-", &Value{Type: TypeSymbol, StrVal: "builtin:-"})
	env.Set("*", &Value{Type: TypeSymbol, StrVal: "builtin:*"})
	env.Set("/", &Value{Type: TypeSymbol, StrVal: "builtin:/"})

	// Comparisons
	env.Set("<", &Value{Type: TypeSymbol, StrVal: "builtin:<"})
	env.Set(">", &Value{Type: TypeSymbol, StrVal: "builtin:>"})
	env.Set("=", &Value{Type: TypeSymbol, StrVal: "builtin:="})
	env.Set("<=", &Value{Type: TypeSymbol, StrVal: "builtin:<="})

	// Logical
	env.Set("not", &Value{Type: TypeSymbol, StrVal: "builtin:not"})

	// List operations
	env.Set("cons", &Value{Type: TypeSymbol, StrVal: "builtin:cons"})
	env.Set("car", &Value{Type: TypeSymbol, StrVal: "builtin:car"})
	env.Set("cdr", &Value{Type: TypeSymbol, StrVal: "builtin:cdr"})
	env.Set("null?", &Value{Type: TypeSymbol, StrVal: "builtin:null?"})
	env.Set("list", &Value{Type: TypeSymbol, StrVal: "builtin:list"})
	env.Set("length", &Value{Type: TypeSymbol, StrVal: "builtin:length"})
	env.Set("pair?", &Value{Type: TypeSymbol, StrVal: "builtin:pair?"})
	env.Set("append", &Value{Type: TypeSymbol, StrVal: "builtin:append"})

	// Type predicates
	env.Set("boolean?", &Value{Type: TypeSymbol, StrVal: "builtin:boolean?"})
	env.Set("number?", &Value{Type: TypeSymbol, StrVal: "builtin:number?"})
	env.Set("string?", &Value{Type: TypeSymbol, StrVal: "builtin:string?"})
	env.Set("symbol?", &Value{Type: TypeSymbol, StrVal: "builtin:symbol?"})

	// Comparison
	env.Set(">=", &Value{Type: TypeSymbol, StrVal: "builtin:>="})

	// I/O
	env.Set("display", &Value{Type: TypeSymbol, StrVal: "builtin:display"})
	env.Set("write", &Value{Type: TypeSymbol, StrVal: "builtin:write"})
	env.Set("newline", &Value{Type: TypeSymbol, StrVal: "builtin:newline"})

	// String operations
	env.Set("string-append", &Value{Type: TypeSymbol, StrVal: "builtin:string-append"})
	env.Set("string-length", &Value{Type: TypeSymbol, StrVal: "builtin:string-length"})
	env.Set("substring", &Value{Type: TypeSymbol, StrVal: "builtin:substring"})
	env.Set("string->number", &Value{Type: TypeSymbol, StrVal: "builtin:string->number"})
	env.Set("number->string", &Value{Type: TypeSymbol, StrVal: "builtin:number->string"})
	env.Set("symbol->string", &Value{Type: TypeSymbol, StrVal: "builtin:symbol->string"})
	env.Set("string->symbol", &Value{Type: TypeSymbol, StrVal: "builtin:string->symbol"})
	env.Set("string-ref", &Value{Type: TypeSymbol, StrVal: "builtin:string-ref"})
	env.Set("char?", &Value{Type: TypeSymbol, StrVal: "builtin:char?"})

	// Mutable strings (L06)
	env.Set("string-set!", &Value{Type: TypeSymbol, StrVal: "builtin:string-set!"})
	env.Set("string-copy", &Value{Type: TypeSymbol, StrVal: "builtin:string-copy"})

	// Apply (L08)
	env.Set("apply", &Value{Type: TypeSymbol, StrVal: "builtin:apply"})

	// Numeric utilities (L09)
	env.Set("abs", &Value{Type: TypeSymbol, StrVal: "builtin:abs"})
	env.Set("zero?", &Value{Type: TypeSymbol, StrVal: "builtin:zero?"})
	env.Set("positive?", &Value{Type: TypeSymbol, StrVal: "builtin:positive?"})
	env.Set("negative?", &Value{Type: TypeSymbol, StrVal: "builtin:negative?"})
	env.Set("odd?", &Value{Type: TypeSymbol, StrVal: "builtin:odd?"})
	env.Set("even?", &Value{Type: TypeSymbol, StrVal: "builtin:even?"})
	env.Set("modulo", &Value{Type: TypeSymbol, StrVal: "builtin:modulo"})
	env.Set("remainder", &Value{Type: TypeSymbol, StrVal: "builtin:remainder"})
	env.Set("quotient", &Value{Type: TypeSymbol, StrVal: "builtin:quotient"})
	env.Set("min", &Value{Type: TypeSymbol, StrVal: "builtin:min"})
	env.Set("max", &Value{Type: TypeSymbol, StrVal: "builtin:max"})
	env.Set("expt", &Value{Type: TypeSymbol, StrVal: "builtin:expt"})

	// Char operations (L09)
	env.Set("char=?", &Value{Type: TypeSymbol, StrVal: "builtin:char=?"})
	env.Set("char<?", &Value{Type: TypeSymbol, StrVal: "builtin:char<?"})
	env.Set("char-alphabetic?", &Value{Type: TypeSymbol, StrVal: "builtin:char-alphabetic?"})
	env.Set("char-numeric?", &Value{Type: TypeSymbol, StrVal: "builtin:char-numeric?"})
	env.Set("char-upcase", &Value{Type: TypeSymbol, StrVal: "builtin:char-upcase"})
	env.Set("char-downcase", &Value{Type: TypeSymbol, StrVal: "builtin:char-downcase"})

	// String comparisons (L09)
	env.Set("string=?", &Value{Type: TypeSymbol, StrVal: "builtin:string=?"})
	env.Set("string<?", &Value{Type: TypeSymbol, StrVal: "builtin:string<?"})
	env.Set("string-ci=?", &Value{Type: TypeSymbol, StrVal: "builtin:string-ci=?"})
	env.Set("string-upcase", &Value{Type: TypeSymbol, StrVal: "builtin:string-upcase"})
	env.Set("string-downcase", &Value{Type: TypeSymbol, StrVal: "builtin:string-downcase"})

	// List operations (L09)
	env.Set("list?", &Value{Type: TypeSymbol, StrVal: "builtin:list?"})
	env.Set("list-ref", &Value{Type: TypeSymbol, StrVal: "builtin:list-ref"})
	env.Set("list-tail", &Value{Type: TypeSymbol, StrVal: "builtin:list-tail"})
	env.Set("map", &Value{Type: TypeSymbol, StrVal: "builtin:map"})
	env.Set("assoc", &Value{Type: TypeSymbol, StrVal: "builtin:assoc"})
	env.Set("equal?", &Value{Type: TypeSymbol, StrVal: "builtin:equal?"})
	env.Set("eq?", &Value{Type: TypeSymbol, StrVal: "builtin:eq?"})

	return env
}

func callBuiltin(name string, args []*Value, env *Env, line, col int) (*Value, error) {
	switch name {
	case "builtin:+":
		var sum int64
		for _, a := range args {
			if a.Type != TypeInteger {
				return nil, fmt.Errorf("%d:%d: '+' expects numbers", line, col)
			}
			sum += a.IntVal
		}
		return IntValue(sum), nil

	case "builtin:-":
		if len(args) == 0 {
			return nil, fmt.Errorf("%d:%d: '-' requires at least one argument", line, col)
		}
		if args[0].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: '-' expects numbers", line, col)
		}
		if len(args) == 1 {
			return IntValue(-args[0].IntVal), nil
		}
		result := args[0].IntVal
		for _, a := range args[1:] {
			if a.Type != TypeInteger {
				return nil, fmt.Errorf("%d:%d: '-' expects numbers", line, col)
			}
			result -= a.IntVal
		}
		return IntValue(result), nil

	case "builtin:*":
		var product int64 = 1
		for _, a := range args {
			if a.Type != TypeInteger {
				return nil, fmt.Errorf("%d:%d: '*' expects numbers", line, col)
			}
			product *= a.IntVal
		}
		return IntValue(product), nil

	case "builtin:/":
		if len(args) < 2 {
			return nil, fmt.Errorf("%d:%d: '/' requires at least two arguments", line, col)
		}
		if args[0].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: '/' expects numbers", line, col)
		}
		result := args[0].IntVal
		for _, a := range args[1:] {
			if a.Type != TypeInteger {
				return nil, fmt.Errorf("%d:%d: '/' expects numbers", line, col)
			}
			if a.IntVal == 0 {
				return nil, fmt.Errorf("%d:%d: division by zero", line, col)
			}
			result /= a.IntVal
		}
		return IntValue(result), nil

	case "builtin:<":
		if len(args) != 2 || args[0].Type != TypeInteger || args[1].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: '<' expects two numbers", line, col)
		}
		return BoolValue(args[0].IntVal < args[1].IntVal), nil

	case "builtin:>":
		if len(args) != 2 || args[0].Type != TypeInteger || args[1].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: '>' expects two numbers", line, col)
		}
		return BoolValue(args[0].IntVal > args[1].IntVal), nil

	case "builtin:=":
		if len(args) != 2 || args[0].Type != TypeInteger || args[1].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: '=' expects two numbers", line, col)
		}
		return BoolValue(args[0].IntVal == args[1].IntVal), nil

	case "builtin:<=":
		if len(args) != 2 || args[0].Type != TypeInteger || args[1].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: '<=' expects two numbers", line, col)
		}
		return BoolValue(args[0].IntVal <= args[1].IntVal), nil

	case "builtin:not":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'not' expects one argument", line, col)
		}
		return BoolValue(!isTruthy(args[0])), nil

	case "builtin:>=":
		if len(args) != 2 || args[0].Type != TypeInteger || args[1].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: '>=' expects two numbers", line, col)
		}
		return BoolValue(args[0].IntVal >= args[1].IntVal), nil

	case "builtin:cons":
		if len(args) != 2 {
			return nil, fmt.Errorf("%d:%d: 'cons' expects 2 arguments", line, col)
		}
		return PairValue(args[0], args[1]), nil

	case "builtin:car":
		if len(args) != 1 || args[0].Type != TypePair {
			return nil, fmt.Errorf("%d:%d: 'car' expects a pair", line, col)
		}
		return args[0].Car, nil

	case "builtin:cdr":
		if len(args) != 1 || args[0].Type != TypePair {
			return nil, fmt.Errorf("%d:%d: 'cdr' expects a pair", line, col)
		}
		return args[0].Cdr, nil

	case "builtin:null?":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'null?' expects 1 argument", line, col)
		}
		return BoolValue(args[0].Type == TypeNull), nil

	case "builtin:list":
		result := Null
		for i := len(args) - 1; i >= 0; i-- {
			result = PairValue(args[i], result)
		}
		return result, nil

	case "builtin:length":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'length' expects 1 argument", line, col)
		}
		var count int64
		cur := args[0]
		for cur.Type == TypePair {
			count++
			cur = cur.Cdr
		}
		if cur.Type != TypeNull {
			return nil, fmt.Errorf("%d:%d: 'length' expects a proper list", line, col)
		}
		return IntValue(count), nil

	case "builtin:pair?":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'pair?' expects 1 argument", line, col)
		}
		return BoolValue(args[0].Type == TypePair), nil

	case "builtin:boolean?":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'boolean?' expects 1 argument", line, col)
		}
		return BoolValue(args[0].Type == TypeBoolean), nil

	case "builtin:number?":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'number?' expects 1 argument", line, col)
		}
		return BoolValue(args[0].Type == TypeInteger), nil

	case "builtin:string?":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'string?' expects 1 argument", line, col)
		}
		return BoolValue(args[0].Type == TypeString), nil

	case "builtin:append":
		if len(args) == 0 {
			return Null, nil
		}
		if len(args) == 1 {
			return args[0], nil
		}
		// append two lists
		if len(args) == 2 {
			a, b := args[0], args[1]
			if a.Type == TypeNull {
				return b, nil
			}
			// Build list from a, then attach b
			var items []*Value
			cur := a
			for cur.Type == TypePair {
				items = append(items, cur.Car)
				cur = cur.Cdr
			}
			result := b
			for i := len(items) - 1; i >= 0; i-- {
				result = PairValue(items[i], result)
			}
			return result, nil
		}
		// Multiple args: fold right
		result := args[len(args)-1]
		for i := len(args) - 2; i >= 0; i-- {
			twoArgs := []*Value{args[i], result}
			var err error
			result, err = callBuiltin("builtin:append", twoArgs, env, line, col)
			if err != nil {
				return nil, err
			}
		}
		return result, nil

	case "builtin:symbol?":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'symbol?' expects 1 argument", line, col)
		}
		return BoolValue(args[0].Type == TypeSymbol), nil

	case "builtin:display":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'display' expects 1 argument", line, col)
		}
		if out := env.GetOutput(); out != nil {
			out.WriteString(args[0].DisplayStr())
		}
		return Void, nil

	case "builtin:write":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'write' expects 1 argument", line, col)
		}
		if out := env.GetOutput(); out != nil {
			out.WriteString(args[0].WriteRepr())
		}
		return Void, nil

	case "builtin:newline":
		if len(args) != 0 {
			return nil, fmt.Errorf("%d:%d: 'newline' expects 0 arguments", line, col)
		}
		if out := env.GetOutput(); out != nil {
			out.WriteByte('\n')
		}
		return Void, nil

	case "builtin:string-append":
		var sb strings.Builder
		for _, a := range args {
			if a.Type != TypeString {
				return nil, fmt.Errorf("%d:%d: 'string-append' expects strings", line, col)
			}
			sb.WriteString(a.StrVal)
		}
		return StringValue(sb.String()), nil

	case "builtin:string-length":
		if len(args) != 1 || args[0].Type != TypeString {
			return nil, fmt.Errorf("%d:%d: 'string-length' expects a string", line, col)
		}
		return IntValue(int64(utf8.RuneCountInString(args[0].StrVal))), nil

	case "builtin:substring":
		if len(args) != 3 || args[0].Type != TypeString || args[1].Type != TypeInteger || args[2].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: 'substring' expects string, start, end", line, col)
		}
		runes := []rune(args[0].StrVal)
		start, end := int(args[1].IntVal), int(args[2].IntVal)
		if start < 0 || end < start || end > len(runes) {
			return nil, fmt.Errorf("%d:%d: 'substring' index out of range", line, col)
		}
		return StringValue(string(runes[start:end])), nil

	case "builtin:string->number":
		if len(args) != 1 || args[0].Type != TypeString {
			return nil, fmt.Errorf("%d:%d: 'string->number' expects a string", line, col)
		}
		n, err := strconv.ParseInt(args[0].StrVal, 10, 64)
		if err != nil {
			return False, nil
		}
		return IntValue(n), nil

	case "builtin:number->string":
		if len(args) != 1 || args[0].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: 'number->string' expects a number", line, col)
		}
		return StringValue(strconv.FormatInt(args[0].IntVal, 10)), nil

	case "builtin:symbol->string":
		if len(args) != 1 || args[0].Type != TypeSymbol {
			return nil, fmt.Errorf("%d:%d: 'symbol->string' expects a symbol", line, col)
		}
		return StringValue(args[0].StrVal), nil

	case "builtin:string->symbol":
		if len(args) != 1 || args[0].Type != TypeString {
			return nil, fmt.Errorf("%d:%d: 'string->symbol' expects a string", line, col)
		}
		return SymbolValue(args[0].StrVal), nil

	case "builtin:string-ref":
		if len(args) != 2 || args[0].Type != TypeString || args[1].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: 'string-ref' expects string and index", line, col)
		}
		runes := []rune(args[0].StrVal)
		idx := int(args[1].IntVal)
		if idx < 0 || idx >= len(runes) {
			return nil, fmt.Errorf("%d:%d: 'string-ref' index out of range", line, col)
		}
		return CharValue(runes[idx]), nil

	case "builtin:char?":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'char?' expects 1 argument", line, col)
		}
		return BoolValue(args[0].Type == TypeChar), nil

	case "builtin:string-set!":
		if len(args) != 3 || args[0].Type != TypeString || args[1].Type != TypeInteger || args[2].Type != TypeChar {
			return nil, fmt.Errorf("%d:%d: 'string-set!' expects string, index, char", line, col)
		}
		runes := []rune(args[0].StrVal)
		idx := int(args[1].IntVal)
		if idx < 0 || idx >= len(runes) {
			return nil, fmt.Errorf("%d:%d: 'string-set!' index out of range", line, col)
		}
		runes[idx] = args[2].CharVal
		args[0].StrVal = string(runes)
		return Void, nil

	case "builtin:string-copy":
		if len(args) != 1 || args[0].Type != TypeString {
			return nil, fmt.Errorf("%d:%d: 'string-copy' expects a string", line, col)
		}
		return StringValue(args[0].StrVal), nil

	case "builtin:apply":
		if len(args) < 2 {
			return nil, fmt.Errorf("%d:%d: 'apply' requires at least 2 arguments", line, col)
		}
		fn := args[0]
		// Last arg must be a list; prefix args are prepended
		lastArg := args[len(args)-1]
		// Collect prefix args
		var allArgs []*Value
		for _, a := range args[1 : len(args)-1] {
			allArgs = append(allArgs, a)
		}
		// Unpack the final list
		cur := lastArg
		for cur.Type == TypePair {
			allArgs = append(allArgs, cur.Car)
			cur = cur.Cdr
		}
		// Call the function
		if fn.Type == TypeSymbol && len(fn.StrVal) > 8 && fn.StrVal[:8] == "builtin:" {
			return callBuiltin(fn.StrVal, allArgs, env, line, col)
		}
		if fn.Type == TypeLambda {
			return callLambda(fn, allArgs, line, col)
		}
		return nil, fmt.Errorf("%d:%d: 'apply' first argument must be a procedure", line, col)

	// Numeric utilities (L09)
	case "builtin:abs":
		if len(args) != 1 || args[0].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: 'abs' expects a number", line, col)
		}
		v := args[0].IntVal
		if v < 0 {
			v = -v
		}
		return IntValue(v), nil

	case "builtin:zero?":
		if len(args) != 1 || args[0].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: 'zero?' expects a number", line, col)
		}
		return BoolValue(args[0].IntVal == 0), nil

	case "builtin:positive?":
		if len(args) != 1 || args[0].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: 'positive?' expects a number", line, col)
		}
		return BoolValue(args[0].IntVal > 0), nil

	case "builtin:negative?":
		if len(args) != 1 || args[0].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: 'negative?' expects a number", line, col)
		}
		return BoolValue(args[0].IntVal < 0), nil

	case "builtin:odd?":
		if len(args) != 1 || args[0].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: 'odd?' expects a number", line, col)
		}
		return BoolValue(args[0].IntVal%2 != 0), nil

	case "builtin:even?":
		if len(args) != 1 || args[0].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: 'even?' expects a number", line, col)
		}
		return BoolValue(args[0].IntVal%2 == 0), nil

	case "builtin:modulo":
		if len(args) != 2 || args[0].Type != TypeInteger || args[1].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: 'modulo' expects two numbers", line, col)
		}
		if args[1].IntVal == 0 {
			return nil, fmt.Errorf("%d:%d: division by zero", line, col)
		}
		// Scheme modulo: result has the sign of the divisor
		a, b := args[0].IntVal, args[1].IntVal
		r := a % b
		if r != 0 && (r > 0) != (b > 0) {
			r += b
		}
		return IntValue(r), nil

	case "builtin:remainder":
		if len(args) != 2 || args[0].Type != TypeInteger || args[1].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: 'remainder' expects two numbers", line, col)
		}
		if args[1].IntVal == 0 {
			return nil, fmt.Errorf("%d:%d: division by zero", line, col)
		}
		return IntValue(args[0].IntVal % args[1].IntVal), nil

	case "builtin:quotient":
		if len(args) != 2 || args[0].Type != TypeInteger || args[1].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: 'quotient' expects two numbers", line, col)
		}
		if args[1].IntVal == 0 {
			return nil, fmt.Errorf("%d:%d: division by zero", line, col)
		}
		// Truncate toward zero (Go's default integer division behavior)
		return IntValue(args[0].IntVal / args[1].IntVal), nil

	case "builtin:min":
		if len(args) == 0 {
			return nil, fmt.Errorf("%d:%d: 'min' requires at least one argument", line, col)
		}
		if args[0].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: 'min' expects numbers", line, col)
		}
		result := args[0].IntVal
		for _, a := range args[1:] {
			if a.Type != TypeInteger {
				return nil, fmt.Errorf("%d:%d: 'min' expects numbers", line, col)
			}
			if a.IntVal < result {
				result = a.IntVal
			}
		}
		return IntValue(result), nil

	case "builtin:max":
		if len(args) == 0 {
			return nil, fmt.Errorf("%d:%d: 'max' requires at least one argument", line, col)
		}
		if args[0].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: 'max' expects numbers", line, col)
		}
		result := args[0].IntVal
		for _, a := range args[1:] {
			if a.Type != TypeInteger {
				return nil, fmt.Errorf("%d:%d: 'max' expects numbers", line, col)
			}
			if a.IntVal > result {
				result = a.IntVal
			}
		}
		return IntValue(result), nil

	case "builtin:expt":
		if len(args) != 2 || args[0].Type != TypeInteger || args[1].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: 'expt' expects two numbers", line, col)
		}
		base, exp := args[0].IntVal, args[1].IntVal
		var result int64 = 1
		for i := int64(0); i < exp; i++ {
			result *= base
		}
		return IntValue(result), nil

	// Char operations (L09)
	case "builtin:char=?":
		if len(args) != 2 || args[0].Type != TypeChar || args[1].Type != TypeChar {
			return nil, fmt.Errorf("%d:%d: 'char=?' expects two chars", line, col)
		}
		return BoolValue(args[0].CharVal == args[1].CharVal), nil

	case "builtin:char<?":
		if len(args) != 2 || args[0].Type != TypeChar || args[1].Type != TypeChar {
			return nil, fmt.Errorf("%d:%d: 'char<?' expects two chars", line, col)
		}
		return BoolValue(args[0].CharVal < args[1].CharVal), nil

	case "builtin:char-alphabetic?":
		if len(args) != 1 || args[0].Type != TypeChar {
			return nil, fmt.Errorf("%d:%d: 'char-alphabetic?' expects a char", line, col)
		}
		c := args[0].CharVal
		return BoolValue((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')), nil

	case "builtin:char-numeric?":
		if len(args) != 1 || args[0].Type != TypeChar {
			return nil, fmt.Errorf("%d:%d: 'char-numeric?' expects a char", line, col)
		}
		c := args[0].CharVal
		return BoolValue(c >= '0' && c <= '9'), nil

	case "builtin:char-upcase":
		if len(args) != 1 || args[0].Type != TypeChar {
			return nil, fmt.Errorf("%d:%d: 'char-upcase' expects a char", line, col)
		}
		c := args[0].CharVal
		if c >= 'a' && c <= 'z' {
			c = c - 'a' + 'A'
		}
		return CharValue(c), nil

	case "builtin:char-downcase":
		if len(args) != 1 || args[0].Type != TypeChar {
			return nil, fmt.Errorf("%d:%d: 'char-downcase' expects a char", line, col)
		}
		c := args[0].CharVal
		if c >= 'A' && c <= 'Z' {
			c = c - 'A' + 'a'
		}
		return CharValue(c), nil

	// String comparisons (L09)
	case "builtin:string=?":
		if len(args) != 2 || args[0].Type != TypeString || args[1].Type != TypeString {
			return nil, fmt.Errorf("%d:%d: 'string=?' expects two strings", line, col)
		}
		return BoolValue(args[0].StrVal == args[1].StrVal), nil

	case "builtin:string<?":
		if len(args) != 2 || args[0].Type != TypeString || args[1].Type != TypeString {
			return nil, fmt.Errorf("%d:%d: 'string<?' expects two strings", line, col)
		}
		return BoolValue(args[0].StrVal < args[1].StrVal), nil

	case "builtin:string-ci=?":
		if len(args) != 2 || args[0].Type != TypeString || args[1].Type != TypeString {
			return nil, fmt.Errorf("%d:%d: 'string-ci=?' expects two strings", line, col)
		}
		return BoolValue(strings.EqualFold(args[0].StrVal, args[1].StrVal)), nil

	case "builtin:string-upcase":
		if len(args) != 1 || args[0].Type != TypeString {
			return nil, fmt.Errorf("%d:%d: 'string-upcase' expects a string", line, col)
		}
		return StringValue(strings.ToUpper(args[0].StrVal)), nil

	case "builtin:string-downcase":
		if len(args) != 1 || args[0].Type != TypeString {
			return nil, fmt.Errorf("%d:%d: 'string-downcase' expects a string", line, col)
		}
		return StringValue(strings.ToLower(args[0].StrVal)), nil

	// List operations (L09)
	case "builtin:list?":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'list?' expects 1 argument", line, col)
		}
		cur := args[0]
		for cur.Type == TypePair {
			cur = cur.Cdr
		}
		return BoolValue(cur.Type == TypeNull), nil

	case "builtin:list-ref":
		if len(args) != 2 || args[1].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: 'list-ref' expects a list and index", line, col)
		}
		idx := int(args[1].IntVal)
		cur := args[0]
		for i := 0; i < idx; i++ {
			if cur.Type != TypePair {
				return nil, fmt.Errorf("%d:%d: 'list-ref' index out of range", line, col)
			}
			cur = cur.Cdr
		}
		if cur.Type != TypePair {
			return nil, fmt.Errorf("%d:%d: 'list-ref' index out of range", line, col)
		}
		return cur.Car, nil

	case "builtin:list-tail":
		if len(args) != 2 || args[1].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: 'list-tail' expects a list and index", line, col)
		}
		idx := int(args[1].IntVal)
		cur := args[0]
		for i := 0; i < idx; i++ {
			if cur.Type != TypePair {
				return nil, fmt.Errorf("%d:%d: 'list-tail' index out of range", line, col)
			}
			cur = cur.Cdr
		}
		return cur, nil

	case "builtin:map":
		if len(args) < 2 {
			return nil, fmt.Errorf("%d:%d: 'map' requires a procedure and at least one list", line, col)
		}
		fn := args[0]
		lists := args[1:]
		var resultItems []*Value
		for {
			// Check if any list is exhausted
			allPairs := true
			for _, l := range lists {
				if l.Type != TypePair {
					allPairs = false
					break
				}
			}
			if !allPairs {
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
			if fn.Type == TypeSymbol && len(fn.StrVal) > 8 && fn.StrVal[:8] == "builtin:" {
				val, err = callBuiltin(fn.StrVal, callArgs, env, line, col)
			} else if fn.Type == TypeLambda {
				val, err = callLambda(fn, callArgs, line, col)
			} else {
				return nil, fmt.Errorf("%d:%d: 'map' first argument must be a procedure", line, col)
			}
			if err != nil {
				return nil, err
			}
			resultItems = append(resultItems, val)
			// Advance all lists
			for i, l := range lists {
				lists[i] = l.Cdr
			}
		}
		// Build result list
		result := Null
		for i := len(resultItems) - 1; i >= 0; i-- {
			result = PairValue(resultItems[i], result)
		}
		return result, nil

	case "builtin:assoc":
		if len(args) != 2 {
			return nil, fmt.Errorf("%d:%d: 'assoc' expects 2 arguments", line, col)
		}
		key := args[0]
		cur := args[1]
		for cur.Type == TypePair {
			pair := cur.Car
			if pair.Type == TypePair {
				if valuesEqual(key, pair.Car) {
					return pair, nil
				}
			}
			cur = cur.Cdr
		}
		return False, nil

	case "builtin:equal?":
		if len(args) != 2 {
			return nil, fmt.Errorf("%d:%d: 'equal?' expects 2 arguments", line, col)
		}
		return BoolValue(valuesEqual(args[0], args[1])), nil

	case "builtin:eq?":
		if len(args) != 2 {
			return nil, fmt.Errorf("%d:%d: 'eq?' expects 2 arguments", line, col)
		}
		return BoolValue(valuesEq(args[0], args[1])), nil
	}

	return nil, fmt.Errorf("%d:%d: unknown builtin %s", line, col, name)
}

func valuesEqual(a, b *Value) bool {
	if a.Type != b.Type {
		return false
	}
	switch a.Type {
	case TypeInteger:
		return a.IntVal == b.IntVal
	case TypeBoolean:
		return a.BoolVal == b.BoolVal
	case TypeString:
		return a.StrVal == b.StrVal
	case TypeSymbol:
		return a.StrVal == b.StrVal
	case TypeChar:
		return a.CharVal == b.CharVal
	case TypeNull:
		return true
	case TypePair:
		return valuesEqual(a.Car, b.Car) && valuesEqual(a.Cdr, b.Cdr)
	}
	return a == b
}

func valuesEq(a, b *Value) bool {
	if a == b {
		return true
	}
	if a.Type != b.Type {
		return false
	}
	switch a.Type {
	case TypeInteger:
		return a.IntVal == b.IntVal
	case TypeBoolean:
		return a.BoolVal == b.BoolVal
	case TypeSymbol:
		return a.StrVal == b.StrVal
	case TypeChar:
		return a.CharVal == b.CharVal
	case TypeNull:
		return true
	}
	return false
}
