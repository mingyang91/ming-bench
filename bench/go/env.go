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

	// Mutable strings (L06) — string-set! now errors (L15 immutability)
	env.Set("string-set!", &Value{Type: TypeSymbol, StrVal: "builtin:string-set!"})
	env.Set("string-copy", &Value{Type: TypeSymbol, StrVal: "builtin:string-copy"})

	// String/list/char conversions (L15)
	env.Set("string->list", &Value{Type: TypeSymbol, StrVal: "builtin:string->list"})
	env.Set("list->string", &Value{Type: TypeSymbol, StrVal: "builtin:list->string"})
	env.Set("char->integer", &Value{Type: TypeSymbol, StrVal: "builtin:char->integer"})
	env.Set("integer->char", &Value{Type: TypeSymbol, StrVal: "builtin:integer->char"})

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

	// Exact/inexact (L11)
	env.Set("exact?", &Value{Type: TypeSymbol, StrVal: "builtin:exact?"})
	env.Set("inexact?", &Value{Type: TypeSymbol, StrVal: "builtin:inexact?"})
	env.Set("exact->inexact", &Value{Type: TypeSymbol, StrVal: "builtin:exact->inexact"})
	env.Set("inexact->exact", &Value{Type: TypeSymbol, StrVal: "builtin:inexact->exact"})
	env.Set("numerator", &Value{Type: TypeSymbol, StrVal: "builtin:numerator"})
	env.Set("denominator", &Value{Type: TypeSymbol, StrVal: "builtin:denominator"})
	env.Set("integer?", &Value{Type: TypeSymbol, StrVal: "builtin:integer?"})
	env.Set("rational?", &Value{Type: TypeSymbol, StrVal: "builtin:rational?"})

	// List operations (L09)
	env.Set("list?", &Value{Type: TypeSymbol, StrVal: "builtin:list?"})
	env.Set("list-ref", &Value{Type: TypeSymbol, StrVal: "builtin:list-ref"})
	env.Set("list-tail", &Value{Type: TypeSymbol, StrVal: "builtin:list-tail"})
	env.Set("map", &Value{Type: TypeSymbol, StrVal: "builtin:map"})
	env.Set("assoc", &Value{Type: TypeSymbol, StrVal: "builtin:assoc"})
	env.Set("equal?", &Value{Type: TypeSymbol, StrVal: "builtin:equal?"})
	env.Set("eq?", &Value{Type: TypeSymbol, StrVal: "builtin:eq?"})
	env.Set("eqv?", &Value{Type: TypeSymbol, StrVal: "builtin:eqv?"})
	env.Set("procedure?", &Value{Type: TypeSymbol, StrVal: "builtin:procedure?"})

	// Vectors (L14)
	env.Set("vector", &Value{Type: TypeSymbol, StrVal: "builtin:vector"})
	env.Set("make-vector", &Value{Type: TypeSymbol, StrVal: "builtin:make-vector"})
	env.Set("vector-ref", &Value{Type: TypeSymbol, StrVal: "builtin:vector-ref"})
	env.Set("vector-set!", &Value{Type: TypeSymbol, StrVal: "builtin:vector-set!"})
	env.Set("vector-length", &Value{Type: TypeSymbol, StrVal: "builtin:vector-length"})
	env.Set("vector?", &Value{Type: TypeSymbol, StrVal: "builtin:vector?"})
	env.Set("vector->list", &Value{Type: TypeSymbol, StrVal: "builtin:vector->list"})
	env.Set("list->vector", &Value{Type: TypeSymbol, StrVal: "builtin:list->vector"})

	return env
}

func callBuiltin(name string, args []*Value, env *Env, line, col int) (*Value, error) {
	switch name {
	case "builtin:+":
		for _, a := range args {
			if !isNumeric(a) {
				return nil, fmt.Errorf("%d:%d: '+' expects numbers", line, col)
			}
		}
		if hasFloat(args) {
			var sum float64
			for _, a := range args {
				sum += toFloat64(a)
			}
			return FloatValue(sum), nil
		}
		if hasRational(args) {
			var rn, rd int64 = 0, 1
			for _, a := range args {
				an, ad := toRational(a)
				rn, rd = addRat(rn, rd, an, ad)
			}
			return RationalValue(rn, rd), nil
		}
		var sum int64
		for _, a := range args {
			sum += a.IntVal
		}
		return IntValue(sum), nil

	case "builtin:-":
		if len(args) == 0 {
			return nil, fmt.Errorf("%d:%d: '-' requires at least one argument", line, col)
		}
		for _, a := range args {
			if !isNumeric(a) {
				return nil, fmt.Errorf("%d:%d: '-' expects numbers", line, col)
			}
		}
		if len(args) == 1 {
			switch args[0].Type {
			case TypeFloat:
				return FloatValue(-args[0].FloatVal), nil
			case TypeRational:
				return RationalValue(-args[0].Num, args[0].Den), nil
			default:
				return IntValue(-args[0].IntVal), nil
			}
		}
		if hasFloat(args) {
			result := toFloat64(args[0])
			for _, a := range args[1:] {
				result -= toFloat64(a)
			}
			return FloatValue(result), nil
		}
		if hasRational(args) {
			rn, rd := toRational(args[0])
			for _, a := range args[1:] {
				an, ad := toRational(a)
				rn, rd = subRat(rn, rd, an, ad)
			}
			return RationalValue(rn, rd), nil
		}
		result := args[0].IntVal
		for _, a := range args[1:] {
			result -= a.IntVal
		}
		return IntValue(result), nil

	case "builtin:*":
		for _, a := range args {
			if !isNumeric(a) {
				return nil, fmt.Errorf("%d:%d: '*' expects numbers", line, col)
			}
		}
		if hasFloat(args) {
			var product float64 = 1
			for _, a := range args {
				product *= toFloat64(a)
			}
			return FloatValue(product), nil
		}
		if hasRational(args) {
			var rn, rd int64 = 1, 1
			for _, a := range args {
				an, ad := toRational(a)
				rn, rd = mulRat(rn, rd, an, ad)
			}
			return RationalValue(rn, rd), nil
		}
		var product int64 = 1
		for _, a := range args {
			product *= a.IntVal
		}
		return IntValue(product), nil

	case "builtin:/":
		if len(args) < 2 {
			return nil, fmt.Errorf("%d:%d: '/' requires at least two arguments", line, col)
		}
		for _, a := range args {
			if !isNumeric(a) {
				return nil, fmt.Errorf("%d:%d: '/' expects numbers", line, col)
			}
		}
		if hasFloat(args) {
			result := toFloat64(args[0])
			for _, a := range args[1:] {
				f := toFloat64(a)
				if f == 0 {
					return nil, fmt.Errorf("%d:%d: division by zero", line, col)
				}
				result /= f
			}
			return FloatValue(result), nil
		}
		// Exact division: use rationals
		rn, rd := toRational(args[0])
		for _, a := range args[1:] {
			an, ad := toRational(a)
			if an == 0 {
				return nil, fmt.Errorf("%d:%d: division by zero", line, col)
			}
			rn, rd = divRat(rn, rd, an, ad)
		}
		return RationalValue(rn, rd), nil

	case "builtin:<":
		if len(args) != 2 || !isNumeric(args[0]) || !isNumeric(args[1]) {
			return nil, fmt.Errorf("%d:%d: '<' expects two numbers", line, col)
		}
		return BoolValue(numericLess(args[0], args[1])), nil

	case "builtin:>":
		if len(args) != 2 || !isNumeric(args[0]) || !isNumeric(args[1]) {
			return nil, fmt.Errorf("%d:%d: '>' expects two numbers", line, col)
		}
		return BoolValue(numericLess(args[1], args[0])), nil

	case "builtin:=":
		if len(args) != 2 || !isNumeric(args[0]) || !isNumeric(args[1]) {
			return nil, fmt.Errorf("%d:%d: '=' expects two numbers", line, col)
		}
		return BoolValue(numericEqual(args[0], args[1])), nil

	case "builtin:<=":
		if len(args) != 2 || !isNumeric(args[0]) || !isNumeric(args[1]) {
			return nil, fmt.Errorf("%d:%d: '<=' expects two numbers", line, col)
		}
		return BoolValue(!numericLess(args[1], args[0])), nil

	case "builtin:not":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'not' expects one argument", line, col)
		}
		return BoolValue(!isTruthy(args[0])), nil

	case "builtin:>=":
		if len(args) != 2 || !isNumeric(args[0]) || !isNumeric(args[1]) {
			return nil, fmt.Errorf("%d:%d: '>=' expects two numbers", line, col)
		}
		return BoolValue(!numericLess(args[0], args[1])), nil

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
		return BoolValue(isNumeric(args[0])), nil

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
		if len(args) != 1 || !isNumeric(args[0]) {
			return nil, fmt.Errorf("%d:%d: 'number->string' expects a number", line, col)
		}
		return StringValue(args[0].Display()), nil

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
			return nil, fmt.Errorf("%d:%d: 'string-set!' expects a string, an integer, and a character", line, col)
		}
		if !args[0].Mutable {
			return nil, fmt.Errorf("%d:%d: string is immutable", line, col)
		}
		runes := []rune(args[0].StrVal)
		idx := int(args[1].IntVal)
		if idx < 0 || idx >= len(runes) {
			return nil, fmt.Errorf("%d:%d: string-set! index out of range", line, col)
		}
		runes[idx] = args[2].CharVal
		args[0].StrVal = string(runes)
		return Void, nil

	case "builtin:string-copy":
		if len(args) != 1 || args[0].Type != TypeString {
			return nil, fmt.Errorf("%d:%d: 'string-copy' expects a string", line, col)
		}
		v := StringValue(args[0].StrVal)
		v.Mutable = true
		return v, nil

	case "builtin:string->list":
		if len(args) != 1 || args[0].Type != TypeString {
			return nil, fmt.Errorf("%d:%d: 'string->list' expects a string", line, col)
		}
		runes := []rune(args[0].StrVal)
		result := Null
		for i := len(runes) - 1; i >= 0; i-- {
			result = PairValue(CharValue(runes[i]), result)
		}
		return result, nil

	case "builtin:list->string":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'list->string' expects a list", line, col)
		}
		var runes []rune
		cur := args[0]
		for cur.Type == TypePair {
			if cur.Car.Type != TypeChar {
				return nil, fmt.Errorf("%d:%d: 'list->string' expects a list of chars", line, col)
			}
			runes = append(runes, cur.Car.CharVal)
			cur = cur.Cdr
		}
		return StringValue(string(runes)), nil

	case "builtin:char->integer":
		if len(args) != 1 || args[0].Type != TypeChar {
			return nil, fmt.Errorf("%d:%d: 'char->integer' expects a char", line, col)
		}
		return IntValue(int64(args[0].CharVal)), nil

	case "builtin:integer->char":
		if len(args) != 1 || args[0].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: 'integer->char' expects an integer", line, col)
		}
		return CharValue(rune(args[0].IntVal)), nil

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
		if fn.Type == TypeCaseLambda {
			return callCaseLambda(fn, allArgs, line, col)
		}
		if fn.Type == TypeGoFunc {
			return fn.GoFunc(allArgs)
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
			} else if fn.Type == TypeCaseLambda {
				val, err = callCaseLambda(fn, callArgs, line, col)
			} else if fn.Type == TypeGoFunc {
				val, err = fn.GoFunc(callArgs)
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

	// L11: Exact arithmetic & rationals
	case "builtin:exact?":
		if len(args) != 1 || !isNumeric(args[0]) {
			return nil, fmt.Errorf("%d:%d: 'exact?' expects a number", line, col)
		}
		return BoolValue(args[0].Type == TypeInteger || args[0].Type == TypeRational), nil

	case "builtin:inexact?":
		if len(args) != 1 || !isNumeric(args[0]) {
			return nil, fmt.Errorf("%d:%d: 'inexact?' expects a number", line, col)
		}
		return BoolValue(args[0].Type == TypeFloat), nil

	case "builtin:exact->inexact":
		if len(args) != 1 || !isNumeric(args[0]) {
			return nil, fmt.Errorf("%d:%d: 'exact->inexact' expects a number", line, col)
		}
		return FloatValue(toFloat64(args[0])), nil

	case "builtin:inexact->exact":
		if len(args) != 1 || !isNumeric(args[0]) {
			return nil, fmt.Errorf("%d:%d: 'inexact->exact' expects a number", line, col)
		}
		if args[0].Type == TypeInteger || args[0].Type == TypeRational {
			return args[0], nil // already exact
		}
		num, den := floatToRational(args[0].FloatVal)
		return RationalValue(num, den), nil

	case "builtin:numerator":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'numerator' expects 1 argument", line, col)
		}
		switch args[0].Type {
		case TypeInteger:
			return args[0], nil
		case TypeRational:
			return IntValue(args[0].Num), nil
		default:
			return nil, fmt.Errorf("%d:%d: 'numerator' expects an exact number", line, col)
		}

	case "builtin:denominator":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'denominator' expects 1 argument", line, col)
		}
		switch args[0].Type {
		case TypeInteger:
			return IntValue(1), nil
		case TypeRational:
			return IntValue(args[0].Den), nil
		default:
			return nil, fmt.Errorf("%d:%d: 'denominator' expects an exact number", line, col)
		}

	case "builtin:integer?":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'integer?' expects 1 argument", line, col)
		}
		return BoolValue(args[0].Type == TypeInteger), nil

	case "builtin:rational?":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'rational?' expects 1 argument", line, col)
		}
		return BoolValue(args[0].Type == TypeInteger || args[0].Type == TypeRational), nil

	case "builtin:procedure?":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'procedure?' expects 1 argument", line, col)
		}
		t := args[0].Type
		isProcedure := t == TypeLambda || t == TypeGoFunc || t == TypeCaseLambda ||
			(t == TypeSymbol && len(args[0].StrVal) > 8 && args[0].StrVal[:8] == "builtin:")
		return BoolValue(isProcedure), nil

	case "builtin:eqv?":
		if len(args) != 2 {
			return nil, fmt.Errorf("%d:%d: 'eqv?' expects 2 arguments", line, col)
		}
		return BoolValue(valuesEqv(args[0], args[1])), nil

	// Vectors (L14)
	case "builtin:vector":
		elems := make([]*Value, len(args))
		copy(elems, args)
		return VectorVal(elems), nil

	case "builtin:make-vector":
		if len(args) < 1 || len(args) > 2 || args[0].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: 'make-vector' expects size and optional fill", line, col)
		}
		size := int(args[0].IntVal)
		fill := IntValue(0)
		if len(args) == 2 {
			fill = args[1]
		}
		elems := make([]*Value, size)
		for i := range elems {
			elems[i] = fill
		}
		return VectorVal(elems), nil

	case "builtin:vector-ref":
		if len(args) != 2 || args[0].Type != TypeVector || args[1].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: 'vector-ref' expects vector and index", line, col)
		}
		idx := int(args[1].IntVal)
		if idx < 0 || idx >= len(args[0].VecElems) {
			return nil, fmt.Errorf("%d:%d: 'vector-ref' index out of range", line, col)
		}
		return args[0].VecElems[idx], nil

	case "builtin:vector-set!":
		if len(args) != 3 || args[0].Type != TypeVector || args[1].Type != TypeInteger {
			return nil, fmt.Errorf("%d:%d: 'vector-set!' expects vector, index, value", line, col)
		}
		idx := int(args[1].IntVal)
		if idx < 0 || idx >= len(args[0].VecElems) {
			return nil, fmt.Errorf("%d:%d: 'vector-set!' index out of range", line, col)
		}
		args[0].VecElems[idx] = args[2]
		return Void, nil

	case "builtin:vector-length":
		if len(args) != 1 || args[0].Type != TypeVector {
			return nil, fmt.Errorf("%d:%d: 'vector-length' expects a vector", line, col)
		}
		return IntValue(int64(len(args[0].VecElems))), nil

	case "builtin:vector?":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'vector?' expects 1 argument", line, col)
		}
		return BoolValue(args[0].Type == TypeVector), nil

	case "builtin:vector->list":
		if len(args) != 1 || args[0].Type != TypeVector {
			return nil, fmt.Errorf("%d:%d: 'vector->list' expects a vector", line, col)
		}
		result := Null
		for i := len(args[0].VecElems) - 1; i >= 0; i-- {
			result = PairValue(args[0].VecElems[i], result)
		}
		return result, nil

	case "builtin:list->vector":
		if len(args) != 1 {
			return nil, fmt.Errorf("%d:%d: 'list->vector' expects a list", line, col)
		}
		var elems []*Value
		cur := args[0]
		for cur.Type == TypePair {
			elems = append(elems, cur.Car)
			cur = cur.Cdr
		}
		return VectorVal(elems), nil
	}

	return nil, fmt.Errorf("%d:%d: unknown builtin %s", line, col, name)
}

func valuesEqual(a, b *Value) bool {
	// Numeric cross-type comparison
	if isNumeric(a) && isNumeric(b) {
		return numericEqual(a, b)
	}
	if a.Type != b.Type {
		return false
	}
	switch a.Type {
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
	case TypeVector:
		if len(a.VecElems) != len(b.VecElems) {
			return false
		}
		for i := range a.VecElems {
			if !valuesEqual(a.VecElems[i], b.VecElems[i]) {
				return false
			}
		}
		return true
	}
	return a == b
}

func valuesEqv(a, b *Value) bool {
	if a == b {
		return true
	}
	if isNumeric(a) && isNumeric(b) {
		return numericEqual(a, b)
	}
	if a.Type != b.Type {
		return false
	}
	switch a.Type {
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
	case TypeRational:
		return a.Num == b.Num && a.Den == b.Den
	case TypeFloat:
		return a.FloatVal == b.FloatVal
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
