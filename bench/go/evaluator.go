package ming

import (
	"fmt"
	"strconv"
	"strings"
)

// EvalStr evaluates one or more Scheme expressions and returns the string
// representation of the last result.
func EvalStr(input string) (string, error) {
	result, _, err := evalStrInternal(input)
	if err != nil {
		return "", ensureSourcePos(err)
	}
	return formatValue(result), nil
}

// EvalStrWithOutput evaluates Scheme expressions and returns both the result
// string and any captured output from display/write/newline.
func EvalStrWithOutput(input string) (result string, output string, err error) {
	value, output, err := evalStrInternal(input)
	if err != nil {
		return "", output, ensureSourcePos(err)
	}
	return formatValue(value), output, nil
}

type stringExpr struct {
	value string
	pos   sourcePos
}

type symbolExpr struct {
	name string
	key  string
	pos  sourcePos
}

type listExpr struct {
	elements []any
	pos      sourcePos
}

type sourcePos struct {
	line int
	col  int
}

type pairValue struct {
	car any
	cdr any
}

type mutableString struct {
	runes []rune
}

type charValue rune

type emptyListValue struct{}

type voidValue struct{}

type parser struct {
	input string
	pos   int
}

type bindingCell struct {
	value any
}

type env struct {
	parent   *env
	bindings map[string]*bindingCell
	macros   map[string]*syntaxMacro
}

type builtinFunc func(args []any) (any, error)

type builtinProc struct {
	name string
	fn   builtinFunc
}

type closure struct {
	params    []symbolExpr
	restParam symbolExpr
	hasRest   bool
	body      []any
	env       *env
}

type caseClosure struct {
	clauses []closure
}

func newEnv(parent *env) *env {
	return &env{
		parent:   parent,
		bindings: map[string]*bindingCell{},
		macros:   map[string]*syntaxMacro{},
	}
}

func (s symbolExpr) bindingKey() string {
	if s.key != "" {
		return s.key
	}
	return s.name
}

func (e *env) define(name string, value any) {
	e.defineKey(name, value)
}

func (e *env) defineKey(key string, value any) {
	if cell, ok := e.bindings[key]; ok {
		cell.value = value
		return
	}
	e.bindings[key] = &bindingCell{value: value}
}

func (e *env) defineSymbol(symbol symbolExpr, value any) {
	e.defineKey(symbol.bindingKey(), value)
}

func (e *env) defineAlias(key string, cell *bindingCell) {
	e.bindings[key] = cell
}

func (e *env) lookup(name string) (any, bool) {
	return e.lookupByKey(name)
}

func (e *env) lookupByKey(key string) (any, bool) {
	cell, ok := e.lookupCellByKey(key)
	if !ok {
		return nil, false
	}
	return cell.value, true
}

func (e *env) lookupSymbol(symbol symbolExpr) (any, bool) {
	return e.lookupByKey(symbol.bindingKey())
}

func (e *env) lookupCellByKey(key string) (*bindingCell, bool) {
	for scope := e; scope != nil; scope = scope.parent {
		if cell, ok := scope.bindings[key]; ok {
			return cell, true
		}
	}
	return nil, false
}

func (e *env) set(name string, value any) bool {
	return e.setByKey(name, value)
}

func (e *env) setByKey(key string, value any) bool {
	cell, ok := e.lookupCellByKey(key)
	if !ok {
		return false
	}
	cell.value = value
	return true
}

func (e *env) setSymbol(symbol symbolExpr, value any) bool {
	return e.setByKey(symbol.bindingKey(), value)
}

func (e *env) defineMacro(name string, macro *syntaxMacro) {
	e.defineMacroKey(name, macro)
}

func (e *env) defineMacroKey(key string, macro *syntaxMacro) {
	e.macros[key] = macro
}

func (e *env) lookupMacro(name string) (*syntaxMacro, bool) {
	return e.lookupMacroByKey(name)
}

func (e *env) lookupMacroByKey(key string) (*syntaxMacro, bool) {
	for scope := e; scope != nil; scope = scope.parent {
		if macro, ok := scope.macros[key]; ok {
			return macro, true
		}
	}
	return nil, false
}

func (e *env) lookupMacroSymbol(symbol symbolExpr) (*syntaxMacro, bool) {
	return e.lookupMacroByKey(symbol.bindingKey())
}

func newGlobalEnv(output *strings.Builder) *env {
	scope := newEnv(nil)
	scope.define("+", builtinProc{name: "+", fn: builtinAdd})
	scope.define("-", builtinProc{name: "-", fn: builtinSub})
	scope.define("*", builtinProc{name: "*", fn: builtinMul})
	scope.define("/", builtinProc{name: "/", fn: builtinDiv})
	scope.define("<", builtinProc{name: "<", fn: func(args []any) (any, error) {
		return builtinCompare(args, func(order int) bool { return order < 0 })
	}})
	scope.define(">", builtinProc{name: ">", fn: func(args []any) (any, error) {
		return builtinCompare(args, func(order int) bool { return order > 0 })
	}})
	scope.define("=", builtinProc{name: "=", fn: func(args []any) (any, error) {
		return builtinCompare(args, func(order int) bool { return order == 0 })
	}})
	scope.define("<=", builtinProc{name: "<=", fn: func(args []any) (any, error) {
		return builtinCompare(args, func(order int) bool { return order <= 0 })
	}})
	scope.define("not", builtinProc{name: "not", fn: builtinNot})
	scope.define("cons", builtinProc{name: "cons", fn: builtinCons})
	scope.define("car", builtinProc{name: "car", fn: builtinCar})
	scope.define("cdr", builtinProc{name: "cdr", fn: builtinCdr})
	scope.define("null?", builtinProc{name: "null?", fn: func(args []any) (any, error) {
		return builtinPredicate("null?", args, func(value any) bool {
			_, ok := value.(emptyListValue)
			return ok
		})
	}})
	scope.define("append", builtinProc{name: "append", fn: builtinAppend})
	scope.define("list", builtinProc{name: "list", fn: builtinList})
	scope.define("length", builtinProc{name: "length", fn: builtinLength})
	scope.define("string?", builtinProc{name: "string?", fn: func(args []any) (any, error) {
		return builtinPredicate("string?", args, func(value any) bool {
			return isStringValue(value)
		})
	}})
	scope.define("number?", builtinProc{name: "number?", fn: func(args []any) (any, error) {
		return builtinPredicate("number?", args, func(value any) bool {
			return isNumberValue(value)
		})
	}})
	scope.define("exact?", builtinProc{name: "exact?", fn: builtinExactPredicate})
	scope.define("inexact?", builtinProc{name: "inexact?", fn: builtinInexactPredicate})
	scope.define("integer?", builtinProc{name: "integer?", fn: builtinIntegerPredicate})
	scope.define("rational?", builtinProc{name: "rational?", fn: builtinRationalPredicate})
	scope.define("boolean?", builtinProc{name: "boolean?", fn: func(args []any) (any, error) {
		return builtinPredicate("boolean?", args, func(value any) bool {
			_, ok := value.(bool)
			return ok
		})
	}})
	scope.define("pair?", builtinProc{name: "pair?", fn: func(args []any) (any, error) {
		return builtinPredicate("pair?", args, func(value any) bool {
			_, ok := value.(pairValue)
			return ok
		})
	}})
	scope.define("symbol?", builtinProc{name: "symbol?", fn: func(args []any) (any, error) {
		return builtinPredicate("symbol?", args, func(value any) bool {
			_, ok := value.(symbolExpr)
			return ok
		})
	}})
	scope.define("display", builtinProc{name: "display", fn: func(args []any) (any, error) {
		return builtinDisplay(args, output)
	}})
	scope.define("write", builtinProc{name: "write", fn: func(args []any) (any, error) {
		return builtinWrite(args, output)
	}})
	scope.define("newline", builtinProc{name: "newline", fn: func(args []any) (any, error) {
		return builtinNewline(args, output)
	}})
	scope.define("string-append", builtinProc{name: "string-append", fn: builtinStringAppend})
	scope.define("string-length", builtinProc{name: "string-length", fn: builtinStringLength})
	scope.define("substring", builtinProc{name: "substring", fn: builtinSubstring})
	scope.define("string->number", builtinProc{name: "string->number", fn: builtinStringToNumber})
	scope.define("number->string", builtinProc{name: "number->string", fn: builtinNumberToString})
	scope.define("symbol->string", builtinProc{name: "symbol->string", fn: builtinSymbolToString})
	scope.define("string->symbol", builtinProc{name: "string->symbol", fn: builtinStringToSymbol})
	scope.define("string-ref", builtinProc{name: "string-ref", fn: builtinStringRef})
	scope.define("string-copy", builtinProc{name: "string-copy", fn: builtinStringCopy})
	scope.define("string-set!", builtinProc{name: "string-set!", fn: builtinStringSet})
	scope.define("exact->inexact", builtinProc{name: "exact->inexact", fn: builtinExactToInexact})
	scope.define("inexact->exact", builtinProc{name: "inexact->exact", fn: builtinInexactToExact})
	scope.define("numerator", builtinProc{name: "numerator", fn: builtinNumerator})
	scope.define("denominator", builtinProc{name: "denominator", fn: builtinDenominator})
	scope.define("abs", builtinProc{name: "abs", fn: builtinAbs})
	scope.define("modulo", builtinProc{name: "modulo", fn: builtinModulo})
	scope.define("remainder", builtinProc{name: "remainder", fn: builtinRemainder})
	scope.define("quotient", builtinProc{name: "quotient", fn: builtinQuotient})
	scope.define("min", builtinProc{name: "min", fn: builtinMin})
	scope.define("max", builtinProc{name: "max", fn: builtinMax})
	scope.define("expt", builtinProc{name: "expt", fn: builtinExpt})
	scope.define("zero?", builtinProc{name: "zero?", fn: builtinZero})
	scope.define("positive?", builtinProc{name: "positive?", fn: builtinPositive})
	scope.define("negative?", builtinProc{name: "negative?", fn: builtinNegative})
	scope.define("odd?", builtinProc{name: "odd?", fn: builtinOdd})
	scope.define("even?", builtinProc{name: "even?", fn: builtinEven})
	scope.define("list-ref", builtinProc{name: "list-ref", fn: builtinListRef})
	scope.define("list-tail", builtinProc{name: "list-tail", fn: builtinListTail})
	scope.define("list?", builtinProc{name: "list?", fn: builtinListP})
	scope.define("eq?", builtinProc{name: "eq?", fn: builtinEq})
	scope.define("equal?", builtinProc{name: "equal?", fn: builtinEqual})
	scope.define("assoc", builtinProc{name: "assoc", fn: builtinAssoc})
	scope.define("map", builtinProc{name: "map", fn: builtinMap})
	scope.define("char?", builtinProc{name: "char?", fn: func(args []any) (any, error) {
		return builtinPredicate("char?", args, func(value any) bool {
			_, ok := value.(charValue)
			return ok
		})
	}})
	scope.define("char-alphabetic?", builtinProc{name: "char-alphabetic?", fn: builtinCharAlphabetic})
	scope.define("char-numeric?", builtinProc{name: "char-numeric?", fn: builtinCharNumeric})
	scope.define("char-upcase", builtinProc{name: "char-upcase", fn: builtinCharUpcase})
	scope.define("char-downcase", builtinProc{name: "char-downcase", fn: builtinCharDowncase})
	scope.define("char=?", builtinProc{name: "char=?", fn: builtinCharEqual})
	scope.define("char<?", builtinProc{name: "char<?", fn: builtinCharLess})
	scope.define("string=?", builtinProc{name: "string=?", fn: builtinStringEqual})
	scope.define("string<?", builtinProc{name: "string<?", fn: builtinStringLess})
	scope.define("string-ci=?", builtinProc{name: "string-ci=?", fn: builtinStringCIEqual})
	scope.define("string-upcase", builtinProc{name: "string-upcase", fn: builtinStringUpcase})
	scope.define("string-downcase", builtinProc{name: "string-downcase", fn: builtinStringDowncase})
	scope.define("apply", builtinProc{name: "apply", fn: builtinApply})
	scope.define("procedure?", builtinProc{name: "procedure?", fn: builtinProcedurePredicate})
	return scope
}

func evalStrInternal(input string) (any, string, error) {
	var output strings.Builder
	p := parser{input: input}
	exprs, err := p.parseProgram()
	if err != nil {
		return nil, "", err
	}
	if len(exprs) == 0 {
		return nil, "", sourcePos{line: 1, col: 1}.errorf("empty input")
	}

	scope := newGlobalEnv(&output)
	result := any(voidValue{})
	for _, expr := range exprs {
		result, err = eval(scope, expr)
		if err != nil {
			return nil, output.String(), err
		}
	}

	return result, output.String(), nil
}

func (p *parser) parseProgram() ([]any, error) {
	var exprs []any
	for {
		p.skipIgnorable()
		if p.atEnd() {
			return exprs, nil
		}

		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, expr)
	}
}

func (p *parser) parseExpr() (any, error) {
	p.skipIgnorable()
	if p.atEnd() {
		return nil, p.currentPos().errorf("unexpected end of input")
	}

	switch p.peek() {
	case '(':
		return p.parseList()
	case '"':
		return p.parseString()
	case '\'':
		return p.parseQuoteShorthand()
	default:
		return p.parseAtom()
	}
}

func (p *parser) parseQuoteShorthand() (any, error) {
	start := p.pos
	p.pos++

	expr, err := p.parseExpr()
	if err != nil {
		return nil, err
	}

	pos := p.posAt(start)
	return listExpr{
		elements: []any{
			symbolExpr{name: "quote", pos: pos},
			expr,
		},
		pos: pos,
	}, nil
}

func (p *parser) parseList() (any, error) {
	start := p.pos
	p.pos++

	var elements []any
	for {
		p.skipIgnorable()
		if p.atEnd() {
			return nil, p.posAt(start).errorf("unterminated list")
		}
		if p.peek() == ')' {
			p.pos++
			return listExpr{elements: elements, pos: p.posAt(start)}, nil
		}

		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		elements = append(elements, expr)
	}
}

func (p *parser) parseString() (any, error) {
	start := p.pos
	p.pos++

	var b strings.Builder
	for !p.atEnd() {
		ch := p.peek()
		p.pos++

		switch ch {
		case '"':
			return stringExpr{value: b.String(), pos: p.posAt(start)}, nil
		case '\\':
			if p.atEnd() {
				return nil, p.posAt(start).errorf("unterminated string")
			}

			escaped := p.peek()
			p.pos++
			switch escaped {
			case 'n':
				b.WriteByte('\n')
			case 't':
				b.WriteByte('\t')
			case '"':
				b.WriteByte('"')
			case '\\':
				b.WriteByte('\\')
			default:
				b.WriteByte(escaped)
			}
		default:
			b.WriteByte(ch)
		}
	}

	return nil, p.posAt(start).errorf("unterminated string")
}

func (p *parser) parseAtom() (any, error) {
	start := p.pos
	for !p.atEnd() {
		ch := p.peek()
		if isDelimiter(ch) {
			break
		}
		p.pos++
	}

	token := p.input[start:p.pos]
	if token == "" {
		return nil, p.posAt(start).errorf("unexpected token")
	}

	switch token {
	case "#t":
		return true, nil
	case "#f":
		return false, nil
	}

	if strings.HasPrefix(token, "#\\") {
		charName := token[2:]
		switch charName {
		case "space":
			return charValue(' '), nil
		case "newline":
			return charValue('\n'), nil
		}

		runes := []rune(charName)
		if len(runes) == 1 {
			return charValue(runes[0]), nil
		}

		return nil, p.posAt(start).errorf("invalid character literal")
	}

	if number, ok, err := parseNumberLiteral(token); ok {
		if err != nil {
			return nil, p.posAt(start).errorf(err.Error())
		}
		return number, nil
	}

	return symbolExpr{name: token, pos: p.posAt(start)}, nil
}

func (p *parser) skipIgnorable() {
	for !p.atEnd() {
		ch := p.peek()
		switch ch {
		case ' ', '\t', '\n', '\r':
			p.pos++
		case ';':
			for !p.atEnd() && p.peek() != '\n' {
				p.pos++
			}
		default:
			return
		}
	}
}

func (p *parser) atEnd() bool {
	return p.pos >= len(p.input)
}

func (p *parser) peek() byte {
	return p.input[p.pos]
}

func (p *parser) currentPos() sourcePos {
	return p.posAt(p.pos)
}

func (p *parser) posAt(offset int) sourcePos {
	if offset < 0 {
		offset = 0
	}
	if offset > len(p.input) {
		offset = len(p.input)
	}

	line := 1
	col := 1
	for i := 0; i < offset; i++ {
		if p.input[i] == '\n' {
			line++
			col = 1
			continue
		}
		col++
	}

	return sourcePos{line: line, col: col}
}

func (pos sourcePos) errorf(format string, args ...any) *EvalError {
	return &EvalError{
		Message: fmt.Sprintf(format, args...),
		Line:    pos.line,
		Col:     pos.col,
	}
}

func ensureSourcePos(err error) error {
	return attachSourcePos(err, sourcePos{line: 1, col: 1})
}

func attachSourcePos(err error, pos sourcePos) error {
	if err == nil {
		return nil
	}

	evalErr, ok := err.(*EvalError)
	if !ok {
		return pos.errorf("%s", err.Error())
	}
	if evalErr.Line > 0 && evalErr.Col > 0 {
		return err
	}
	return &EvalError{
		Message: evalErr.Message,
		Line:    pos.line,
		Col:     pos.col,
	}
}

func isDelimiter(ch byte) bool {
	switch ch {
	case ' ', '\t', '\n', '\r', '(', ')', ';':
		return true
	default:
		return false
	}
}

func eval(scope *env, expr any) (any, error) {
	switch node := expr.(type) {
	case int64:
		return node, nil
	case rationalValue:
		return node, nil
	case float64:
		return node, nil
	case bool:
		return node, nil
	case stringExpr:
		return node.value, nil
	case charValue:
		return node, nil
	case symbolExpr:
		value, ok := scope.lookupSymbol(node)
		if !ok {
			return nil, node.pos.errorf("unbound variable: %s", node.name)
		}
		return value, nil
	case listExpr:
		value, err := evalList(scope, node)
		if err != nil {
			return nil, attachSourcePos(err, node.pos)
		}
		return value, nil
	case string:
		return node, nil
	case *mutableString:
		return node, nil
	case builtinProc:
		return node, nil
	case closure:
		return node, nil
	case caseClosure:
		return node, nil
	case pairValue:
		return node, nil
	case emptyListValue:
		return node, nil
	case voidValue:
		return node, nil
	default:
		return nil, &EvalError{Message: "unsupported expression"}
	}
}

func evalList(scope *env, expr listExpr) (any, error) {
	if len(expr.elements) == 0 {
		return nil, &EvalError{Message: "cannot evaluate empty list"}
	}

	if head, ok := expr.elements[0].(symbolExpr); ok {
		args := expr.elements[1:]
		switch head.name {
		case "define":
			return evalDefine(scope, args)
		case "define-syntax":
			return evalDefineSyntax(scope, args)
		case "define-record-type":
			return evalDefineRecordType(scope, args)
		case "set!":
			return evalSet(scope, args)
		case "if":
			return evalIf(scope, args)
		case "quote":
			return evalQuote(args)
		case "lambda":
			return evalLambda(scope, args)
		case "case-lambda":
			return evalCaseLambda(scope, args)
		case "and":
			return evalAnd(scope, args)
		case "or":
			return evalOr(scope, args)
		case "begin":
			return evalBegin(scope, args)
		case "let":
			return evalLet(scope, args)
		case "cond":
			return evalCond(scope, args)
		}

		if macro, ok := scope.lookupMacroSymbol(head); ok {
			expanded, err := macro.expand(expr)
			if err != nil {
				return nil, err
			}
			return eval(scope, expanded)
		}
	}

	proc, err := eval(scope, expr.elements[0])
	if err != nil {
		return nil, err
	}

	args := make([]any, 0, len(expr.elements)-1)
	for _, argExpr := range expr.elements[1:] {
		arg, err := eval(scope, argExpr)
		if err != nil {
			return nil, err
		}
		args = append(args, arg)
	}

	return applyProcedure(proc, args)
}

func evalDefine(scope *env, args []any) (any, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "define expects a name and value"}
	}

	switch target := args[0].(type) {
	case symbolExpr:
		if len(args) != 2 {
			return nil, &EvalError{Message: "define variable form expects exactly 2 arguments"}
		}

		value, err := eval(scope, args[1])
		if err != nil {
			return nil, err
		}
		scope.defineSymbol(target, value)
		return voidValue{}, nil
	case listExpr:
		if len(target.elements) == 0 {
			return nil, &EvalError{Message: "define function form requires a name"}
		}

		name, ok := target.elements[0].(symbolExpr)
		if !ok {
			return nil, &EvalError{Message: "define function name must be a symbol"}
		}

		params, restParam, hasRest, err := parseParamList(target.elements[1:])
		if err != nil {
			return nil, err
		}

		proc := closure{
			params:    params,
			restParam: restParam,
			hasRest:   hasRest,
			body:      args[1:],
			env:       scope,
		}
		scope.defineSymbol(name, proc)
		return voidValue{}, nil
	default:
		return nil, &EvalError{Message: "define requires a symbol or function signature"}
	}
}

func evalDefineSyntax(scope *env, args []any) (any, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "define-syntax expects exactly 2 arguments"}
	}

	name, ok := args[0].(symbolExpr)
	if !ok {
		return nil, &EvalError{Message: "define-syntax requires a symbol"}
	}

	macro, err := parseSyntaxRules(name, args[1], scope)
	if err != nil {
		return nil, err
	}

	scope.defineMacroKey(name.bindingKey(), macro)
	return voidValue{}, nil
}

func evalSet(scope *env, args []any) (any, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "set! expects exactly 2 arguments"}
	}

	target, ok := args[0].(symbolExpr)
	if !ok {
		return nil, &EvalError{Message: "set! requires a symbol"}
	}

	value, err := eval(scope, args[1])
	if err != nil {
		return nil, err
	}

	if !scope.setSymbol(target, value) {
		return nil, target.pos.errorf("unbound variable: %s", target.name)
	}

	return voidValue{}, nil
}

func evalIf(scope *env, args []any) (any, error) {
	if len(args) != 3 {
		return nil, &EvalError{Message: "if expects exactly 3 arguments"}
	}

	cond, err := eval(scope, args[0])
	if err != nil {
		return nil, err
	}

	if isTruthy(cond) {
		return eval(scope, args[1])
	}
	return eval(scope, args[2])
}

func evalQuote(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "quote expects exactly 1 argument"}
	}
	return quoteDatum(args[0]), nil
}

func evalLambda(scope *env, args []any) (any, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "lambda expects parameters and a body"}
	}

	params, restParam, hasRest, err := parseFormals(args[0])
	if err != nil {
		return nil, err
	}

	return closure{
		params:    params,
		restParam: restParam,
		hasRest:   hasRest,
		body:      args[1:],
		env:       scope,
	}, nil
}

func evalCaseLambda(scope *env, args []any) (any, error) {
	if len(args) == 0 {
		return nil, &EvalError{Message: "case-lambda expects at least 1 clause"}
	}

	clauses := make([]closure, 0, len(args))
	for _, clauseExpr := range args {
		clause, ok := clauseExpr.(listExpr)
		if !ok || len(clause.elements) < 2 {
			return nil, exprSourcePos(clauseExpr).errorf("case-lambda clauses must include parameters and a body")
		}

		params, restParam, hasRest, err := parseFormals(clause.elements[0])
		if err != nil {
			return nil, err
		}

		clauses = append(clauses, closure{
			params:    params,
			restParam: restParam,
			hasRest:   hasRest,
			body:      clause.elements[1:],
			env:       scope,
		})
	}

	return caseClosure{clauses: clauses}, nil
}

func evalAnd(scope *env, args []any) (any, error) {
	result := any(true)
	for _, arg := range args {
		value, err := eval(scope, arg)
		if err != nil {
			return nil, err
		}
		if !isTruthy(value) {
			return value, nil
		}
		result = value
	}
	return result, nil
}

func evalOr(scope *env, args []any) (any, error) {
	result := any(false)
	for _, arg := range args {
		value, err := eval(scope, arg)
		if err != nil {
			return nil, err
		}
		if isTruthy(value) {
			return value, nil
		}
		result = value
	}
	return result, nil
}

func evalBegin(scope *env, args []any) (any, error) {
	return evalSequence(scope, args)
}

func evalLet(scope *env, args []any) (any, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "let expects bindings and a body"}
	}

	if name, ok := args[0].(symbolExpr); ok {
		if len(args) < 3 {
			return nil, &EvalError{Message: "named let expects bindings and a body"}
		}

		bindingsExpr, ok := args[1].(listExpr)
		if !ok {
			return nil, &EvalError{Message: "let bindings must be a list"}
		}

		params, values, err := evalBindings(scope, bindingsExpr.elements)
		if err != nil {
			return nil, err
		}

		letScope := newEnv(scope)
		proc := closure{
			params: params,
			body:   args[2:],
			env:    letScope,
		}
		letScope.defineSymbol(name, proc)
		return applyProcedure(proc, values)
	}

	bindingsExpr, ok := args[0].(listExpr)
	if !ok {
		return nil, &EvalError{Message: "let bindings must be a list"}
	}

	params, values, err := evalBindings(scope, bindingsExpr.elements)
	if err != nil {
		return nil, err
	}

	letScope := newEnv(scope)
	for i, param := range params {
		letScope.defineSymbol(param, values[i])
	}

	return evalSequence(letScope, args[1:])
}

func evalCond(scope *env, args []any) (any, error) {
	for i, clauseExpr := range args {
		clause, ok := clauseExpr.(listExpr)
		if !ok || len(clause.elements) == 0 {
			return nil, &EvalError{Message: "cond clauses must be non-empty lists"}
		}

		if symbol, ok := clause.elements[0].(symbolExpr); ok && symbol.name == "else" {
			if i != len(args)-1 {
				return nil, &EvalError{Message: "cond else clause must be last"}
			}
			if len(clause.elements) == 1 {
				return voidValue{}, nil
			}
			return evalSequence(scope, clause.elements[1:])
		}

		testValue, err := eval(scope, clause.elements[0])
		if err != nil {
			return nil, err
		}
		if !isTruthy(testValue) {
			continue
		}
		if len(clause.elements) == 1 {
			return testValue, nil
		}
		return evalSequence(scope, clause.elements[1:])
	}

	return voidValue{}, nil
}

func evalBindings(scope *env, bindings []any) ([]symbolExpr, []any, error) {
	names := make([]symbolExpr, 0, len(bindings))
	values := make([]any, 0, len(bindings))
	for _, bindingExpr := range bindings {
		binding, ok := bindingExpr.(listExpr)
		if !ok || len(binding.elements) != 2 {
			return nil, nil, &EvalError{Message: "let bindings must be name/value pairs"}
		}

		name, ok := binding.elements[0].(symbolExpr)
		if !ok {
			return nil, nil, &EvalError{Message: "let binding name must be a symbol"}
		}

		value, err := eval(scope, binding.elements[1])
		if err != nil {
			return nil, nil, err
		}

		names = append(names, name)
		values = append(values, value)
	}
	return names, values, nil
}

func parseFormals(formals any) ([]symbolExpr, symbolExpr, bool, error) {
	switch formals := formals.(type) {
	case symbolExpr:
		return nil, formals, true, nil
	case listExpr:
		return parseParamList(formals.elements)
	default:
		return nil, symbolExpr{}, false, &EvalError{Message: "lambda parameters must be a list or symbol"}
	}
}

func parseParamList(params []any) ([]symbolExpr, symbolExpr, bool, error) {
	names := make([]symbolExpr, 0, len(params))
	for i := 0; i < len(params); i++ {
		name, ok := params[i].(symbolExpr)
		if !ok {
			return nil, symbolExpr{}, false, &EvalError{Message: "parameter names must be symbols"}
		}

		if name.name == "." {
			if i == len(params)-1 {
				return nil, symbolExpr{}, false, &EvalError{Message: "dot must be followed by a rest parameter"}
			}

			rest, ok := params[i+1].(symbolExpr)
			if !ok || rest.name == "." {
				return nil, symbolExpr{}, false, &EvalError{Message: "rest parameter name must be a symbol"}
			}
			if i+2 != len(params) {
				return nil, symbolExpr{}, false, &EvalError{Message: "dot must appear before the final parameter"}
			}
			return names, rest, true, nil
		}

		names = append(names, name)
	}
	return names, symbolExpr{}, false, nil
}

func applyProcedure(proc any, args []any) (any, error) {
	switch callable := proc.(type) {
	case builtinProc:
		return callable.fn(args)
	case closure:
		if !closureMatchesArity(callable, len(args)) {
			return nil, closureArgCountError(callable, len(args))
		}
		return applyClosure(callable, args)
	case caseClosure:
		for _, clause := range callable.clauses {
			if closureMatchesArity(clause, len(args)) {
				return applyClosure(clause, args)
			}
		}
		return nil, &EvalError{Message: fmt.Sprintf("no matching case-lambda clause for %d arguments", len(args))}
	default:
		return nil, &EvalError{Message: fmt.Sprintf("expected procedure, got %s", typeName(proc))}
	}
}

func applyClosure(callable closure, args []any) (any, error) {
	callScope := newEnv(callable.env)
	for i, param := range callable.params {
		callScope.defineSymbol(param, args[i])
	}
	if callable.hasRest {
		callScope.defineSymbol(callable.restParam, makeListValue(args[len(callable.params):]))
	}

	return evalSequence(callScope, callable.body)
}

func closureMatchesArity(callable closure, argCount int) bool {
	if callable.hasRest {
		return argCount >= len(callable.params)
	}
	return argCount == len(callable.params)
}

func closureArgCountError(callable closure, argCount int) error {
	if callable.hasRest {
		return &EvalError{Message: fmt.Sprintf("expected at least %d arguments, got %d", len(callable.params), argCount)}
	}
	return &EvalError{Message: fmt.Sprintf("expected %d arguments, got %d", len(callable.params), argCount)}
}

func evalSequence(scope *env, exprs []any) (any, error) {
	result := any(voidValue{})
	for _, expr := range exprs {
		value, err := eval(scope, expr)
		if err != nil {
			return nil, err
		}
		result = value
	}
	return result, nil
}

func builtinNot(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "not expects exactly 1 argument"}
	}
	return !isTruthy(args[0]), nil
}

func builtinPredicate(name string, args []any, pred func(any) bool) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: fmt.Sprintf("%s expects exactly 1 argument", name)}
	}
	return pred(args[0]), nil
}

func builtinAdd(args []any) (any, error) {
	sum := any(int64(0))
	for _, arg := range args {
		next, err := addNumberValues(sum, arg)
		if err != nil {
			return nil, err
		}
		sum = next
	}
	return sum, nil
}

func builtinSub(args []any) (any, error) {
	if len(args) == 0 {
		return nil, &EvalError{Message: "- expects at least 1 argument"}
	}

	if len(args) == 1 {
		return negateNumber(args[0])
	}

	result := args[0]
	for _, arg := range args[1:] {
		next, err := subtractNumberValues(result, arg)
		if err != nil {
			return nil, err
		}
		result = next
	}
	return result, nil
}

func builtinMul(args []any) (any, error) {
	result := any(int64(1))
	for _, arg := range args {
		next, err := multiplyNumberValues(result, arg)
		if err != nil {
			return nil, err
		}
		result = next
	}
	return result, nil
}

func builtinDiv(args []any) (any, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "/ expects at least 2 arguments"}
	}

	result := args[0]
	for _, arg := range args[1:] {
		next, err := divideNumberValues(result, arg)
		if err != nil {
			return nil, err
		}
		result = next
	}

	return result, nil
}

func builtinCompare(args []any, cmp func(order int) bool) (any, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "comparison expects at least 2 arguments"}
	}

	prev := args[0]
	for _, arg := range args[1:] {
		order, err := compareNumberValues(prev, arg)
		if err != nil {
			return nil, err
		}
		if !cmp(order) {
			return false, nil
		}
		prev = arg
	}

	return true, nil
}

func builtinCons(args []any) (any, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "cons expects exactly 2 arguments"}
	}
	return pairValue{car: args[0], cdr: args[1]}, nil
}

func builtinCar(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "car expects exactly 1 argument"}
	}

	pair, ok := args[0].(pairValue)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("car expects a pair, got %s", typeName(args[0]))}
	}
	return pair.car, nil
}

func builtinCdr(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "cdr expects exactly 1 argument"}
	}

	pair, ok := args[0].(pairValue)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("cdr expects a pair, got %s", typeName(args[0]))}
	}
	return pair.cdr, nil
}

func builtinList(args []any) (any, error) {
	return makeListValue(args), nil
}

func builtinLength(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "length expects exactly 1 argument"}
	}

	var length int64
	current := args[0]
	for {
		switch value := current.(type) {
		case emptyListValue:
			return length, nil
		case pairValue:
			length++
			current = value.cdr
		default:
			return nil, &EvalError{Message: fmt.Sprintf("length expects a list, got %s", typeName(args[0]))}
		}
	}
}

func builtinAppend(args []any) (any, error) {
	if len(args) == 0 {
		return emptyListValue{}, nil
	}
	if len(args) == 1 {
		return args[0], nil
	}

	result := args[len(args)-1]
	for i := len(args) - 2; i >= 0; i-- {
		elements, err := properListElements(args[i], "append")
		if err != nil {
			return nil, err
		}
		for j := len(elements) - 1; j >= 0; j-- {
			result = pairValue{car: elements[j], cdr: result}
		}
	}

	return result, nil
}

func builtinDisplay(args []any, output *strings.Builder) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "display expects exactly 1 argument"}
	}
	writeOutput(output, formatDisplayValue(args[0]))
	return voidValue{}, nil
}

func builtinWrite(args []any, output *strings.Builder) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "write expects exactly 1 argument"}
	}
	writeOutput(output, formatValue(args[0]))
	return voidValue{}, nil
}

func builtinNewline(args []any, output *strings.Builder) (any, error) {
	if len(args) != 0 {
		return nil, &EvalError{Message: "newline expects exactly 0 arguments"}
	}
	writeOutput(output, "\n")
	return voidValue{}, nil
}

func builtinStringAppend(args []any) (any, error) {
	var b strings.Builder
	for _, arg := range args {
		s, err := expectString(arg)
		if err != nil {
			return nil, err
		}
		b.WriteString(s)
	}
	return b.String(), nil
}

func builtinStringLength(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "string-length expects exactly 1 argument"}
	}

	s, err := expectString(args[0])
	if err != nil {
		return nil, err
	}
	return int64(len([]rune(s))), nil
}

func builtinSubstring(args []any) (any, error) {
	if len(args) != 3 {
		return nil, &EvalError{Message: "substring expects exactly 3 arguments"}
	}

	s, err := expectString(args[0])
	if err != nil {
		return nil, err
	}

	start, err := expectNonNegativeIndex(args[1], "substring")
	if err != nil {
		return nil, err
	}
	end, err := expectNonNegativeIndex(args[2], "substring")
	if err != nil {
		return nil, err
	}

	runes := []rune(s)
	if start > end || end > int64(len(runes)) {
		return nil, &EvalError{Message: "substring index out of range"}
	}
	return string(runes[start:end]), nil
}

func builtinStringToNumber(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "string->number expects exactly 1 argument"}
	}

	s, err := expectString(args[0])
	if err != nil {
		return nil, err
	}

	number, ok, parseErr := parseNumberLiteral(s)
	if parseErr != nil || !ok {
		return false, nil
	}
	return number, nil
}

func builtinNumberToString(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "number->string expects exactly 1 argument"}
	}

	if !isNumberValue(args[0]) {
		return nil, &EvalError{Message: fmt.Sprintf("expected number, got %s", typeName(args[0]))}
	}
	return formatNumberValue(args[0]), nil
}

func builtinSymbolToString(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "symbol->string expects exactly 1 argument"}
	}

	symbol, err := expectSymbol(args[0])
	if err != nil {
		return nil, err
	}
	return symbol.name, nil
}

func builtinStringToSymbol(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "string->symbol expects exactly 1 argument"}
	}

	s, err := expectString(args[0])
	if err != nil {
		return nil, err
	}
	return symbolExpr{name: s}, nil
}

func builtinStringRef(args []any) (any, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "string-ref expects exactly 2 arguments"}
	}

	s, err := expectString(args[0])
	if err != nil {
		return nil, err
	}

	index, err := expectNonNegativeIndex(args[1], "string-ref")
	if err != nil {
		return nil, err
	}

	runes := []rune(s)
	if index >= int64(len(runes)) {
		return nil, &EvalError{Message: "string-ref index out of range"}
	}
	return charValue(runes[index]), nil
}

func builtinStringCopy(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "string-copy expects exactly 1 argument"}
	}

	s, err := expectString(args[0])
	if err != nil {
		return nil, err
	}

	return &mutableString{runes: []rune(s)}, nil
}

func builtinStringSet(args []any) (any, error) {
	if len(args) != 3 {
		return nil, &EvalError{Message: "string-set! expects exactly 3 arguments"}
	}

	s, err := expectMutableString(args[0])
	if err != nil {
		return nil, err
	}

	index, err := expectNonNegativeIndex(args[1], "string-set!")
	if err != nil {
		return nil, err
	}

	ch, ok := args[2].(charValue)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("expected char, got %s", typeName(args[2]))}
	}

	if index >= int64(len(s.runes)) {
		return nil, &EvalError{Message: "string-set! index out of range"}
	}

	s.runes[index] = rune(ch)
	return voidValue{}, nil
}

func builtinApply(args []any) (any, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "apply expects at least 2 arguments"}
	}

	restArgs, err := properListElements(args[len(args)-1], "apply")
	if err != nil {
		return nil, err
	}

	callArgs := make([]any, 0, len(args)-2+len(restArgs))
	callArgs = append(callArgs, args[1:len(args)-1]...)
	callArgs = append(callArgs, restArgs...)

	return applyProcedure(args[0], callArgs)
}

func builtinProcedurePredicate(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "procedure? expects exactly 1 argument"}
	}
	return isProcedureValue(args[0]), nil
}

func expectInt(value any) (int64, error) {
	n, ok := value.(int64)
	if !ok {
		return 0, &EvalError{Message: fmt.Sprintf("expected number, got %s", typeName(value))}
	}
	return n, nil
}

func expectString(value any) (string, error) {
	switch s := value.(type) {
	case string:
		return s, nil
	case *mutableString:
		return string(s.runes), nil
	default:
		return "", &EvalError{Message: fmt.Sprintf("expected string, got %s", typeName(value))}
	}
}

func expectSymbol(value any) (symbolExpr, error) {
	symbol, ok := value.(symbolExpr)
	if !ok {
		return symbolExpr{}, &EvalError{Message: fmt.Sprintf("expected symbol, got %s", typeName(value))}
	}
	return symbol, nil
}

func expectNonNegativeIndex(value any, builtinName string) (int64, error) {
	index, err := expectInt(value)
	if err != nil {
		return 0, err
	}
	if index < 0 {
		return 0, &EvalError{Message: fmt.Sprintf("%s expects a non-negative index", builtinName)}
	}
	return index, nil
}

func expectMutableString(value any) (*mutableString, error) {
	s, ok := value.(*mutableString)
	if !ok {
		return nil, &EvalError{Message: fmt.Sprintf("expected mutable string, got %s", typeName(value))}
	}
	return s, nil
}

func isStringValue(value any) bool {
	switch value.(type) {
	case string, *mutableString:
		return true
	default:
		return false
	}
}

func quoteDatum(expr any) any {
	switch node := expr.(type) {
	case int64:
		return node
	case rationalValue:
		return node
	case float64:
		return node
	case bool:
		return node
	case stringExpr:
		return node.value
	case charValue:
		return node
	case symbolExpr:
		return node
	case listExpr:
		elements := make([]any, len(node.elements))
		for i, elem := range node.elements {
			elements[i] = quoteDatum(elem)
		}
		return makeListValue(elements)
	default:
		return expr
	}
}

func makeListValue(elements []any) any {
	result := any(emptyListValue{})
	for i := len(elements) - 1; i >= 0; i-- {
		result = pairValue{car: elements[i], cdr: result}
	}
	return result
}

func properListElements(value any, builtinName string) ([]any, error) {
	var elements []any
	current := value
	for {
		switch list := current.(type) {
		case emptyListValue:
			return elements, nil
		case pairValue:
			elements = append(elements, list.car)
			current = list.cdr
		default:
			return nil, &EvalError{Message: fmt.Sprintf("%s expects a list, got %s", builtinName, typeName(value))}
		}
	}
}

func isTruthy(value any) bool {
	if b, ok := value.(bool); ok {
		return b
	}
	return true
}

func typeName(value any) string {
	switch value.(type) {
	case int64, rationalValue, float64:
		return "number"
	case bool:
		return "boolean"
	case charValue:
		return "char"
	case string:
		return "string"
	case *mutableString:
		return "string"
	case symbolExpr:
		return "symbol"
	case pairValue:
		return "pair"
	case emptyListValue:
		return "list"
	case listExpr:
		return "list"
	case builtinProc, closure, caseClosure:
		return "procedure"
	case voidValue:
		return "void"
	case *recordValue:
		return "record"
	default:
		return "value"
	}
}

func isProcedureValue(value any) bool {
	switch value.(type) {
	case builtinProc, closure, caseClosure:
		return true
	default:
		return false
	}
}

func formatValue(value any) string {
	switch v := value.(type) {
	case voidValue:
		return ""
	case int64, rationalValue, float64:
		return formatNumberValue(v)
	case bool:
		if v {
			return "#t"
		}
		return "#f"
	case string:
		return strconv.Quote(v)
	case *mutableString:
		return strconv.Quote(string(v.runes))
	case charValue:
		return formatChar(v)
	case symbolExpr:
		return v.name
	case emptyListValue:
		return "()"
	case pairValue:
		return formatPairValue(v)
	case *recordValue:
		return fmt.Sprintf("#<record %s>", v.typ.name)
	case listExpr:
		if len(v.elements) == 0 {
			return "()"
		}

		parts := make([]string, len(v.elements))
		for i, elem := range v.elements {
			parts[i] = formatValue(elem)
		}
		return "(" + strings.Join(parts, " ") + ")"
	default:
		return ""
	}
}

func formatDisplayValue(value any) string {
	switch v := value.(type) {
	case string:
		return v
	case *mutableString:
		return string(v.runes)
	case charValue:
		return string(rune(v))
	default:
		return formatValue(value)
	}
}

func formatChar(value charValue) string {
	switch rune(value) {
	case ' ':
		return "#\\space"
	case '\n':
		return "#\\newline"
	default:
		return "#\\" + string(rune(value))
	}
}

func writeOutput(output *strings.Builder, text string) {
	if output == nil {
		return
	}
	output.WriteString(text)
}

func formatPairValue(pair pairValue) string {
	parts := []string{formatValue(pair.car)}
	current := pair.cdr
	for {
		switch value := current.(type) {
		case emptyListValue:
			return "(" + strings.Join(parts, " ") + ")"
		case pairValue:
			parts = append(parts, formatValue(value.car))
			current = value.cdr
		default:
			return "(" + strings.Join(parts, " ") + " . " + formatValue(value) + ")"
		}
	}
}
