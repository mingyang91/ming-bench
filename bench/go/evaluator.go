package ming

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"unicode/utf8"
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

type pairCell struct {
	car any
	cdr any
}

type pairValue struct {
	*pairCell
}

type mutableString struct {
	runes []rune
}

type vectorValue struct {
	elements []any
}

type charValue rune

type emptyListValue struct{}

type voidValue struct{}

type uninitializedValue struct{}

type tailEvalState struct {
	scope *env
	expr  any
}

type parser struct {
	input string
	pos   int
}

type bindingCell struct {
	value any
}

type env struct {
	parent   *env
	runtime  *runtimeState
	bindings map[string]*bindingCell
	macros   map[string]*syntaxMacro
}

type builtinFunc func(args []any) (any, error)

type builtinProc struct {
	name string
	fn   builtinFunc
}

const stringImmutabilityLevel = 15

func activeBenchLevel() (int, bool) {
	levelText := os.Getenv("BENCH_LEVEL")
	if levelText == "" {
		return 0, false
	}

	level, err := strconv.Atoi(levelText)
	if err != nil {
		return 0, false
	}
	return level, true
}

func stringsAreImmutable() bool {
	level, ok := activeBenchLevel()
	if !ok {
		return true
	}
	return level >= stringImmutabilityLevel
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
	runtime := &runtimeState{}
	if parent != nil {
		runtime = parent.runtime
	}
	return &env{
		parent:   parent,
		runtime:  runtime,
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
	immutableStrings := stringsAreImmutable()
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
	scope.define(">=", builtinProc{name: ">=", fn: func(args []any) (any, error) {
		return builtinCompare(args, func(order int) bool { return order >= 0 })
	}})
	scope.define("not", builtinProc{name: "not", fn: builtinNot})
	scope.define("cons", builtinProc{name: "cons", fn: builtinCons})
	scope.define("car", builtinProc{name: "car", fn: builtinCar})
	scope.define("cdr", builtinProc{name: "cdr", fn: builtinCdr})
	registerCxrBuiltins(scope)
	scope.define("set-car!", builtinProc{name: "set-car!", fn: builtinSetCar})
	scope.define("set-cdr!", builtinProc{name: "set-cdr!", fn: builtinSetCdr})
	scope.define("null?", builtinProc{name: "null?", fn: func(args []any) (any, error) {
		return builtinPredicate("null?", args, func(value any) bool {
			_, ok := value.(emptyListValue)
			return ok
		})
	}})
	scope.define("append", builtinProc{name: "append", fn: builtinAppend})
	scope.define("list", builtinProc{name: "list", fn: builtinList})
	scope.define("length", builtinProc{name: "length", fn: builtinLength})
	scope.define("reverse", builtinProc{name: "reverse", fn: builtinReverse})
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
	scope.define("string", builtinProc{name: "string", fn: builtinString})
	scope.define("make-string", builtinProc{name: "make-string", fn: builtinMakeString})
	scope.define("string-length", builtinProc{name: "string-length", fn: builtinStringLength})
	scope.define("substring", builtinProc{name: "substring", fn: builtinSubstring})
	scope.define("string->number", builtinProc{name: "string->number", fn: builtinStringToNumber})
	scope.define("number->string", builtinProc{name: "number->string", fn: builtinNumberToString})
	scope.define("symbol->string", builtinProc{name: "symbol->string", fn: builtinSymbolToString})
	scope.define("string->symbol", builtinProc{name: "string->symbol", fn: builtinStringToSymbol})
	scope.define("string-ref", builtinProc{name: "string-ref", fn: builtinStringRef})
	scope.define("string-copy", builtinProc{name: "string-copy", fn: func(args []any) (any, error) {
		return builtinStringCopy(args, immutableStrings)
	}})
	scope.define("string-set!", builtinProc{name: "string-set!", fn: func(args []any) (any, error) {
		return builtinStringSet(args, immutableStrings)
	}})
	scope.define("string->list", builtinProc{name: "string->list", fn: builtinStringToList})
	scope.define("list->string", builtinProc{name: "list->string", fn: builtinListToString})
	scope.define("exact->inexact", builtinProc{name: "exact->inexact", fn: builtinExactToInexact})
	scope.define("inexact->exact", builtinProc{name: "inexact->exact", fn: builtinInexactToExact})
	scope.define("numerator", builtinProc{name: "numerator", fn: builtinNumerator})
	scope.define("denominator", builtinProc{name: "denominator", fn: builtinDenominator})
	scope.define("abs", builtinProc{name: "abs", fn: builtinAbs})
	scope.define("gcd", builtinProc{name: "gcd", fn: builtinGCD})
	scope.define("lcm", builtinProc{name: "lcm", fn: builtinLCM})
	scope.define("truncate", builtinProc{name: "truncate", fn: builtinTruncate})
	scope.define("round", builtinProc{name: "round", fn: builtinRound})
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
	scope.define("eqv?", builtinProc{name: "eqv?", fn: builtinEqv})
	scope.define("equal?", builtinProc{name: "equal?", fn: builtinEqual})
	scope.define("assoc", builtinProc{name: "assoc", fn: builtinAssoc})
	scope.define("assv", builtinProc{name: "assv", fn: builtinAssv})
	scope.define("member", builtinProc{name: "member", fn: builtinMember})
	scope.define("map", builtinProc{name: "map", fn: builtinMap})
	scope.define("for-each", builtinProc{name: "for-each", fn: builtinForEach})
	scope.define("vector", builtinProc{name: "vector", fn: builtinVector})
	scope.define("make-vector", builtinProc{name: "make-vector", fn: builtinMakeVector})
	scope.define("vector-ref", builtinProc{name: "vector-ref", fn: builtinVectorRef})
	scope.define("vector-set!", builtinProc{name: "vector-set!", fn: builtinVectorSet})
	scope.define("vector-length", builtinProc{name: "vector-length", fn: builtinVectorLength})
	scope.define("vector?", builtinProc{name: "vector?", fn: builtinVectorPredicate})
	scope.define("vector->list", builtinProc{name: "vector->list", fn: builtinVectorToList})
	scope.define("list->vector", builtinProc{name: "list->vector", fn: builtinListToVector})
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
	scope.define("char->integer", builtinProc{name: "char->integer", fn: builtinCharToInteger})
	scope.define("integer->char", builtinProc{name: "integer->char", fn: builtinIntegerToChar})
	scope.define("string=?", builtinProc{name: "string=?", fn: builtinStringEqual})
	scope.define("string<?", builtinProc{name: "string<?", fn: builtinStringLess})
	scope.define("string>?", builtinProc{name: "string>?", fn: builtinStringGreater})
	scope.define("string<=?", builtinProc{name: "string<=?", fn: builtinStringLessEqual})
	scope.define("string>=?", builtinProc{name: "string>=?", fn: builtinStringGreaterEqual})
	scope.define("string-ci=?", builtinProc{name: "string-ci=?", fn: builtinStringCIEqual})
	scope.define("string-upcase", builtinProc{name: "string-upcase", fn: builtinStringUpcase})
	scope.define("string-downcase", builtinProc{name: "string-downcase", fn: builtinStringDowncase})
	scope.define("apply", builtinProc{name: "apply", fn: builtinApply})
	scope.define("procedure?", builtinProc{name: "procedure?", fn: builtinProcedurePredicate})
	if level18UsesCPS() {
		scope.define("call/cc", callCCProc{})
		scope.define("call-with-current-continuation", callCCProc{})
		runtime := scope.runtime
		scope.defineKey(level18ApplyCPSKey, builtinProc{name: "__apply_cps", fn: func(args []any) (any, error) {
			return builtinApplyCPS(runtime, args)
		}})
	}
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
	if level18UsesCPS() && programUsesDynamicControl(exprs) {
		if err := predeclareLevel18TopLevelDefines(scope, exprs); err != nil {
			return nil, output.String(), err
		}

		normalizedExprs, err := normalizeLevel18TopLevelExprs(exprs)
		if err != nil {
			return nil, output.String(), err
		}

		cpsExpr, err := transformLevel18Program(normalizedExprs)
		if err != nil {
			return nil, output.String(), err
		}

		result, err := evalCPSProgram(scope, cpsExpr)
		if err != nil {
			return nil, output.String(), err
		}
		return result, output.String(), nil
	}

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
	for {
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
			if _, ok := value.(uninitializedValue); ok {
				return nil, node.pos.errorf("uninitialized variable: %s", node.name)
			}
			return value, nil
		case listExpr:
			value, next, err := evalListTail(scope, node)
			if err != nil {
				return nil, attachSourcePos(err, node.pos)
			}
			if next != nil {
				scope = next.scope
				expr = next.expr
				continue
			}
			return value, nil
		case string:
			return node, nil
		case *mutableString:
			return node, nil
		case *vectorValue:
			return node, nil
		case builtinProc:
			return node, nil
		case continuationProc:
			return node, nil
		case callCCProc:
			return node, nil
		case dynamicWindEnterProc:
			return node, nil
		case dynamicWindExitProc:
			return node, nil
		case dynamicWindCompleteProc:
			return node, nil
		case dynamicWindPopProc:
			return node, nil
		case dynamicWindReenterProc:
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
}

func evalListTail(scope *env, expr listExpr) (any, *tailEvalState, error) {
	if len(expr.elements) == 0 {
		return nil, nil, &EvalError{Message: "cannot evaluate empty list"}
	}

	if head, ok := expr.elements[0].(symbolExpr); ok {
		args := expr.elements[1:]
		switch head.name {
		case "define":
			value, err := evalDefine(scope, args)
			return value, nil, err
		case "define-syntax":
			value, err := evalDefineSyntax(scope, args)
			return value, nil, err
		case "define-record-type":
			value, err := evalDefineRecordType(scope, args)
			return value, nil, err
		case "set!":
			value, err := evalSet(scope, args)
			return value, nil, err
		case "if":
			if len(args) != 2 && len(args) != 3 {
				return nil, nil, &EvalError{Message: "if expects 2 or 3 arguments"}
			}

			cond, err := eval(scope, args[0])
			if err != nil {
				return nil, nil, err
			}

			if isTruthy(cond) {
				return nil, &tailEvalState{scope: scope, expr: args[1]}, nil
			}
			if len(args) == 2 {
				return voidValue{}, nil, nil
			}
			return nil, &tailEvalState{scope: scope, expr: args[2]}, nil
		case "quote":
			value, err := evalQuote(args)
			return value, nil, err
		case "lambda":
			value, err := evalLambda(scope, args)
			return value, nil, err
		case "case-lambda":
			value, err := evalCaseLambda(scope, args)
			return value, nil, err
		case "and":
			if len(args) == 0 {
				return true, nil, nil
			}
			for i, arg := range args {
				if i == len(args)-1 {
					return nil, &tailEvalState{scope: scope, expr: arg}, nil
				}

				value, err := eval(scope, arg)
				if err != nil {
					return nil, nil, err
				}
				if !isTruthy(value) {
					return value, nil, nil
				}
			}
			return true, nil, nil
		case "or":
			if len(args) == 0 {
				return false, nil, nil
			}
			for i, arg := range args {
				if i == len(args)-1 {
					return nil, &tailEvalState{scope: scope, expr: arg}, nil
				}

				value, err := eval(scope, arg)
				if err != nil {
					return nil, nil, err
				}
				if isTruthy(value) {
					return value, nil, nil
				}
			}
			return false, nil, nil
		case "begin":
			return prepareTailSequence(scope, args)
		case "let":
			return evalLetTail(scope, args)
		case "let*":
			return evalLetStarTail(scope, args)
		case "letrec":
			return evalLetrecTail(scope, args, false)
		case "letrec*":
			return evalLetrecTail(scope, args, true)
		case "cond":
			return evalCondTail(scope, args)
		case "case":
			return evalCaseTail(scope, args)
		case "do":
			return evalDoTail(scope, args)
		case level18ApplyCPSKey:
			return evalLevel18ApplyCPSTail(scope, args)
		case level19DynamicWindKey:
			return evalLevel19DynamicWindTail(scope, args)
		}

		if macro, ok := scope.lookupMacroSymbol(head); ok {
			expanded, err := macro.expand(expr)
			if err != nil {
				return nil, nil, err
			}
			return nil, &tailEvalState{scope: scope, expr: expanded}, nil
		}
	}

	proc, err := eval(scope, expr.elements[0])
	if err != nil {
		return nil, nil, err
	}

	args := make([]any, 0, len(expr.elements)-1)
	for _, argExpr := range expr.elements[1:] {
		arg, err := eval(scope, argExpr)
		if err != nil {
			return nil, nil, err
		}
		args = append(args, arg)
	}

	return prepareProcedureCall(proc, args)
}

func prepareTailSequence(scope *env, exprs []any) (any, *tailEvalState, error) {
	if len(exprs) == 0 {
		return voidValue{}, nil, nil
	}

	for _, expr := range exprs[:len(exprs)-1] {
		if _, err := eval(scope, expr); err != nil {
			return nil, nil, err
		}
	}

	return nil, &tailEvalState{
		scope: scope,
		expr:  exprs[len(exprs)-1],
	}, nil
}

func prepareProcedureCall(proc any, args []any) (any, *tailEvalState, error) {
	switch callable := proc.(type) {
	case builtinProc:
		value, err := callable.fn(args)
		return value, nil, err
	case dynamicWindEnterProc:
		return prepareDynamicWindEnterCall(callable, args)
	case dynamicWindExitProc:
		return prepareDynamicWindExitCall(callable, args)
	case dynamicWindCompleteProc:
		return prepareDynamicWindCompleteCall(callable, args)
	case dynamicWindPopProc:
		return prepareDynamicWindPopCall(callable, args)
	case dynamicWindReenterProc:
		return prepareDynamicWindReenterCall(callable, args)
	case closure:
		return prepareClosureCall(callable, args)
	case caseClosure:
		for _, clause := range callable.clauses {
			if closureMatchesArity(clause, len(args)) {
				return prepareClosureCall(clause, args)
			}
		}
		return nil, nil, &EvalError{Message: fmt.Sprintf("no matching case-lambda clause for %d arguments", len(args))}
	default:
		return nil, nil, &EvalError{Message: fmt.Sprintf("expected procedure, got %s", typeName(proc))}
	}
}

func prepareClosureCall(callable closure, args []any) (any, *tailEvalState, error) {
	if !closureMatchesArity(callable, len(args)) {
		return nil, nil, closureArgCountError(callable, len(args))
	}

	callScope := bindClosureArgs(callable, args)
	return prepareTailSequence(callScope, callable.body)
}

func bindClosureArgs(callable closure, args []any) *env {
	callScope := newEnv(callable.env)
	for i, param := range callable.params {
		callScope.defineSymbol(param, args[i])
	}
	if callable.hasRest {
		callScope.defineSymbol(callable.restParam, makeListValue(args[len(callable.params):]))
	}
	return callScope
}

func evalLetTail(scope *env, args []any) (any, *tailEvalState, error) {
	if len(args) < 2 {
		return nil, nil, &EvalError{Message: "let expects bindings and a body"}
	}

	if name, ok := args[0].(symbolExpr); ok {
		if len(args) < 3 {
			return nil, nil, &EvalError{Message: "named let expects bindings and a body"}
		}

		bindingsExpr, ok := args[1].(listExpr)
		if !ok {
			return nil, nil, &EvalError{Message: "let bindings must be a list"}
		}

		params, values, err := evalBindings(scope, bindingsExpr.elements)
		if err != nil {
			return nil, nil, err
		}

		letScope := newEnv(scope)
		proc := closure{
			params: params,
			body:   args[2:],
			env:    letScope,
		}
		letScope.defineSymbol(name, proc)
		return prepareClosureCall(proc, values)
	}

	bindingsExpr, ok := args[0].(listExpr)
	if !ok {
		return nil, nil, &EvalError{Message: "let bindings must be a list"}
	}

	params, values, err := evalBindings(scope, bindingsExpr.elements)
	if err != nil {
		return nil, nil, err
	}

	letScope := newEnv(scope)
	for i, param := range params {
		letScope.defineSymbol(param, values[i])
	}

	return prepareTailSequence(letScope, args[1:])
}

func evalLetrecTail(scope *env, args []any, sequential bool) (any, *tailEvalState, error) {
	formName := "letrec"
	if sequential {
		formName = "letrec*"
	}

	if len(args) < 2 {
		return nil, nil, &EvalError{Message: fmt.Sprintf("%s expects bindings and a body", formName)}
	}

	bindingsExpr, ok := args[0].(listExpr)
	if !ok {
		return nil, nil, &EvalError{Message: fmt.Sprintf("%s bindings must be a list", formName)}
	}

	bindings, err := parseLetBindingSpecs(bindingsExpr.elements, formName)
	if err != nil {
		return nil, nil, err
	}

	letrecScope := newEnv(scope)
	for _, binding := range bindings {
		letrecScope.defineSymbol(binding.name, uninitializedValue{})
	}

	if sequential {
		for _, binding := range bindings {
			value, err := eval(letrecScope, binding.value)
			if err != nil {
				return nil, nil, err
			}
			letrecScope.setSymbol(binding.name, value)
		}
	} else {
		values := make([]any, len(bindings))
		for i, binding := range bindings {
			value, err := eval(letrecScope, binding.value)
			if err != nil {
				return nil, nil, err
			}
			values[i] = value
		}
		for i, binding := range bindings {
			letrecScope.setSymbol(binding.name, values[i])
		}
	}

	return prepareTailSequence(letrecScope, args[1:])
}

func evalCondTail(scope *env, args []any) (any, *tailEvalState, error) {
	for i, clauseExpr := range args {
		clause, ok := clauseExpr.(listExpr)
		if !ok || len(clause.elements) == 0 {
			return nil, nil, &EvalError{Message: "cond clauses must be non-empty lists"}
		}

		if symbol, ok := clause.elements[0].(symbolExpr); ok && symbol.name == "else" {
			if i != len(args)-1 {
				return nil, nil, &EvalError{Message: "cond else clause must be last"}
			}
			if len(clause.elements) == 1 {
				return voidValue{}, nil, nil
			}
			return prepareTailSequence(scope, clause.elements[1:])
		}

		testValue, err := eval(scope, clause.elements[0])
		if err != nil {
			return nil, nil, err
		}
		if !isTruthy(testValue) {
			continue
		}
		if len(clause.elements) == 1 {
			return testValue, nil, nil
		}
		return prepareTailSequence(scope, clause.elements[1:])
	}

	return voidValue{}, nil, nil
}

func evalCaseTail(scope *env, args []any) (any, *tailEvalState, error) {
	if len(args) < 1 {
		return nil, nil, &EvalError{Message: "case expects a key and clauses"}
	}

	key, err := eval(scope, args[0])
	if err != nil {
		return nil, nil, err
	}

	for i, clauseExpr := range args[1:] {
		clause, ok := clauseExpr.(listExpr)
		if !ok || len(clause.elements) == 0 {
			return nil, nil, exprSourcePos(clauseExpr).errorf("case clauses must be non-empty lists")
		}

		if symbol, ok := clause.elements[0].(symbolExpr); ok && symbol.name == "else" {
			if i != len(args)-2 {
				return nil, nil, symbol.pos.errorf("case else clause must be last")
			}
			if len(clause.elements) == 1 {
				return voidValue{}, nil, nil
			}
			return prepareTailSequence(scope, clause.elements[1:])
		}

		datums, ok := clause.elements[0].(listExpr)
		if !ok {
			return nil, nil, exprSourcePos(clause.elements[0]).errorf("case clause datums must be a list")
		}

		for _, datumExpr := range datums.elements {
			if valuesEqv(key, quoteDatum(datumExpr)) {
				if len(clause.elements) == 1 {
					return voidValue{}, nil, nil
				}
				return prepareTailSequence(scope, clause.elements[1:])
			}
		}
	}

	return voidValue{}, nil, nil
}

func evalDoTail(scope *env, args []any) (any, *tailEvalState, error) {
	if len(args) < 2 {
		return nil, nil, &EvalError{Message: "do expects bindings, a test clause, and optional body expressions"}
	}

	bindingsExpr, ok := args[0].(listExpr)
	if !ok {
		return nil, nil, &EvalError{Message: "do bindings must be a list"}
	}

	testClause, ok := args[1].(listExpr)
	if !ok || len(testClause.elements) == 0 {
		return nil, nil, exprSourcePos(args[1]).errorf("do test clause must be a non-empty list")
	}

	bindings, err := parseDoBindings(bindingsExpr.elements)
	if err != nil {
		return nil, nil, err
	}

	loopScope := newEnv(scope)
	initValues := make([]any, len(bindings))
	for i, binding := range bindings {
		value, err := eval(scope, binding.init)
		if err != nil {
			return nil, nil, err
		}
		initValues[i] = value
	}
	for i, binding := range bindings {
		loopScope.defineSymbol(binding.name, initValues[i])
	}

	for {
		testValue, err := eval(loopScope, testClause.elements[0])
		if err != nil {
			return nil, nil, err
		}
		if isTruthy(testValue) {
			if len(testClause.elements) == 1 {
				return voidValue{}, nil, nil
			}
			return prepareTailSequence(loopScope, testClause.elements[1:])
		}

		if _, err := evalSequence(loopScope, args[2:]); err != nil {
			return nil, nil, err
		}

		nextValues := make([]any, len(bindings))
		for i, binding := range bindings {
			if binding.hasStep {
				value, err := eval(loopScope, binding.step)
				if err != nil {
					return nil, nil, err
				}
				nextValues[i] = value
				continue
			}

			value, ok := loopScope.lookupSymbol(binding.name)
			if !ok {
				return nil, nil, binding.name.pos.errorf("unbound variable: %s", binding.name.name)
			}
			nextValues[i] = value
		}
		for i, binding := range bindings {
			loopScope.setSymbol(binding.name, nextValues[i])
		}
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
		case "let*":
			return evalLetStar(scope, args)
		case "letrec":
			return evalLetrec(scope, args, false)
		case "letrec*":
			return evalLetrec(scope, args, true)
		case "cond":
			return evalCond(scope, args)
		case "case":
			return evalCase(scope, args)
		case "do":
			return evalDo(scope, args)
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
	if len(args) != 2 && len(args) != 3 {
		return nil, &EvalError{Message: "if expects 2 or 3 arguments"}
	}

	cond, err := eval(scope, args[0])
	if err != nil {
		return nil, err
	}

	if isTruthy(cond) {
		return eval(scope, args[1])
	}
	if len(args) == 2 {
		return voidValue{}, nil
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
	value, next, err := prepareProcedureCall(proc, args)
	if err != nil {
		return nil, err
	}
	if next != nil {
		return eval(next.scope, next.expr)
	}
	return value, nil
}

func applyClosure(callable closure, args []any) (any, error) {
	value, next, err := prepareClosureCall(callable, args)
	if err != nil {
		return nil, err
	}
	if next != nil {
		return eval(next.scope, next.expr)
	}
	return value, nil
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
	value, next, err := prepareTailSequence(scope, exprs)
	if err != nil {
		return nil, err
	}
	if next != nil {
		return eval(next.scope, next.expr)
	}
	return value, nil
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
	return newPair(args[0], args[1]), nil
}

func builtinCar(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "car expects exactly 1 argument"}
	}

	pair, err := expectPair(args[0], "car")
	if err != nil {
		return nil, err
	}
	return pair.car, nil
}

func builtinCdr(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "cdr expects exactly 1 argument"}
	}

	pair, err := expectPair(args[0], "cdr")
	if err != nil {
		return nil, err
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
	seen := map[*pairCell]struct{}{}
	for {
		switch value := current.(type) {
		case emptyListValue:
			return length, nil
		case pairValue:
			if _, ok := seen[value.pairCell]; ok {
				return nil, &EvalError{Message: "length expects a proper list"}
			}
			seen[value.pairCell] = struct{}{}
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
			result = newPair(elements[j], result)
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

func builtinStringCopy(args []any, immutableStrings bool) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "string-copy expects exactly 1 argument"}
	}

	s, err := expectString(args[0])
	if err != nil {
		return nil, err
	}

	runes := []rune(s)
	if immutableStrings {
		return string(runes), nil
	}
	return &mutableString{runes: runes}, nil
}

func builtinStringSet(args []any, immutableStrings bool) (any, error) {
	if len(args) != 3 {
		return nil, &EvalError{Message: "string-set! expects exactly 3 arguments"}
	}

	if !immutableStrings {
		s, err := expectMutableString(args[0])
		if err != nil {
			return nil, err
		}

		index, err := expectNonNegativeIndex(args[1], "string-set!")
		if err != nil {
			return nil, err
		}

		ch, err := expectChar(args[2])
		if err != nil {
			return nil, err
		}

		if index >= int64(len(s.runes)) {
			return nil, &EvalError{Message: "string-set! index out of range"}
		}

		s.runes[index] = rune(ch)
		return voidValue{}, nil
	}

	if _, err := expectString(args[0]); err != nil {
		return nil, err
	}
	if _, err := expectNonNegativeIndex(args[1], "string-set!"); err != nil {
		return nil, err
	}
	if _, err := expectChar(args[2]); err != nil {
		return nil, err
	}
	return nil, &EvalError{Message: "string-set! is not supported on immutable strings"}
}

func builtinStringToList(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "string->list expects exactly 1 argument"}
	}

	s, err := expectString(args[0])
	if err != nil {
		return nil, err
	}

	runes := []rune(s)
	elements := make([]any, len(runes))
	for i, r := range runes {
		elements[i] = charValue(r)
	}
	return makeListValue(elements), nil
}

func builtinListToString(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "list->string expects exactly 1 argument"}
	}

	elements, err := properListElements(args[0], "list->string")
	if err != nil {
		return nil, err
	}

	var b strings.Builder
	for _, elem := range elements {
		ch, err := expectChar(elem)
		if err != nil {
			return nil, err
		}
		b.WriteRune(rune(ch))
	}
	return b.String(), nil
}

func builtinCharToInteger(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "char->integer expects exactly 1 argument"}
	}

	ch, err := expectChar(args[0])
	if err != nil {
		return nil, err
	}
	return int64(rune(ch)), nil
}

func builtinIntegerToChar(args []any) (any, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "integer->char expects exactly 1 argument"}
	}

	n, err := expectInt(args[0])
	if err != nil {
		return nil, err
	}
	if n < 0 || n > utf8.MaxRune {
		return nil, &EvalError{Message: "integer->char expects a valid Unicode scalar value"}
	}

	ch := rune(n)
	if !utf8.ValidRune(ch) {
		return nil, &EvalError{Message: "integer->char expects a valid Unicode scalar value"}
	}

	return charValue(ch), nil
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
		result = newPair(elements[i], result)
	}
	return result
}

func properListElements(value any, builtinName string) ([]any, error) {
	var elements []any
	current := value
	seen := map[*pairCell]struct{}{}
	for {
		switch list := current.(type) {
		case emptyListValue:
			return elements, nil
		case pairValue:
			if _, ok := seen[list.pairCell]; ok {
				return nil, &EvalError{Message: fmt.Sprintf("%s expects a proper list", builtinName)}
			}
			seen[list.pairCell] = struct{}{}
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
	case *vectorValue:
		return "vector"
	case builtinProc, closure, caseClosure, continuationProc, callCCProc, dynamicWindEnterProc, dynamicWindExitProc, dynamicWindCompleteProc, dynamicWindPopProc, dynamicWindReenterProc:
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
	case builtinProc, closure, caseClosure, continuationProc, callCCProc, dynamicWindEnterProc, dynamicWindExitProc, dynamicWindCompleteProc, dynamicWindPopProc, dynamicWindReenterProc:
		return true
	default:
		return false
	}
}

func formatValue(value any) string {
	state := &formatState{
		pairs:   map[*pairCell]bool{},
		vectors: map[*vectorValue]bool{},
	}
	return formatValueWithState(value, state)
}

func formatValueWithState(value any, state *formatState) string {
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
		return formatPairValueWithState(v, state)
	case *vectorValue:
		return formatVectorValueWithState(v, state)
	case *recordValue:
		return fmt.Sprintf("#<record %s>", v.typ.name)
	case listExpr:
		if len(v.elements) == 0 {
			return "()"
		}

		parts := make([]string, len(v.elements))
		for i, elem := range v.elements {
			parts[i] = formatValueWithState(elem, state)
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
	state := &formatState{
		pairs:   map[*pairCell]bool{},
		vectors: map[*vectorValue]bool{},
	}
	return formatPairValueWithState(pair, state)
}

func formatVectorValue(vector *vectorValue) string {
	state := &formatState{
		pairs:   map[*pairCell]bool{},
		vectors: map[*vectorValue]bool{},
	}
	return formatVectorValueWithState(vector, state)
}

type formatState struct {
	pairs   map[*pairCell]bool
	vectors map[*vectorValue]bool
}

func formatPairValueWithState(pair pairValue, state *formatState) string {
	parts := []string{}
	marked := []*pairCell{}
	unmark := func() {
		for i := len(marked) - 1; i >= 0; i-- {
			delete(state.pairs, marked[i])
		}
	}

	current := pair
	for {
		if state.pairs[current.pairCell] {
			unmark()
			if len(parts) == 0 {
				return "#<cycle>"
			}
			return "(" + strings.Join(parts, " ") + " . #<cycle>)"
		}

		state.pairs[current.pairCell] = true
		marked = append(marked, current.pairCell)
		parts = append(parts, formatValueWithState(current.car, state))

		switch next := current.cdr.(type) {
		case emptyListValue:
			unmark()
			return "(" + strings.Join(parts, " ") + ")"
		case pairValue:
			current = next
		default:
			tail := formatValueWithState(next, state)
			unmark()
			return "(" + strings.Join(parts, " ") + " . " + tail + ")"
		}
	}
}

func formatVectorValueWithState(vector *vectorValue, state *formatState) string {
	if state.vectors[vector] {
		return "#<cycle>"
	}

	state.vectors[vector] = true
	defer delete(state.vectors, vector)

	parts := make([]string, len(vector.elements))
	for i, elem := range vector.elements {
		parts[i] = formatValueWithState(elem, state)
	}
	return "#(" + strings.Join(parts, " ") + ")"
}
