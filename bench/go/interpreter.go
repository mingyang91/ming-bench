package ming

import (
	"fmt"
	"math/big"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"
)

type expr interface{}

type intExpr int
type boolExpr bool
type stringExpr struct {
	runes   []rune
	mutable bool
}
type charExpr rune
type symbolExpr struct {
	name      string
	pos       sourcePos
	lookupEnv *env
}
type listExpr struct {
	items []expr
	pos   sourcePos
}
type pairExpr struct {
	car expr
	cdr expr
}
type voidExpr struct{}

type builtinFunc func([]expr) (expr, error)

type builtinProc struct {
	name string
	fn   builtinFunc
}

type closureExpr struct {
	params    []string
	restParam string
	variadic  bool
	body      []expr
	env       *env
}

type caseClosureExpr struct {
	clauses []closureExpr
}

type env struct {
	parent   *env
	bindings map[string]expr
}

type tokenKind int

const (
	tokenLParen tokenKind = iota
	tokenRParen
	tokenQuote
	tokenAtom
	tokenString
)

type token struct {
	kind tokenKind
	text string
	pos  sourcePos
}

type parser struct {
	tokens []token
	pos    int
}

type runtime struct {
	output strings.Builder
}

type evalStep struct {
	value    expr
	nextEnv  *env
	nextForm expr
	tail     bool
}

func doneStep(value expr) evalStep {
	return evalStep{value: value}
}

func tailStep(environment *env, form expr) evalStep {
	return evalStep{
		nextEnv:  environment,
		nextForm: form,
		tail:     true,
	}
}

func (r *runtime) writeString(text string) {
	if r == nil {
		return
	}
	r.output.WriteString(text)
}

func newStringExpr(text string, mutable bool) *stringExpr {
	return &stringExpr{
		runes:   []rune(text),
		mutable: mutable,
	}
}

func newAllocatedString(text string) *stringExpr {
	return newStringExpr(text, !stringsImmutableEnabled())
}

func (s *stringExpr) text() string {
	if s == nil {
		return ""
	}
	return string(s.runes)
}

func (s *stringExpr) copy(mutable bool) *stringExpr {
	if s == nil {
		return newStringExpr("", mutable)
	}
	runes := append([]rune(nil), s.runes...)
	return &stringExpr{
		runes:   runes,
		mutable: mutable,
	}
}

func asString(value expr) (*stringExpr, bool) {
	text, ok := value.(*stringExpr)
	return text, ok
}

func (p sourcePos) advance(r rune) sourcePos {
	if r == '\n' {
		return sourcePos{Line: p.Line + 1, Col: 1}
	}
	return sourcePos{Line: p.Line, Col: p.Col + 1}
}

func formPos(form expr) sourcePos {
	switch v := form.(type) {
	case symbolExpr:
		return v.pos
	case listExpr:
		return v.pos
	default:
		return sourcePos{}
	}
}

func evalProgram(input string) (string, error) {
	result, _, err := evalProgramWithOutput(input)
	return result, err
}

func evalProgramWithOutput(input string) (string, string, error) {
	tokens, err := tokenize(input)
	if err != nil {
		return "", "", err
	}

	p := parser{tokens: tokens}
	program, err := p.parseProgram()
	if err != nil {
		return "", "", err
	}
	if len(program) == 0 {
		return "", "", errorAt(sourcePos{Line: 1, Col: 1}, "empty program")
	}

	rt := &runtime{}
	environment := newGlobalEnv(rt)
	var result expr
	if currentBenchLevel() >= 18 {
		result, err = evalSequenceLevel18(environment, program)
	} else {
		result, err = evalSequence(environment, program)
	}
	if err != nil {
		return "", rt.output.String(), err
	}

	return renderExpr(result), rt.output.String(), nil
}

func newGlobalEnv(rt *runtime) *env {
	root := &env{bindings: map[string]expr{}}

	root.define("+", builtinProc{name: "+", fn: builtinAdd})
	root.define("-", builtinProc{name: "-", fn: builtinSub})
	root.define("*", builtinProc{name: "*", fn: builtinMul})
	root.define("/", builtinProc{name: "/", fn: builtinDiv})
	root.define("<", builtinProc{name: "<", fn: comparisonBuiltin("<", func(a, b int) bool { return a < b })})
	root.define(">", builtinProc{name: ">", fn: comparisonBuiltin(">", func(a, b int) bool { return a > b })})
	root.define("=", builtinProc{name: "=", fn: comparisonBuiltin("=", func(a, b int) bool { return a == b })})
	root.define("<=", builtinProc{name: "<=", fn: comparisonBuiltin("<=", func(a, b int) bool { return a <= b })})
	root.define(">=", builtinProc{name: ">=", fn: comparisonBuiltin(">=", func(a, b int) bool { return a >= b })})
	root.define("abs", builtinProc{name: "abs", fn: builtinAbs})
	root.define("assoc", builtinProc{name: "assoc", fn: builtinAssoc})
	root.define("assq", builtinProc{name: "assq", fn: builtinAssq})
	root.define("assv", builtinProc{name: "assv", fn: builtinAssv})
	root.define("not", builtinProc{name: "not", fn: builtinNot})
	root.define("apply", builtinProc{name: "apply", fn: builtinApply})
	root.define("append", builtinProc{name: "append", fn: builtinAppend})
	root.define("car", builtinProc{name: "car", fn: builtinCar})
	root.define("cdr", builtinProc{name: "cdr", fn: builtinCdr})
	root.define("call/cc", builtinProc{name: "call/cc", fn: builtinContinuationSentinel})
	root.define("call-with-current-continuation", builtinProc{name: "call-with-current-continuation", fn: builtinContinuationSentinel})
	if currentBenchLevel() >= 19 {
		root.define("dynamic-wind", builtinProc{name: "dynamic-wind", fn: builtinDynamicWindSentinel})
	}
	if currentBenchLevel() >= 20 {
		root.define("raise", builtinProc{name: "raise", fn: builtinRaiseSentinel})
		root.define("with-exception-handler", builtinProc{name: "with-exception-handler", fn: builtinWithExceptionHandlerSentinel})
	}
	root.define("char-alphabetic?", builtinProc{name: "char-alphabetic?", fn: builtinCharAlphabetic})
	root.define("char->integer", builtinProc{name: "char->integer", fn: builtinCharToInteger})
	root.define("char-downcase", builtinProc{name: "char-downcase", fn: builtinCharDowncase})
	root.define("char-numeric?", builtinProc{name: "char-numeric?", fn: builtinCharNumeric})
	root.define("char-upcase", builtinProc{name: "char-upcase", fn: builtinCharUpcase})
	root.define("char=?", builtinProc{name: "char=?", fn: charComparisonBuiltin("char=?", func(a, b rune) bool { return a == b })})
	root.define("char<?", builtinProc{name: "char<?", fn: charComparisonBuiltin("char<?", func(a, b rune) bool { return a < b })})
	root.define("char?", builtinProc{name: "char?", fn: typePredicate(func(value expr) bool {
		_, ok := value.(charExpr)
		return ok
	})})
	root.define("cons", builtinProc{name: "cons", fn: builtinCons})
	root.define("display", builtinProc{name: "display", fn: makeDisplayBuiltin(rt)})
	root.define("eq?", builtinProc{name: "eq?", fn: builtinEq})
	root.define("eqv?", builtinProc{name: "eqv?", fn: builtinEqv})
	root.define("equal?", builtinProc{name: "equal?", fn: builtinEqual})
	root.define("even?", builtinProc{name: "even?", fn: builtinEven})
	root.define("error", builtinProc{name: "error", fn: builtinError})
	root.define("exact?", builtinProc{name: "exact?", fn: builtinExact})
	root.define("exact->inexact", builtinProc{name: "exact->inexact", fn: builtinExactToInexact})
	root.define("expt", builtinProc{name: "expt", fn: builtinExpt})
	root.define("for-each", builtinProc{name: "for-each", fn: builtinForEach})
	root.define("gcd", builtinProc{name: "gcd", fn: builtinGCD})
	root.define("inexact?", builtinProc{name: "inexact?", fn: builtinInexact})
	root.define("inexact->exact", builtinProc{name: "inexact->exact", fn: builtinInexactToExact})
	root.define("integer->char", builtinProc{name: "integer->char", fn: builtinIntegerToChar})
	root.define("integer?", builtinProc{name: "integer?", fn: builtinInteger})
	root.define("lcm", builtinProc{name: "lcm", fn: builtinLCM})
	root.define("length", builtinProc{name: "length", fn: builtinLength})
	root.define("list", builtinProc{name: "list", fn: builtinList})
	root.define("list?", builtinProc{name: "list?", fn: builtinListPred})
	root.define("list-ref", builtinProc{name: "list-ref", fn: builtinListRef})
	root.define("list->string", builtinProc{name: "list->string", fn: builtinListToString})
	root.define("list-tail", builtinProc{name: "list-tail", fn: builtinListTail})
	root.define("map", builtinProc{name: "map", fn: builtinMap})
	root.define("max", builtinProc{name: "max", fn: builtinMax})
	root.define("make-string", builtinProc{name: "make-string", fn: builtinMakeString})
	root.define("min", builtinProc{name: "min", fn: builtinMin})
	root.define("modulo", builtinProc{name: "modulo", fn: builtinModulo})
	root.define("member", builtinProc{name: "member", fn: builtinMember})
	root.define("memq", builtinProc{name: "memq", fn: builtinMemq})
	root.define("memv", builtinProc{name: "memv", fn: builtinMemv})
	root.define("negative?", builtinProc{name: "negative?", fn: builtinNegative})
	root.define("newline", builtinProc{name: "newline", fn: makeNewlineBuiltin(rt)})
	root.define("null?", builtinProc{name: "null?", fn: builtinNull})
	root.define("odd?", builtinProc{name: "odd?", fn: builtinOdd})
	root.define("boolean?", builtinProc{name: "boolean?", fn: typePredicate(func(value expr) bool {
		_, ok := value.(boolExpr)
		return ok
	})})
	root.define("number?", builtinProc{name: "number?", fn: typePredicate(func(value expr) bool {
		return isNumber(value)
	})})
	root.define("numerator", builtinProc{name: "numerator", fn: builtinNumerator})
	root.define("pair?", builtinProc{name: "pair?", fn: typePredicate(func(value expr) bool {
		return isPairValue(value)
	})})
	root.define("positive?", builtinProc{name: "positive?", fn: builtinPositive})
	root.define("procedure?", builtinProc{name: "procedure?", fn: typePredicate(isProcedure)})
	root.define("quotient", builtinProc{name: "quotient", fn: builtinQuotient})
	root.define("rational?", builtinProc{name: "rational?", fn: builtinRational})
	root.define("remainder", builtinProc{name: "remainder", fn: builtinRemainder})
	root.define("reverse", builtinProc{name: "reverse", fn: builtinReverse})
	root.define("round", builtinProc{name: "round", fn: builtinRound})
	root.define("denominator", builtinProc{name: "denominator", fn: builtinDenominator})
	root.define("number->string", builtinProc{name: "number->string", fn: builtinNumberToString})
	root.define("string", builtinProc{name: "string", fn: builtinString})
	root.define("string?", builtinProc{name: "string?", fn: typePredicate(func(value expr) bool {
		_, ok := asString(value)
		return ok
	})})
	root.define("string-append", builtinProc{name: "string-append", fn: builtinStringAppend})
	root.define("string-ci=?", builtinProc{name: "string-ci=?", fn: stringComparisonBuiltin("string-ci=?", func(a, b string) bool {
		return strings.EqualFold(a, b)
	})})
	root.define("string-copy", builtinProc{name: "string-copy", fn: builtinStringCopy})
	root.define("string-downcase", builtinProc{name: "string-downcase", fn: builtinStringDowncase})
	root.define("string=?", builtinProc{name: "string=?", fn: stringComparisonBuiltin("string=?", func(a, b string) bool {
		return a == b
	})})
	root.define("string<?", builtinProc{name: "string<?", fn: stringComparisonBuiltin("string<?", func(a, b string) bool {
		return compareStringLex(a, b) < 0
	})})
	root.define("string>?", builtinProc{name: "string>?", fn: stringComparisonBuiltin("string>?", func(a, b string) bool {
		return compareStringLex(a, b) > 0
	})})
	root.define("string<=?", builtinProc{name: "string<=?", fn: stringComparisonBuiltin("string<=?", func(a, b string) bool {
		return compareStringLex(a, b) <= 0
	})})
	root.define("string>=?", builtinProc{name: "string>=?", fn: stringComparisonBuiltin("string>=?", func(a, b string) bool {
		return compareStringLex(a, b) >= 0
	})})
	root.define("string-length", builtinProc{name: "string-length", fn: builtinStringLength})
	root.define("string-ref", builtinProc{name: "string-ref", fn: builtinStringRef})
	root.define("string-set!", builtinProc{name: "string-set!", fn: builtinStringSet})
	root.define("string->list", builtinProc{name: "string->list", fn: builtinStringToList})
	root.define("string->number", builtinProc{name: "string->number", fn: builtinStringToNumber})
	root.define("string->symbol", builtinProc{name: "string->symbol", fn: builtinStringToSymbol})
	root.define("string-upcase", builtinProc{name: "string-upcase", fn: builtinStringUpcase})
	root.define("substring", builtinProc{name: "substring", fn: builtinSubstring})
	root.define("symbol?", builtinProc{name: "symbol?", fn: typePredicate(func(value expr) bool {
		_, ok := value.(symbolExpr)
		return ok
	})})
	root.define("symbol->string", builtinProc{name: "symbol->string", fn: builtinSymbolToString})
	root.define("set-car!", builtinProc{name: "set-car!", fn: builtinSetCar})
	root.define("set-cdr!", builtinProc{name: "set-cdr!", fn: builtinSetCdr})
	root.define("truncate", builtinProc{name: "truncate", fn: builtinTruncate})
	root.define("vector", builtinProc{name: "vector", fn: builtinVector})
	root.define("make-vector", builtinProc{name: "make-vector", fn: builtinMakeVector})
	root.define("vector->list", builtinProc{name: "vector->list", fn: builtinVectorToList})
	root.define("list->vector", builtinProc{name: "list->vector", fn: builtinListToVector})
	root.define("vector-length", builtinProc{name: "vector-length", fn: builtinVectorLength})
	root.define("vector-ref", builtinProc{name: "vector-ref", fn: builtinVectorRef})
	root.define("vector-set!", builtinProc{name: "vector-set!", fn: builtinVectorSet})
	root.define("vector?", builtinProc{name: "vector?", fn: builtinVectorPred})
	root.define("write", builtinProc{name: "write", fn: makeWriteBuiltin(rt)})
	root.define("zero?", builtinProc{name: "zero?", fn: builtinZero})
	registerCxrBuiltins(root)

	return root
}

func (e *env) define(name string, value expr) {
	e.bindings[name] = value
}

func (e *env) assign(name string, value expr) bool {
	for current := e; current != nil; current = current.parent {
		if _, ok := current.bindings[name]; ok {
			current.bindings[name] = value
			return true
		}
	}
	return false
}

func (e *env) lookup(name string) (expr, bool) {
	for current := e; current != nil; current = current.parent {
		if value, ok := current.bindings[name]; ok {
			return value, true
		}
	}
	return nil, false
}

func tokenize(input string) ([]token, error) {
	var tokens []token
	pos := sourcePos{Line: 1, Col: 1}

	for len(input) > 0 {
		r, size := utf8.DecodeRuneInString(input)
		start := pos
		switch {
		case unicode.IsSpace(r):
			input = input[size:]
			pos = pos.advance(r)
		case r == ';':
			input = input[size:]
			pos = pos.advance(r)
			input, pos = skipLineComment(input, pos)
		case r == '(':
			tokens = append(tokens, token{kind: tokenLParen, text: "(", pos: start})
			input = input[size:]
			pos = pos.advance(r)
		case r == ')':
			tokens = append(tokens, token{kind: tokenRParen, text: ")", pos: start})
			input = input[size:]
			pos = pos.advance(r)
		case r == '\'':
			tokens = append(tokens, token{kind: tokenQuote, text: "'", pos: start})
			input = input[size:]
			pos = pos.advance(r)
		case r == '"':
			text, rest, nextPos, err := scanString(input[size:], pos.advance(r), start)
			if err != nil {
				return nil, err
			}
			tokens = append(tokens, token{kind: tokenString, text: text, pos: start})
			input = rest
			pos = nextPos
		default:
			text, rest, nextPos := scanAtom(input, pos)
			tokens = append(tokens, token{kind: tokenAtom, text: text, pos: start})
			input = rest
			pos = nextPos
		}
	}

	return tokens, nil
}

func skipLineComment(input string, pos sourcePos) (string, sourcePos) {
	for len(input) > 0 {
		r, size := utf8.DecodeRuneInString(input)
		input = input[size:]
		pos = pos.advance(r)
		if r == '\n' {
			return input, pos
		}
	}
	return "", pos
}

func scanString(input string, pos sourcePos, start sourcePos) (string, string, sourcePos, error) {
	var b strings.Builder

	for len(input) > 0 {
		r, size := utf8.DecodeRuneInString(input)
		input = input[size:]
		current := pos
		pos = pos.advance(r)

		switch r {
		case '"':
			return b.String(), input, pos, nil
		case '\\':
			if len(input) == 0 {
				return "", "", pos, errorAt(current, "unterminated string escape")
			}
			esc, escSize := utf8.DecodeRuneInString(input)
			input = input[escSize:]
			pos = pos.advance(esc)
			switch esc {
			case '"', '\\':
				b.WriteRune(esc)
			case 'n':
				b.WriteByte('\n')
			case 't':
				b.WriteByte('\t')
			default:
				b.WriteRune(esc)
			}
		default:
			b.WriteRune(r)
		}
	}

	return "", "", pos, errorAt(start, "unterminated string literal")
}

func scanAtom(input string, pos sourcePos) (string, string, sourcePos) {
	for i, r := range input {
		if unicode.IsSpace(r) || r == '(' || r == ')' || r == ';' {
			return input[:i], input[i:], pos
		}
		pos = pos.advance(r)
	}
	return input, "", pos
}

func (p *parser) parseProgram() ([]expr, error) {
	var forms []expr
	for p.pos < len(p.tokens) {
		form, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		forms = append(forms, form)
	}
	return forms, nil
}

func (p *parser) parseExpr() (expr, error) {
	if p.pos >= len(p.tokens) {
		return nil, errorAt(sourcePos{Line: 1, Col: 1}, "unexpected end of input")
	}

	tok := p.tokens[p.pos]
	p.pos++

	switch tok.kind {
	case tokenLParen:
		var items []expr
		for {
			if p.pos >= len(p.tokens) {
				return nil, errorAt(tok.pos, "unterminated list")
			}
			if p.tokens[p.pos].kind == tokenRParen {
				p.pos++
				return listExpr{items: items, pos: tok.pos}, nil
			}
			item, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			items = append(items, item)
		}
	case tokenRParen:
		return nil, errorAt(tok.pos, "unexpected ')'")
	case tokenQuote:
		quoted, err := p.parseExpr()
		if err != nil {
			return nil, attachPos(err, tok.pos)
		}
		return listExpr{
			items: []expr{
				symbolExpr{name: "quote", pos: tok.pos},
				quoted,
			},
			pos: tok.pos,
		}, nil
	case tokenString:
		return newStringExpr(tok.text, false), nil
	case tokenAtom:
		return parseAtom(tok), nil
	default:
		return nil, errorAt(tok.pos, "unknown token")
	}
}

func parseAtom(tok token) expr {
	switch tok.text {
	case "#t":
		return boolExpr(true)
	case "#f":
		return boolExpr(false)
	}

	if value, ok := parseCharLiteral(tok.text); ok {
		return charExpr(value)
	}

	if value, ok := parseNumberLiteral(tok.text); ok {
		return value
	}

	return symbolExpr{name: tok.text, pos: tok.pos}
}

func parseCharLiteral(text string) (rune, bool) {
	if !strings.HasPrefix(text, "#\\") {
		return 0, false
	}

	literal := text[2:]
	switch literal {
	case "space":
		return ' ', true
	case "newline":
		return '\n', true
	}

	runes := []rune(literal)
	if len(runes) != 1 {
		return 0, false
	}
	return runes[0], true
}

func evalSequence(environment *env, forms []expr) (expr, error) {
	result := expr(voidExpr{})
	for _, form := range forms {
		value, err := evalExpr(environment, form)
		if err != nil {
			return nil, attachPos(err, formPos(form))
		}
		result = value
	}
	return result, nil
}

func evalSequenceTail(environment *env, forms []expr) (evalStep, error) {
	if len(forms) == 0 {
		return doneStep(voidExpr{}), nil
	}

	for _, form := range forms[:len(forms)-1] {
		if _, err := evalExpr(environment, form); err != nil {
			return evalStep{}, attachPos(err, formPos(form))
		}
	}

	return tailStep(environment, forms[len(forms)-1]), nil
}

func evalExpr(environment *env, form expr) (expr, error) {
	currentEnv := environment
	currentForm := form

	for {
		step, err := evalExprStep(currentEnv, currentForm)
		if err != nil {
			return nil, err
		}
		if !step.tail {
			return step.value, nil
		}

		currentEnv = step.nextEnv
		currentForm = step.nextForm
	}
}

func evalExprStep(environment *env, form expr) (evalStep, error) {
	switch v := form.(type) {
	case intExpr, rationalExpr, inexactExpr, boolExpr, charExpr, *stringExpr:
		return doneStep(v), nil
	case symbolExpr:
		lookupEnv := environment
		if v.lookupEnv != nil {
			lookupEnv = v.lookupEnv
		}
		value, ok := lookupEnv.lookup(v.name)
		if !ok {
			return evalStep{}, errorAt(v.pos, fmt.Sprintf("unbound symbol: %s", v.name))
		}
		if _, ok := value.(uninitializedExpr); ok {
			return evalStep{}, errorAt(v.pos, fmt.Sprintf("uninitialized binding: %s", v.name))
		}
		return doneStep(value), nil
	case listExpr:
		return evalListStep(environment, v)
	default:
		return evalStep{}, &EvalError{Message: "unsupported expression"}
	}
}

func evalListStep(environment *env, items listExpr) (evalStep, error) {
	if len(items.items) == 0 {
		return evalStep{}, errorAt(items.pos, "cannot evaluate empty list")
	}

	if operator, ok := items.items[0].(symbolExpr); ok {
		switch operator.name {
		case "define":
			value, err := evalDefine(environment, items.items[1:])
			return doneStep(value), attachPos(err, operator.pos)
		case "define-record-type":
			value, err := evalDefineRecordType(environment, items.items[1:])
			return doneStep(value), attachPos(err, operator.pos)
		case "define-syntax":
			value, err := evalDefineSyntax(environment, items.items[1:])
			return doneStep(value), attachPos(err, operator.pos)
		case "set!":
			value, err := evalSet(environment, items.items[1:])
			return doneStep(value), attachPos(err, operator.pos)
		case "if":
			step, err := evalIf(environment, items.items[1:])
			return step, attachPos(err, operator.pos)
		case "begin":
			step, err := evalBegin(environment, items.items[1:])
			return step, attachPos(err, operator.pos)
		case "cond":
			step, err := evalCond(environment, items.items[1:])
			return step, attachPos(err, operator.pos)
		case "do":
			step, err := evalDo(environment, items.items[1:])
			return step, attachPos(err, operator.pos)
		case "case":
			step, err := evalCase(environment, items.items[1:])
			return step, attachPos(err, operator.pos)
		case "let":
			step, err := evalLet(environment, items.items[1:])
			return step, attachPos(err, operator.pos)
		case "let*":
			step, err := evalLetStar(environment, items.items[1:])
			return step, attachPos(err, operator.pos)
		case "letrec":
			step, err := evalLetrec(environment, items.items[1:], false)
			return step, attachPos(err, operator.pos)
		case "letrec*":
			step, err := evalLetrec(environment, items.items[1:], true)
			return step, attachPos(err, operator.pos)
		case "quote":
			value, err := evalQuote(items.items[1:])
			return doneStep(value), attachPos(err, operator.pos)
		case "lambda":
			value, err := evalLambda(environment, items.items[1:])
			return doneStep(value), attachPos(err, operator.pos)
		case "case-lambda":
			value, err := evalCaseLambda(environment, items.items[1:])
			return doneStep(value), attachPos(err, operator.pos)
		case "and":
			step, err := evalAnd(environment, items.items[1:])
			return step, attachPos(err, operator.pos)
		case "or":
			step, err := evalOr(environment, items.items[1:])
			return step, attachPos(err, operator.pos)
		}

		lookupEnv := environment
		if operator.lookupEnv != nil {
			lookupEnv = operator.lookupEnv
		}
		if macroValue, ok := lookupEnv.lookup(operator.name); ok {
			if macro, ok := macroValue.(macroExpr); ok {
				expanded, err := expandMacro(macro, items)
				if err != nil {
					return evalStep{}, attachPos(err, operator.pos)
				}
				return tailStep(environment, expanded), nil
			}
		}
	}

	operatorValue, err := evalExpr(environment, items.items[0])
	if err != nil {
		return evalStep{}, err
	}

	return applyProcedureStep(environment, operatorValue, items.items[1:], items.pos)
}

func evalDefine(environment *env, forms []expr) (expr, error) {
	if len(forms) < 2 {
		return nil, &EvalError{Message: "define expects a name and value"}
	}

	switch target := forms[0].(type) {
	case symbolExpr:
		if len(forms) != 2 {
			return nil, &EvalError{Message: "define expects exactly 2 arguments"}
		}
		value, err := evalExpr(environment, forms[1])
		if err != nil {
			return nil, err
		}
		environment.define(target.name, value)
		return voidExpr{}, nil
	case listExpr:
		if len(target.items) == 0 {
			return nil, &EvalError{Message: "define function name cannot be empty"}
		}
		name, ok := target.items[0].(symbolExpr)
		if !ok {
			return nil, &EvalError{Message: "define function name must be a symbol"}
		}
		params, restParam, variadic, err := parseParamList(target.items[1:])
		if err != nil {
			return nil, err
		}
		closure := closureExpr{
			params:    params,
			restParam: restParam,
			variadic:  variadic,
			body:      append([]expr(nil), forms[1:]...),
			env:       environment,
		}
		environment.define(name.name, closure)
		return voidExpr{}, nil
	default:
		return nil, &EvalError{Message: "define target must be a symbol or parameter list"}
	}
}

func evalSet(environment *env, forms []expr) (expr, error) {
	if len(forms) != 2 {
		return nil, &EvalError{Message: "set! expects exactly 2 arguments"}
	}

	target, ok := forms[0].(symbolExpr)
	if !ok {
		return nil, &EvalError{Message: "set! target must be a symbol"}
	}

	value, err := evalExpr(environment, forms[1])
	if err != nil {
		return nil, err
	}

	if !environment.assign(target.name, value) {
		return nil, errorAt(target.pos, fmt.Sprintf("unbound symbol: %s", target.name))
	}

	return voidExpr{}, nil
}

func evalIf(environment *env, forms []expr) (evalStep, error) {
	if len(forms) != 2 && len(forms) != 3 {
		return evalStep{}, &EvalError{Message: "if expects 2 or 3 arguments"}
	}

	condition, err := evalExpr(environment, forms[0])
	if err != nil {
		return evalStep{}, err
	}
	if isTruthy(condition) {
		return tailStep(environment, forms[1]), nil
	}
	if len(forms) == 2 {
		return doneStep(voidExpr{}), nil
	}
	return tailStep(environment, forms[2]), nil
}

func evalBegin(environment *env, forms []expr) (evalStep, error) {
	return evalSequenceTail(environment, forms)
}

func evalCond(environment *env, forms []expr) (evalStep, error) {
	for i, form := range forms {
		clause, ok := form.(listExpr)
		if !ok || len(clause.items) == 0 {
			return evalStep{}, &EvalError{Message: "cond clauses must be non-empty lists"}
		}

		if symbol, ok := clause.items[0].(symbolExpr); ok && symbol.name == "else" {
			if i != len(forms)-1 {
				return evalStep{}, &EvalError{Message: "cond else clause must be last"}
			}
			if len(clause.items) == 1 {
				return doneStep(voidExpr{}), nil
			}
			return evalSequenceTail(environment, clause.items[1:])
		}

		testValue, err := evalExpr(environment, clause.items[0])
		if err != nil {
			return evalStep{}, err
		}
		if !isTruthy(testValue) {
			continue
		}
		if len(clause.items) == 1 {
			return doneStep(testValue), nil
		}
		return evalSequenceTail(environment, clause.items[1:])
	}

	return doneStep(voidExpr{}), nil
}

func evalQuote(forms []expr) (expr, error) {
	if len(forms) != 1 {
		return nil, &EvalError{Message: "quote expects exactly 1 argument"}
	}
	return quoteDatum(forms[0]), nil
}

func evalLet(environment *env, forms []expr) (evalStep, error) {
	if len(forms) < 2 {
		return evalStep{}, &EvalError{Message: "let expects bindings and a body"}
	}

	if name, ok := forms[0].(symbolExpr); ok {
		return evalNamedLet(environment, name.name, forms[1:])
	}

	bindings, ok := forms[0].(listExpr)
	if !ok {
		return evalStep{}, &EvalError{Message: "let bindings must be a list"}
	}

	names, values, err := evalBindings(environment, bindings)
	if err != nil {
		return evalStep{}, err
	}

	letEnv := &env{
		parent:   environment,
		bindings: map[string]expr{},
	}
	for i, name := range names {
		letEnv.define(name, values[i])
	}

	return evalSequenceTail(letEnv, forms[1:])
}

func evalNamedLet(environment *env, name string, forms []expr) (evalStep, error) {
	if len(forms) < 2 {
		return evalStep{}, &EvalError{Message: "named let expects bindings and a body"}
	}

	bindings, ok := forms[0].(listExpr)
	if !ok {
		return evalStep{}, &EvalError{Message: "named let bindings must be a list"}
	}

	names, values, err := evalBindings(environment, bindings)
	if err != nil {
		return evalStep{}, err
	}

	letEnv := &env{
		parent:   environment,
		bindings: map[string]expr{},
	}
	closure := closureExpr{
		params: append([]string(nil), names...),
		body:   append([]expr(nil), forms[1:]...),
		env:    letEnv,
	}
	letEnv.define(name, closure)

	return applyCallableStep(closure, values)
}

func evalLambda(environment *env, forms []expr) (expr, error) {
	if len(forms) < 2 {
		return nil, &EvalError{Message: "lambda expects parameters and a body"}
	}

	params, restParam, variadic, err := parseLambdaParams(forms[0])
	if err != nil {
		return nil, err
	}

	return closureExpr{
		params:    params,
		restParam: restParam,
		variadic:  variadic,
		body:      append([]expr(nil), forms[1:]...),
		env:       environment,
	}, nil
}

func evalCaseLambda(environment *env, forms []expr) (expr, error) {
	if len(forms) == 0 {
		return nil, &EvalError{Message: "case-lambda expects at least 1 clause"}
	}

	clauses := make([]closureExpr, 0, len(forms))
	for _, form := range forms {
		clause, ok := form.(listExpr)
		if !ok || len(clause.items) < 2 {
			return nil, &EvalError{Message: "case-lambda clauses must include parameters and a body"}
		}

		params, restParam, variadic, err := parseLambdaParams(clause.items[0])
		if err != nil {
			return nil, attachPos(err, clause.pos)
		}

		clauses = append(clauses, closureExpr{
			params:    params,
			restParam: restParam,
			variadic:  variadic,
			body:      append([]expr(nil), clause.items[1:]...),
			env:       environment,
		})
	}

	return caseClosureExpr{clauses: clauses}, nil
}

func parseLambdaParams(form expr) ([]string, string, bool, error) {
	switch params := form.(type) {
	case symbolExpr:
		return nil, params.name, true, nil
	case listExpr:
		return parseParamList(params.items)
	default:
		return nil, "", false, &EvalError{Message: "lambda parameters must be a list or symbol"}
	}
}

func parseParamList(items []expr) ([]string, string, bool, error) {
	params := make([]string, 0, len(items))
	for i, item := range items {
		symbol, ok := item.(symbolExpr)
		if !ok {
			return nil, "", false, &EvalError{Message: "parameter name must be a symbol"}
		}
		if symbol.name == "." {
			if i != len(items)-2 {
				return nil, "", false, &EvalError{Message: "invalid dotted parameter list"}
			}

			restSymbol, ok := items[i+1].(symbolExpr)
			if !ok || restSymbol.name == "." {
				return nil, "", false, &EvalError{Message: "rest parameter name must be a symbol"}
			}
			return params, restSymbol.name, true, nil
		}
		params = append(params, symbol.name)
	}
	return params, "", false, nil
}

func evalBindings(environment *env, bindings listExpr) ([]string, []expr, error) {
	names := make([]string, 0, len(bindings.items))
	values := make([]expr, 0, len(bindings.items))

	for _, binding := range bindings.items {
		pair, ok := binding.(listExpr)
		if !ok || len(pair.items) != 2 {
			return nil, nil, &EvalError{Message: "let bindings must have the form (name value)"}
		}

		name, ok := pair.items[0].(symbolExpr)
		if !ok {
			return nil, nil, &EvalError{Message: "let binding name must be a symbol"}
		}

		value, err := evalExpr(environment, pair.items[1])
		if err != nil {
			return nil, nil, err
		}

		names = append(names, name.name)
		values = append(values, value)
	}

	return names, values, nil
}

func applyProcedureStep(environment *env, proc expr, argForms []expr, callPos sourcePos) (evalStep, error) {
	args, err := evalArgs(environment, argForms)
	if err != nil {
		return evalStep{}, attachPos(err, callPos)
	}

	step, err := applyCallableStep(proc, args)
	return step, attachPos(err, callPos)
}

func applyCallable(proc expr, args []expr) (expr, error) {
	if currentBenchLevel() >= 18 {
		return applyCallableLevel18(proc, args)
	}

	step, err := applyCallableStep(proc, args)
	if err != nil {
		return nil, err
	}
	if step.tail {
		return evalExpr(step.nextEnv, step.nextForm)
	}
	return step.value, nil
}

func applyCallableStep(proc expr, args []expr) (evalStep, error) {
	switch callable := proc.(type) {
	case builtinProc:
		if callable.name == "apply" {
			return applyBuiltinStep(args)
		}

		value, err := callable.fn(args)
		if err != nil {
			return evalStep{}, err
		}
		return doneStep(value), nil
	case closureExpr:
		return prepareClosureCall(callable, args)
	case caseClosureExpr:
		for _, clause := range callable.clauses {
			if closureMatchesArity(clause, len(args)) {
				return prepareClosureCall(clause, args)
			}
		}
		return evalStep{}, &EvalError{Message: fmt.Sprintf("no matching case-lambda clause for %d arguments", len(args))}
	case recordConstructorProc:
		value, err := applyRecordConstructor(callable, args)
		if err != nil {
			return evalStep{}, err
		}
		return doneStep(value), nil
	case recordPredicateProc:
		value, err := applyRecordPredicate(callable, args)
		if err != nil {
			return evalStep{}, err
		}
		return doneStep(value), nil
	case recordAccessorProc:
		value, err := applyRecordAccessor(callable, args)
		if err != nil {
			return evalStep{}, err
		}
		return doneStep(value), nil
	case recordMutatorProc:
		value, err := applyRecordMutator(callable, args)
		if err != nil {
			return evalStep{}, err
		}
		return doneStep(value), nil
	default:
		return evalStep{}, &EvalError{Message: "first list element is not a procedure"}
	}
}

func closureMatchesArity(callable closureExpr, argc int) bool {
	if callable.variadic {
		return argc >= len(callable.params)
	}
	return argc == len(callable.params)
}

func prepareClosureCall(callable closureExpr, args []expr) (evalStep, error) {
	if !callable.variadic && len(args) != len(callable.params) {
		return evalStep{}, &EvalError{Message: fmt.Sprintf("expected %d arguments, got %d", len(callable.params), len(args))}
	}
	if callable.variadic && len(args) < len(callable.params) {
		return evalStep{}, &EvalError{Message: fmt.Sprintf("expected at least %d arguments, got %d", len(callable.params), len(args))}
	}

	callEnv := &env{
		parent:   callable.env,
		bindings: map[string]expr{},
	}
	for i, name := range callable.params {
		callEnv.define(name, args[i])
	}
	if callable.variadic {
		rest := append([]expr(nil), args[len(callable.params):]...)
		callEnv.define(callable.restParam, properListFromSlice(rest))
	}
	return evalSequenceTail(callEnv, callable.body)
}

func evalArgs(environment *env, forms []expr) ([]expr, error) {
	args := make([]expr, 0, len(forms))
	for _, form := range forms {
		value, err := evalExpr(environment, form)
		if err != nil {
			return nil, err
		}
		args = append(args, value)
	}
	return args, nil
}

func evalAnd(environment *env, forms []expr) (evalStep, error) {
	if len(forms) == 0 {
		return doneStep(boolExpr(true)), nil
	}

	for _, form := range forms[:len(forms)-1] {
		value, err := evalExpr(environment, form)
		if err != nil {
			return evalStep{}, err
		}
		if !isTruthy(value) {
			return doneStep(value), nil
		}
	}

	return tailStep(environment, forms[len(forms)-1]), nil
}

func evalOr(environment *env, forms []expr) (evalStep, error) {
	if len(forms) == 0 {
		return doneStep(boolExpr(false)), nil
	}

	for _, form := range forms[:len(forms)-1] {
		value, err := evalExpr(environment, form)
		if err != nil {
			return evalStep{}, err
		}
		if isTruthy(value) {
			return doneStep(value), nil
		}
	}

	return tailStep(environment, forms[len(forms)-1]), nil
}

func applyBuiltinStep(args []expr) (evalStep, error) {
	if len(args) < 2 {
		return evalStep{}, &EvalError{Message: "apply expects at least 2 arguments"}
	}

	tailList, ok := listElements(args[len(args)-1])
	if !ok {
		return evalStep{}, &EvalError{Message: "apply expects a list as its final argument"}
	}

	callArgs := make([]expr, 0, len(args)-2+len(tailList))
	callArgs = append(callArgs, args[1:len(args)-1]...)
	callArgs = append(callArgs, tailList...)
	return applyCallableStep(args[0], callArgs)
}

func builtinAdd(args []expr) (expr, error) {
	if total, ok := tryIntSum(args); ok {
		return intExpr(total), nil
	}

	numbers, err := numericArgs(args)
	if err != nil {
		return nil, err
	}

	if numbersContainInexact(numbers) {
		total := 0.0
		for _, n := range numbers {
			total += numberToFloat(n)
		}
		return newInexactExpr(total), nil
	}

	total := new(big.Rat)
	for _, n := range numbers {
		total.Add(total, n.exact)
	}
	return exprFromRat(total), nil
}

func builtinSub(args []expr) (expr, error) {
	if len(args) == 0 {
		return nil, &EvalError{Message: "- expects at least 1 argument"}
	}
	if total, ok := tryIntDifference(args); ok {
		return intExpr(total), nil
	}

	numbers, err := numericArgs(args)
	if err != nil {
		return nil, err
	}

	if numbersContainInexact(numbers) {
		result := numberToFloat(numbers[0])
		if len(numbers) == 1 {
			return newInexactExpr(-result), nil
		}

		for _, n := range numbers[1:] {
			result -= numberToFloat(n)
		}
		return newInexactExpr(result), nil
	}

	if len(numbers) == 1 {
		result := copyRat(numbers[0].exact)
		result.Neg(result)
		return exprFromRat(result), nil
	}

	result := copyRat(numbers[0].exact)
	for _, n := range numbers[1:] {
		result.Sub(result, n.exact)
	}
	return exprFromRat(result), nil
}

func builtinMul(args []expr) (expr, error) {
	if total, ok := tryIntProduct(args); ok {
		return intExpr(total), nil
	}

	numbers, err := numericArgs(args)
	if err != nil {
		return nil, err
	}

	if numbersContainInexact(numbers) {
		result := 1.0
		for _, n := range numbers {
			result *= numberToFloat(n)
		}
		return newInexactExpr(result), nil
	}

	result := big.NewRat(1, 1)
	for _, n := range numbers {
		result.Mul(result, n.exact)
	}
	return exprFromRat(result), nil
}

func builtinDiv(args []expr) (expr, error) {
	numbers, err := numericArgs(args)
	if err != nil {
		return nil, err
	}
	if len(numbers) == 0 {
		return nil, &EvalError{Message: "/ expects at least 1 argument"}
	}

	if numbersContainInexact(numbers) {
		result := numberToFloat(numbers[0])
		if len(numbers) == 1 {
			if result == 0 {
				return nil, &EvalError{Message: "division by zero"}
			}
			return newInexactExpr(1 / result), nil
		}

		for _, n := range numbers[1:] {
			divisor := numberToFloat(n)
			if divisor == 0 {
				return nil, &EvalError{Message: "division by zero"}
			}
			result /= divisor
		}
		return newInexactExpr(result), nil
	}

	if len(numbers) == 1 {
		if numberIsZero(numbers[0]) {
			return nil, &EvalError{Message: "division by zero"}
		}
		result := big.NewRat(1, 1)
		result.Quo(result, numbers[0].exact)
		return exprFromRat(result), nil
	}

	result := copyRat(numbers[0].exact)
	for _, n := range numbers[1:] {
		if numberIsZero(n) {
			return nil, &EvalError{Message: "division by zero"}
		}
		result.Quo(result, n.exact)
	}
	return exprFromRat(result), nil
}

func builtinNot(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "not expects exactly 1 argument"}
	}
	return boolExpr(!isTruthy(args[0])), nil
}

func builtinAbs(args []expr) (expr, error) {
	number, err := unaryNumberArg(args, "abs")
	if err != nil {
		return nil, err
	}

	if number.isInexact {
		value := numberToFloat(number)
		if value < 0 {
			value = -value
		}
		return newInexactExpr(value), nil
	}

	result := copyRat(number.exact)
	if result.Sign() < 0 {
		result.Neg(result)
	}
	return exprFromRat(result), nil
}

func builtinAppend(args []expr) (expr, error) {
	result := make([]expr, 0)
	for _, arg := range args {
		items, ok := listElements(arg)
		if !ok {
			return nil, &EvalError{Message: "append expects list arguments"}
		}
		result = append(result, items...)
	}
	return properListFromSlice(result), nil
}

func builtinApply(args []expr) (expr, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "apply expects at least 2 arguments"}
	}

	tailList, ok := listElements(args[len(args)-1])
	if !ok {
		return nil, &EvalError{Message: "apply expects a list as its final argument"}
	}

	callArgs := make([]expr, 0, len(args)-2+len(tailList))
	callArgs = append(callArgs, args[1:len(args)-1]...)
	callArgs = append(callArgs, tailList...)
	return applyCallable(args[0], callArgs)
}

func builtinCar(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "car expects exactly 1 argument"}
	}

	value, ok := carValue(args[0])
	if !ok {
		return nil, &EvalError{Message: "car expects a non-empty list"}
	}
	return value, nil
}

func builtinCdr(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "cdr expects exactly 1 argument"}
	}

	value, ok := cdrValue(args[0])
	if !ok {
		return nil, &EvalError{Message: "cdr expects a non-empty list"}
	}
	return value, nil
}

func builtinCons(args []expr) (expr, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "cons expects exactly 2 arguments"}
	}

	return &pairExpr{car: args[0], cdr: args[1]}, nil
}

func builtinLength(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "length expects exactly 1 argument"}
	}

	list, ok := listElements(args[0])
	if !ok {
		return nil, &EvalError{Message: "length expects a list"}
	}
	return intExpr(len(list)), nil
}

func builtinList(args []expr) (expr, error) {
	result := make([]expr, len(args))
	copy(result, args)
	return properListFromSlice(result), nil
}

func builtinListPred(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "list? expects exactly 1 argument"}
	}

	return boolExpr(isProperListValue(args[0])), nil
}

func builtinListRef(args []expr) (expr, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "list-ref expects exactly 2 arguments"}
	}

	list, ok := listElements(args[0])
	if !ok {
		return nil, &EvalError{Message: "list-ref expects a list as its first argument"}
	}

	index, ok := args[1].(intExpr)
	if !ok {
		return nil, &EvalError{Message: "list-ref expects a numeric index"}
	}

	idx := int(index)
	if idx < 0 || idx >= len(list) {
		return nil, &EvalError{Message: "list-ref index out of range"}
	}

	return list[idx], nil
}

func builtinListTail(args []expr) (expr, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "list-tail expects exactly 2 arguments"}
	}

	list, ok := listElements(args[0])
	if !ok {
		return nil, &EvalError{Message: "list-tail expects a list as its first argument"}
	}

	index, ok := args[1].(intExpr)
	if !ok {
		return nil, &EvalError{Message: "list-tail expects a numeric index"}
	}

	idx := int(index)
	if idx < 0 || idx > len(list) {
		return nil, &EvalError{Message: "list-tail index out of range"}
	}

	items := append([]expr(nil), list[idx:]...)
	return properListFromSlice(items), nil
}

func builtinMap(args []expr) (expr, error) {
	if len(args) < 2 {
		return nil, &EvalError{Message: "map expects a procedure and at least one list"}
	}

	lists := make([][]expr, len(args)-1)
	expectedLen := -1
	for i, arg := range args[1:] {
		list, ok := listElements(arg)
		if !ok {
			return nil, &EvalError{Message: "map expects list arguments"}
		}
		if expectedLen == -1 {
			expectedLen = len(list)
		} else if len(list) != expectedLen {
			return nil, &EvalError{Message: "map expects lists of equal length"}
		}
		lists[i] = list
	}

	result := make([]expr, expectedLen)
	for i := 0; i < expectedLen; i++ {
		callArgs := make([]expr, len(lists))
		for j, list := range lists {
			callArgs[j] = list[i]
		}

		value, err := applyCallable(args[0], callArgs)
		if err != nil {
			return nil, err
		}
		result[i] = value
	}

	return properListFromSlice(result), nil
}

func builtinMax(args []expr) (expr, error) {
	numbers, err := numericArgs(args)
	if err != nil {
		return nil, err
	}
	if len(numbers) == 0 {
		return nil, &EvalError{Message: "max expects at least 1 argument"}
	}

	inexact := numbersContainInexact(numbers)
	result := numbers[0]
	for _, n := range numbers[1:] {
		if compareNumbers(n, result) > 0 {
			result = n
		}
	}
	if inexact {
		return newInexactExpr(numberToFloat(result)), nil
	}
	return exprFromRat(result.exact), nil
}

func builtinMin(args []expr) (expr, error) {
	numbers, err := numericArgs(args)
	if err != nil {
		return nil, err
	}
	if len(numbers) == 0 {
		return nil, &EvalError{Message: "min expects at least 1 argument"}
	}

	inexact := numbersContainInexact(numbers)
	result := numbers[0]
	for _, n := range numbers[1:] {
		if compareNumbers(n, result) < 0 {
			result = n
		}
	}
	if inexact {
		return newInexactExpr(numberToFloat(result)), nil
	}
	return exprFromRat(result.exact), nil
}

func builtinModulo(args []expr) (expr, error) {
	a, b, err := exactIntegerPair(args, "modulo")
	if err != nil {
		return nil, err
	}
	if b == 0 {
		return nil, &EvalError{Message: "division by zero"}
	}

	result := a % b
	if result != 0 && ((result < 0) != (b < 0)) {
		result += b
	}
	return intExpr(result), nil
}

func builtinNull(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "null? expects exactly 1 argument"}
	}

	return boolExpr(isEmptyList(args[0])), nil
}

func makeDisplayBuiltin(rt *runtime) builtinFunc {
	return func(args []expr) (expr, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "display expects exactly 1 argument"}
		}
		rt.writeString(displayExpr(args[0]))
		return voidExpr{}, nil
	}
}

func makeWriteBuiltin(rt *runtime) builtinFunc {
	return func(args []expr) (expr, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "write expects exactly 1 argument"}
		}
		rt.writeString(renderExpr(args[0]))
		return voidExpr{}, nil
	}
}

func makeNewlineBuiltin(rt *runtime) builtinFunc {
	return func(args []expr) (expr, error) {
		if len(args) != 0 {
			return nil, &EvalError{Message: "newline expects exactly 0 arguments"}
		}
		rt.writeString("\n")
		return voidExpr{}, nil
	}
}

func builtinOdd(args []expr) (expr, error) {
	n, err := unaryExactIntegerArg(args, "odd?")
	if err != nil {
		return nil, err
	}
	return boolExpr(n%2 != 0), nil
}

func builtinEven(args []expr) (expr, error) {
	n, err := unaryExactIntegerArg(args, "even?")
	if err != nil {
		return nil, err
	}
	return boolExpr(n%2 == 0), nil
}

func builtinPositive(args []expr) (expr, error) {
	n, err := unaryNumberArg(args, "positive?")
	if err != nil {
		return nil, err
	}
	return boolExpr(numberSign(n) > 0), nil
}

func builtinNegative(args []expr) (expr, error) {
	n, err := unaryNumberArg(args, "negative?")
	if err != nil {
		return nil, err
	}
	return boolExpr(numberSign(n) < 0), nil
}

func builtinZero(args []expr) (expr, error) {
	n, err := unaryNumberArg(args, "zero?")
	if err != nil {
		return nil, err
	}
	return boolExpr(numberIsZero(n)), nil
}

func builtinQuotient(args []expr) (expr, error) {
	a, b, err := exactIntegerPair(args, "quotient")
	if err != nil {
		return nil, err
	}
	if b == 0 {
		return nil, &EvalError{Message: "division by zero"}
	}
	return intExpr(a / b), nil
}

func builtinRemainder(args []expr) (expr, error) {
	a, b, err := exactIntegerPair(args, "remainder")
	if err != nil {
		return nil, err
	}
	if b == 0 {
		return nil, &EvalError{Message: "division by zero"}
	}
	return intExpr(a % b), nil
}

func builtinExpt(args []expr) (expr, error) {
	base, exponent, err := exactIntegerPair(args, "expt")
	if err != nil {
		return nil, err
	}
	if exponent < 0 {
		return nil, &EvalError{Message: "expt expects a non-negative exponent"}
	}

	result := 1
	power := base
	exp := exponent
	for exp > 0 {
		if exp%2 == 1 {
			result *= power
		}
		exp /= 2
		if exp > 0 {
			power *= power
		}
	}
	return intExpr(result), nil
}

func builtinStringAppend(args []expr) (expr, error) {
	var b strings.Builder
	for _, arg := range args {
		text, ok := asString(arg)
		if !ok {
			return nil, &EvalError{Message: "string-append expects string arguments"}
		}
		b.WriteString(text.text())
	}
	return newAllocatedString(b.String()), nil
}

func builtinStringCopy(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "string-copy expects exactly 1 argument"}
	}

	text, ok := asString(args[0])
	if !ok {
		return nil, &EvalError{Message: "string-copy expects a string"}
	}
	return text.copy(!stringsImmutableEnabled()), nil
}

func builtinStringDowncase(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "string-downcase expects exactly 1 argument"}
	}

	text, ok := asString(args[0])
	if !ok {
		return nil, &EvalError{Message: "string-downcase expects a string"}
	}
	return newAllocatedString(strings.ToLower(text.text())), nil
}

func builtinStringLength(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "string-length expects exactly 1 argument"}
	}

	text, ok := asString(args[0])
	if !ok {
		return nil, &EvalError{Message: "string-length expects a string"}
	}
	return intExpr(len(text.runes)), nil
}

func builtinSubstring(args []expr) (expr, error) {
	if len(args) != 3 {
		return nil, &EvalError{Message: "substring expects exactly 3 arguments"}
	}

	text, ok := asString(args[0])
	if !ok {
		return nil, &EvalError{Message: "substring expects a string as its first argument"}
	}

	start, ok := args[1].(intExpr)
	if !ok {
		return nil, &EvalError{Message: "substring expects numeric start and end indexes"}
	}
	end, ok := args[2].(intExpr)
	if !ok {
		return nil, &EvalError{Message: "substring expects numeric start and end indexes"}
	}

	startIdx := int(start)
	endIdx := int(end)
	if startIdx < 0 || endIdx < startIdx || endIdx > len(text.runes) {
		return nil, &EvalError{Message: "substring index out of range"}
	}

	return newAllocatedString(string(text.runes[startIdx:endIdx])), nil
}

func builtinStringToNumber(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "string->number expects exactly 1 argument"}
	}

	text, ok := asString(args[0])
	if !ok {
		return nil, &EvalError{Message: "string->number expects a string"}
	}

	value, ok := parseNumberLiteral(text.text())
	if !ok {
		return boolExpr(false), nil
	}
	return value, nil
}

func builtinNumberToString(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "number->string expects exactly 1 argument"}
	}

	text, ok := renderNumber(args[0])
	if !ok {
		return nil, &EvalError{Message: "number->string expects a number"}
	}
	return newAllocatedString(text), nil
}

func builtinSymbolToString(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "symbol->string expects exactly 1 argument"}
	}

	symbol, ok := args[0].(symbolExpr)
	if !ok {
		return nil, &EvalError{Message: "symbol->string expects a symbol"}
	}
	return newAllocatedString(symbol.name), nil
}

func builtinStringToSymbol(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "string->symbol expects exactly 1 argument"}
	}

	text, ok := asString(args[0])
	if !ok {
		return nil, &EvalError{Message: "string->symbol expects a string"}
	}
	return symbolExpr{name: text.text()}, nil
}

func builtinStringRef(args []expr) (expr, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "string-ref expects exactly 2 arguments"}
	}

	text, ok := asString(args[0])
	if !ok {
		return nil, &EvalError{Message: "string-ref expects a string as its first argument"}
	}

	index, ok := args[1].(intExpr)
	if !ok {
		return nil, &EvalError{Message: "string-ref expects a numeric index"}
	}

	idx := int(index)
	if idx < 0 || idx >= len(text.runes) {
		return nil, &EvalError{Message: "string-ref index out of range"}
	}

	return charExpr(text.runes[idx]), nil
}

func builtinStringSet(args []expr) (expr, error) {
	if len(args) != 3 {
		return nil, &EvalError{Message: "string-set! expects exactly 3 arguments"}
	}

	text, ok := asString(args[0])
	if !ok {
		return nil, &EvalError{Message: "string-set! expects a string as its first argument"}
	}
	if stringsImmutableEnabled() {
		return nil, &EvalError{Message: "string-set! expects a mutable string"}
	}
	if !text.mutable {
		return nil, &EvalError{Message: "string-set! expects a mutable string"}
	}

	index, ok := args[1].(intExpr)
	if !ok {
		return nil, &EvalError{Message: "string-set! expects a numeric index"}
	}

	ch, ok := args[2].(charExpr)
	if !ok {
		return nil, &EvalError{Message: "string-set! expects a character as its third argument"}
	}

	idx := int(index)
	if idx < 0 || idx >= len(text.runes) {
		return nil, &EvalError{Message: "string-set! index out of range"}
	}

	text.runes[idx] = rune(ch)
	return voidExpr{}, nil
}

func builtinStringUpcase(args []expr) (expr, error) {
	if len(args) != 1 {
		return nil, &EvalError{Message: "string-upcase expects exactly 1 argument"}
	}

	text, ok := asString(args[0])
	if !ok {
		return nil, &EvalError{Message: "string-upcase expects a string"}
	}
	return newAllocatedString(strings.ToUpper(text.text())), nil
}

func builtinCharAlphabetic(args []expr) (expr, error) {
	ch, err := unaryCharArg(args, "char-alphabetic?")
	if err != nil {
		return nil, err
	}
	return boolExpr(unicode.IsLetter(ch)), nil
}

func builtinCharNumeric(args []expr) (expr, error) {
	ch, err := unaryCharArg(args, "char-numeric?")
	if err != nil {
		return nil, err
	}
	return boolExpr(unicode.IsDigit(ch)), nil
}

func builtinCharUpcase(args []expr) (expr, error) {
	ch, err := unaryCharArg(args, "char-upcase")
	if err != nil {
		return nil, err
	}
	return charExpr(unicode.ToUpper(ch)), nil
}

func builtinCharDowncase(args []expr) (expr, error) {
	ch, err := unaryCharArg(args, "char-downcase")
	if err != nil {
		return nil, err
	}
	return charExpr(unicode.ToLower(ch)), nil
}

func builtinEq(args []expr) (expr, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "eq? expects exactly 2 arguments"}
	}
	return boolExpr(eqExpr(args[0], args[1])), nil
}

func builtinEqual(args []expr) (expr, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "equal? expects exactly 2 arguments"}
	}
	return boolExpr(equalExpr(args[0], args[1])), nil
}

func builtinAssoc(args []expr) (expr, error) {
	if len(args) != 2 {
		return nil, &EvalError{Message: "assoc expects exactly 2 arguments"}
	}

	alist, ok := listElements(args[1])
	if !ok {
		return nil, &EvalError{Message: "assoc expects a list as its second argument"}
	}

	for _, entry := range alist {
		key, ok := carValue(entry)
		if !ok {
			return nil, &EvalError{Message: "assoc expects association entries to be pairs"}
		}
		if equalExpr(args[0], key) {
			return entry, nil
		}
	}

	return boolExpr(false), nil
}

func comparisonBuiltin(name string, cmp func(int, int) bool) builtinFunc {
	return func(args []expr) (expr, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%s expects at least 2 arguments", name)}
		}
		if previous, ok := plainIntExpr(args[0]); ok {
			intsOnly := true
			for i := 1; i < len(args); i++ {
				current, ok := plainIntExpr(args[i])
				if !ok {
					intsOnly = false
					break
				}
				if !cmp(compareInts(previous, current), 0) {
					return boolExpr(false), nil
				}
				previous = current
			}
			if intsOnly {
				return boolExpr(true), nil
			}
		}

		numbers, err := numericArgs(args)
		if err != nil {
			return nil, err
		}

		for i := 0; i < len(numbers)-1; i++ {
			order := compareNumbers(numbers[i], numbers[i+1])
			if !cmp(order, 0) {
				return boolExpr(false), nil
			}
		}
		return boolExpr(true), nil
	}
}

func charComparisonBuiltin(name string, cmp func(rune, rune) bool) builtinFunc {
	return func(args []expr) (expr, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%s expects at least 2 arguments", name)}
		}

		values := make([]rune, 0, len(args))
		for _, arg := range args {
			ch, ok := arg.(charExpr)
			if !ok {
				return nil, &EvalError{Message: fmt.Sprintf("%s expects character arguments", name)}
			}
			values = append(values, rune(ch))
		}

		for i := 0; i < len(values)-1; i++ {
			if !cmp(values[i], values[i+1]) {
				return boolExpr(false), nil
			}
		}
		return boolExpr(true), nil
	}
}

func stringComparisonBuiltin(name string, cmp func(string, string) bool) builtinFunc {
	return func(args []expr) (expr, error) {
		if len(args) < 2 {
			return nil, &EvalError{Message: fmt.Sprintf("%s expects at least 2 arguments", name)}
		}

		values := make([]string, 0, len(args))
		for _, arg := range args {
			text, ok := asString(arg)
			if !ok {
				return nil, &EvalError{Message: fmt.Sprintf("%s expects string arguments", name)}
			}
			values = append(values, text.text())
		}

		for i := 0; i < len(values)-1; i++ {
			if !cmp(values[i], values[i+1]) {
				return boolExpr(false), nil
			}
		}
		return boolExpr(true), nil
	}
}

func typePredicate(test func(expr) bool) builtinFunc {
	return func(args []expr) (expr, error) {
		if len(args) != 1 {
			return nil, &EvalError{Message: "predicate expects exactly 1 argument"}
		}
		return boolExpr(test(args[0])), nil
	}
}

func isProcedure(value expr) bool {
	switch value.(type) {
	case builtinProc, closureExpr, caseClosureExpr, *continuationExpr, recordConstructorProc, recordPredicateProc, recordAccessorProc, recordMutatorProc:
		return true
	default:
		return false
	}
}

func unaryCharArg(args []expr, name string) (rune, error) {
	if len(args) != 1 {
		return 0, &EvalError{Message: fmt.Sprintf("%s expects exactly 1 argument", name)}
	}

	ch, ok := args[0].(charExpr)
	if !ok {
		return 0, &EvalError{Message: fmt.Sprintf("%s expects a character", name)}
	}
	return rune(ch), nil
}

func eqExpr(a, b expr) bool {
	if equal, ok := numericEqualExpr(a, b); ok {
		return equal
	}

	switch left := a.(type) {
	case intExpr:
		right, ok := b.(intExpr)
		return ok && left == right
	case boolExpr:
		right, ok := b.(boolExpr)
		return ok && left == right
	case charExpr:
		right, ok := b.(charExpr)
		return ok && left == right
	case symbolExpr:
		right, ok := b.(symbolExpr)
		return ok && left.name == right.name
	case *stringExpr:
		right, ok := asString(b)
		return ok && left == right
	case listExpr:
		right, ok := b.(listExpr)
		return ok && len(left.items) == 0 && len(right.items) == 0
	case *pairExpr:
		right, ok := b.(*pairExpr)
		return ok && left == right
	case *vectorExpr:
		right, ok := b.(*vectorExpr)
		return ok && left == right
	case builtinProc:
		right, ok := b.(builtinProc)
		return ok && left.name == right.name
	case closureExpr:
		return false
	case voidExpr:
		_, ok := b.(voidExpr)
		return ok
	default:
		return false
	}
}

func equalExpr(a, b expr) bool {
	return equalExprSeen(a, b, map[pairCompareKey]struct{}{}, map[vectorCompareKey]struct{}{})
}

func isTruthy(value expr) bool {
	b, ok := value.(boolExpr)
	return !ok || bool(b)
}

func renderExpr(value expr) string {
	if text, ok := renderNumber(value); ok {
		return text
	}

	switch v := value.(type) {
	case boolExpr:
		if bool(v) {
			return "#t"
		}
		return "#f"
	case *stringExpr:
		return strconv.Quote(v.text())
	case charExpr:
		return renderChar(v)
	case symbolExpr:
		return v.name
	case listExpr:
		parts := make([]string, 0, len(v.items))
		for _, item := range v.items {
			parts = append(parts, renderExpr(item))
		}
		return "(" + strings.Join(parts, " ") + ")"
	case *pairExpr:
		return renderPair(v, renderExpr)
	case *vectorExpr:
		return renderVector(v, renderExpr)
	case voidExpr:
		return ""
	case builtinProc:
		return "#<procedure:" + v.name + ">"
	case closureExpr:
		return "#<procedure>"
	case caseClosureExpr:
		return "#<procedure>"
	case *continuationExpr:
		return "#<continuation>"
	case recordConstructorProc:
		return "#<procedure:" + v.recordType.constructorName + ">"
	case recordPredicateProc:
		return "#<procedure:" + v.name + ">"
	case recordAccessorProc:
		return "#<procedure:" + v.name + ">"
	case recordMutatorProc:
		return "#<procedure:" + v.name + ">"
	case *recordExpr:
		return "#<record:" + v.recordType.name + ">"
	case macroExpr:
		return "#<macro>"
	case uninitializedExpr:
		return "#<uninitialized>"
	default:
		return ""
	}
}

func displayExpr(value expr) string {
	switch v := value.(type) {
	case *stringExpr:
		return v.text()
	case charExpr:
		return string(rune(v))
	case listExpr:
		parts := make([]string, 0, len(v.items))
		for _, item := range v.items {
			parts = append(parts, displayExpr(item))
		}
		return "(" + strings.Join(parts, " ") + ")"
	case *pairExpr:
		return renderPair(v, displayExpr)
	case *vectorExpr:
		return renderVector(v, displayExpr)
	case uninitializedExpr:
		return "#<uninitialized>"
	default:
		return renderExpr(value)
	}
}

func renderPair(pair *pairExpr, render func(expr) string) string {
	parts := []string{}
	seen := map[*pairExpr]struct{}{}
	tail := expr(pair)

	for {
		switch v := tail.(type) {
		case *pairExpr:
			if _, ok := seen[v]; ok {
				return "(" + strings.Join(parts, " ") + " . #<cycle>)"
			}
			seen[v] = struct{}{}
			parts = append(parts, render(v.car))
			tail = v.cdr
		case listExpr:
			for _, item := range v.items {
				parts = append(parts, render(item))
			}
			return "(" + strings.Join(parts, " ") + ")"
		default:
			return "(" + strings.Join(parts, " ") + " . " + render(tail) + ")"
		}
	}
}

func compareStringLex(a, b string) int {
	left := []rune(a)
	right := []rune(b)
	for i := 0; i < len(left) && i < len(right); i++ {
		if left[i] < right[i] {
			return -1
		}
		if left[i] > right[i] {
			return 1
		}
	}
	switch {
	case len(left) < len(right):
		return -1
	case len(left) > len(right):
		return 1
	default:
		return 0
	}
}

func renderChar(value charExpr) string {
	switch rune(value) {
	case ' ':
		return "#\\space"
	case '\n':
		return "#\\newline"
	default:
		return "#\\" + string(rune(value))
	}
}
