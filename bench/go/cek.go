package ming

import "fmt"

// CEK machine for first-class continuations (call/cc).
// Arguments are evaluated right-to-left so that continuations
// captured by call/cc re-evaluate preceding arguments on reinvocation.

// ---------- Continuation interface ----------

type kont interface{ isKont() }

// ---------- Continuation frame types ----------

type kontHalt struct{}

func (*kontHalt) isKont() {}

// Top-level expression sequence — tracks last non-void result.
type kontTopSeq struct {
	remaining []*expr
	env       *env
	lastVal   value
	hasResult bool
	parent    kont
}

func (*kontTopSeq) isKont() {}

// Evaluated operator, now evaluate arguments (right-to-left).
type kontEvFn struct {
	argExprs []*expr
	callExpr *expr
	callEnv  *env
	parent   kont
}

func (*kontEvFn) isKont() {}

// Evaluating arguments right-to-left.
type kontEvArg struct {
	op       value
	argExprs []*expr  // all arg exprs (original order)
	evaled   []value  // partially filled (immutable per frame)
	idx      int      // current arg index
	callExpr *expr
	callEnv  *env
	parent   kont
}

func (*kontEvArg) isKont() {}

type kontIf struct {
	conseq *expr
	alt    *expr // nil if no else
	env    *env
	parent kont
}

func (*kontIf) isKont() {}

type kontDefineK struct {
	name   string
	env    *env
	parent kont
}

func (*kontDefineK) isKont() {}

type kontSetK struct {
	name   string
	expr   *expr
	env    *env
	parent kont
}

func (*kontSetK) isKont() {}

// Sequence: discard current value, evaluate remaining exprs.
type kontSeq struct {
	exprs  []*expr
	env    *env
	parent kont
}

func (*kontSeq) isKont() {}

type kontAnd struct {
	remaining []*expr
	env       *env
	parent    kont
}

func (*kontAnd) isKont() {}

type kontOr struct {
	remaining []*expr
	env       *env
	parent    kont
}

func (*kontOr) isKont() {}

// Let/let* binding evaluation.
type kontLetBind struct {
	name      string
	remaining []*expr // remaining binding lists
	letEnv    *env
	evalEnv   *env // outer for let, letEnv for let*
	body      []*expr
	parent    kont
}

func (*kontLetBind) isKont() {}

// Named let init evaluation (right-to-left).
type kontNamedLetInit struct {
	lam      *lambda
	argExprs []*expr
	evaled   []value
	idx      int
	evalEnv  *env
	callExpr *expr
	parent   kont
}

func (*kontNamedLetInit) isKont() {}

// Letrec/letrec* binding evaluation.
type kontLetrecBind struct {
	names  []string
	idx    int
	inits  []*expr
	letEnv *env
	body   []*expr
	parent kont
}

func (*kontLetrecBind) isKont() {}

type kontCondTest struct {
	body   []*expr
	rest   []*expr // remaining clauses
	env    *env
	parent kont
}

func (*kontCondTest) isKont() {}

// cond => arrow: proc evaluated, now apply to test value
type kontCondArrow struct {
	testVal  value
	callExpr *expr
	env      *env
	parent   kont
}

func (*kontCondArrow) isKont() {}

type kontCaseKey struct {
	clauses []*expr
	env     *env
	parent  kont
}

func (*kontCaseKey) isKont() {}

// ---------- Do form frames ----------

type doVarCEK struct {
	name     string
	stepExpr *expr
}

type kontDoInit struct {
	vars        []doVarCEK
	initExprs   []*expr
	initVals    []value
	initIdx     int
	testExpr    *expr
	resultExprs []*expr
	bodyExprs   []*expr
	outerEnv    *env
	parent      kont
}

func (*kontDoInit) isKont() {}

type kontDoTest struct {
	vars        []doVarCEK
	resultExprs []*expr
	bodyExprs   []*expr
	testExpr    *expr
	doEnv       *env
	parent      kont
}

func (*kontDoTest) isKont() {}

type kontDoBody struct {
	remaining   []*expr
	vars        []doVarCEK
	resultExprs []*expr
	bodyExprs   []*expr
	testExpr    *expr
	doEnv       *env
	parent      kont
}

func (*kontDoBody) isKont() {}

type kontDoStep struct {
	vars        []doVarCEK
	stepIdx     int
	stepVals    []value
	testExpr    *expr
	resultExprs []*expr
	bodyExprs   []*expr
	doEnv       *env
	parent      kont
}

func (*kontDoStep) isKont() {}

// ---------- Map / ForEach ----------

type kontMapK struct {
	fn       value
	lists    []value
	results  []value
	callExpr *expr
	callEnv  *env
	parent   kont
}

func (*kontMapK) isKont() {}

type kontForEachK struct {
	fn       value
	lists    []value
	callExpr *expr
	callEnv  *env
	parent   kont
}

func (*kontForEachK) isKont() {}

// ---------- dynamic-wind frames ----------

type windEntry struct {
	inThunk  value
	outThunk value
}

// After in-thunk finishes, push wind entry and call body-thunk.
type kontDynWindIn struct {
	bodyThunk value
	outThunk  value
	inThunk   value
	callExpr  *expr
	callEnv   *env
	parent    kont
}

func (*kontDynWindIn) isKont() {}

// After body-thunk finishes, pop wind entry and call out-thunk.
type kontDynWindBody struct {
	outThunk value
	callExpr *expr
	callEnv  *env
	parent   kont
}

func (*kontDynWindBody) isKont() {}

// After out-thunk finishes, return saved body result.
type kontDynWindOut struct {
	bodyResult value
	parent     kont
}

func (*kontDynWindOut) isKont() {}

// Unwind: call out-thunks one at a time.
type kontWindUnwind struct {
	outs      []value      // remaining out-thunks to call
	rewindIns []*windEntry // entries to rewind after unwinding
	targetVal value
	targetK   kont
	callExpr  *expr
	callEnv   *env
}

func (*kontWindUnwind) isKont() {}

// Rewind: call in-thunks one at a time.
type kontWindRewind struct {
	entries   []*windEntry // remaining entries to push/call-in
	targetVal value
	targetK   kont
	callExpr  *expr
	callEnv   *env
}

func (*kontWindRewind) isKont() {}

// ---------- Exception handler frames ----------

type handlerStackEntry struct {
	handler   value        // handler proc (for with-exception-handler)
	isGuard   bool         // true if this is a guard handler
	varName   string       // guard variable name
	clauses   []*expr      // guard clause expressions
	guardEnv  *env         // guard's lexical environment
	guardK    kont         // guard's continuation
	guardWind []*windEntry // wind stack at guard point
}

// Pop exception handler on normal return from protected thunk.
type kontPopHandler struct {
	parent kont
}

func (*kontPopHandler) isKont() {}

// After raise handler returns — error for non-continuable raise.
type kontRaiseCheck struct {
	parent kont
}

func (*kontRaiseCheck) isKont() {}

// Guard clause test evaluation.
type kontGuardClause struct {
	body    []*expr // body exprs if test passes
	rest    []*expr // remaining clauses to try
	exnVal  value
	varName string
	env     *env // guard's original env (without var bound)
	parent  kont // guard's continuation
}

func (*kontGuardClause) isKont() {}

// After unwinding, start testing guard clauses.
type kontGuardStartTest struct {
	clauses []*expr
	exnVal  value
	varName string
	env     *env // guard's original env
	guardK  kont
}

func (*kontGuardStartTest) isKont() {}

// After unwinding for else clause, evaluate else body.
type kontGuardElseBody struct {
	body   []*expr
	env    *env
	parent kont
}

func (*kontGuardElseBody) isKont() {}

// ---------- call-with-values ----------

// After producer thunk returns, apply consumer to the produced values.
type kontCallWithValues struct {
	consumer value
	callExpr *expr
	callEnv  *env
	parent   kont
}

func (*kontCallWithValues) isKont() {}

// ---------- CEK machine ----------

type cekM struct {
	ctrl     *expr
	env      *env
	val      value
	kont     kont
	isValue  bool
	wind     []*windEntry        // dynamic-wind stack
	handlers []*handlerStackEntry // exception handler stack
}

func (m *cekM) setEval(e *expr, environ *env, k kont) {
	m.ctrl = e
	m.env = environ
	m.kont = k
	m.isValue = false
}

func (m *cekM) setApply(v value, k kont) {
	m.val = v
	m.kont = k
	m.isValue = true
}

func (m *cekM) run() (value, error) {
	for {
		if !m.isValue {
			if err := m.stepEval(); err != nil {
				return value{}, err
			}
		} else {
			if _, ok := m.kont.(*kontHalt); ok {
				return m.val, nil
			}
			if err := m.stepApply(); err != nil {
				return value{}, err
			}
		}
	}
}

// ---------- Eval step ----------

func (m *cekM) stepEval() error {
	e := m.ctrl
	environ := m.env
	k := m.kont

	if e.kind == exprAtom {
		if e.atom.kind == valSymbol {
			v, ok := environ.get(e.atom.sval)
			if !ok {
				return &EvalError{Message: fmt.Sprintf("%d:%d: unbound variable: %s", e.line, e.col, e.atom.sval)}
			}
			m.setApply(v, k)
			return nil
		}
		m.setApply(e.atom, k)
		return nil
	}

	if len(e.list) == 0 {
		return &EvalError{Message: fmt.Sprintf("%d:%d: empty application", e.line, e.col)}
	}

	head := e.list[0]

	if head.kind == exprAtom && head.atom.kind == valSymbol {
		switch head.atom.sval {
		case "define":
			return m.cekDefine(e, environ, k)
		case "set!":
			return m.cekSetBang(e, environ, k)
		case "if":
			return m.cekIf(e, environ, k)
		case "quote":
			if len(e.list) != 2 {
				return &EvalError{Message: fmt.Sprintf("%d:%d: quote: expected 1 argument", e.line, e.col)}
			}
			m.setApply(quoteExpr(e.list[1]), k)
			return nil
		case "quasiquote":
			if len(e.list) != 2 {
				return &EvalError{Message: fmt.Sprintf("%d:%d: quasiquote: expected 1 argument", e.line, e.col)}
			}
			v, err := evalQuasiquote(e.list[1], environ)
			if err != nil {
				return err
			}
			m.setApply(v, k)
			return nil
		case "lambda":
			v, err := evalLambdaForm(e, environ)
			if err != nil {
				return err
			}
			m.setApply(v, k)
			return nil
		case "case-lambda":
			v, err := evalCaseLambda(e, environ)
			if err != nil {
				return err
			}
			m.setApply(v, k)
			return nil
		case "and":
			return m.cekAnd(e, environ, k)
		case "or":
			return m.cekOr(e, environ, k)
		case "let":
			return m.cekLet(e, environ, k)
		case "let*":
			return m.cekLetStar(e, environ, k)
		case "begin":
			return m.cekBegin(e, environ, k)
		case "cond":
			return m.cekCond(e, environ, k)
		case "define-syntax":
			v, err := evalDefineSyntax(e, environ)
			if err != nil {
				return err
			}
			m.setApply(v, k)
			return nil
		case "define-record-type":
			v, err := evalDefineRecordType(e, environ)
			if err != nil {
				return err
			}
			m.setApply(v, k)
			return nil
		case "letrec":
			return m.cekLetrec(e, environ, k)
		case "letrec*":
			return m.cekLetrec(e, environ, k) // same impl
		case "case":
			return m.cekCase(e, environ, k)
		case "do":
			return m.cekDo(e, environ, k)
		case "guard":
			return m.cekGuard(e, environ, k)
		case "syntax-case":
			v, err := evalSyntaxCase(e, environ)
			if err != nil {
				return err
			}
			m.setApply(v, k)
			return nil
		case "syntax":
			v, err := evalSyntaxForm(e, environ)
			if err != nil {
				return err
			}
			m.setApply(v, k)
			return nil
		case "with-syntax":
			v, err := evalWithSyntax(e, environ)
			if err != nil {
				return err
			}
			m.setApply(v, k)
			return nil
		}
	}

	// Macro
	if head.kind == exprAtom && head.atom.kind == valSymbol {
		if v, ok := environ.get(head.atom.sval); ok && v.kind == valMacro {
			expanded, err := expandMacro(v.macro, e, environ)
			if err != nil {
				return err
			}
			m.setEval(expanded, environ, k)
			return nil
		}
	}

	// Fast path: if operator is a known symbol, try to evaluate all args inline
	// to avoid creating kontEvFn/kontEvArg frames.
	if head.kind == exprAtom && head.atom.kind == valSymbol {
		if op, ok := environ.get(head.atom.sval); ok && (op.kind == valBuiltin || op.kind == valLambda || op.kind == valCaseLambda || op.kind == valContinuation) {
			args := e.list[1:]
			evaled := make([]value, len(args))
			allSimple := true
			for i, a := range args {
				if a.kind == exprAtom {
					if a.atom.kind == valSymbol {
						v, found := environ.get(a.atom.sval)
						if !found {
							allSimple = false
							break
						}
						evaled[i] = v
					} else {
						evaled[i] = a.atom
					}
				} else {
					allSimple = false
					break
				}
			}
			if allSimple {
				return m.applyProc(op, evaled, e, environ, k)
			}
		}
	}

	// Application: evaluate operator
	m.setEval(head, environ, &kontEvFn{
		argExprs: e.list[1:],
		callExpr: e,
		callEnv:  environ,
		parent:   k,
	})
	return nil
}

// ---------- Special form handlers ----------

func (m *cekM) cekDefine(e *expr, environ *env, k kont) error {
	if len(e.list) < 3 {
		return &EvalError{Message: fmt.Sprintf("%d:%d: define: bad syntax", e.line, e.col)}
	}
	target := e.list[1]
	if target.kind == exprList && len(target.list) > 0 {
		nameExpr := target.list[0]
		if nameExpr.kind != exprAtom || nameExpr.atom.kind != valSymbol {
			return &EvalError{Message: fmt.Sprintf("%d:%d: define: expected symbol", nameExpr.line, nameExpr.col)}
		}
		params, restParam, err := parseDottedParams(target.list[1:], e)
		if err != nil {
			return err
		}
		lam := &lambda{params: params, restParam: restParam, body: e.list[2:], env: environ}
		environ.set(nameExpr.atom.sval, value{kind: valLambda, lambda: lam})
		m.setApply(voidVal, k)
		return nil
	}
	if target.kind != exprAtom || target.atom.kind != valSymbol {
		return &EvalError{Message: fmt.Sprintf("%d:%d: define: expected symbol", target.line, target.col)}
	}
	if len(e.list) != 3 {
		return &EvalError{Message: fmt.Sprintf("%d:%d: define: expected 1 expression", e.line, e.col)}
	}
	m.setEval(e.list[2], environ, &kontDefineK{name: target.atom.sval, env: environ, parent: k})
	return nil
}

func (m *cekM) cekSetBang(e *expr, environ *env, k kont) error {
	if len(e.list) != 3 {
		return &EvalError{Message: fmt.Sprintf("%d:%d: set!: bad syntax", e.line, e.col)}
	}
	target := e.list[1]
	if target.kind != exprAtom || target.atom.kind != valSymbol {
		return &EvalError{Message: fmt.Sprintf("%d:%d: set!: expected symbol", target.line, target.col)}
	}
	m.setEval(e.list[2], environ, &kontSetK{name: target.atom.sval, expr: e, env: environ, parent: k})
	return nil
}

func (m *cekM) cekIf(e *expr, environ *env, k kont) error {
	if len(e.list) < 3 || len(e.list) > 4 {
		return &EvalError{Message: fmt.Sprintf("%d:%d: if: expected 2 or 3 arguments", e.line, e.col)}
	}
	var alt *expr
	if len(e.list) == 4 {
		alt = e.list[3]
	}
	m.setEval(e.list[1], environ, &kontIf{conseq: e.list[2], alt: alt, env: environ, parent: k})
	return nil
}

func (m *cekM) cekAnd(e *expr, environ *env, k kont) error {
	args := e.list[1:]
	if len(args) == 0 {
		m.setApply(boolVal(true), k)
		return nil
	}
	if len(args) == 1 {
		m.setEval(args[0], environ, k)
		return nil
	}
	m.setEval(args[0], environ, &kontAnd{remaining: args[1:], env: environ, parent: k})
	return nil
}

func (m *cekM) cekOr(e *expr, environ *env, k kont) error {
	args := e.list[1:]
	if len(args) == 0 {
		m.setApply(boolVal(false), k)
		return nil
	}
	if len(args) == 1 {
		m.setEval(args[0], environ, k)
		return nil
	}
	m.setEval(args[0], environ, &kontOr{remaining: args[1:], env: environ, parent: k})
	return nil
}

func (m *cekM) cekBegin(e *expr, environ *env, k kont) error {
	args := e.list[1:]
	if len(args) == 0 {
		m.setApply(voidVal, k)
		return nil
	}
	return m.evalBody(args, environ, k)
}

func (m *cekM) cekLet(e *expr, environ *env, k kont) error {
	if len(e.list) < 3 {
		return &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", e.line, e.col)}
	}
	// Named let
	if e.list[1].kind == exprAtom && e.list[1].atom.kind == valSymbol {
		return m.cekNamedLet(e, environ, k)
	}
	bindingsExpr := e.list[1]
	if bindingsExpr.kind != exprList {
		return &EvalError{Message: fmt.Sprintf("%d:%d: let: expected bindings list", bindingsExpr.line, bindingsExpr.col)}
	}
	body := e.list[2:]
	letEnv := newEnv(environ)
	if len(bindingsExpr.list) == 0 {
		return m.evalBody(body, letEnv, k)
	}
	first := bindingsExpr.list[0]
	if first.kind != exprList || len(first.list) != 2 || first.list[0].kind != exprAtom || first.list[0].atom.kind != valSymbol {
		return &EvalError{Message: fmt.Sprintf("%d:%d: let: bad binding", first.line, first.col)}
	}
	m.setEval(first.list[1], environ, &kontLetBind{
		name: first.list[0].atom.sval, remaining: bindingsExpr.list[1:],
		letEnv: letEnv, evalEnv: environ, body: body, parent: k,
	})
	return nil
}

func (m *cekM) cekNamedLet(e *expr, environ *env, k kont) error {
	if len(e.list) < 4 {
		return &EvalError{Message: fmt.Sprintf("%d:%d: let: bad syntax", e.line, e.col)}
	}
	loopName := e.list[1].atom.sval
	bindingsExpr := e.list[2]
	if bindingsExpr.kind != exprList {
		return &EvalError{Message: fmt.Sprintf("%d:%d: let: expected bindings list", bindingsExpr.line, bindingsExpr.col)}
	}
	params := make([]string, len(bindingsExpr.list))
	initExprs := make([]*expr, len(bindingsExpr.list))
	for i, b := range bindingsExpr.list {
		if b.kind != exprList || len(b.list) != 2 || b.list[0].kind != exprAtom || b.list[0].atom.kind != valSymbol {
			return &EvalError{Message: fmt.Sprintf("%d:%d: let: bad binding", b.line, b.col)}
		}
		params[i] = b.list[0].atom.sval
		initExprs[i] = b.list[1]
	}
	letEnv := newEnv(environ)
	lam := &lambda{params: params, body: e.list[3:], env: letEnv}
	letEnv.set(loopName, value{kind: valLambda, lambda: lam})
	if len(initExprs) == 0 {
		return m.applyLambdaCEK(lam, nil, e, k)
	}
	evaled := make([]value, len(initExprs))
	idx := len(initExprs) - 1
	m.setEval(initExprs[idx], environ, &kontNamedLetInit{
		lam: lam, argExprs: initExprs, evaled: evaled, idx: idx,
		evalEnv: environ, callExpr: e, parent: k,
	})
	return nil
}

func (m *cekM) cekLetStar(e *expr, environ *env, k kont) error {
	if len(e.list) < 3 {
		return &EvalError{Message: fmt.Sprintf("%d:%d: let*: bad syntax", e.line, e.col)}
	}
	bindingsExpr := e.list[1]
	if bindingsExpr.kind != exprList {
		return &EvalError{Message: fmt.Sprintf("%d:%d: let*: expected bindings list", bindingsExpr.line, bindingsExpr.col)}
	}
	body := e.list[2:]
	letEnv := newEnv(environ)
	if len(bindingsExpr.list) == 0 {
		return m.evalBody(body, letEnv, k)
	}
	first := bindingsExpr.list[0]
	if first.kind != exprList || len(first.list) != 2 || first.list[0].kind != exprAtom || first.list[0].atom.kind != valSymbol {
		return &EvalError{Message: fmt.Sprintf("%d:%d: let*: bad binding", first.line, first.col)}
	}
	m.setEval(first.list[1], letEnv, &kontLetBind{
		name: first.list[0].atom.sval, remaining: bindingsExpr.list[1:],
		letEnv: letEnv, evalEnv: letEnv, body: body, parent: k,
	})
	return nil
}

func (m *cekM) cekLetrec(e *expr, environ *env, k kont) error {
	if len(e.list) < 3 {
		return &EvalError{Message: fmt.Sprintf("%d:%d: letrec: bad syntax", e.line, e.col)}
	}
	bindingsExpr := e.list[1]
	if bindingsExpr.kind != exprList {
		return &EvalError{Message: fmt.Sprintf("%d:%d: letrec: expected bindings list", e.line, e.col)}
	}
	letEnv := newEnv(environ)
	names := make([]string, len(bindingsExpr.list))
	inits := make([]*expr, len(bindingsExpr.list))
	for i, b := range bindingsExpr.list {
		if b.kind != exprList || len(b.list) != 2 || b.list[0].kind != exprAtom || b.list[0].atom.kind != valSymbol {
			return &EvalError{Message: fmt.Sprintf("%d:%d: letrec: bad binding", b.line, b.col)}
		}
		names[i] = b.list[0].atom.sval
		inits[i] = b.list[1]
		letEnv.set(names[i], voidVal)
	}
	body := e.list[2:]
	if len(inits) == 0 {
		return m.evalBody(body, letEnv, k)
	}
	m.setEval(inits[0], letEnv, &kontLetrecBind{
		names: names, idx: 0, inits: inits, letEnv: letEnv, body: body, parent: k,
	})
	return nil
}

func (m *cekM) cekCond(e *expr, environ *env, k kont) error {
	return m.evalCondClauses(e.list[1:], environ, k)
}

func (m *cekM) evalCondClauses(clauses []*expr, environ *env, k kont) error {
	if len(clauses) == 0 {
		m.setApply(voidVal, k)
		return nil
	}
	clause := clauses[0]
	if clause.kind != exprList || len(clause.list) < 1 {
		return &EvalError{Message: "cond: bad clause"}
	}
	if clause.list[0].kind == exprAtom && clause.list[0].atom.kind == valSymbol && clause.list[0].atom.sval == "else" {
		return m.evalBody(clause.list[1:], environ, k)
	}
	m.setEval(clause.list[0], environ, &kontCondTest{
		body: clause.list[1:], rest: clauses[1:], env: environ, parent: k,
	})
	return nil
}

func (m *cekM) cekCase(e *expr, environ *env, k kont) error {
	if len(e.list) < 3 {
		return &EvalError{Message: fmt.Sprintf("%d:%d: case: bad syntax", e.line, e.col)}
	}
	m.setEval(e.list[1], environ, &kontCaseKey{clauses: e.list[2:], env: environ, parent: k})
	return nil
}

func (m *cekM) evalCaseClauses(key value, clauses []*expr, environ *env, k kont) error {
	for _, clause := range clauses {
		if clause.kind != exprList || len(clause.list) < 2 {
			return &EvalError{Message: "case: bad clause"}
		}
		if clause.list[0].kind == exprAtom && clause.list[0].atom.kind == valSymbol && clause.list[0].atom.sval == "else" {
			return m.evalBody(clause.list[1:], environ, k)
		}
		datumList := clause.list[0]
		if datumList.kind != exprList {
			return &EvalError{Message: "case: expected datum list"}
		}
		for _, datum := range datumList.list {
			if valuesEqv(key, quoteExpr(datum)) {
				return m.evalBody(clause.list[1:], environ, k)
			}
		}
	}
	m.setApply(voidVal, k)
	return nil
}

func (m *cekM) cekDo(e *expr, environ *env, k kont) error {
	if len(e.list) < 3 {
		return &EvalError{Message: fmt.Sprintf("%d:%d: do: bad syntax", e.line, e.col)}
	}
	varSpecs := e.list[1]
	if varSpecs.kind != exprList {
		return &EvalError{Message: fmt.Sprintf("%d:%d: do: expected variable specs", e.line, e.col)}
	}
	testClause := e.list[2]
	if testClause.kind != exprList || len(testClause.list) < 1 {
		return &EvalError{Message: fmt.Sprintf("%d:%d: do: expected test clause", e.line, e.col)}
	}
	bodyExprs := e.list[3:]
	vars := make([]doVarCEK, len(varSpecs.list))
	initExprs := make([]*expr, len(varSpecs.list))
	for i, spec := range varSpecs.list {
		if spec.kind != exprList || len(spec.list) < 2 || len(spec.list) > 3 {
			return &EvalError{Message: fmt.Sprintf("%d:%d: do: bad variable spec", spec.line, spec.col)}
		}
		if spec.list[0].kind != exprAtom || spec.list[0].atom.kind != valSymbol {
			return &EvalError{Message: fmt.Sprintf("%d:%d: do: expected symbol", spec.list[0].line, spec.list[0].col)}
		}
		vars[i].name = spec.list[0].atom.sval
		initExprs[i] = spec.list[1]
		if len(spec.list) == 3 {
			vars[i].stepExpr = spec.list[2]
		}
	}
	resultExprs := testClause.list[1:]
	if len(vars) == 0 {
		doEnv := newEnv(environ)
		m.setEval(testClause.list[0], doEnv, &kontDoTest{
			vars: vars, resultExprs: resultExprs, bodyExprs: bodyExprs,
			testExpr: testClause.list[0], doEnv: doEnv, parent: k,
		})
		return nil
	}
	initVals := make([]value, len(vars))
	m.setEval(initExprs[0], environ, &kontDoInit{
		vars: vars, initExprs: initExprs, initVals: initVals, initIdx: 0,
		testExpr: testClause.list[0], resultExprs: resultExprs, bodyExprs: bodyExprs,
		outerEnv: environ, parent: k,
	})
	return nil
}

// ---------- Apply step ----------

func (m *cekM) stepApply() error {
	val := m.val
	k := m.kont

	switch kk := k.(type) {
	case *kontTopSeq:
		nl := kk.lastVal
		nh := kk.hasResult
		if val.kind != valVoid {
			nl = val
			nh = true
		}
		if len(kk.remaining) == 0 {
			if nh {
				m.setApply(nl, kk.parent)
			} else {
				m.setApply(voidVal, kk.parent)
			}
		} else {
			m.setEval(kk.remaining[0], kk.env, &kontTopSeq{
				remaining: kk.remaining[1:], env: kk.env, lastVal: nl, hasResult: nh, parent: kk.parent,
			})
		}
		return nil

	case *kontEvFn:
		if len(kk.argExprs) == 0 {
			return m.applyProc(val, nil, kk.callExpr, kk.callEnv, kk.parent)
		}
		evaled := make([]value, len(kk.argExprs))
		idx := len(kk.argExprs) - 1
		m.setEval(kk.argExprs[idx], kk.callEnv, &kontEvArg{
			op: val, argExprs: kk.argExprs, evaled: evaled, idx: idx,
			callExpr: kk.callExpr, callEnv: kk.callEnv, parent: kk.parent,
		})
		return nil

	case *kontEvArg:
		kk.evaled[kk.idx] = val
		ni := kk.idx - 1
		if ni < 0 {
			return m.applyProc(kk.op, kk.evaled, kk.callExpr, kk.callEnv, kk.parent)
		}
		m.setEval(kk.argExprs[ni], kk.callEnv, &kontEvArg{
			op: kk.op, argExprs: kk.argExprs, evaled: kk.evaled, idx: ni,
			callExpr: kk.callExpr, callEnv: kk.callEnv, parent: kk.parent,
		})
		return nil

	case *kontIf:
		if isTruthy(val) {
			m.setEval(kk.conseq, kk.env, kk.parent)
		} else if kk.alt != nil {
			m.setEval(kk.alt, kk.env, kk.parent)
		} else {
			m.setApply(voidVal, kk.parent)
		}
		return nil

	case *kontDefineK:
		kk.env.set(kk.name, val)
		m.setApply(voidVal, kk.parent)
		return nil

	case *kontSetK:
		if !kk.env.setExisting(kk.name, val) {
			return &EvalError{Message: fmt.Sprintf("set!: unbound variable %s", kk.name)}
		}
		m.setApply(voidVal, kk.parent)
		return nil

	case *kontSeq:
		if len(kk.exprs) == 1 {
			m.setEval(kk.exprs[0], kk.env, kk.parent)
		} else {
			m.setEval(kk.exprs[0], kk.env, &kontSeq{exprs: kk.exprs[1:], env: kk.env, parent: kk.parent})
		}
		return nil

	case *kontAnd:
		if !isTruthy(val) {
			m.setApply(val, kk.parent)
			return nil
		}
		if len(kk.remaining) == 1 {
			m.setEval(kk.remaining[0], kk.env, kk.parent)
		} else {
			m.setEval(kk.remaining[0], kk.env, &kontAnd{remaining: kk.remaining[1:], env: kk.env, parent: kk.parent})
		}
		return nil

	case *kontOr:
		if isTruthy(val) {
			m.setApply(val, kk.parent)
			return nil
		}
		if len(kk.remaining) == 1 {
			m.setEval(kk.remaining[0], kk.env, kk.parent)
		} else {
			m.setEval(kk.remaining[0], kk.env, &kontOr{remaining: kk.remaining[1:], env: kk.env, parent: kk.parent})
		}
		return nil

	case *kontLetBind:
		kk.letEnv.set(kk.name, val)
		if len(kk.remaining) == 0 {
			return m.evalBody(kk.body, kk.letEnv, kk.parent)
		}
		next := kk.remaining[0]
		if next.kind != exprList || len(next.list) != 2 || next.list[0].kind != exprAtom || next.list[0].atom.kind != valSymbol {
			return &EvalError{Message: "let: bad binding"}
		}
		m.setEval(next.list[1], kk.evalEnv, &kontLetBind{
			name: next.list[0].atom.sval, remaining: kk.remaining[1:],
			letEnv: kk.letEnv, evalEnv: kk.evalEnv, body: kk.body, parent: kk.parent,
		})
		return nil

	case *kontNamedLetInit:
		kk.evaled[kk.idx] = val
		ni := kk.idx - 1
		if ni < 0 {
			return m.applyLambdaCEK(kk.lam, kk.evaled, kk.callExpr, kk.parent)
		}
		m.setEval(kk.argExprs[ni], kk.evalEnv, &kontNamedLetInit{
			lam: kk.lam, argExprs: kk.argExprs, evaled: kk.evaled, idx: ni,
			evalEnv: kk.evalEnv, callExpr: kk.callExpr, parent: kk.parent,
		})
		return nil

	case *kontLetrecBind:
		kk.letEnv.set(kk.names[kk.idx], val)
		ni := kk.idx + 1
		if ni >= len(kk.inits) {
			return m.evalBody(kk.body, kk.letEnv, kk.parent)
		}
		m.setEval(kk.inits[ni], kk.letEnv, &kontLetrecBind{
			names: kk.names, idx: ni, inits: kk.inits, letEnv: kk.letEnv, body: kk.body, parent: kk.parent,
		})
		return nil

	case *kontCondTest:
		if isTruthy(val) {
			if len(kk.body) == 0 {
				m.setApply(val, kk.parent)
				return nil
			}
			// (cond (test => proc)) — evaluate proc then apply to test value
			if len(kk.body) == 2 && kk.body[0].kind == exprAtom && kk.body[0].atom.kind == valSymbol && kk.body[0].atom.sval == "=>" {
				m.setEval(kk.body[1], kk.env, &kontCondArrow{testVal: val, callExpr: kk.body[1], env: kk.env, parent: kk.parent})
				return nil
			}
			return m.evalBody(kk.body, kk.env, kk.parent)
		}
		return m.evalCondClauses(kk.rest, kk.env, kk.parent)

	case *kontCondArrow:
		// val is the proc; apply it to the test value
		return m.applyProc(val, []value{kk.testVal}, kk.callExpr, kk.env, kk.parent)

	case *kontCaseKey:
		return m.evalCaseClauses(val, kk.clauses, kk.env, kk.parent)

	case *kontDoInit:
		nv := make([]value, len(kk.initVals))
		copy(nv, kk.initVals)
		nv[kk.initIdx] = val
		ni := kk.initIdx + 1
		if ni < len(kk.initExprs) {
			m.setEval(kk.initExprs[ni], kk.outerEnv, &kontDoInit{
				vars: kk.vars, initExprs: kk.initExprs, initVals: nv, initIdx: ni,
				testExpr: kk.testExpr, resultExprs: kk.resultExprs, bodyExprs: kk.bodyExprs,
				outerEnv: kk.outerEnv, parent: kk.parent,
			})
			return nil
		}
		doEnv := newEnv(kk.outerEnv)
		for i, v := range kk.vars {
			doEnv.set(v.name, nv[i])
		}
		m.setEval(kk.testExpr, doEnv, &kontDoTest{
			vars: kk.vars, resultExprs: kk.resultExprs, bodyExprs: kk.bodyExprs,
			testExpr: kk.testExpr, doEnv: doEnv, parent: kk.parent,
		})
		return nil

	case *kontDoTest:
		if isTruthy(val) {
			if len(kk.resultExprs) == 0 {
				m.setApply(voidVal, kk.parent)
				return nil
			}
			return m.evalBody(kk.resultExprs, kk.doEnv, kk.parent)
		}
		if len(kk.bodyExprs) == 0 {
			return m.doStartSteps(kk)
		}
		m.setEval(kk.bodyExprs[0], kk.doEnv, &kontDoBody{
			remaining: kk.bodyExprs[1:], vars: kk.vars, resultExprs: kk.resultExprs,
			bodyExprs: kk.bodyExprs, testExpr: kk.testExpr, doEnv: kk.doEnv, parent: kk.parent,
		})
		return nil

	case *kontDoBody:
		if len(kk.remaining) > 0 {
			m.setEval(kk.remaining[0], kk.doEnv, &kontDoBody{
				remaining: kk.remaining[1:], vars: kk.vars, resultExprs: kk.resultExprs,
				bodyExprs: kk.bodyExprs, testExpr: kk.testExpr, doEnv: kk.doEnv, parent: kk.parent,
			})
			return nil
		}
		return m.doStartSteps(&kontDoTest{
			vars: kk.vars, resultExprs: kk.resultExprs, bodyExprs: kk.bodyExprs,
			testExpr: kk.testExpr, doEnv: kk.doEnv, parent: kk.parent,
		})

	case *kontDoStep:
		nv := make([]value, len(kk.stepVals))
		copy(nv, kk.stepVals)
		nv[kk.stepIdx] = val
		nextIdx := -1
		for i := kk.stepIdx + 1; i < len(kk.vars); i++ {
			if kk.vars[i].stepExpr != nil {
				nextIdx = i
				break
			}
		}
		if nextIdx >= 0 {
			m.setEval(kk.vars[nextIdx].stepExpr, kk.doEnv, &kontDoStep{
				vars: kk.vars, stepIdx: nextIdx, stepVals: nv,
				testExpr: kk.testExpr, resultExprs: kk.resultExprs, bodyExprs: kk.bodyExprs,
				doEnv: kk.doEnv, parent: kk.parent,
			})
			return nil
		}
		for i, v := range kk.vars {
			if v.stepExpr != nil {
				kk.doEnv.set(v.name, nv[i])
			}
		}
		m.setEval(kk.testExpr, kk.doEnv, &kontDoTest{
			vars: kk.vars, resultExprs: kk.resultExprs, bodyExprs: kk.bodyExprs,
			testExpr: kk.testExpr, doEnv: kk.doEnv, parent: kk.parent,
		})
		return nil

	case *kontMapK:
		nr := make([]value, len(kk.results)+1)
		copy(nr, kk.results)
		nr[len(kk.results)] = val
		nl := make([]value, len(kk.lists))
		for i, lst := range kk.lists {
			nl[i] = lst.pair.cdr
		}
		return m.doMapStep(kk.fn, nl, nr, kk.callExpr, kk.callEnv, kk.parent)

	case *kontForEachK:
		nl := make([]value, len(kk.lists))
		for i, lst := range kk.lists {
			nl[i] = lst.pair.cdr
		}
		return m.doForEachStep(kk.fn, nl, kk.callExpr, kk.callEnv, kk.parent)

	case *kontDynWindIn:
		// in-thunk done; push wind entry, call body-thunk
		entry := &windEntry{inThunk: kk.inThunk, outThunk: kk.outThunk}
		m.wind = append(m.wind, entry)
		return m.applyProc(kk.bodyThunk, nil, kk.callExpr, kk.callEnv, &kontDynWindBody{
			outThunk: kk.outThunk, callExpr: kk.callExpr, callEnv: kk.callEnv, parent: kk.parent,
		})

	case *kontDynWindBody:
		// body done; pop wind entry, call out-thunk
		if len(m.wind) > 0 {
			m.wind = m.wind[:len(m.wind)-1]
		}
		return m.applyProc(kk.outThunk, nil, kk.callExpr, kk.callEnv, &kontDynWindOut{
			bodyResult: val, parent: kk.parent,
		})

	case *kontDynWindOut:
		// out-thunk done; return body result
		m.setApply(kk.bodyResult, kk.parent)
		return nil

	case *kontWindUnwind:
		// an out-thunk finished; continue unwinding or start rewinding
		if len(kk.outs) > 0 {
			return m.applyProc(kk.outs[0], nil, kk.callExpr, kk.callEnv, &kontWindUnwind{
				outs: kk.outs[1:], rewindIns: kk.rewindIns,
				targetVal: kk.targetVal, targetK: kk.targetK,
				callExpr: kk.callExpr, callEnv: kk.callEnv,
			})
		}
		return m.startRewind(kk.rewindIns, kk.targetVal, kk.targetK, kk.callExpr, kk.callEnv)

	case *kontWindRewind:
		// an in-thunk finished; push entry, continue rewinding or deliver value
		if len(kk.entries) > 0 {
			entry := kk.entries[0]
			m.wind = append(m.wind, entry)
			return m.applyProc(entry.inThunk, nil, kk.callExpr, kk.callEnv, &kontWindRewind{
				entries: kk.entries[1:],
				targetVal: kk.targetVal, targetK: kk.targetK,
				callExpr: kk.callExpr, callEnv: kk.callEnv,
			})
		}
		m.setApply(kk.targetVal, kk.targetK)
		return nil

	case *kontPopHandler:
		if len(m.handlers) > 0 {
			m.handlers = m.handlers[:len(m.handlers)-1]
		}
		m.setApply(val, kk.parent)
		return nil

	case *kontRaiseCheck:
		// Handler returned normally for non-continuable raise — error
		return &EvalError{Message: "raise: exception handler returned"}

	case *kontGuardStartTest:
		// Arrived after unwinding. val is the exception value (ignored, we use kk.exnVal).
		guardEnv := newEnv(kk.env)
		guardEnv.set(kk.varName, kk.exnVal)
		clause := kk.clauses[0]
		if clause.kind != exprList || len(clause.list) == 0 {
			return &EvalError{Message: "guard: bad clause"}
		}
		if clause.list[0].kind == exprAtom && clause.list[0].atom.kind == valSymbol && clause.list[0].atom.sval == "else" {
			return m.evalBody(clause.list[1:], guardEnv, kk.guardK)
		}
		m.setEval(clause.list[0], guardEnv, &kontGuardClause{
			body: clause.list[1:], rest: kk.clauses[1:],
			exnVal: kk.exnVal, varName: kk.varName, env: kk.env, parent: kk.guardK,
		})
		return nil

	case *kontGuardElseBody:
		// Arrived after unwinding for else. val is ignored.
		return m.evalBody(kk.body, kk.env, kk.parent)

	case *kontGuardClause:
		if isTruthy(val) {
			if len(kk.body) == 0 {
				m.setApply(val, kk.parent)
				return nil
			}
			guardEnv := newEnv(kk.env)
			guardEnv.set(kk.varName, kk.exnVal)
			return m.evalBody(kk.body, guardEnv, kk.parent)
		}
		// Test failed, try next clause
		guardEnv := newEnv(kk.env)
		guardEnv.set(kk.varName, kk.exnVal)
		if len(kk.rest) == 0 {
			// No more clauses, re-raise
			return &EvalError{Message: fmt.Sprintf("unhandled exception: %s", kk.exnVal.String())}
		}
		clause := kk.rest[0]
		if clause.kind != exprList || len(clause.list) == 0 {
			return &EvalError{Message: "guard: bad clause"}
		}
		if clause.list[0].kind == exprAtom && clause.list[0].atom.kind == valSymbol && clause.list[0].atom.sval == "else" {
			return m.evalBody(clause.list[1:], guardEnv, kk.parent)
		}
		m.setEval(clause.list[0], guardEnv, &kontGuardClause{
			body: clause.list[1:], rest: kk.rest[1:],
			exnVal: kk.exnVal, varName: kk.varName, env: kk.env, parent: kk.parent,
		})
		return nil

	case *kontCallWithValues:
		// Producer returned; unpack multiple values and apply consumer
		var consumerArgs []value
		if val.kind == valMultipleValues {
			consumerArgs = *val.multiVals
		} else {
			consumerArgs = []value{val}
		}
		return m.applyProc(kk.consumer, consumerArgs, kk.callExpr, kk.callEnv, kk.parent)

	default:
		return &EvalError{Message: "internal: unknown continuation frame"}
	}
}

// ---------- Procedure application ----------

func (m *cekM) applyProc(op value, args []value, callExpr *expr, callEnv *env, k kont) error {
	switch op.kind {
	case valLambda:
		return m.applyLambdaCEK(op.lambda, args, callExpr, k)
	case valCaseLambda:
		for _, lam := range op.clauses {
			if lam.restParam != "" {
				if len(args) >= len(lam.params) {
					return m.applyLambdaCEK(lam, args, callExpr, k)
				}
			} else if len(args) == len(lam.params) {
				return m.applyLambdaCEK(lam, args, callExpr, k)
			}
		}
		return &EvalError{Message: fmt.Sprintf("%d:%d: case-lambda: no matching clause for %d arguments", callExpr.line, callExpr.col, len(args))}
	case valBuiltin:
		return m.applyBuiltinCEK(op, args, callExpr, callEnv, k)
	case valContinuation:
		if len(args) == 0 {
			return &EvalError{Message: fmt.Sprintf("%d:%d: continuation: expected at least 1 argument", callExpr.line, callExpr.col)}
		}
		var arg value
		if len(args) == 1 {
			arg = args[0]
		} else {
			arg = value{kind: valMultipleValues, multiVals: &args}
		}
		return m.invokeContinuation(op, arg, callExpr, callEnv)
	default:
		return &EvalError{Message: fmt.Sprintf("%d:%d: not a procedure: %s", callExpr.line, callExpr.col, op.String())}
	}
}

func (m *cekM) applyLambdaCEK(lam *lambda, args []value, callExpr *expr, k kont) error {
	if lam.restParam != "" {
		if len(args) < len(lam.params) {
			return &EvalError{Message: fmt.Sprintf("%d:%d: expected at least %d arguments, got %d", callExpr.line, callExpr.col, len(lam.params), len(args))}
		}
	} else {
		if len(args) != len(lam.params) {
			return &EvalError{Message: fmt.Sprintf("%d:%d: expected %d arguments, got %d", callExpr.line, callExpr.col, len(lam.params), len(args))}
		}
	}
	ce := newEnv(lam.env)
	for i, p := range lam.params {
		ce.set(p, args[i])
	}
	if lam.restParam != "" {
		rest := nullVal
		for i := len(args) - 1; i >= len(lam.params); i-- {
			rest = pairVal(args[i], rest)
		}
		ce.set(lam.restParam, rest)
	}
	return m.evalBody(lam.body, ce, k)
}

func (m *cekM) applyBuiltinCEK(op value, args []value, callExpr *expr, callEnv *env, k kont) error {
	name := op.sval
	switch name {
	case "__native":
		nativeFuncsMu.Lock()
		fn := nativeFuncs[int(op.ival)]
		nativeFuncsMu.Unlock()
		v, err := fn(args)
		if err != nil {
			return err
		}
		m.setApply(v, k)
		return nil

	case "call/cc", "call-with-current-continuation":
		if len(args) != 1 {
			return &EvalError{Message: fmt.Sprintf("%d:%d: call/cc: expected 1 argument", callExpr.line, callExpr.col)}
		}
		windCopy := make([]*windEntry, len(m.wind))
		copy(windCopy, m.wind)
		contVal := value{kind: valContinuation, cont: k, wind: windCopy}
		return m.applyProc(args[0], []value{contVal}, callExpr, callEnv, k)

	case "dynamic-wind":
		if len(args) != 3 {
			return &EvalError{Message: fmt.Sprintf("%d:%d: dynamic-wind: expected 3 arguments", callExpr.line, callExpr.col)}
		}
		inThunk, bodyThunk, outThunk := args[0], args[1], args[2]
		// Call in-thunk first
		return m.applyProc(inThunk, nil, callExpr, callEnv, &kontDynWindIn{
			bodyThunk: bodyThunk, outThunk: outThunk, inThunk: inThunk,
			callExpr: callExpr, callEnv: callEnv, parent: k,
		})

	case "values":
		if len(args) == 1 {
			// Single value is transparent
			m.setApply(args[0], k)
			return nil
		}
		mv := value{kind: valMultipleValues, multiVals: &args}
		m.setApply(mv, k)
		return nil

	case "call-with-values":
		if len(args) != 2 {
			return &EvalError{Message: fmt.Sprintf("%d:%d: call-with-values: expected 2 arguments", callExpr.line, callExpr.col)}
		}
		producer, consumer := args[0], args[1]
		// Call producer thunk with no args, then feed results to consumer
		return m.applyProc(producer, nil, callExpr, callEnv, &kontCallWithValues{
			consumer: consumer,
			callExpr: callExpr,
			callEnv:  callEnv,
			parent:   k,
		})

	case "raise":
		if len(args) != 1 {
			return &EvalError{Message: fmt.Sprintf("%d:%d: raise: expected 1 argument", callExpr.line, callExpr.col)}
		}
		return m.doRaise(args[0], callExpr, callEnv, k)

	case "with-exception-handler":
		if len(args) != 2 {
			return &EvalError{Message: fmt.Sprintf("%d:%d: with-exception-handler: expected 2 arguments", callExpr.line, callExpr.col)}
		}
		handler, thunk := args[0], args[1]
		m.handlers = append(m.handlers, &handlerStackEntry{handler: handler})
		return m.applyProc(thunk, nil, callExpr, callEnv, &kontPopHandler{parent: k})

	case "apply":
		if len(args) < 2 {
			return &EvalError{Message: fmt.Sprintf("%d:%d: apply: expected at least 2 arguments", callExpr.line, callExpr.col)}
		}
		fn := args[0]
		lastArg := args[len(args)-1]
		var allArgs []value
		for _, a := range args[1 : len(args)-1] {
			allArgs = append(allArgs, a)
		}
		cur := lastArg
		for cur.kind == valPair {
			allArgs = append(allArgs, cur.pair.car)
			cur = cur.pair.cdr
		}
		if cur.kind != valNull {
			return &EvalError{Message: fmt.Sprintf("%d:%d: apply: last argument must be a proper list", callExpr.line, callExpr.col)}
		}
		return m.applyProc(fn, allArgs, callExpr, callEnv, k)

	case "map":
		if len(args) < 2 {
			return &EvalError{Message: fmt.Sprintf("%d:%d: map: expected at least 2 arguments", callExpr.line, callExpr.col)}
		}
		return m.doMapStep(args[0], args[1:], nil, callExpr, callEnv, k)

	case "for-each":
		if len(args) < 2 {
			return &EvalError{Message: fmt.Sprintf("%d:%d: for-each: expected at least 2 arguments", callExpr.line, callExpr.col)}
		}
		return m.doForEachStep(args[0], args[1:], callExpr, callEnv, k)

	default:
		v, err := evalBuiltin(name, args, callExpr, callEnv)
		if err != nil {
			return err
		}
		m.setApply(v, k)
		return nil
	}
}

// ---------- Helpers ----------

func (m *cekM) evalBody(body []*expr, environ *env, k kont) error {
	if len(body) == 0 {
		m.setApply(voidVal, k)
		return nil
	}
	if len(body) == 1 {
		m.setEval(body[0], environ, k)
		return nil
	}
	m.setEval(body[0], environ, &kontSeq{exprs: body[1:], env: environ, parent: k})
	return nil
}

func (m *cekM) doMapStep(fn value, lists []value, results []value, callExpr *expr, callEnv *env, k kont) error {
	mapArgs := make([]value, len(lists))
	for i, lst := range lists {
		if lst.kind != valPair {
			result := nullVal
			for j := len(results) - 1; j >= 0; j-- {
				result = pairVal(results[j], result)
			}
			m.setApply(result, k)
			return nil
		}
		mapArgs[i] = lst.pair.car
	}
	return m.applyProc(fn, mapArgs, callExpr, callEnv, &kontMapK{
		fn: fn, lists: lists, results: results, callExpr: callExpr, callEnv: callEnv, parent: k,
	})
}

func (m *cekM) doForEachStep(fn value, lists []value, callExpr *expr, callEnv *env, k kont) error {
	feArgs := make([]value, len(lists))
	for i, lst := range lists {
		if lst.kind != valPair {
			m.setApply(voidVal, k)
			return nil
		}
		feArgs[i] = lst.pair.car
	}
	return m.applyProc(fn, feArgs, callExpr, callEnv, &kontForEachK{
		fn: fn, lists: lists, callExpr: callExpr, callEnv: callEnv, parent: k,
	})
}

func (m *cekM) doStartSteps(dt *kontDoTest) error {
	firstIdx := -1
	for i, v := range dt.vars {
		if v.stepExpr != nil {
			firstIdx = i
			break
		}
	}
	if firstIdx < 0 {
		m.setEval(dt.testExpr, dt.doEnv, &kontDoTest{
			vars: dt.vars, resultExprs: dt.resultExprs, bodyExprs: dt.bodyExprs,
			testExpr: dt.testExpr, doEnv: dt.doEnv, parent: dt.parent,
		})
		return nil
	}
	sv := make([]value, len(dt.vars))
	m.setEval(dt.vars[firstIdx].stepExpr, dt.doEnv, &kontDoStep{
		vars: dt.vars, stepIdx: firstIdx, stepVals: sv,
		testExpr: dt.testExpr, resultExprs: dt.resultExprs, bodyExprs: dt.bodyExprs,
		doEnv: dt.doEnv, parent: dt.parent,
	})
	return nil
}

// ---------- Continuation invocation with wind shifting ----------

func (m *cekM) cekGuard(e *expr, environ *env, k kont) error {
	// (guard (var clause ...) body ...)
	if len(e.list) < 3 {
		return &EvalError{Message: fmt.Sprintf("%d:%d: guard: bad syntax", e.line, e.col)}
	}
	header := e.list[1]
	if header.kind != exprList || len(header.list) < 1 {
		return &EvalError{Message: fmt.Sprintf("%d:%d: guard: expected (var clause ...)", e.line, e.col)}
	}
	varExpr := header.list[0]
	if varExpr.kind != exprAtom || varExpr.atom.kind != valSymbol {
		return &EvalError{Message: fmt.Sprintf("%d:%d: guard: expected variable name", varExpr.line, varExpr.col)}
	}
	varName := varExpr.atom.sval
	clauses := header.list[1:]
	body := e.list[2:]

	// Save current wind stack for unwinding on exception
	windCopy := make([]*windEntry, len(m.wind))
	copy(windCopy, m.wind)

	// Push guard handler
	m.handlers = append(m.handlers, &handlerStackEntry{
		isGuard:   true,
		varName:   varName,
		clauses:   clauses,
		guardEnv:  environ,
		guardK:    k,
		guardWind: windCopy,
	})

	// Evaluate body; pop handler on normal completion
	return m.evalBody(body, environ, &kontPopHandler{parent: k})
}

func (m *cekM) doRaise(exnVal value, callExpr *expr, callEnv *env, k kont) error {
	if len(m.handlers) == 0 {
		return &EvalError{Message: fmt.Sprintf("unhandled exception: %s", exnVal.String())}
	}
	entry := m.handlers[len(m.handlers)-1]
	m.handlers = m.handlers[:len(m.handlers)-1]

	if entry.isGuard {
		// Unwind dynamic-wind to guard point, then test clauses.
		// Build a kontGuardClause chain starting from the first clause.
		guardK := entry.guardK
		guardEnv := newEnv(entry.guardEnv)
		guardEnv.set(entry.varName, exnVal)

		// Build the target continuation: evaluate first clause test
		var targetK kont
		if len(entry.clauses) == 0 {
			return &EvalError{Message: fmt.Sprintf("unhandled exception: %s", exnVal.String())}
		}

		// We'll create a special "landing" continuation that starts clause testing.
		// Since invokeContinuation delivers a value to targetK, we use a kontGuardClause
		// but we need to trigger the first test evaluation. We'll use a trick:
		// deliver the exception value to a frame that starts clause testing.
		clause := entry.clauses[0]
		if clause.kind != exprList || len(clause.list) == 0 {
			return &EvalError{Message: "guard: bad clause"}
		}
		// Check for else
		if clause.list[0].kind == exprAtom && clause.list[0].atom.kind == valSymbol && clause.list[0].atom.sval == "else" {
			// Unwind to guard point, then evaluate else body
			targetK = &kontSeq{exprs: clause.list[1:], env: guardEnv, parent: guardK}
			if len(clause.list[1:]) == 1 {
				targetK = guardK // will eval single expr below
			}
			fakeContVal := value{kind: valContinuation, cont: targetK, wind: entry.guardWind}
			// For else, we need to evaluate the body after unwinding.
			// Use evalBody by going through a special path.
			// Actually, let's use a simpler approach: unwind, then the value delivered
			// is ignored and we evaluate else body.
			// Hmm, invokeContinuation delivers arg to targetK.
			// Let's make targetK something that discards the value and evaluates else body.
			targetK = &kontGuardElseBody{body: clause.list[1:], env: guardEnv, parent: guardK}
			fakeContVal = value{kind: valContinuation, cont: targetK, wind: entry.guardWind}
			return m.invokeContinuation(fakeContVal, exnVal, callExpr, callEnv)
		}

		// Need to evaluate the test after unwinding. Set up a frame that,
		// when it receives the exception value, evaluates the test.
		targetK = &kontGuardStartTest{
			clauses: entry.clauses,
			exnVal:  exnVal,
			varName: entry.varName,
			env:     entry.guardEnv,
			guardK:  guardK,
		}
		fakeContVal := value{kind: valContinuation, cont: targetK, wind: entry.guardWind}
		return m.invokeContinuation(fakeContVal, exnVal, callExpr, callEnv)
	}

	// with-exception-handler: call handler procedure
	return m.applyProc(entry.handler, []value{exnVal}, callExpr, callEnv, &kontRaiseCheck{parent: k})
}

func (m *cekM) invokeContinuation(contVal value, arg value, callExpr *expr, callEnv *env) error {
	targetK := contVal.cont
	targetWind := contVal.wind

	// Find common prefix length
	commonLen := 0
	for commonLen < len(m.wind) && commonLen < len(targetWind) && m.wind[commonLen] == targetWind[commonLen] {
		commonLen++
	}

	// Out-thunks: current[commonLen:] in reverse (innermost first)
	outsToCall := make([]value, 0, len(m.wind)-commonLen)
	for i := len(m.wind) - 1; i >= commonLen; i-- {
		outsToCall = append(outsToCall, m.wind[i].outThunk)
	}

	// Entries to rewind: target[commonLen:]
	entriesToRewind := targetWind[commonLen:]

	// No wind shifting needed
	if len(outsToCall) == 0 && len(entriesToRewind) == 0 {
		m.setApply(arg, targetK)
		return nil
	}

	// Trim wind stack to common prefix
	m.wind = m.wind[:commonLen]

	if len(outsToCall) > 0 {
		// Start unwinding: call first out-thunk
		return m.applyProc(outsToCall[0], nil, callExpr, callEnv, &kontWindUnwind{
			outs:      outsToCall[1:],
			rewindIns: entriesToRewind,
			targetVal: arg,
			targetK:   targetK,
			callExpr:  callExpr,
			callEnv:   callEnv,
		})
	}

	// No unwinding needed, start rewinding directly
	return m.startRewind(entriesToRewind, arg, targetK, callExpr, callEnv)
}

// ---------- Wind helpers ----------

func (m *cekM) startRewind(entries []*windEntry, targetVal value, targetK kont, callExpr *expr, callEnv *env) error {
	if len(entries) == 0 {
		m.setApply(targetVal, targetK)
		return nil
	}
	entry := entries[0]
	m.wind = append(m.wind, entry)
	return m.applyProc(entry.inThunk, nil, callExpr, callEnv, &kontWindRewind{
		entries:   entries[1:],
		targetVal: targetVal, targetK: targetK,
		callExpr: callExpr, callEnv: callEnv,
	})
}

// ---------- Top-level CEK entry ----------

func cekEvalProgram(exprs []*expr, environ *env) (value, error) {
	if len(exprs) == 0 {
		return voidVal, nil
	}
	m := &cekM{}
	var k kont = &kontHalt{}
	if len(exprs) > 1 {
		k = &kontTopSeq{
			remaining: exprs[1:], env: environ,
			lastVal: voidVal, hasResult: false, parent: &kontHalt{},
		}
	}
	m.setEval(exprs[0], environ, k)
	return m.run()
}
