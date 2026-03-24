package ming

import (
	"fmt"
	"sync/atomic"
)

var gensymCounter uint64

func gensym(base string) string {
	n := atomic.AddUint64(&gensymCounter, 1)
	return fmt.Sprintf("%s__m%d", base, n)
}

// SyntaxVal represents a syntax-rules macro transformer.
type SyntaxVal struct {
	Name     string
	Literals map[string]bool
	Rules    []syntaxRule
	DefEnv   *Env
}

func (v *SyntaxVal) String() string { return "#<syntax>" }

type syntaxRule struct {
	Pattern  *Expr
	Template *Expr
}

type macroBindings struct {
	singles map[string]*Expr
	lists   map[string][]*Expr
}

func newMacroBindings() *macroBindings {
	return &macroBindings{
		singles: make(map[string]*Expr),
		lists:   make(map[string][]*Expr),
	}
}

var specialForms = map[string]bool{
	"define": true, "if": true, "quote": true, "lambda": true,
	"let": true, "let*": true, "begin": true, "cond": true, "set!": true,
	"and": true, "or": true, "define-syntax": true, "syntax-rules": true,
	"else": true, "syntax-case": true, "syntax": true, "with-syntax": true,
}

// evalDefineSyntax handles (define-syntax name transformer)
// transformer can be (syntax-rules ...) or a lambda (for syntax-case).
func evalDefineSyntax(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) != 3 {
		return nil, &EvalError{Message: "define-syntax: bad syntax"}
	}
	nameExpr := expr.List[1]
	if nameExpr.Kind != ExprSymbol {
		return nil, &EvalError{Message: "define-syntax: expected symbol"}
	}
	srExpr := expr.List[2]

	// Check if it's syntax-rules
	if srExpr.Kind == ExprList && len(srExpr.List) >= 2 &&
		srExpr.List[0].Kind == ExprSymbol && srExpr.List[0].SVal == "syntax-rules" {
		litExpr := srExpr.List[1]
		if litExpr.Kind != ExprList {
			return nil, &EvalError{Message: "syntax-rules: expected literals list"}
		}
		literals := make(map[string]bool)
		for _, l := range litExpr.List {
			if l.Kind == ExprSymbol {
				literals[l.SVal] = true
			}
		}
		var rules []syntaxRule
		for _, r := range srExpr.List[2:] {
			if r.Kind != ExprList || len(r.List) != 2 {
				return nil, &EvalError{Message: "syntax-rules: bad rule"}
			}
			rules = append(rules, syntaxRule{Pattern: r.List[0], Template: r.List[1]})
		}
		env.Set(nameExpr.SVal, &SyntaxVal{
			Name:     nameExpr.SVal,
			Literals: literals,
			Rules:    rules,
			DefEnv:   env,
		})
		return &VoidVal{}, nil
	}

	// Otherwise evaluate as a transformer expression (e.g. lambda for syntax-case)
	val, err := eval(srExpr, env)
	if err != nil {
		return nil, err
	}
	env.Set(nameExpr.SVal, &TransformerVal{Proc: val})
	return &VoidVal{}, nil
}

// expandMacro expands a macro call and evaluates the result.
func expandMacro(sv *SyntaxVal, callExpr *Expr, useEnv *Env) (Value, error) {
	for _, rule := range sv.Rules {
		bindings := newMacroBindings()
		if matchPattern(callExpr, rule.Pattern, sv.Literals, sv.Name, bindings) {
			patVars := make(map[string]bool)
			for k := range bindings.singles {
				patVars[k] = true
			}
			for k := range bindings.lists {
				patVars[k] = true
			}

			gensyms := make(map[string]string)
			collectIntroduced(rule.Template, patVars, sv.Literals, gensyms)

			expanded := expandTemplate(rule.Template, bindings, gensyms)

			if len(gensyms) > 0 {
				evalEnv := NewEnv(useEnv)
				for orig, gs := range gensyms {
					if v, ok := sv.DefEnv.Get(orig); ok {
						evalEnv.Set(gs, v)
					}
				}
				return &tailCall{expr: expanded, env: evalEnv}, nil
			}
			return &tailCall{expr: expanded, env: useEnv}, nil
		}
	}
	return nil, &EvalError{Message: fmt.Sprintf("no matching pattern for macro %s", sv.Name)}
}

// Pattern matching

func matchPattern(call *Expr, pattern *Expr, literals map[string]bool, macroName string, bindings *macroBindings) bool {
	if call.Kind != ExprList || pattern.Kind != ExprList {
		return false
	}
	if len(pattern.List) == 0 {
		return len(call.List) == 0
	}
	return matchListElems(call.List[1:], pattern.List[1:], literals, macroName, bindings)
}

func matchListElems(callElems []*Expr, patElems []*Expr, literals map[string]bool, macroName string, bindings *macroBindings) bool {
	ci, pi := 0, 0
	for pi < len(patElems) {
		if pi+1 < len(patElems) && isEllipsis(patElems[pi+1]) {
			subPat := patElems[pi]
			remaining := 0
			for j := pi + 2; j < len(patElems); j++ {
				if !isEllipsis(patElems[j]) {
					remaining++
				}
			}
			pvars := collectPatVars(subPat, literals, macroName)
			for _, pv := range pvars {
				bindings.lists[pv] = nil
			}
			limit := len(callElems) - remaining
			for ci < limit {
				sub := newMacroBindings()
				if !matchExpr(callElems[ci], subPat, literals, macroName, sub) {
					return false
				}
				for _, pv := range pvars {
					if e, ok := sub.singles[pv]; ok {
						bindings.lists[pv] = append(bindings.lists[pv], e)
					}
				}
				ci++
			}
			pi += 2
			continue
		}
		if ci >= len(callElems) {
			return false
		}
		if !matchExpr(callElems[ci], patElems[pi], literals, macroName, bindings) {
			return false
		}
		ci++
		pi++
	}
	return ci == len(callElems)
}

func matchExpr(call *Expr, pat *Expr, literals map[string]bool, macroName string, bindings *macroBindings) bool {
	if pat.Kind == ExprSymbol && pat.SVal == "_" {
		return true
	}
	if pat.Kind == ExprSymbol && literals[pat.SVal] {
		return call.Kind == ExprSymbol && call.SVal == pat.SVal
	}
	if pat.Kind == ExprSymbol {
		bindings.singles[pat.SVal] = call
		return true
	}
	if pat.Kind == ExprBool {
		return call.Kind == ExprBool && call.BVal == pat.BVal
	}
	if pat.Kind == ExprInt {
		return call.Kind == ExprInt && call.IVal == pat.IVal
	}
	if pat.Kind == ExprString {
		return call.Kind == ExprString && call.SVal == pat.SVal
	}
	if pat.Kind == ExprList {
		if call.Kind != ExprList {
			return false
		}
		return matchListElems(call.List, pat.List, literals, macroName, bindings)
	}
	return false
}

func isEllipsis(e *Expr) bool {
	return e.Kind == ExprSymbol && e.SVal == "..."
}

func collectPatVars(pat *Expr, literals map[string]bool, macroName string) []string {
	var vars []string
	collectPatVarsRec(pat, literals, macroName, &vars)
	return vars
}

func collectPatVarsRec(pat *Expr, literals map[string]bool, macroName string, vars *[]string) {
	if pat.Kind == ExprSymbol && !literals[pat.SVal] && pat.SVal != "_" && pat.SVal != macroName && pat.SVal != "..." {
		*vars = append(*vars, pat.SVal)
	}
	if pat.Kind == ExprList {
		for _, e := range pat.List {
			if !isEllipsis(e) {
				collectPatVarsRec(e, literals, macroName, vars)
			}
		}
	}
}

// Template expansion

func expandTemplate(tmpl *Expr, bindings *macroBindings, gensyms map[string]string) *Expr {
	switch tmpl.Kind {
	case ExprSymbol:
		if e, ok := bindings.singles[tmpl.SVal]; ok {
			return e
		}
		if gs, ok := gensyms[tmpl.SVal]; ok {
			return &Expr{Kind: ExprSymbol, SVal: gs, Line: tmpl.Line, Col: tmpl.Col}
		}
		return tmpl
	case ExprList:
		var result []*Expr
		for i := 0; i < len(tmpl.List); i++ {
			elem := tmpl.List[i]
			if i+1 < len(tmpl.List) && isEllipsis(tmpl.List[i+1]) {
				evars := findEllipsisVars(elem, bindings)
				if len(evars) > 0 {
					count := len(bindings.lists[evars[0]])
					for j := 0; j < count; j++ {
						iter := &macroBindings{
							singles: make(map[string]*Expr),
							lists:   bindings.lists,
						}
						for k, v := range bindings.singles {
							iter.singles[k] = v
						}
						for _, ev := range evars {
							iter.singles[ev] = bindings.lists[ev][j]
						}
						result = append(result, expandTemplate(elem, iter, gensyms))
					}
				}
				i++ // skip ellipsis
				continue
			}
			result = append(result, expandTemplate(elem, bindings, gensyms))
		}
		return &Expr{Kind: ExprList, List: result, Line: tmpl.Line, Col: tmpl.Col}
	default:
		return tmpl
	}
}

func findEllipsisVars(tmpl *Expr, bindings *macroBindings) []string {
	var vars []string
	findEllipsisVarsRec(tmpl, bindings, &vars)
	return vars
}

func findEllipsisVarsRec(tmpl *Expr, bindings *macroBindings, vars *[]string) {
	if tmpl.Kind == ExprSymbol {
		if _, ok := bindings.lists[tmpl.SVal]; ok {
			*vars = append(*vars, tmpl.SVal)
		}
	}
	if tmpl.Kind == ExprList {
		for _, e := range tmpl.List {
			findEllipsisVarsRec(e, bindings, vars)
		}
	}
}

// Hygiene: collect introduced identifiers (not pattern vars, not special forms)

func collectIntroduced(tmpl *Expr, patVars map[string]bool, literals map[string]bool, gensyms map[string]string) {
	if tmpl.Kind == ExprSymbol {
		name := tmpl.SVal
		if !patVars[name] && !specialForms[name] && !literals[name] && name != "..." {
			if _, ok := gensyms[name]; !ok {
				gensyms[name] = gensym(name)
			}
		}
	}
	if tmpl.Kind == ExprList {
		for _, e := range tmpl.List {
			collectIntroduced(e, patVars, literals, gensyms)
		}
	}
}

// --- syntax-case support ---

// expandTransformerMacro expands a macro defined via a transformer procedure (syntax-case style).
func expandTransformerMacro(tv *TransformerVal, callExpr *Expr, useEnv *Env) (Value, error) {
	stx := &SyntaxObjectVal{Expr: callExpr}
	result, err := applyProc(tv.Proc, []Value{stx}, callExpr)
	if err != nil {
		return nil, err
	}
	// Handle tail call from applyProc
	for {
		tc, ok := result.(*tailCall)
		if !ok {
			break
		}
		for i := 0; i < tc.popFrames; i++ {
			if len(contFrameStack) > 0 {
				contFrameStack = contFrameStack[:len(contFrameStack)-1]
			}
		}
		result, err = eval(tc.expr, tc.env)
		if err != nil {
			return nil, err
		}
	}
	resultStx, ok := result.(*SyntaxObjectVal)
	if !ok {
		return nil, &EvalError{Message: "transformer must return syntax object"}
	}
	// Apply hygiene: bind gensym'd names from defEnv into use-site env.
	// Set directly in useEnv (not a child) so that define in expanded form persists.
	if len(resultStx.Gensyms) > 0 && resultStx.DefEnv != nil {
		for orig, gs := range resultStx.Gensyms {
			if v, ok := resultStx.DefEnv.Get(orig); ok {
				useEnv.Set(gs, v)
			}
		}
	}
	return &tailCall{expr: resultStx.Expr, env: useEnv}, nil
}

// evalSyntaxCase handles (syntax-case expr (literals...) clause...)
func evalSyntaxCase(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) < 4 {
		return nil, &EvalError{Message: "syntax-case: bad syntax"}
	}
	// Evaluate the scrutinee
	scrutineeVal, err := eval(expr.List[1], env)
	if err != nil {
		return nil, err
	}
	var scrutineeExpr *Expr
	if stx, ok := scrutineeVal.(*SyntaxObjectVal); ok {
		scrutineeExpr = stx.Expr
	} else {
		return nil, &EvalError{Message: "syntax-case: expected syntax object"}
	}
	// Parse literals list
	litExpr := expr.List[2]
	if litExpr.Kind != ExprList {
		return nil, &EvalError{Message: "syntax-case: expected literals list"}
	}
	literals := make(map[string]bool)
	for _, l := range litExpr.List {
		if l.Kind == ExprSymbol {
			literals[l.SVal] = true
		}
	}
	// Try each clause
	for _, clause := range expr.List[3:] {
		if clause.Kind != ExprList || len(clause.List) < 2 || len(clause.List) > 3 {
			return nil, &EvalError{Message: "syntax-case: bad clause"}
		}
		pattern := clause.List[0]
		bindings := newMacroBindings()

		if !matchSyntaxCasePattern(scrutineeExpr, pattern, literals, bindings) {
			continue
		}
		// Bind pattern variables as SyntaxObjectVal/SyntaxListVal
		childEnv := NewEnv(env)
		for name, e := range bindings.singles {
			childEnv.Set(name, &SyntaxObjectVal{Expr: e})
		}
		for name, es := range bindings.lists {
			childEnv.Set(name, &SyntaxListVal{Exprs: es})
		}
		// If fender present (3-element clause), check it
		bodyIdx := len(clause.List) - 1
		if len(clause.List) == 3 {
			fenderVal, ferr := eval(clause.List[1], childEnv)
			if ferr != nil {
				return nil, ferr
			}
			if !isTruthy(fenderVal) {
				continue
			}
		}
		return &tailCall{expr: clause.List[bodyIdx], env: childEnv}, nil
	}
	return nil, &EvalError{Message: "syntax-case: no matching clause"}
}

// matchSyntaxCasePattern matches a syntax-case pattern against an expr.
func matchSyntaxCasePattern(scrutinee *Expr, pattern *Expr, literals map[string]bool, bindings *macroBindings) bool {
	if pattern.Kind == ExprSymbol {
		if pattern.SVal == "_" {
			return true
		}
		if literals[pattern.SVal] {
			return scrutinee.Kind == ExprSymbol && scrutinee.SVal == pattern.SVal
		}
		bindings.singles[pattern.SVal] = scrutinee
		return true
	}
	if pattern.Kind == ExprList && scrutinee.Kind == ExprList {
		return matchListElems(scrutinee.List, pattern.List, literals, "", bindings)
	}
	return matchExpr(scrutinee, pattern, literals, "", bindings)
}

// evalSyntaxTemplate handles (syntax template) aka #'template
func evalSyntaxTemplate(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) != 2 {
		return nil, &EvalError{Message: "syntax: expected exactly 1 argument"}
	}
	tmpl := expr.List[1]

	// Simple symbol: look up as pattern variable
	if tmpl.Kind == ExprSymbol {
		if v, ok := env.Get(tmpl.SVal); ok {
			if stx, ok := v.(*SyntaxObjectVal); ok {
				return stx, nil
			}
			if sl, ok := v.(*SyntaxListVal); ok {
				return sl, nil
			}
		}
		return &SyntaxObjectVal{Expr: tmpl}, nil
	}

	// List template: expand with pattern variable substitution + hygiene
	if tmpl.Kind == ExprList {
		bindings := newMacroBindings()
		patVars := make(map[string]bool)
		collectSyntaxPatVars(tmpl, env, bindings, patVars)

		gensyms := make(map[string]string)
		collectIntroducedNoQuote(tmpl, patVars, gensyms)

		expanded := expandTemplateNoQuote(tmpl, bindings, gensyms)

		return &SyntaxObjectVal{Expr: expanded, Gensyms: gensyms, DefEnv: env}, nil
	}

	// Literal value
	return &SyntaxObjectVal{Expr: tmpl}, nil
}

// collectSyntaxPatVars finds symbols in template that are bound to SyntaxObjectVal/SyntaxListVal.
func collectSyntaxPatVars(tmpl *Expr, env *Env, bindings *macroBindings, patVars map[string]bool) {
	if tmpl.Kind == ExprSymbol {
		if v, ok := env.Get(tmpl.SVal); ok {
			switch sv := v.(type) {
			case *SyntaxObjectVal:
				bindings.singles[tmpl.SVal] = sv.Expr
				patVars[tmpl.SVal] = true
			case *SyntaxListVal:
				bindings.lists[tmpl.SVal] = sv.Exprs
				patVars[tmpl.SVal] = true
			}
		}
		return
	}
	if tmpl.Kind == ExprList {
		// Skip (quote ...) forms — contents are literal data
		if len(tmpl.List) >= 1 && tmpl.List[0].Kind == ExprSymbol && tmpl.List[0].SVal == "quote" {
			return
		}
		for _, e := range tmpl.List {
			collectSyntaxPatVars(e, env, bindings, patVars)
		}
	}
}

// collectIntroducedNoQuote collects introduced identifiers, skipping (quote ...) forms.
func collectIntroducedNoQuote(tmpl *Expr, patVars map[string]bool, gensyms map[string]string) {
	if tmpl.Kind == ExprSymbol {
		name := tmpl.SVal
		if !patVars[name] && !specialForms[name] && name != "..." {
			if _, ok := gensyms[name]; !ok {
				gensyms[name] = gensym(name)
			}
		}
	}
	if tmpl.Kind == ExprList {
		if len(tmpl.List) >= 1 && tmpl.List[0].Kind == ExprSymbol && tmpl.List[0].SVal == "quote" {
			return
		}
		for _, e := range tmpl.List {
			collectIntroducedNoQuote(e, patVars, gensyms)
		}
	}
}

// expandTemplateNoQuote is like expandTemplate but skips (quote ...) forms.
func expandTemplateNoQuote(tmpl *Expr, bindings *macroBindings, gensyms map[string]string) *Expr {
	switch tmpl.Kind {
	case ExprSymbol:
		if e, ok := bindings.singles[tmpl.SVal]; ok {
			return e
		}
		if gs, ok := gensyms[tmpl.SVal]; ok {
			return &Expr{Kind: ExprSymbol, SVal: gs, Line: tmpl.Line, Col: tmpl.Col}
		}
		return tmpl
	case ExprList:
		if len(tmpl.List) >= 1 && tmpl.List[0].Kind == ExprSymbol && tmpl.List[0].SVal == "quote" {
			return tmpl
		}
		var result []*Expr
		for i := 0; i < len(tmpl.List); i++ {
			elem := tmpl.List[i]
			if i+1 < len(tmpl.List) && isEllipsis(tmpl.List[i+1]) {
				evars := findEllipsisVars(elem, bindings)
				if len(evars) > 0 {
					count := len(bindings.lists[evars[0]])
					for j := 0; j < count; j++ {
						iter := &macroBindings{
							singles: make(map[string]*Expr),
							lists:   bindings.lists,
						}
						for k, v := range bindings.singles {
							iter.singles[k] = v
						}
						for _, ev := range evars {
							iter.singles[ev] = bindings.lists[ev][j]
						}
						result = append(result, expandTemplateNoQuote(elem, iter, gensyms))
					}
				}
				i++
				continue
			}
			result = append(result, expandTemplateNoQuote(elem, bindings, gensyms))
		}
		return &Expr{Kind: ExprList, List: result, Line: tmpl.Line, Col: tmpl.Col}
	default:
		return tmpl
	}
}

// evalWithSyntax handles (with-syntax ((pattern expr) ...) body ...)
func evalWithSyntax(expr *Expr, env *Env) (Value, error) {
	if len(expr.List) < 3 {
		return nil, &EvalError{Message: "with-syntax: bad syntax"}
	}
	bindingsExpr := expr.List[1]
	if bindingsExpr.Kind != ExprList {
		return nil, &EvalError{Message: "with-syntax: expected bindings list"}
	}
	childEnv := NewEnv(env)
	for _, binding := range bindingsExpr.List {
		if binding.Kind != ExprList || len(binding.List) != 2 {
			return nil, &EvalError{Message: "with-syntax: bad binding"}
		}
		pat := binding.List[0]
		val, err := eval(binding.List[1], env)
		if err != nil {
			return nil, err
		}
		if pat.Kind == ExprSymbol {
			childEnv.Set(pat.SVal, val)
		} else {
			return nil, &EvalError{Message: "with-syntax: expected symbol pattern"}
		}
	}
	var result Value
	var err error
	for _, body := range expr.List[2:] {
		result, err = eval(body, childEnv)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

// syntaxToValue converts a syntax object's Expr to a Scheme Value.
func syntaxToValue(e *Expr) (Value, error) {
	switch e.Kind {
	case ExprInt:
		return &IntVal{Val: e.IVal}, nil
	case ExprFloat:
		return &FloatVal{Val: e.FVal}, nil
	case ExprRational:
		return makeRational(e.Num, e.Denom), nil
	case ExprBool:
		return &BoolVal{Val: e.BVal}, nil
	case ExprString:
		return &StringVal{Val: e.SVal, Immutable: true}, nil
	case ExprChar:
		return &CharVal{Val: e.RVal}, nil
	case ExprSymbol:
		return &SymbolVal{Name: e.SVal}, nil
	case ExprList:
		if len(e.List) == 0 {
			return &NilVal{}, nil
		}
		result := Value(&NilVal{})
		for i := len(e.List) - 1; i >= 0; i-- {
			v, err := syntaxToValue(e.List[i])
			if err != nil {
				return nil, err
			}
			result = &PairVal{Car: v, Cdr: result}
		}
		return result, nil
	}
	return nil, fmt.Errorf("syntax->datum: unsupported expression kind")
}

// valueToExpr converts a Scheme Value to an Expr (for datum->syntax).
func valueToExpr(v Value) *Expr {
	switch val := v.(type) {
	case *IntVal:
		return &Expr{Kind: ExprInt, IVal: val.Val}
	case *FloatVal:
		return &Expr{Kind: ExprFloat, FVal: val.Val}
	case *RationalVal:
		return &Expr{Kind: ExprRational, Num: val.Num, Denom: val.Denom}
	case *BoolVal:
		return &Expr{Kind: ExprBool, BVal: val.Val}
	case *StringVal:
		return &Expr{Kind: ExprString, SVal: val.Val}
	case *CharVal:
		return &Expr{Kind: ExprChar, RVal: val.Val}
	case *SymbolVal:
		return &Expr{Kind: ExprSymbol, SVal: val.Name}
	case *NilVal:
		return &Expr{Kind: ExprList}
	case *PairVal:
		var elems []*Expr
		cur := Value(val)
		for {
			p, ok := cur.(*PairVal)
			if !ok {
				break
			}
			elems = append(elems, valueToExpr(p.Car))
			cur = p.Cdr
		}
		return &Expr{Kind: ExprList, List: elems}
	}
	return &Expr{Kind: ExprSymbol, SVal: v.String()}
}
