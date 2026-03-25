package ming

import "fmt"

// CallCCVal is the call/cc built-in value.
type CallCCVal struct{}

func (c *CallCCVal) String() string { return "#<procedure call/cc>" }

// ContinuationVal is a first-class continuation captured by call/cc.
type ContinuationVal struct {
	id            int64
	exprIdx       int
	protectedEnvs map[int64]*Env
}

func (c *ContinuationVal) String() string { return "#<continuation>" }

// continuationJump is the panic value when a continuation is invoked.
type continuationJump struct {
	cont  *ContinuationVal
	value Value
}

// EvalState manages continuation state during a single evaluation session.
type EvalState struct {
	contCounter      int64
	letCounter       int64
	pendingReturn    *pendingContReturn
	continuations    map[int64]*ContinuationVal
	currentExprIdx   int
	protectedLetEnvs map[int64]*Env
	activeLetStack   []letEnvEntry
	exprContStarts   map[int]int64
	exprLetStarts    map[int]int64
}

type letEnvEntry struct {
	id  int64
	env *Env
}

type pendingContReturn struct {
	contID int64
	value  Value
}

func newEvalState() *EvalState {
	return &EvalState{
		continuations:    make(map[int64]*ContinuationVal),
		protectedLetEnvs: make(map[int64]*Env),
		exprContStarts:   make(map[int]int64),
		exprLetStarts:    make(map[int]int64),
	}
}

func handleCallCC(f Value, ln, cl int, env *Env) (Value, error) {
	state := env.evalState
	id := state.contCounter
	state.contCounter++

	// Check for pending return targeting this call/cc
	var pendingVal *pendingContReturn
	if state.pendingReturn != nil && state.pendingReturn.contID == id {
		pendingVal = state.pendingReturn
		state.pendingReturn = nil
	}

	// Capture protected let environments (all enclosing lets)
	protectedEnvs := make(map[int64]*Env, len(state.activeLetStack))
	for _, entry := range state.activeLetStack {
		protectedEnvs[entry.id] = entry.env
	}

	k := &ContinuationVal{
		id:            id,
		exprIdx:       state.currentExprIdx,
		protectedEnvs: protectedEnvs,
	}
	state.continuations[id] = k

	// Always call f(k) — even during re-execution, f(k) must run
	// so that its side effects (like saving k to a variable) happen.
	var result Value
	var resultErr error
	escaped := false

	func() {
		defer func() {
			if r := recover(); r != nil {
				if j, ok := r.(*continuationJump); ok && j.cont.id == id {
					result = j.value
					escaped = true
					return
				}
				panic(r) // not for us, re-panic
			}
		}()
		result, resultErr = callProc(f, []Value{k}, ln, cl)
	}()

	if resultErr != nil {
		return nil, resultErr
	}

	// If there was a pending return (re-execution), use that value
	if pendingVal != nil {
		return pendingVal.value, nil
	}
	if escaped {
		return result, nil
	}
	return result, nil
}

// callProc applies a callable value to arguments, resolving tail calls.
func callProc(fn Value, args []Value, ln, cl int) (Value, error) {
	switch f := fn.(type) {
	case *BuiltinFunc:
		result, err := f.Fn(args)
		if err != nil {
			if ee, ok := err.(*EvalError); ok {
				if len(ee.Message) == 0 || ee.Message[0] < '0' || ee.Message[0] > '9' {
					ee.Message = fmt.Sprintf("%d:%d: %s", ln, cl, ee.Message)
				}
			}
			return nil, err
		}
		return result, nil
	case *LambdaVal:
		return resolveTC(applyLambda(f, args, ln, cl))
	case *CaseLambdaVal:
		return resolveTC(applyCaseLambda(f, args, ln, cl))
	case *ContinuationVal:
		if len(args) != 1 {
			return nil, &EvalError{Message: fmt.Sprintf("%d:%d: continuation: requires exactly 1 argument", ln, cl)}
		}
		panic(&continuationJump{cont: f, value: args[0]})
	default:
		return nil, &EvalError{Message: fmt.Sprintf("%d:%d: not a procedure: %s", ln, cl, fn.String())}
	}
}

const maxContIterations = 100000

func evalTopLevel(exprs []Expr, env *Env) (Value, error) {
	state := env.evalState

	startIdx := 0
	for iter := 0; iter < maxContIterations; iter++ {
		// Set counters to match where we're starting from
		if cs, ok := state.exprContStarts[startIdx]; ok {
			state.contCounter = cs
		} else {
			state.contCounter = 0
		}
		if ls, ok := state.exprLetStarts[startIdx]; ok {
			state.letCounter = ls
		} else {
			state.letCounter = 0
		}

		var lastVal Value = &VoidVal{}
		var lastErr error
		var jump *continuationJump

		func() {
			defer func() {
				if r := recover(); r != nil {
					if j, ok := r.(*continuationJump); ok {
						jump = j
						return
					}
					panic(r)
				}
			}()

			for i := startIdx; i < len(exprs); i++ {
				// Record counter values at start of each expression (first time only)
				if _, ok := state.exprContStarts[i]; !ok {
					state.exprContStarts[i] = state.contCounter
					state.exprLetStarts[i] = state.letCounter
				}
				state.currentExprIdx = i
				lastVal, lastErr = eval(exprs[i], env)
				if lastErr != nil {
					return
				}
			}
		}()

		if jump == nil {
			return lastVal, lastErr
		}

		// Continuation invoked from outside its call/cc extent — re-execute
		// from the expression that contains the call/cc
		state.pendingReturn = &pendingContReturn{
			contID: jump.cont.id,
			value:  jump.value,
		}
		state.protectedLetEnvs = jump.cont.protectedEnvs
		startIdx = jump.cont.exprIdx
	}
	return nil, &EvalError{Message: "call/cc: exceeded maximum re-execution iterations"}
}
