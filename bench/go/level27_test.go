package ming

import "testing"

// ===== Level 27: Step-Limited Evaluation =====

func skipIfBelowL27(t *testing.T) {
	t.Helper()
	level := benchLevel()
	if level > 0 && level < 27 {
		t.Skip("Skipping: BENCH_LEVEL < 27")
	}
}

func TestL27StepLimitNormal(t *testing.T) {
	skipIfBelowL27(t)
	r, err := EvalStrWithLimit("(+ 1 2)", 1000)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if r != "3" {
		t.Fatalf("expected \"3\", got %q", r)
	}
}

func TestL27StepLimitLoopWithinBudget(t *testing.T) {
	skipIfBelowL27(t)
	r, err := EvalStrWithLimit("(let loop ((n 50)) (if (= n 0) 'done (loop (- n 1))))", 10000)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if r != "done" {
		t.Fatalf("expected \"done\", got %q", r)
	}
}

func TestL27StepLimitInfiniteLoop(t *testing.T) {
	skipIfBelowL27(t)
	_, err := EvalStrWithLimit("(let loop () (loop))", 1000)
	if err == nil {
		t.Fatal("infinite loop should hit step limit")
	}
}

func TestL27StepLimitExceeded(t *testing.T) {
	skipIfBelowL27(t)
	_, err := EvalStrWithLimit("(let loop ((n 1000)) (if (= n 0) 'done (loop (- n 1))))", 50)
	if err == nil {
		t.Fatal("1000-iter loop should exceed 50-step budget")
	}
}

func TestL27StepLimitFactorial(t *testing.T) {
	skipIfBelowL27(t)
	r, err := EvalStrWithLimit("(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 10)", 10000)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if r != "3628800" {
		t.Fatalf("expected \"3628800\", got %q", r)
	}
}

func TestL27NormalEvalUnaffected(t *testing.T) {
	skipIfBelowL27(t)
	r, err := EvalStr("(let loop ((n 100000)) (if (= n 0) 'done (loop (- n 1))))")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if r != "done" {
		t.Fatalf("expected \"done\", got %q", r)
	}
}
