package ming

import (
	"fmt"
	"testing"
)

// ===== Level 27: Concurrent Evaluation =====
// eval_str must be safe for concurrent use from multiple goroutines.

func skipIfBelowL27(t *testing.T) {
	t.Helper()
	level := benchLevel()
	if level > 0 && level < 27 {
		t.Skip("Skipping: BENCH_LEVEL < 27")
	}
}

func TestL27ConcurrentIndependentEval(t *testing.T) {
	skipIfBelowL27(t)
	const n = 8
	type result struct {
		idx int
		val string
		err error
	}
	ch := make(chan result, n)
	for i := 0; i < n; i++ {
		go func(seed int) {
			program := fmt.Sprintf("(let loop ((n 1000) (acc 0)) (if (= n 0) acc (loop (- n 1) (+ acc %d))))", seed)
			val, err := EvalStr(program)
			ch <- result{seed, val, err}
		}(i)
	}
	for i := 0; i < n; i++ {
		r := <-ch
		if r.err != nil {
			t.Fatalf("thread %d: unexpected error: %v", r.idx, r.err)
		}
		expected := fmt.Sprintf("%d", r.idx*1000)
		if r.val != expected {
			t.Fatalf("thread %d: expected %q, got %q", r.idx, expected, r.val)
		}
	}
}

func TestL27ConcurrentOutputIsolation(t *testing.T) {
	skipIfBelowL27(t)
	const n = 4
	type result struct {
		idx    int
		val    string
		output string
		err    error
	}
	ch := make(chan result, n)
	for i := 0; i < n; i++ {
		go func(idx int) {
			program := fmt.Sprintf(`(begin (display "thread%d") (display " ") (display "done%d") "ok")`, idx, idx)
			val, output, err := EvalStrWithOutput(program)
			ch <- result{idx, val, output, err}
		}(i)
	}
	for i := 0; i < n; i++ {
		r := <-ch
		if r.err != nil {
			t.Fatalf("thread %d: unexpected error: %v", r.idx, r.err)
		}
		if r.val != "ok" {
			t.Fatalf("thread %d: expected \"ok\", got %q", r.idx, r.val)
		}
		expectedOutput := fmt.Sprintf("thread%d done%d", r.idx, r.idx)
		if r.output != expectedOutput {
			t.Fatalf("thread %d: expected output %q, got %q", r.idx, expectedOutput, r.output)
		}
	}
}

func TestL27ConcurrentClosuresAndMutation(t *testing.T) {
	skipIfBelowL27(t)
	const n = 4
	ch := make(chan struct {
		val string
		err error
	}, n)
	for i := 0; i < n; i++ {
		go func() {
			val, err := EvalStr(`(let ((count 0))
				(define (inc!) (set! count (+ count 1)) count)
				(inc!) (inc!) (inc!)
				count)`)
			ch <- struct {
				val string
				err error
			}{val, err}
		}()
	}
	for i := 0; i < n; i++ {
		r := <-ch
		if r.err != nil {
			t.Fatalf("unexpected error: %v", r.err)
		}
		if r.val != "3" {
			t.Fatalf("expected \"3\", got %q", r.val)
		}
	}
}

func TestL27ConcurrentStress(t *testing.T) {
	skipIfBelowL27(t)
	const n = 16
	type result struct {
		idx int
		val string
		err error
	}
	ch := make(chan result, n)
	for i := 0; i < n; i++ {
		go func(idx int) {
			program := fmt.Sprintf("(let ((x %d)) (define (f n) (if (= n 0) x (f (- n 1)))) (f 100))", idx)
			val, err := EvalStr(program)
			ch <- result{idx, val, err}
		}(i)
	}
	for i := 0; i < n; i++ {
		r := <-ch
		if r.err != nil {
			t.Fatalf("thread %d: unexpected error: %v", r.idx, r.err)
		}
		expected := fmt.Sprintf("%d", r.idx)
		if r.val != expected {
			t.Fatalf("thread %d: expected %q, got %q", r.idx, expected, r.val)
		}
	}
}

// ===== State isolation tests (sequential) =====

func TestL27SequentialStateLeak(t *testing.T) {
	skipIfBelowL27(t)
	r1, err := EvalStr("(begin (define x 42) x)")
	if err != nil {
		t.Fatalf("first eval failed: %v", err)
	}
	if r1 != "42" {
		t.Fatalf("expected \"42\", got %q", r1)
	}
	_, err2 := EvalStr("x")
	if err2 == nil {
		t.Fatal("variable 'x' leaked between independent EvalStr calls")
	}
}

func TestL27SequentialOutputLeak(t *testing.T) {
	skipIfBelowL27(t)
	_, out1, err1 := EvalStrWithOutput(`(display "aaa")`)
	if err1 != nil {
		t.Fatalf("first eval failed: %v", err1)
	}
	_, out2, err2 := EvalStrWithOutput(`(display "bbb")`)
	if err2 != nil {
		t.Fatalf("second eval failed: %v", err2)
	}
	if out1 != "aaa" {
		t.Fatalf("expected output \"aaa\", got %q", out1)
	}
	if out2 != "bbb" {
		t.Fatalf("output buffer leaked: expected \"bbb\", got %q", out2)
	}
}

func TestL27ConcurrentCallccCollision(t *testing.T) {
	skipIfBelowL27(t)
	const n = 8
	ch := make(chan struct {
		val string
		err error
	}, n)
	for i := 0; i < n; i++ {
		go func() {
			val, err := EvalStr(`(let ((count 0))
				(set! count (+ count (call/cc (lambda (k) (k 10)))))
				count)`)
			ch <- struct {
				val string
				err error
			}{val, err}
		}()
	}
	for i := 0; i < n; i++ {
		r := <-ch
		if r.err != nil {
			t.Fatalf("unexpected error: %v", r.err)
		}
		if r.val != "10" {
			t.Fatalf("expected \"10\", got %q", r.val)
		}
	}
}

func TestL27ConcurrentMacroHygiene(t *testing.T) {
	skipIfBelowL27(t)
	const n = 4
	ch := make(chan struct {
		val string
		err error
	}, n)
	for i := 0; i < n; i++ {
		go func() {
			val, err := EvalStr(`(begin
				(define-syntax my-swap!
					(syntax-rules ()
						((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))
				(let ((x 1) (y 2))
					(my-swap! x y)
					(list x y)))`)
			ch <- struct {
				val string
				err error
			}{val, err}
		}()
	}
	for i := 0; i < n; i++ {
		r := <-ch
		if r.err != nil {
			t.Fatalf("unexpected error: %v", r.err)
		}
		if r.val != "(2 1)" {
			t.Fatalf("expected \"(2 1)\", got %q", r.val)
		}
	}
}
