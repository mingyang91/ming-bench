package ming

import (
	"encoding/json"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"testing"
)

// testEntry mirrors the JSON structure in tests.json.
type testEntry struct {
	Name            string `json:"name"`
	Level           int    `json:"level"`
	Fixture         string `json:"fixture"`
	Kind            string `json:"kind"`
	Expected        string `json:"expected"`
	ExpectedOutput  string `json:"expected_output"`
	DeprecatedAfter *int   `json:"deprecated_after"`
}

var allTests []testEntry
var fixtureCache = map[string]string{}

func TestMain(m *testing.M) {
	data, err := os.ReadFile("../tests.json")
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to read tests.json: %v\n", err)
		os.Exit(1)
	}
	if err := json.Unmarshal(data, &allTests); err != nil {
		fmt.Fprintf(os.Stderr, "failed to parse tests.json: %v\n", err)
		os.Exit(1)
	}

	// Pre-load all fixture files.
	for _, tc := range allTests {
		if _, ok := fixtureCache[tc.Fixture]; ok {
			continue
		}
		content, err := os.ReadFile("../fixtures/" + tc.Fixture)
		if err != nil {
			fmt.Fprintf(os.Stderr, "failed to read fixture %s: %v\n", tc.Fixture, err)
			os.Exit(1)
		}
		fixtureCache[tc.Fixture] = string(content)
	}

	os.Exit(m.Run())
}

// benchLevel returns the BENCH_LEVEL env var as an int, or 0 if unset.
func benchLevel() int {
	s := os.Getenv("BENCH_LEVEL")
	if s == "" {
		return 0
	}
	n, err := strconv.Atoi(s)
	if err != nil {
		return 0
	}
	return n
}

// hasDigitColon matches a digit followed by a colon, used to verify
// position info in error messages.
var hasDigitColon = regexp.MustCompile(`\d:`)

// runLevelTests runs all test entries for the given level as subtests.
// Subtest names use the suffix after the "lNN_" prefix, producing
// top-level names like TestL01/integer, TestL01/add, etc.
func runLevelTests(t *testing.T, level int) {
	t.Helper()
	bl := benchLevel()

	if bl > 0 && level > bl {
		t.Skipf("level %d > BENCH_LEVEL %d", level, bl)
	}

	prefix := fmt.Sprintf("l%02d_", level)

	for _, tc := range allTests {
		if tc.Level != level {
			continue
		}
		tc := tc // capture

		// Strip the "lNN_" prefix to get the subtest name.
		subName := tc.Name[len(prefix):]

		t.Run(subName, func(t *testing.T) {
			// Skip deprecated tests: if deprecated_after is set and
			// BENCH_LEVEL exceeds it, the test is no longer relevant.
			if tc.DeprecatedAfter != nil && bl > *tc.DeprecatedAfter {
				t.Skipf("deprecated after level %d (BENCH_LEVEL=%d)", *tc.DeprecatedAfter, bl)
			}

			input := fixtureCache[tc.Fixture]

			switch tc.Kind {
			case "eval_str_ok":
				got, err := EvalStr(input)
				if err != nil {
					t.Fatalf("expected success, got error: %v", err)
				}
				if got != tc.Expected {
					t.Fatalf("expected %q, got %q", tc.Expected, got)
				}

			case "eval_str_err":
				_, err := EvalStr(input)
				if err == nil {
					t.Fatalf("expected error, got success")
				}

			case "eval_str_err_with_position":
				_, err := EvalStr(input)
				if err == nil {
					t.Fatalf("expected error, got success")
				}
				if !hasDigitColon.MatchString(err.Error()) {
					t.Fatalf("error %q should contain a digit followed by colon (position info)", err.Error())
				}

			case "eval_str_with_output":
				_, output, err := EvalStrWithOutput(input)
				if err != nil {
					t.Fatalf("expected success, got error: %v", err)
				}
				if output != tc.ExpectedOutput {
					t.Fatalf("expected output %q, got %q", tc.ExpectedOutput, output)
				}

			default:
				t.Fatalf("unknown test kind: %s", tc.Kind)
			}
		})
	}
}

func TestL01(t *testing.T) { runLevelTests(t, 1) }
func TestL02(t *testing.T) { runLevelTests(t, 2) }
func TestL03(t *testing.T) { runLevelTests(t, 3) }
func TestL04(t *testing.T) { runLevelTests(t, 4) }
func TestL05(t *testing.T) { runLevelTests(t, 5) }
func TestL06(t *testing.T) { runLevelTests(t, 6) }
func TestL07(t *testing.T) { runLevelTests(t, 7) }
func TestL08(t *testing.T) { runLevelTests(t, 8) }
func TestL09(t *testing.T) { runLevelTests(t, 9) }
func TestL10(t *testing.T) { runLevelTests(t, 10) }
func TestL11(t *testing.T) { runLevelTests(t, 11) }
func TestL12(t *testing.T) { runLevelTests(t, 12) }
func TestL13(t *testing.T) { runLevelTests(t, 13) }
func TestL14(t *testing.T) { runLevelTests(t, 14) }
func TestL15(t *testing.T) { runLevelTests(t, 15) }
func TestL16(t *testing.T) { runLevelTests(t, 16) }
func TestL17(t *testing.T) { runLevelTests(t, 17) }
func TestL18(t *testing.T) { runLevelTests(t, 18) }
func TestL19(t *testing.T) { runLevelTests(t, 19) }
func TestL20(t *testing.T) { runLevelTests(t, 20) }
func TestL21(t *testing.T) { runLevelTests(t, 21) }
func TestL22(t *testing.T) { runLevelTests(t, 22) }
func TestL23(t *testing.T) { runLevelTests(t, 23) }

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
	// x must not leak to next EvalStr call
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
