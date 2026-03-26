package ming;

import java.util.*;
import java.util.concurrent.*;

/**
 * Level 27: Concurrent Evaluation — standalone runner.
 * Called by the orchestrator after injecting into the worktree.
 * Usage: java ming.L28Tests
 */
public class L28Tests {
    static int passed = 0;
    static int failed = 0;

    public static void main(String[] args) {
        try {
            testSequentialStateLeak();
            testSequentialOutputLeak();
            testConcurrentIndependentEval();
            testConcurrentOutputIsolation();
            testConcurrentClosuresAndMutation();
            testConcurrentStress();
            testConcurrentCallccCollision();
            testConcurrentMacroHygiene();
        } catch (Exception e) {
            System.out.println("FAIL unexpected: " + e);
            failed++;
        }
        System.out.println(passed + " passed, " + failed + " failed out of " + (passed + failed) + " tests");
        System.exit(failed > 0 ? 1 : 0);
    }

    static void pass(String name) { System.out.println("PASS " + name); passed++; }
    static void fail(String name, String msg) { System.out.println("FAIL " + name + ": " + msg); failed++; }

    static void testSequentialStateLeak() throws Exception {
        Evaluator e1 = new Evaluator();
        String r1 = e1.evalStr("(begin (define x 42) x)");
        if (!"42".equals(r1)) { fail("l28_sequential_state_leak", "first eval: expected 42, got " + r1); return; }
        Evaluator e2 = new Evaluator();
        try {
            e2.evalStr("x");
            fail("l28_sequential_state_leak", "variable 'x' leaked between independent evalStr calls");
        } catch (EvalError e) {
            pass("l28_sequential_state_leak");
        }
    }

    static void testSequentialOutputLeak() throws Exception {
        Evaluator e1 = new Evaluator();
        EvalResult r1 = e1.evalStrWithOutput("(display \"aaa\")");
        Evaluator e2 = new Evaluator();
        EvalResult r2 = e2.evalStrWithOutput("(display \"bbb\")");
        if (!"aaa".equals(r1.output())) { fail("l28_sequential_output_leak", "first output: " + r1.output()); return; }
        if (!"bbb".equals(r2.output())) { fail("l28_sequential_output_leak", "output leaked: got " + r2.output()); return; }
        pass("l28_sequential_output_leak");
    }

    static void testConcurrentIndependentEval() throws Exception {
        ExecutorService pool = Executors.newFixedThreadPool(8);
        List<Future<String>> futures = new ArrayList<>();
        for (int i = 0; i < 8; i++) {
            final int seed = i;
            futures.add(pool.submit(() -> {
                Evaluator e = new Evaluator();
                return e.evalStr(String.format(
                    "(let loop ((n 1000) (acc 0)) (if (= n 0) acc (loop (- n 1) (+ acc %d))))", seed));
            }));
        }
        boolean ok = true;
        for (int i = 0; i < 8; i++) {
            String r = futures.get(i).get(30, TimeUnit.SECONDS);
            if (!String.valueOf(i * 1000).equals(r)) {
                fail("l28_concurrent_independent_eval", "thread " + i + ": expected " + (i*1000) + ", got " + r);
                ok = false; break;
            }
        }
        if (ok) pass("l28_concurrent_independent_eval");
        pool.shutdown();
    }

    static void testConcurrentOutputIsolation() throws Exception {
        ExecutorService pool = Executors.newFixedThreadPool(4);
        List<Future<EvalResult>> futures = new ArrayList<>();
        for (int i = 0; i < 4; i++) {
            final int idx = i;
            futures.add(pool.submit(() -> {
                Evaluator e = new Evaluator();
                return e.evalStrWithOutput(String.format(
                    "(begin (display \"thread%d\") (display \" \") (display \"done%d\") \"ok\")", idx, idx));
            }));
        }
        boolean ok = true;
        for (int i = 0; i < 4; i++) {
            EvalResult r = futures.get(i).get(30, TimeUnit.SECONDS);
            String expected = String.format("thread%d done%d", i, i);
            if (!"ok".equals(r.result()) || !expected.equals(r.output())) {
                fail("l28_concurrent_output_isolation", "thread " + i + ": result=" + r.result() + " output=" + r.output());
                ok = false; break;
            }
        }
        if (ok) pass("l28_concurrent_output_isolation");
        pool.shutdown();
    }

    static void testConcurrentClosuresAndMutation() throws Exception {
        ExecutorService pool = Executors.newFixedThreadPool(4);
        List<Future<String>> futures = new ArrayList<>();
        for (int i = 0; i < 4; i++) {
            futures.add(pool.submit(() -> {
                Evaluator e = new Evaluator();
                return e.evalStr("(let ((count 0)) (define (inc!) (set! count (+ count 1)) count) (inc!) (inc!) (inc!) count)");
            }));
        }
        boolean ok = true;
        for (Future<String> f : futures) {
            if (!"3".equals(f.get(30, TimeUnit.SECONDS))) {
                fail("l28_concurrent_closures_mutation", "expected 3"); ok = false; break;
            }
        }
        if (ok) pass("l28_concurrent_closures_mutation");
        pool.shutdown();
    }

    static void testConcurrentStress() throws Exception {
        ExecutorService pool = Executors.newFixedThreadPool(16);
        List<Future<String>> futures = new ArrayList<>();
        for (int i = 0; i < 16; i++) {
            final int idx = i;
            futures.add(pool.submit(() -> {
                Evaluator e = new Evaluator();
                return e.evalStr(String.format("(let ((x %d)) (define (f n) (if (= n 0) x (f (- n 1)))) (f 100))", idx));
            }));
        }
        boolean ok = true;
        for (int i = 0; i < 16; i++) {
            if (!String.valueOf(i).equals(futures.get(i).get(30, TimeUnit.SECONDS))) {
                fail("l28_concurrent_stress", "thread " + i + " wrong result"); ok = false; break;
            }
        }
        if (ok) pass("l28_concurrent_stress");
        pool.shutdown();
    }

    static void testConcurrentCallccCollision() throws Exception {
        ExecutorService pool = Executors.newFixedThreadPool(8);
        List<Future<String>> futures = new ArrayList<>();
        for (int i = 0; i < 8; i++) {
            futures.add(pool.submit(() -> {
                Evaluator e = new Evaluator();
                return e.evalStr("(let ((count 0)) (set! count (+ count (call/cc (lambda (k) (k 10))))) count)");
            }));
        }
        boolean ok = true;
        for (Future<String> f : futures) {
            if (!"10".equals(f.get(30, TimeUnit.SECONDS))) {
                fail("l28_concurrent_callcc_collision", "expected 10"); ok = false; break;
            }
        }
        if (ok) pass("l28_concurrent_callcc_collision");
        pool.shutdown();
    }

    static void testConcurrentMacroHygiene() throws Exception {
        ExecutorService pool = Executors.newFixedThreadPool(4);
        List<Future<String>> futures = new ArrayList<>();
        for (int i = 0; i < 4; i++) {
            futures.add(pool.submit(() -> {
                Evaluator e = new Evaluator();
                return e.evalStr(
                    "(begin (define-syntax my-swap! (syntax-rules () ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))" +
                    " (let ((x 1) (y 2)) (my-swap! x y) (list x y)))");
            }));
        }
        boolean ok = true;
        for (Future<String> f : futures) {
            if (!"(2 1)".equals(f.get(30, TimeUnit.SECONDS))) {
                fail("l28_concurrent_macro_hygiene", "expected (2 1)"); ok = false; break;
            }
        }
        if (ok) pass("l28_concurrent_macro_hygiene");
        pool.shutdown();
    }
}
