package ming;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assumptions;
import java.util.List;
import java.util.ArrayList;
import java.util.concurrent.*;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Level 27: Concurrent Evaluation
 * eval_str must be safe for concurrent use from multiple threads.
 */
class ConcurrencyTest {

    private static int benchLevel() {
        String env = System.getenv("BENCH_LEVEL");
        if (env == null) env = System.getProperty("bench.level", "0");
        try { return Integer.parseInt(env); } catch (NumberFormatException e) { return 0; }
    }

    private void skipIfBelowLevel() {
        int level = benchLevel();
        Assumptions.assumeTrue(level == 0 || level >= 27, "Skipping: BENCH_LEVEL < 27");
    }

    @Test
    void testL27ConcurrentIndependentEval() throws Exception {
        skipIfBelowLevel();
        ExecutorService pool = Executors.newFixedThreadPool(8);
        List<Future<String>> futures = new ArrayList<>();

        for (int i = 0; i < 8; i++) {
            final int seed = i;
            futures.add(pool.submit(() -> {
                Interpreter interp = new Interpreter();
                return interp.evalStr(String.format(
                    "(let loop ((n 1000) (acc 0)) (if (= n 0) acc (loop (- n 1) (+ acc %d))))", seed));
            }));
        }

        for (int i = 0; i < 8; i++) {
            assertEquals(String.valueOf(i * 1000), futures.get(i).get(30, TimeUnit.SECONDS));
        }
        pool.shutdown();
    }

    @Test
    void testL27ConcurrentOutputIsolation() throws Exception {
        skipIfBelowLevel();
        ExecutorService pool = Executors.newFixedThreadPool(4);
        List<Future<Interpreter.EvalResult>> futures = new ArrayList<>();

        for (int i = 0; i < 4; i++) {
            final int idx = i;
            futures.add(pool.submit(() -> {
                Interpreter interp = new Interpreter();
                return interp.evalStrWithOutput(String.format(
                    "(begin (display \"thread%d\") (display \" \") (display \"done%d\") \"ok\")", idx, idx));
            }));
        }

        for (int i = 0; i < 4; i++) {
            Interpreter.EvalResult r = futures.get(i).get(30, TimeUnit.SECONDS);
            assertEquals("ok", r.result());
            assertEquals(String.format("thread%d done%d", i, i), r.output());
        }
        pool.shutdown();
    }

    @Test
    void testL27ConcurrentClosuresAndMutation() throws Exception {
        skipIfBelowLevel();
        ExecutorService pool = Executors.newFixedThreadPool(4);
        List<Future<String>> futures = new ArrayList<>();

        for (int i = 0; i < 4; i++) {
            futures.add(pool.submit(() -> {
                Interpreter interp = new Interpreter();
                return interp.evalStr(
                    "(let ((count 0))" +
                    "  (define (inc!) (set! count (+ count 1)) count)" +
                    "  (inc!) (inc!) (inc!)" +
                    "  count)");
            }));
        }

        for (Future<String> f : futures) {
            assertEquals("3", f.get(30, TimeUnit.SECONDS));
        }
        pool.shutdown();
    }

    @Test
    void testL27ConcurrentStress() throws Exception {
        skipIfBelowLevel();
        ExecutorService pool = Executors.newFixedThreadPool(16);
        List<Future<String>> futures = new ArrayList<>();

        for (int i = 0; i < 16; i++) {
            final int idx = i;
            futures.add(pool.submit(() -> {
                Interpreter interp = new Interpreter();
                return interp.evalStr(String.format(
                    "(let ((x %d)) (define (f n) (if (= n 0) x (f (- n 1)))) (f 100))", idx));
            }));
        }

        for (int i = 0; i < 16; i++) {
            assertEquals(String.valueOf(i), futures.get(i).get(30, TimeUnit.SECONDS));
        }
        pool.shutdown();
    }

    // ===== State isolation tests (sequential) =====

    @Test
    void testL27SequentialStateLeak() throws Exception {
        skipIfBelowLevel();
        Interpreter interp = new Interpreter();
        String r1 = interp.evalStr("(begin (define x 42) x)");
        assertEquals("42", r1);

        // x must not leak to next eval_str call
        Interpreter interp2 = new Interpreter();
        assertThrows(EvalError.class, () -> interp2.evalStr("x"),
            "variable 'x' leaked between independent evalStr calls");
    }

    @Test
    void testL27SequentialOutputLeak() throws Exception {
        skipIfBelowLevel();
        Interpreter i1 = new Interpreter();
        Interpreter.EvalResult r1 = i1.evalStrWithOutput("(display \"aaa\")");
        Interpreter i2 = new Interpreter();
        Interpreter.EvalResult r2 = i2.evalStrWithOutput("(display \"bbb\")");
        assertEquals("aaa", r1.output());
        assertEquals("bbb", r2.output(), "output buffer leaked between evalStrWithOutput calls");
    }

    // ===== Concurrent continuation/macro collision tests =====

    @Test
    void testL27ConcurrentCallccCollision() throws Exception {
        skipIfBelowLevel();
        ExecutorService pool = Executors.newFixedThreadPool(8);
        List<Future<String>> futures = new ArrayList<>();

        for (int i = 0; i < 8; i++) {
            futures.add(pool.submit(() -> {
                Interpreter interp = new Interpreter();
                return interp.evalStr(
                    "(let ((count 0))" +
                    "  (set! count (+ count (call/cc (lambda (k) (k 10)))))" +
                    "  count)");
            }));
        }

        for (Future<String> f : futures) {
            assertEquals("10", f.get(30, TimeUnit.SECONDS));
        }
        pool.shutdown();
    }

    @Test
    void testL27ConcurrentMacroHygiene() throws Exception {
        skipIfBelowLevel();
        ExecutorService pool = Executors.newFixedThreadPool(4);
        List<Future<String>> futures = new ArrayList<>();

        for (int i = 0; i < 4; i++) {
            futures.add(pool.submit(() -> {
                Interpreter interp = new Interpreter();
                return interp.evalStr(
                    "(begin" +
                    "  (define-syntax my-swap!" +
                    "    (syntax-rules ()" +
                    "      ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))" +
                    "  (let ((x 1) (y 2))" +
                    "    (my-swap! x y)" +
                    "    (list x y)))");
            }));
        }

        for (Future<String> f : futures) {
            assertEquals("(2 1)", f.get(30, TimeUnit.SECONDS));
        }
        pool.shutdown();
    }
}
