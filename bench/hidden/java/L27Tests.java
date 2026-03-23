package ming;

/**
 * Level 27: Step-Limited Evaluation — standalone runner.
 * Tests eval_str_with_limit(input, maxSteps).
 * Usage: java ming.L27Tests
 */
public class L27Tests {
    static int passed = 0;
    static int failed = 0;

    public static void main(String[] args) {
        try {
            testStepLimitNormal();
            testStepLimitLoopWithinBudget();
            testStepLimitInfiniteLoop();
            testStepLimitExceeded();
            testStepLimitFactorial();
            testNormalEvalUnaffected();
        } catch (Exception e) {
            System.out.println("FAIL unexpected: " + e);
            failed++;
        }
        System.out.println(passed + " passed, " + failed + " failed out of " + (passed + failed) + " tests");
        System.exit(failed > 0 ? 1 : 0);
    }

    static void pass(String name) { System.out.println("PASS " + name); passed++; }
    static void fail(String name, String msg) { System.out.println("FAIL " + name + ": " + msg); failed++; }

    static void testStepLimitNormal() {
        try {
            Evaluator e = new Evaluator();
            String r = e.evalStrWithLimit("(+ 1 2)", 1000);
            if ("3".equals(r)) pass("l27_step_limit_normal");
            else fail("l27_step_limit_normal", "expected 3, got " + r);
        } catch (Exception e) {
            fail("l27_step_limit_normal", e.toString());
        }
    }

    static void testStepLimitLoopWithinBudget() {
        try {
            Evaluator e = new Evaluator();
            String r = e.evalStrWithLimit(
                "(let loop ((n 50)) (if (= n 0) 'done (loop (- n 1))))", 10000);
            if ("done".equals(r)) pass("l27_step_limit_loop_within_budget");
            else fail("l27_step_limit_loop_within_budget", "expected done, got " + r);
        } catch (Exception e) {
            fail("l27_step_limit_loop_within_budget", e.toString());
        }
    }

    static void testStepLimitInfiniteLoop() {
        try {
            Evaluator e = new Evaluator();
            e.evalStrWithLimit("(let loop () (loop))", 1000);
            fail("l27_step_limit_infinite_loop", "infinite loop should hit step limit");
        } catch (EvalError e) {
            pass("l27_step_limit_infinite_loop");
        } catch (Exception e) {
            fail("l27_step_limit_infinite_loop", "wrong exception: " + e);
        }
    }

    static void testStepLimitExceeded() {
        try {
            Evaluator e = new Evaluator();
            e.evalStrWithLimit(
                "(let loop ((n 1000)) (if (= n 0) 'done (loop (- n 1))))", 50);
            fail("l27_step_limit_exceeded", "1000-iter loop should exceed 50-step budget");
        } catch (EvalError e) {
            pass("l27_step_limit_exceeded");
        } catch (Exception e) {
            fail("l27_step_limit_exceeded", "wrong exception: " + e);
        }
    }

    static void testStepLimitFactorial() {
        try {
            Evaluator e = new Evaluator();
            String r = e.evalStrWithLimit(
                "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 10)", 10000);
            if ("3628800".equals(r)) pass("l27_step_limit_factorial");
            else fail("l27_step_limit_factorial", "expected 3628800, got " + r);
        } catch (Exception e) {
            fail("l27_step_limit_factorial", e.toString());
        }
    }

    static void testNormalEvalUnaffected() {
        try {
            Evaluator e = new Evaluator();
            String r = e.evalStr("(let loop ((n 100000)) (if (= n 0) 'done (loop (- n 1))))");
            if ("done".equals(r)) pass("l27_normal_eval_unaffected");
            else fail("l27_normal_eval_unaffected", "expected done, got " + r);
        } catch (Exception e) {
            fail("l27_normal_eval_unaffected", e.toString());
        }
    }
}
