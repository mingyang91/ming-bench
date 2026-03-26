package ming

// Level 27: Step-Limited Evaluation
class Level27Spec extends munit.FunSuite:

  private def benchLevel: Int =
    sys.env.getOrElse("BENCH_LEVEL", sys.props.getOrElse("bench.level", "0")).toIntOption.getOrElse(0)

  private def skipIfBelow(): Unit =
    if benchLevel > 0 && benchLevel < 27 then assume(false, "Skipping: BENCH_LEVEL < 27")

  test("l27_step_limit_normal") {
    skipIfBelow()
    assertEquals(Evaluator.evalStrWithLimit("(+ 1 2)", 1000), "3")
  }

  test("l27_step_limit_loop_within_budget") {
    skipIfBelow()
    assertEquals(
      Evaluator.evalStrWithLimit("(let loop ((n 50)) (if (= n 0) 'done (loop (- n 1))))", 10000),
      "done")
  }

  test("l27_step_limit_infinite_loop") {
    skipIfBelow()
    intercept[EvalError] { Evaluator.evalStrWithLimit("(let loop () (loop))", 1000) }
  }

  test("l27_step_limit_exceeded") {
    skipIfBelow()
    intercept[EvalError] {
      Evaluator.evalStrWithLimit("(let loop ((n 1000)) (if (= n 0) 'done (loop (- n 1))))", 50)
    }
  }

  test("l27_step_limit_factorial") {
    skipIfBelow()
    assertEquals(
      Evaluator.evalStrWithLimit("(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 10)", 10000),
      "3628800")
  }

  test("l27_normal_eval_unaffected") {
    skipIfBelow()
    assertEquals(
      Evaluator.evalStr("(let loop ((n 100000)) (if (= n 0) 'done (loop (- n 1))))"),
      "done")
  }
