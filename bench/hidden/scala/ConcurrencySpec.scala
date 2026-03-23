package ming

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

// Level 27: Concurrent Evaluation
// evalStr must be safe for concurrent use from multiple threads.
class ConcurrencySpec extends munit.FunSuite:

  private def benchLevel: Int =
    sys.env.getOrElse("BENCH_LEVEL", sys.props.getOrElse("bench.level", "0")).toIntOption.getOrElse(0)

  private def skipIfBelow(): Unit =
    val level = benchLevel
    if level > 0 && level < 27 then
      assume(false, "Skipping: BENCH_LEVEL < 27")

  test("l27_concurrent_independent_eval") {
    skipIfBelow()
    val futures = (0 until 8).map { i =>
      Future {
        val program = s"(let loop ((n 1000) (acc 0)) (if (= n 0) acc (loop (- n 1) (+ acc $i))))"
        Evaluator.evalStr(program)
      }
    }

    val results = Await.result(Future.sequence(futures), 30.seconds)
    results.zipWithIndex.foreach { case (result, i) =>
      assertEquals(result, (i * 1000).toString)
    }
  }

  test("l27_concurrent_output_isolation") {
    skipIfBelow()
    val futures = (0 until 4).map { i =>
      Future {
        val program = s"""(begin (display "thread$i") (display " ") (display "done$i") "ok")"""
        Evaluator.evalStrWithOutput(program)
      }
    }

    val results = Await.result(Future.sequence(futures), 30.seconds)
    results.zipWithIndex.foreach { case ((result, output), i) =>
      assertEquals(result, "ok")
      assertEquals(output, s"thread$i done$i")
    }
  }

  test("l27_concurrent_closures_and_mutation") {
    skipIfBelow()
    val futures = (0 until 4).map { _ =>
      Future {
        Evaluator.evalStr(
          """(let ((count 0))
            |  (define (inc!) (set! count (+ count 1)) count)
            |  (inc!) (inc!) (inc!)
            |  count)""".stripMargin)
      }
    }

    val results = Await.result(Future.sequence(futures), 30.seconds)
    results.foreach(r => assertEquals(r, "3"))
  }

  test("l27_concurrent_stress") {
    skipIfBelow()
    val futures = (0 until 16).map { i =>
      Future {
        val program = s"(let ((x $i)) (define (f n) (if (= n 0) x (f (- n 1)))) (f 100))"
        Evaluator.evalStr(program)
      }
    }

    val results = Await.result(Future.sequence(futures), 30.seconds)
    results.zipWithIndex.foreach { case (result, i) =>
      assertEquals(result, i.toString)
    }
  }

  // ===== State isolation tests (sequential) =====

  test("l27_sequential_state_leak") {
    skipIfBelow()
    val r1 = Evaluator.evalStr("(begin (define x 42) x)")
    assertEquals(r1, "42")

    // x must not leak to next evalStr call
    val caught = intercept[EvalError] { Evaluator.evalStr("x") }
    assert(caught != null, "variable 'x' leaked between independent evalStr calls")
  }

  test("l27_sequential_output_leak") {
    skipIfBelow()
    val (_, out1) = Evaluator.evalStrWithOutput("""(display "aaa")""")
    val (_, out2) = Evaluator.evalStrWithOutput("""(display "bbb")""")
    assertEquals(out1, "aaa")
    assertEquals(out2, "bbb", "output buffer leaked between evalStrWithOutput calls")
  }

  // ===== Concurrent continuation/macro collision tests =====

  test("l27_concurrent_callcc_collision") {
    skipIfBelow()
    val futures = (0 until 8).map { _ =>
      Future {
        Evaluator.evalStr(
          """(let ((count 0))
            |  (set! count (+ count (call/cc (lambda (k) (k 10)))))
            |  count)""".stripMargin)
      }
    }

    val results = Await.result(Future.sequence(futures), 30.seconds)
    results.foreach(r => assertEquals(r, "10"))
  }

  test("l27_concurrent_macro_hygiene") {
    skipIfBelow()
    val futures = (0 until 4).map { _ =>
      Future {
        Evaluator.evalStr(
          """(begin
            |  (define-syntax my-swap!
            |    (syntax-rules ()
            |      ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))
            |  (let ((x 1) (y 2))
            |    (my-swap! x y)
            |    (list x y)))""".stripMargin)
      }
    }

    val results = Await.result(Future.sequence(futures), 30.seconds)
    results.foreach(r => assertEquals(r, "(2 1)"))
  }
