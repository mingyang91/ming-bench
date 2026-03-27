package ming

import com.google.gson.{Gson, JsonArray, JsonObject}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Standalone test runner — no test framework needed.
  *
  * Reads tests.json + fixtures, calls evalStr/evalStrWithOutput, compares results. Exit 0 = all pass, exit 1 = failures
  * found.
  *
  * Usage: java -jar test.jar 01 # run level 1 java -jar test.jar all # run all levels
  *
  * Env vars: TESTS_JSON — path to tests.json (default ../tests.json) FIXTURES_DIR — path to fixtures dir (default
  * ../fixtures)
  */
object TestRunner:

  private case class TestResult(name: String, passed: Boolean, message: String)

  def main(args: Array[String]): Unit =
    val levelArg = args.headOption.getOrElse("all")
    val benchLevel = levelArg match
      case "all" => 0
      case s     => s.toIntOption.getOrElse(0)
    System.setProperty("bench.level", benchLevel.toString)

    val testsJsonPath = Path.of(
      Option(System.getenv("TESTS_JSON")).getOrElse("../tests.json")
    )
    val fixturesDir = Path.of(
      Option(System.getenv("FIXTURES_DIR")).getOrElse("../fixtures")
    )

    val testCases: JsonArray =
      val json = Files.readString(testsJsonPath)
      Gson().fromJson(json, classOf[JsonArray])

    val results: List[TestResult] =
      testCases.iterator().asScala.toList.flatMap { elem =>
        val tc      = elem.getAsJsonObject
        val name    = tc.get("name").getAsString
        val level   = tc.get("level").getAsInt
        val fixture = tc.get("fixture").getAsString
        val kind    = tc.get("kind").getAsString

        val skip =
          (benchLevel > 0 && level > benchLevel) ||
            (tc.has("deprecated_after") && benchLevel > 0 &&
              benchLevel > tc.get("deprecated_after").getAsInt)

        if skip then None
        else
          val input = Files.readString(fixturesDir.resolve(fixture))
          Some(runTest(name, kind, input, tc))
      }

    results.foreach { r =>
      if r.passed then println(s"PASS ${r.name}")
      else println(s"FAIL ${r.name}: ${r.message}")
    }

    val passed = results.count(_.passed)
    val failed = results.count(!_.passed)
    val total  = results.size
    println(s"$passed passed, $failed failed out of $total tests")

    if failed > 0 then System.exit(1)

  private def runTest(
    name: String,
    kind: String,
    input: String,
    tc: JsonObject
  ): TestResult =
    try
      kind match
        case "eval_str_ok" =>
          val expected = tc.get("expected").getAsString
          val result   = evalInput(input, tc)
          if result == expected then TestResult(name, passed = true, "")
          else TestResult(name, passed = false, s"expected $expected got $result")

        case "eval_str_err" =>
          try
            val result = evalInput(input, tc)
            TestResult(name, passed = false, s"expected EvalError but got $result")
          catch case _: EvalError => TestResult(name, passed = true, "")

        case "eval_str_err_with_position" =>
          try
            val result = evalInput(input, tc)
            TestResult(name, passed = false, s"expected EvalError but got $result")
          catch
            case e: EvalError =>
              if e.getMessage.matches(".*\\d+:\\d+.*") then TestResult(name, passed = true, "")
              else
                TestResult(
                  name,
                  passed = false,
                  s"error message should contain line:col position, got: ${e.getMessage}"
                )

        case "eval_str_with_output" =>
          val expectedOutput = tc.get("expected_output").getAsString
          val (_, output)    = Evaluator.evalStrWithOutput(input)
          if output == expectedOutput then TestResult(name, passed = true, "")
          else TestResult(name, passed = false, s"expected output $expectedOutput got $output")

        case other =>
          TestResult(name, passed = false, s"unknown test kind: $other")
    catch
      case e: Exception =>
        TestResult(name, passed = false, s"${e.getClass.getSimpleName}: ${e.getMessage}")

  private def evalInput(input: String, tc: JsonObject): String =
    Option.when(tc.has("max_steps"))(tc.get("max_steps").getAsInt) match
      case Some(maxSteps) =>
        Evaluator.evalStrWithLimit(input, maxSteps)
      case None =>
        Evaluator.evalStr(input)
