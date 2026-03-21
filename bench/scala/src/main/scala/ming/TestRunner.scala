package ming

import com.google.gson.{Gson, JsonArray}
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Standalone test runner — no test framework needed. Reads tests.json + fixtures, calls evalStr/evalStrWithOutput,
  * compares results. Exit 0 = all pass, exit 1 = failures found.
  *
  * Usage: java -jar ming.jar [--level NN] [--bench-dir /path/to/bench]
  */
object TestRunner:

  def main(args: Array[String]): Unit =
    val parsedArgs  = parseArgs(args.toList)
    val benchDir    = Path.of(parsedArgs.getOrElse("bench-dir", sys.props.getOrElse("bench.dir", ".")))
    val levelFilter = parsedArgs.get("level").map(_.toInt)
    val benchLevel  = parsedArgs.get("bench-level").map(_.toInt).getOrElse(levelFilter.getOrElse(0))
    val tagFilter   = parsedArgs.get("include-tags") // e.g. "l01"

    val testsJson   = Files.readString(benchDir.resolve("tests.json"))
    val testCases   = new Gson().fromJson(testsJson, classOf[JsonArray])
    val fixturesDir = benchDir.resolve("fixtures")

    val results = testCases
      .iterator()
      .asScala
      .flatMap { elem =>
        val tc      = elem.getAsJsonObject
        val name    = tc.get("name").getAsString
        val level   = tc.get("level").getAsInt
        val fixture = tc.get("fixture").getAsString
        val kind    = tc.get("kind").getAsString

        // Level filtering
        if benchLevel > 0 && level > benchLevel then None
        else if tc.has("deprecated_after") && benchLevel > 0 &&
          benchLevel > tc.get("deprecated_after").getAsInt
        then None
        // Tag filtering (e.g. --include-tags=l01)
        else if tagFilter.exists(tag => f"l$level%02d" != tag) then None
        else
          val input = Files.readString(fixturesDir.resolve(fixture)).trim
          Some(runTest(name, kind, input, tc))
      }
      .toList

    val passed = results.count(_.passed)
    val failed = results.count(!_.passed)
    val total  = results.size

    if total == 0 then
      System.err.println("No tests matched the filter.")
      sys.exit(1)

    results.filterNot(_.passed).foreach { r =>
      System.err.println(s"FAIL ${r.name}: ${r.message}")
    }

    println(s"test result: ${if failed == 0 then "ok" else "FAILED"}. $passed passed; $failed failed; $total total")
    sys.exit(if failed == 0 then 0 else 1)

  private case class TestResult(name: String, passed: Boolean, message: String)

  private def runTest(name: String, kind: String, input: String, tc: com.google.gson.JsonObject): TestResult =
    try
      kind match
        case "eval_str_ok" =>
          val expected = tc.get("expected").getAsString
          val result   = Evaluator.evalStr(input)
          if result == expected then TestResult(name, passed = true, "")
          else TestResult(name, passed = false, s"expected '$expected', got '$result'")

        case "eval_str_err" =>
          try
            val result = Evaluator.evalStr(input)
            TestResult(name, passed = false, s"expected error, got '$result'")
          catch case _: EvalError => TestResult(name, passed = true, "")

        case "eval_str_err_with_position" =>
          try
            val result = Evaluator.evalStr(input)
            TestResult(name, passed = false, s"expected error with position, got '$result'")
          catch
            case e: EvalError =>
              if e.getMessage.matches(".*\\d+:\\d+.*") then TestResult(name, passed = true, "")
              else TestResult(name, passed = false, s"error lacks position info: ${e.getMessage}")

        case "eval_str_with_output" =>
          val expectedOutput = tc.get("expected_output").getAsString
          val (_, output)    = Evaluator.evalStrWithOutput(input)
          if output == expectedOutput then TestResult(name, passed = true, "")
          else TestResult(name, passed = false, s"expected output '$expectedOutput', got '$output'")

        case other =>
          TestResult(name, passed = false, s"unknown test kind: $other")
    catch
      case e: Exception =>
        TestResult(name, passed = false, s"${e.getClass.getSimpleName}: ${e.getMessage}")

  private def parseArgs(args: List[String]): Map[String, String] = args match
    case Nil                              => Map.empty
    case "--level" :: value :: rest       => parseArgs(rest) + ("level"       -> value) + ("bench-level" -> value)
    case "--bench-dir" :: value :: rest   => parseArgs(rest) + ("bench-dir"   -> value)
    case "--bench-level" :: value :: rest => parseArgs(rest) + ("bench-level" -> value)
    case arg :: rest if arg.startsWith("--include-tags=") =>
      parseArgs(rest) + ("include-tags" -> arg.stripPrefix("--include-tags="))
    case _ :: rest => parseArgs(rest)
