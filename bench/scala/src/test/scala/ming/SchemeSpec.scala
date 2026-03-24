package ming

import com.google.gson.{Gson, JsonArray, JsonObject}
import munit.{FunSuite, Tag}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

class SchemeSpec extends FunSuite:

  private val benchDir: Path =
    // Mill passes bench dir via system property; fall back to parent of user.dir
    val fromProp = Option(System.getProperty("bench.dir")).map(Path.of(_))
    fromProp.filter(p => Files.exists(p.resolve("tests.json"))).getOrElse {
      val scalaDir = Path.of(System.getProperty("user.dir"))
      val parent   = scalaDir.getParent
      if parent != null && Files.exists(parent.resolve("tests.json")) then parent
      else scalaDir
    }

  private val fixturesDir: Path = benchDir.resolve("fixtures")

  private val testCases: JsonArray =
    val json = Files.readString(benchDir.resolve("tests.json"))
    new Gson().fromJson(json, classOf[JsonArray])

  private val benchLevel: Int =
    // Mill passes BENCH_LEVEL via system property (env vars don't propagate to forked JVM)
    val fromProp = Option(System.getProperty("bench.level")).getOrElse("")
    val fromEnv  = Option(System.getenv("BENCH_LEVEL")).getOrElse("")
    val raw      = if fromProp.nonEmpty then fromProp else fromEnv
    raw.toIntOption.getOrElse(0)

  locally {
    for elem <- testCases.iterator().asScala do
      val tc      = elem.getAsJsonObject
      val name    = tc.get("name").getAsString
      val level   = tc.get("level").getAsInt
      val fixture = tc.get("fixture").getAsString
      val kind    = tc.get("kind").getAsString

      // Skip tests above the requested level
      if benchLevel > 0 && level > benchLevel then ()
      // Deprecation: skip if current bench level exceeds deprecated_after
      else if tc.has("deprecated_after") && benchLevel > 0 &&
        benchLevel > tc.get("deprecated_after").getAsInt
      then ()
      else
        val levelTag = Tag(f"l$level%02d")

        test(name.tag(levelTag)) {
          val input = Files.readString(fixturesDir.resolve(fixture))

          kind match
            case "eval_str_ok" =>
              val expected = tc.get("expected").getAsString
              val result   = Evaluator.evalStr(input)
              assertEquals(result, expected, s"$name: wrong result")

            case "eval_str_err" =>
              intercept[EvalError] {
                Evaluator.evalStr(input)
              }

            case "eval_str_err_with_position" =>
              val err = intercept[EvalError] {
                Evaluator.evalStr(input)
              }
              assert(
                err.getMessage.matches(".*\\d+:\\d+.*"),
                s"$name: error message should contain line:col position, got: ${err.getMessage}"
              )

            case "eval_str_with_output" =>
              val expectedOutput = tc.get("expected_output").getAsString
              val (_, output)    = Evaluator.evalStrWithOutput(input)
              assertEquals(output, expectedOutput, s"$name: wrong output")

            case other =>
              fail(s"Unknown test kind: $other")
        }
  }
