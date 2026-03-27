package ming;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class TestRunner {

    public static void main(String[] args) {
        if (args.length < 1) {
            System.err.println("Usage: java ming.TestRunner <level|all>");
            System.exit(2);
        }

        String levelArg = args[0];
        int benchLevel = 0;
        if (!levelArg.equals("all")) {
            try {
                benchLevel = Integer.parseInt(levelArg);
            } catch (NumberFormatException e) {
                System.err.println("Invalid level: " + levelArg);
                System.exit(2);
            }
        }
        System.setProperty("bench.level", Integer.toString(benchLevel));

        String testsJsonPath = System.getenv("TESTS_JSON");
        if (testsJsonPath == null || testsJsonPath.isEmpty()) {
            testsJsonPath = "../tests.json";
        }
        String fixturesDir = System.getenv("FIXTURES_DIR");
        if (fixturesDir == null || fixturesDir.isEmpty()) {
            fixturesDir = "../fixtures";
        }

        Path testsJson = Path.of(testsJsonPath);
        Path fixtures = Path.of(fixturesDir);

        JsonArray testCases;
        try {
            String json = Files.readString(testsJson);
            testCases = new Gson().fromJson(json, JsonArray.class);
        } catch (IOException e) {
            System.err.println("Failed to load tests.json from " + testsJson + ": " + e.getMessage());
            System.exit(2);
            return;
        }

        int passed = 0;
        int failed = 0;
        int total = 0;

        for (JsonElement elem : testCases) {
            JsonObject tc = elem.getAsJsonObject();
            String name = tc.get("name").getAsString();
            int level = tc.get("level").getAsInt();
            String fixture = tc.get("fixture").getAsString();
            String kind = tc.get("kind").getAsString();

            if (benchLevel > 0 && level > benchLevel) {
                continue;
            }

            if (tc.has("deprecated_after") && benchLevel > 0) {
                int deprecatedAfter = tc.get("deprecated_after").getAsInt();
                if (benchLevel > deprecatedAfter) {
                    continue;
                }
            }

            String testName = "test_" + name;
            total++;

            try {
                String input = Files.readString(fixtures.resolve(fixture));
                Evaluator evaluator = new Evaluator();

                switch (kind) {
                    case "eval_str_ok" -> {
                        String expected = tc.get("expected").getAsString();
                        String result = evaluator.evalStr(input);
                        if (expected.equals(result)) {
                            System.out.println("PASS " + testName);
                            passed++;
                        } else {
                            System.out.println("FAIL " + testName + ": expected " + expected + " got " + result);
                            failed++;
                        }
                    }
                    case "eval_str_err" -> {
                        try {
                            String result = evaluator.evalStr(input);
                            System.out.println("FAIL " + testName + ": expected EvalError but got " + result);
                            failed++;
                        } catch (EvalError e) {
                            System.out.println("PASS " + testName);
                            passed++;
                        }
                    }
                    case "eval_str_err_with_position" -> {
                        try {
                            String result = evaluator.evalStr(input);
                            System.out.println("FAIL " + testName + ": expected EvalError but got " + result);
                            failed++;
                        } catch (EvalError e) {
                            if (e.getMessage() != null && e.getMessage().matches(".*\\d+:\\d+.*")) {
                                System.out.println("PASS " + testName);
                                passed++;
                            } else {
                                System.out.println("FAIL " + testName + ": error message should contain line:col position, got: " + e.getMessage());
                                failed++;
                            }
                        }
                    }
                    case "eval_str_with_output" -> {
                        String expectedOutput = tc.get("expected_output").getAsString();
                        EvalResult result = evaluator.evalStrWithOutput(input);
                        if (expectedOutput.equals(result.output())) {
                            System.out.println("PASS " + testName);
                            passed++;
                        } else {
                            System.out.println("FAIL " + testName + ": expected output " + expectedOutput + " got " + result.output());
                            failed++;
                        }
                    }
                    default -> {
                        System.out.println("FAIL " + testName + ": unknown test kind: " + kind);
                        failed++;
                    }
                }
            } catch (EvalError e) {
                System.out.println("FAIL " + testName + ": unexpected EvalError: " + e.getMessage());
                failed++;
            } catch (IOException e) {
                System.out.println("FAIL " + testName + ": failed to read fixture: " + e.getMessage());
                failed++;
            } catch (Exception e) {
                System.out.println("FAIL " + testName + ": " + e.getClass().getSimpleName() + ": " + e.getMessage());
                failed++;
            }
        }

        System.out.println(passed + " passed, " + failed + " failed out of " + total + " tests");
        System.exit(failed > 0 ? 1 : 0);
    }
}
