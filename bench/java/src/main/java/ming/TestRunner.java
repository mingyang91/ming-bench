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

        // Run L27+ standalone tests if level includes them
        boolean extraFailed = false;
        if (benchLevel == 0 || benchLevel >= 27) {
            extraFailed |= runStandaloneTests("ming.L27Tests");
        }
        if (benchLevel == 0 || benchLevel >= 28) {
            extraFailed |= runStandaloneTests("ming.L28Tests");
        }

        System.exit((failed > 0 || extraFailed) ? 1 : 0);
    }

    private static boolean runStandaloneTests(String className) {
        try {
            Class<?> cls = Class.forName(className);
            // Reset pass/fail counters
            var passedField = cls.getDeclaredField("passed");
            var failedField = cls.getDeclaredField("failed");
            passedField.setInt(null, 0);
            failedField.setInt(null, 0);
            // Invoke each test method (static void testXxx) directly, bypassing main/System.exit
            for (var m : cls.getDeclaredMethods()) {
                if (m.getName().startsWith("test") && m.getParameterCount() == 0) {
                    m.setAccessible(true);
                    m.invoke(null);
                }
            }
            int p = passedField.getInt(null);
            int f = failedField.getInt(null);
            System.out.println(p + " passed, " + f + " failed out of " + (p + f) + " tests");
            return f > 0;
        } catch (ClassNotFoundException e) {
            return false; // not present — skip
        } catch (Exception e) {
            System.out.println("FAIL " + className + ": " + e);
            return true; // failure
        }
    }
}
