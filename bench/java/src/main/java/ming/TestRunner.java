package ming;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class TestRunner {

    /**
     * Run a surprise-level test class by invoking each testXxx method directly
     * (skipping main() which calls System.exit). Reads the static passed/failed counters.
     */
    private static int runSurpriseLevel(String className) {
        try {
            Class<?> cls = Class.forName(className);

            // Reset counters
            var passedField = cls.getDeclaredField("passed");
            var failedField = cls.getDeclaredField("failed");
            passedField.setInt(null, 0);
            failedField.setInt(null, 0);

            // Invoke all static test methods (those starting with "test")
            for (var method : cls.getDeclaredMethods()) {
                if (method.getName().startsWith("test") && java.lang.reflect.Modifier.isStatic(method.getModifiers())) {
                    method.setAccessible(true);
                    try {
                        method.invoke(null);
                    } catch (java.lang.reflect.InvocationTargetException e) {
                        System.out.println("FAIL " + method.getName() + ": " + e.getCause());
                        failedField.setInt(null, failedField.getInt(null) + 1);
                    }
                }
            }

            int p = passedField.getInt(null);
            int f = failedField.getInt(null);
            System.out.println(p + " passed, " + f + " failed out of " + (p + f) + " tests");
            return f;
        } catch (ClassNotFoundException ignored) {
            return 0; // test class not present — skip
        } catch (Exception e) {
            System.out.println("FAIL " + className + ": " + e);
            return 1;
        }
    }

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

        // Delegate to standalone surprise-level tests, calling methods directly
        // to avoid System.exit() in their main() killing the JVM prematurely.
        if (benchLevel == 0 || benchLevel >= 27) {
            failed += runSurpriseLevel("ming.L27Tests");
        }
        if (benchLevel == 0 || benchLevel >= 28) {
            failed += runSurpriseLevel("ming.L28Tests");
        }

        System.exit(failed > 0 ? 1 : 0);
    }
}
