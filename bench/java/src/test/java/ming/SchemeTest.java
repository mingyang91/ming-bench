package ming;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;

import static org.junit.jupiter.api.Assertions.*;

public class SchemeTest {

    private static final JsonArray TEST_CASES;
    private static final Path FIXTURES_DIR;

    static {
        // tests.json and fixtures/ are at ../tests.json and ../fixtures/ relative to bench/java/
        Path javaDir = Path.of(System.getProperty("user.dir"));
        Path benchDir = javaDir.getParent();
        if (!Files.exists(benchDir.resolve("tests.json"))) {
            // Fallback: maybe we're already in bench/
            benchDir = javaDir;
        }
        FIXTURES_DIR = benchDir.resolve("fixtures");

        Path testsJson = benchDir.resolve("tests.json");
        try {
            String json = Files.readString(testsJson);
            TEST_CASES = new Gson().fromJson(json, JsonArray.class);
        } catch (IOException e) {
            throw new RuntimeException("Failed to load tests.json from " + testsJson, e);
        }
    }

    @TestFactory
    Collection<DynamicTest> schemeTests() {
        String levelEnv = System.getProperty("bench.level", "");
        if (levelEnv == null || levelEnv.isEmpty()) {
            levelEnv = System.getenv("BENCH_LEVEL");
        }
        int benchLevel = 0;
        if (levelEnv != null && !levelEnv.isEmpty()) {
            try {
                benchLevel = Integer.parseInt(levelEnv);
            } catch (NumberFormatException ignored) {
            }
        }

        String testFilter = System.getProperty("test.filter", "");

        Collection<DynamicTest> tests = new ArrayList<>();

        for (JsonElement elem : TEST_CASES) {
            JsonObject tc = elem.getAsJsonObject();
            String name = tc.get("name").getAsString();
            int level = tc.get("level").getAsInt();
            String fixture = tc.get("fixture").getAsString();
            String kind = tc.get("kind").getAsString();

            // Level-based filtering: skip tests above the requested level
            if (benchLevel > 0 && level > benchLevel) {
                continue;
            }

            // Test name filter (e.g., "test_l01" filters to level 1 tests)
            String testName = "test_" + name;
            if (testFilter != null && !testFilter.isEmpty() && !testName.contains(testFilter)) {
                continue;
            }

            // Deprecation: skip if current bench level exceeds deprecated_after
            if (tc.has("deprecated_after") && benchLevel > 0) {
                int deprecatedAfter = tc.get("deprecated_after").getAsInt();
                if (benchLevel > deprecatedAfter) {
                    continue;
                }
            }

            tests.add(DynamicTest.dynamicTest(testName, () -> {
                String input = Files.readString(FIXTURES_DIR.resolve(fixture));
                Evaluator evaluator = new Evaluator();

                switch (kind) {
                    case "eval_str_ok" -> {
                        String expected = tc.get("expected").getAsString();
                        String result = evaluator.evalStr(input);
                        assertEquals(expected, result, testName + ": wrong result");
                    }
                    case "eval_str_err" -> {
                        assertThrows(EvalError.class, () -> evaluator.evalStr(input),
                                testName + ": expected EvalError");
                    }
                    case "eval_str_err_with_position" -> {
                        EvalError err = assertThrows(EvalError.class,
                                () -> evaluator.evalStr(input),
                                testName + ": expected EvalError");
                        assertTrue(err.getMessage().matches(".*\\d+:\\d+.*"),
                                testName + ": error message should contain line:col position, got: "
                                        + err.getMessage());
                    }
                    case "eval_str_with_output" -> {
                        String expectedOutput = tc.get("expected_output").getAsString();
                        EvalResult result = evaluator.evalStrWithOutput(input);
                        assertEquals(expectedOutput, result.output(),
                                testName + ": wrong output");
                    }
                    default -> fail("Unknown test kind: " + kind);
                }
            }));
        }

        return tests;
    }
}
