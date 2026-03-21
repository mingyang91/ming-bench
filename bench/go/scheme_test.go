package ming

import (
	"encoding/json"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"testing"
)

// testEntry mirrors the JSON structure in tests.json.
type testEntry struct {
	Name            string `json:"name"`
	Level           int    `json:"level"`
	Fixture         string `json:"fixture"`
	Kind            string `json:"kind"`
	Expected        string `json:"expected"`
	ExpectedOutput  string `json:"expected_output"`
	DeprecatedAfter *int   `json:"deprecated_after"`
}

var allTests []testEntry
var fixtureCache = map[string]string{}

func TestMain(m *testing.M) {
	data, err := os.ReadFile("../tests.json")
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to read tests.json: %v\n", err)
		os.Exit(1)
	}
	if err := json.Unmarshal(data, &allTests); err != nil {
		fmt.Fprintf(os.Stderr, "failed to parse tests.json: %v\n", err)
		os.Exit(1)
	}

	// Pre-load all fixture files.
	for _, tc := range allTests {
		if _, ok := fixtureCache[tc.Fixture]; ok {
			continue
		}
		content, err := os.ReadFile("../fixtures/" + tc.Fixture)
		if err != nil {
			fmt.Fprintf(os.Stderr, "failed to read fixture %s: %v\n", tc.Fixture, err)
			os.Exit(1)
		}
		fixtureCache[tc.Fixture] = string(content)
	}

	os.Exit(m.Run())
}

// benchLevel returns the BENCH_LEVEL env var as an int, or 0 if unset.
func benchLevel() int {
	s := os.Getenv("BENCH_LEVEL")
	if s == "" {
		return 0
	}
	n, err := strconv.Atoi(s)
	if err != nil {
		return 0
	}
	return n
}

// hasDigitColon matches a digit followed by a colon, used to verify
// position info in error messages.
var hasDigitColon = regexp.MustCompile(`\d:`)

// runLevelTests runs all test entries for the given level as subtests.
// Subtest names use the suffix after the "lNN_" prefix, producing
// top-level names like TestL01/integer, TestL01/add, etc.
func runLevelTests(t *testing.T, level int) {
	t.Helper()
	bl := benchLevel()

	if bl > 0 && level > bl {
		t.Skipf("level %d > BENCH_LEVEL %d", level, bl)
	}

	prefix := fmt.Sprintf("l%02d_", level)

	for _, tc := range allTests {
		if tc.Level != level {
			continue
		}
		tc := tc // capture

		// Strip the "lNN_" prefix to get the subtest name.
		subName := tc.Name[len(prefix):]

		t.Run(subName, func(t *testing.T) {
			// Skip deprecated tests: if deprecated_after is set and
			// BENCH_LEVEL exceeds it, the test is no longer relevant.
			if tc.DeprecatedAfter != nil && bl > *tc.DeprecatedAfter {
				t.Skipf("deprecated after level %d (BENCH_LEVEL=%d)", *tc.DeprecatedAfter, bl)
			}

			input := fixtureCache[tc.Fixture]

			switch tc.Kind {
			case "eval_str_ok":
				got, err := EvalStr(input)
				if err != nil {
					t.Fatalf("expected success, got error: %v", err)
				}
				if got != tc.Expected {
					t.Fatalf("expected %q, got %q", tc.Expected, got)
				}

			case "eval_str_err":
				_, err := EvalStr(input)
				if err == nil {
					t.Fatalf("expected error, got success")
				}

			case "eval_str_err_with_position":
				_, err := EvalStr(input)
				if err == nil {
					t.Fatalf("expected error, got success")
				}
				if !hasDigitColon.MatchString(err.Error()) {
					t.Fatalf("error %q should contain a digit followed by colon (position info)", err.Error())
				}

			case "eval_str_with_output":
				_, output, err := EvalStrWithOutput(input)
				if err != nil {
					t.Fatalf("expected success, got error: %v", err)
				}
				if output != tc.ExpectedOutput {
					t.Fatalf("expected output %q, got %q", tc.ExpectedOutput, output)
				}

			default:
				t.Fatalf("unknown test kind: %s", tc.Kind)
			}
		})
	}
}

func TestL01(t *testing.T) { runLevelTests(t, 1) }
func TestL02(t *testing.T) { runLevelTests(t, 2) }
func TestL03(t *testing.T) { runLevelTests(t, 3) }
func TestL04(t *testing.T) { runLevelTests(t, 4) }
func TestL05(t *testing.T) { runLevelTests(t, 5) }
func TestL06(t *testing.T) { runLevelTests(t, 6) }
func TestL07(t *testing.T) { runLevelTests(t, 7) }
func TestL08(t *testing.T) { runLevelTests(t, 8) }
func TestL09(t *testing.T) { runLevelTests(t, 9) }
func TestL10(t *testing.T) { runLevelTests(t, 10) }
func TestL11(t *testing.T) { runLevelTests(t, 11) }
func TestL12(t *testing.T) { runLevelTests(t, 12) }
func TestL13(t *testing.T) { runLevelTests(t, 13) }
func TestL14(t *testing.T) { runLevelTests(t, 14) }
func TestL15(t *testing.T) { runLevelTests(t, 15) }
func TestL16(t *testing.T) { runLevelTests(t, 16) }
func TestL17(t *testing.T) { runLevelTests(t, 17) }
func TestL18(t *testing.T) { runLevelTests(t, 18) }
func TestL19(t *testing.T) { runLevelTests(t, 19) }
func TestL20(t *testing.T) { runLevelTests(t, 20) }
func TestL21(t *testing.T) { runLevelTests(t, 21) }
func TestL22(t *testing.T) { runLevelTests(t, 22) }
func TestL23(t *testing.T) { runLevelTests(t, 23) }
