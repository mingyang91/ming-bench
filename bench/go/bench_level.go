package ming

import (
	"os"
	"strconv"
)

func currentBenchLevel() int {
	levelText := os.Getenv("BENCH_LEVEL")
	if levelText == "" {
		return 15
	}

	level, err := strconv.Atoi(levelText)
	if err != nil {
		return 15
	}
	return level
}

func stringsImmutableEnabled() bool {
	return currentBenchLevel() >= 15
}
