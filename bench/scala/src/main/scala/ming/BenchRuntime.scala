package ming

private[ming] object BenchRuntime:

  def currentLevel: Int =
    Option(System.getProperty("bench.level"))
      .orElse(Option(System.getenv("BENCH_LEVEL")))
      .flatMap(_.toIntOption)
      .getOrElse(0)

  def stringsAreImmutable: Boolean =
    val level = currentLevel
    level == 0 || level >= 15
