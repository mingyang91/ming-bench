package ming

private[ming] object CxrBuiltins:

  import BuiltinSupport.*
  import SchemeInterpreter.Value

  def all: List[Value.Builtin] =
    List(2, 3, 4).flatMap(generateNames).map(build)

  private def generateNames(depth: Int): List[String] =
    def loop(remaining: Int, acc: String): List[String] =
      if remaining == 0 then List(s"c${acc}r")
      else loop(remaining - 1, acc + "a") ++ loop(remaining - 1, acc + "d")

    loop(depth, "")

  private def build(name: String): Value.Builtin =
    Value.Builtin(
      name,
      (args, pos) =>
        name
          .substring(1, name.length - 1)
          .reverse
          .foldLeft(singleArg(name, args, pos)) { (current, selector) =>
            val (carValue, cdrValue) = asPair(current, name, pos)
            if selector == 'a' then carValue else cdrValue
          }
    )
