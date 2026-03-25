package ming

private[ming] object CharBuiltins:

  import BuiltinSupport.*
  import SchemeInterpreter.Value

  def all: List[Value.Builtin] =
    List(
      charAlphabeticBuiltin,
      charNumericBuiltin,
      charUpcaseBuiltin,
      charDowncaseBuiltin,
      charEqualsBuiltin,
      charLessBuiltin
    )

  private val charAlphabeticBuiltin: Value.Builtin =
    charPredicateBuiltin("char-alphabetic?")(_.isLetter)

  private val charNumericBuiltin: Value.Builtin =
    charPredicateBuiltin("char-numeric?")(_.isDigit)

  private val charUpcaseBuiltin: Value.Builtin =
    Value.Builtin(
      "char-upcase",
      (args, pos) => Value.Character(asCharacter(singleArg("char-upcase", args, pos), "char-upcase", pos).toUpper)
    )

  private val charDowncaseBuiltin: Value.Builtin =
    Value.Builtin(
      "char-downcase",
      (args, pos) => Value.Character(asCharacter(singleArg("char-downcase", args, pos), "char-downcase", pos).toLower)
    )

  private val charEqualsBuiltin: Value.Builtin =
    charComparisonBuiltin("char=?")(_ == _)

  private val charLessBuiltin: Value.Builtin =
    charComparisonBuiltin("char<?")(_ < _)

  private def charPredicateBuiltin(
    name: String
  )(predicate: Char => Boolean): Value.Builtin =
    Value.Builtin(
      name,
      (args, pos) => Value.Bool(predicate(asCharacter(singleArg(name, args, pos), name, pos)))
    )

  private def charComparisonBuiltin(
    name: String
  )(predicate: (Char, Char) => Boolean): Value.Builtin =
    Value.Builtin(
      name,
      (args, pos) =>
        requireAtLeast(name, args, expected = 2, pos)
        val chars = args.map(asCharacter(_, name, pos))
        Value.Bool(chars.zip(chars.tail).forall { case (left, right) => predicate(left, right) })
    )
