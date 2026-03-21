package ming

object StringProcedure:

  def apply(
    name: String,
    arguments: List[EvaluatedArg],
    state: EvalState,
    position: SourcePos
  ): (EvalState, Value) =
    name match
      case "display" =>
        val argument = expectSingleArg(arguments, "display", position)
        (state.appendOutput(argument.value.renderDisplay(state)), Value.Void)
      case "write" =>
        val argument = expectSingleArg(arguments, "write", position)
        (state.appendOutput(argument.value.render(state)), Value.Void)
      case "newline" =>
        expectExactArity(arguments, 0, "newline", position)
        (state.appendOutput("\n"), Value.Void)
      case "string-append" =>
        (state, Value.immutableString(arguments.map(expectString(_, state)).mkString))
      case "string-length" =>
        val argument = expectSingleArg(arguments, "string-length", position)
        (state, Value.Number(expectString(argument, state).length))
      case "substring" =>
        (state, evaluateSubstring(arguments, state, position))
      case "string->number" =>
        (state, evaluateStringToNumber(arguments, state, position))
      case "number->string" =>
        val argument = expectSingleArg(arguments, "number->string", position)
        (state, Value.immutableString(expectNumber(argument).toString))
      case "symbol->string" =>
        val argument = expectSingleArg(arguments, "symbol->string", position)
        (state, Value.immutableString(expectSymbol(argument)))
      case "string->symbol" =>
        val argument = expectSingleArg(arguments, "string->symbol", position)
        (state, Value.Symbol(expectString(argument, state)))
      case "string-ref" =>
        (state, evaluateStringRef(arguments, state, position))
      case "string-copy" =>
        val argument = expectSingleArg(arguments, "string-copy", position)
        state.allocateMutableString(expectString(argument, state))
      case "string-set!" =>
        evaluateStringSet(arguments, state, position)

  private def expectSingleArg(
    arguments: List[EvaluatedArg],
    name: String,
    position: SourcePos
  ): EvaluatedArg =
    expectExactArity(arguments, 1, name, position).head

  private def expectExactArity(
    arguments: List[EvaluatedArg],
    expected: Int,
    name: String,
    position: SourcePos
  ): List[EvaluatedArg] =
    if arguments.length == expected then arguments
    else throw EvalError.at(position, s"'$name' expects exactly $expected arguments")

  private def expectString(argument: EvaluatedArg, state: EvalState): String =
    state.readString(expectStringStorage(argument))

  private def expectStringStorage(argument: EvaluatedArg): StringStorage =
    argument.value match
      case Value.Str(value) => value
      case _                => throw EvalError.at(argument.position, "expected string")

  private def expectSymbol(argument: EvaluatedArg): String =
    argument.value match
      case Value.Symbol(name) => name
      case _                  => throw EvalError.at(argument.position, "expected symbol")

  private def expectNumber(argument: EvaluatedArg): BigInt =
    argument.value match
      case Value.Number(number) => number
      case _                    => throw EvalError.at(argument.position, "expected number")

  private def expectIndex(argument: EvaluatedArg): Int =
    expectNumber(argument) match
      case index if index >= 0 && index.isValidInt =>
        index.toInt
      case _ =>
        throw EvalError.at(argument.position, "expected non-negative integer index")

  private def expectCharacter(argument: EvaluatedArg): Char =
    argument.value match
      case Value.Character(value) => value
      case _                      => throw EvalError.at(argument.position, "expected character")

  private def evaluateSubstring(
    arguments: List[EvaluatedArg],
    state: EvalState,
    position: SourcePos
  ): Value =
    val substringArgs      = expectExactArity(arguments, 3, "substring", position)
    val value              = expectString(substringArgs.head, state)
    val startIndex         = expectIndex(substringArgs(1))
    val endIndex           = expectIndex(substringArgs(2))
    val indicesAreInBounds = startIndex <= endIndex && endIndex <= value.length

    if indicesAreInBounds then Value.immutableString(value.substring(startIndex, endIndex))
    else throw EvalError.at(position, "substring indices out of bounds")

  private def evaluateStringToNumber(
    arguments: List[EvaluatedArg],
    state: EvalState,
    position: SourcePos
  ): Value =
    parseInteger(expectString(expectSingleArg(arguments, "string->number", position), state)) match
      case Some(number) => Value.Number(number)
      case None         => Value.Bool(false)

  private def evaluateStringRef(
    arguments: List[EvaluatedArg],
    state: EvalState,
    position: SourcePos
  ): Value =
    val refArgs         = expectExactArity(arguments, 2, "string-ref", position)
    val value           = expectString(refArgs.head, state)
    val index           = expectIndex(refArgs(1))
    val indexIsInBounds = index < value.length

    if indexIsInBounds then Value.Character(value.charAt(index))
    else throw EvalError.at(refArgs(1).position, "string index out of bounds")

  private def evaluateStringSet(
    arguments: List[EvaluatedArg],
    state: EvalState,
    position: SourcePos
  ): (EvalState, Value) =
    val setArgs = expectExactArity(arguments, 3, "string-set!", position)
    expectStringStorage(setArgs.head) match
      case StringStorage.Immutable(_) =>
        throw EvalError.at(setArgs.head.position, "string is immutable")
      case StringStorage.Mutable(id) =>
        val value = state.readString(StringStorage.Mutable(id))
        val index = expectIndex(setArgs(1))
        if index >= value.length then throw EvalError.at(setArgs(1).position, "string index out of bounds")
        else
          val updated = value.updated(index, expectCharacter(setArgs(2)))
          (state.writeMutableString(id, updated), Value.Void)

  private def parseInteger(value: String): Option[BigInt] =
    if value.nonEmpty && value.forall(_.isDigit) then Some(BigInt(value))
    else if value.startsWith("-") && value.length > 1 && value.tail.forall(_.isDigit) then Some(BigInt(value))
    else None
