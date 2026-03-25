package ming

private[ming] object ListBuiltins extends BuiltinSupport:

  def entries(macros: MacroState): Map[String, Value] = Map(
    "cons"      -> Value.Builtin("cons", cons),
    "car"       -> Value.Builtin("car", car),
    "cdr"       -> Value.Builtin("cdr", cdr),
    "list"      -> Value.Builtin("list", list),
    "list-ref"  -> Value.Builtin("list-ref", listRef),
    "list-tail" -> Value.Builtin("list-tail", listTail),
    "length"    -> Value.Builtin("length", length),
    "append"    -> Value.Builtin("append", append),
    "assoc"     -> Value.Builtin("assoc", assoc),
    "map"       -> Value.Builtin("map", mapBuiltin(macros)),
    "apply"     -> Value.Builtin("apply", applyBuiltin(macros))
  )

  private def cons(args: List[Value], pos: SourcePos): Value =
    args match
      case carValue :: cdrValue :: Nil =>
        Value.PairVal(carValue, cdrValue)

      case _ =>
        throw EvalError.at(pos, "cons expects exactly 2 arguments")

  private def car(args: List[Value], pos: SourcePos): Value =
    expectSingleArg(args, pos, "car") match
      case Value.PairVal(carValue, _) =>
        carValue

      case other =>
        throw EvalError.at(pos, s"car expected a pair, got ${other.typeName}")

  private def cdr(args: List[Value], pos: SourcePos): Value =
    expectSingleArg(args, pos, "cdr") match
      case Value.PairVal(_, cdrValue) =>
        cdrValue

      case other =>
        throw EvalError.at(pos, s"cdr expected a pair, got ${other.typeName}")

  private def list(args: List[Value], pos: SourcePos): Value =
    Value.list(args)

  private def listRef(args: List[Value], pos: SourcePos): Value =
    val (listValue, indexValue) = expectTwoArgs(args, pos, "list-ref")
    advanceList(listValue, expectIndex(indexValue, pos, "list-ref"), pos, "list-ref") match
      case Value.PairVal(carValue, _) =>
        carValue

      case _ =>
        throw EvalError.at(pos, "list-ref index is out of bounds")

  private def listTail(args: List[Value], pos: SourcePos): Value =
    val (listValue, indexValue) = expectTwoArgs(args, pos, "list-tail")
    advanceList(listValue, expectIndex(indexValue, pos, "list-tail"), pos, "list-tail")

  private def length(args: List[Value], pos: SourcePos): Value =
    Value.IntVal(asProperList(expectSingleArg(args, pos, "length"), pos, "length").length)

  private def append(args: List[Value], pos: SourcePos): Value =
    args match
      case Nil =>
        Value.EmptyList

      case _ =>
        val prefixItems = args.init.flatMap(arg => asProperList(arg, pos, "append"))
        val suffix      = expectList(args.last, pos, "append")
        prefixItems.foldRight(suffix)(Value.PairVal(_, _))

  private def assoc(args: List[Value], pos: SourcePos): Value =
    val (key, alist) = expectTwoArgs(args, pos, "assoc")

    def loop(current: Value): Value =
      current match
        case Value.EmptyList =>
          Value.BoolVal(false)

        case Value.PairVal(entry, rest) =>
          entry match
            case pair @ Value.PairVal(entryKey, _) =>
              if Value.equal(key, entryKey) then pair
              else loop(rest)

            case other =>
              throw EvalError.at(pos, s"assoc expected a list of pairs, got ${other.typeName}")

        case other =>
          throw EvalError.at(pos, s"assoc expected a proper list, got ${other.typeName}")

    loop(alist)

  private def mapBuiltin(macros: MacroState)(args: List[Value], pos: SourcePos): Value =
    args match
      case proc :: listArgs if listArgs.nonEmpty =>
        val lists   = listArgs.map(arg => asProperList(arg, pos, "map"))
        val lengths = lists.map(_.length).distinct
        if lengths.lengthCompare(1) > 0 then throw EvalError.at(pos, "map expects lists of the same length")

        Value.list(lists.transpose.map(row => Interpreter.applyProcedure(proc, row, pos, macros)))

      case _ =>
        throw EvalError.at(pos, "map expects a procedure and at least 1 list")

  private def applyBuiltin(macros: MacroState)(args: List[Value], pos: SourcePos): Value =
    args match
      case proc :: appliedArgs if appliedArgs.nonEmpty =>
        val prefixArgs = appliedArgs.dropRight(1)
        val listArgs   = asProperList(appliedArgs.last, pos, "apply")
        Interpreter.applyProcedure(proc, prefixArgs ++ listArgs, pos, macros)

      case _ =>
        throw EvalError.at(pos, "apply expects at least 2 arguments")

  private def advanceList(listValue: Value, steps: Int, pos: SourcePos, name: String): Value =
    if steps == 0 then listValue
    else
      listValue match
        case Value.PairVal(_, cdrValue) =>
          advanceList(cdrValue, steps - 1, pos, name)

        case Value.EmptyList =>
          throw EvalError.at(pos, s"$name index is out of bounds")

        case other =>
          throw EvalError.at(pos, s"$name expected a list, got ${other.typeName}")
