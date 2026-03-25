package ming

private[ming] object PredicateBuiltins extends BuiltinSupport:

  val entries: Map[String, Value] = Map(
    "not"    -> Value.Builtin("not", negate),
    "eq?"    -> Value.Builtin("eq?", eqv),
    "equal?" -> Value.Builtin("equal?", equal),
    "null?"  -> predicate("null?", _ == Value.EmptyList),
    "list?"  -> predicate("list?", Value.isProperList),
    "string?" -> predicate(
      "string?",
      {
        case Value.StringVal(_) => true
        case _                  => false
      }
    ),
    "number?" -> predicate(
      "number?",
      SchemeNumber.isNumberValue
    ),
    "integer?"  -> numericPredicate("integer?", _.isInteger),
    "rational?" -> numericPredicate("rational?", _.isExact),
    "exact?"    -> numericPredicate("exact?", _.isExact),
    "inexact?"  -> numericPredicate("inexact?", _.isInexact),
    "boolean?" -> predicate(
      "boolean?",
      {
        case Value.BoolVal(_) => true
        case _                => false
      }
    ),
    "pair?" -> predicate(
      "pair?",
      {
        case Value.PairVal(_, _) => true
        case _                   => false
      }
    ),
    "symbol?" -> predicate(
      "symbol?",
      {
        case Value.SymbolVal(_) => true
        case _                  => false
      }
    ),
    "char?" -> predicate(
      "char?",
      {
        case Value.CharVal(_) => true
        case _                => false
      }
    )
  )

  private def negate(args: List[Value], pos: SourcePos): Value =
    args match
      case value :: Nil =>
        Value.BoolVal(!Value.isTruthy(value))

      case _ =>
        throw EvalError.at(pos, "not expects exactly 1 argument")

  private def eqv(args: List[Value], pos: SourcePos): Value =
    val (left, right) = expectTwoArgs(args, pos, "eq?")
    Value.BoolVal(Value.eqv(left, right))

  private def equal(args: List[Value], pos: SourcePos): Value =
    val (left, right) = expectTwoArgs(args, pos, "equal?")
    Value.BoolVal(Value.equal(left, right))

  private def predicate(name: String, test: Value => Boolean): Value =
    Value.Builtin(
      name,
      (args, pos) => Value.BoolVal(test(expectSingleArg(args, pos, name)))
    )

  private def numericPredicate(name: String, test: SchemeNumber => Boolean): Value =
    Value.Builtin(
      name,
      (args, pos) =>
        Value.BoolVal(
          SchemeNumber.fromValueOption(expectSingleArg(args, pos, name)).exists(test)
        )
    )
