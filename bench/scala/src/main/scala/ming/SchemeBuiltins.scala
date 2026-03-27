package ming

import BuiltinSupport.*

private[ming] object Builtins:

  private val coreNames = Set(
    "+",
    "-",
    "*",
    "/",
    "<",
    ">",
    "=",
    "<=",
    ">=",
    "not",
    "eq?",
    "eqv?",
    "equal?",
    "map",
    "for-each",
    "apply",
    "dynamic-wind",
    "call/cc",
    "call-with-current-continuation"
  )

  private val builtinNames =
    coreNames ++
      NumericBuiltins.names ++
      PredicateBuiltins.names ++
      ListBuiltins.names ++
      OutputBuiltins.names ++
      StringBuiltins.names ++
      VectorBuiltins.names

  def resolve(name: String): Option[Value] =
    if builtinNames.contains(name) || isComposedPairAccessor(name) then Some(Value.BuiltinProc(name))
    else None

  def invoke(name: String, args: List[Value], pos: SourcePos, context: EvalContext): Value =
    name match
      case builtin if isComposedPairAccessor(builtin) =>
        invokePairAccessor(builtin, args, pos)
      case "+" | "-" | "*" | "/" =>
        invokeArithmetic(name, args, pos)
      case "<" | ">" | "=" | "<=" | ">=" =>
        invokeComparison(name, args, pos)
      case "not" =>
        Value.BoolVal(!ValueSemantics.isTruthy(requireSingleArg(name, args, pos)))
      case "eq?" | "eqv?" | "equal?" =>
        invokeEqualityBuiltin(name, args, pos)
      case builtin if NumericBuiltins.handlesUtility(builtin) =>
        NumericBuiltins.invokeUtility(builtin, args, pos)
      case builtin if NumericBuiltins.handlesPredicate(builtin) =>
        NumericBuiltins.invokePredicate(builtin, args, pos)
      case builtin if ListBuiltins.handlesCore(builtin) =>
        ListBuiltins.invokeCore(builtin, args, pos)
      case builtin if ListBuiltins.handlesUtility(builtin) =>
        ListBuiltins.invokeUtility(builtin, args, pos)
      case builtin if PredicateBuiltins.handles(builtin) =>
        PredicateBuiltins.invoke(builtin, args, pos)
      case builtin if OutputBuiltins.handles(builtin) =>
        OutputBuiltins.invoke(builtin, args, pos, context)
      case builtin if StringBuiltins.handles(builtin) =>
        StringBuiltins.invoke(builtin, args, pos)
      case builtin if VectorBuiltins.handles(builtin) =>
        VectorBuiltins.invoke(builtin, args, pos)
      case _ =>
        unknownProcedure(name, pos)

  private def invokeArithmetic(name: String, args: List[Value], pos: SourcePos): Value =
    name match
      case "+" =>
        SchemeNumber.add(evalNumbers(name, args, pos)).toValue
      case "-" =>
        subtract(name, args, pos)
      case "*" =>
        SchemeNumber.multiply(evalNumbers(name, args, pos)).toValue
      case "/" =>
        divide(name, args, pos)
      case _ =>
        unknownProcedure(name, pos)

  private def subtract(name: String, args: List[Value], pos: SourcePos): Value =
    val numbers = requireMinArgs(name, evalNumbers(name, args, pos), min = 1, pos)
    SchemeNumber.subtract(numbers).toValue

  private def divide(name: String, args: List[Value], pos: SourcePos): Value =
    val numbers = requireMinArgs(name, evalNumbers(name, args, pos), min = 2, pos)
    SchemeNumber.divide(numbers, pos).toValue

  private def invokeComparison(name: String, args: List[Value], pos: SourcePos): Value =
    name match
      case "<" =>
        compareNumbers(name, args, pos)((left, right) => SchemeNumber.compare(left, right) < 0)
      case ">" =>
        compareNumbers(name, args, pos)((left, right) => SchemeNumber.compare(left, right) > 0)
      case "=" =>
        compareNumbers(name, args, pos)(SchemeNumber.areEqual)
      case "<=" =>
        compareNumbers(name, args, pos)((left, right) => SchemeNumber.compare(left, right) <= 0)
      case ">=" =>
        compareNumbers(name, args, pos)((left, right) => SchemeNumber.compare(left, right) >= 0)
      case _ =>
        unknownProcedure(name, pos)

  private def invokeEqualityBuiltin(name: String, args: List[Value], pos: SourcePos): Value =
    val values = requireArgCount(name, args, expected = 2, pos)
    name match
      case "eq?" =>
        Value.BoolVal(ValueSemantics.isEq(values.head, values(1)))
      case "eqv?" =>
        Value.BoolVal(ValueSemantics.isEqv(values.head, values(1)))
      case "equal?" =>
        Value.BoolVal(ValueSemantics.isEqual(values.head, values(1)))
      case _ =>
        unknownProcedure(name, pos)

  private def invokePairAccessor(name: String, args: List[Value], pos: SourcePos): Value =
    name
      .substring(1, name.length - 1)
      .reverseIterator
      .foldLeft(requireSingleArg(name, args, pos)) { (current, operation) =>
        val pair = requirePairValue(name, current, pos)
        if operation == 'a' then pair.car else pair.cdr
      }

  private def isComposedPairAccessor(name: String): Boolean =
    name.lengthCompare(4) >= 0 &&
      name.head == 'c' &&
      name.last == 'r' &&
      name.substring(1, name.length - 1).forall(op => op == 'a' || op == 'd')
