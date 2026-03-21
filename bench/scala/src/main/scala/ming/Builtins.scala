package ming

/** Built-in procedure dispatch and default environment. */
object Builtins:

  def applyBuiltin(
    name: String,
    args: List[Value],
    pos: Option[(Int, Int)]
  ): Value =
    name match
      case "+"  => MathBuiltins.evalAdd(args, pos)
      case "-"  => MathBuiltins.evalSub(args, pos)
      case "*"  => MathBuiltins.evalMul(args, pos)
      case "/"  => MathBuiltins.evalDiv(args, pos)
      case "<"  => MathBuiltins.evalCmp(args, _ < _, pos)
      case ">"  => MathBuiltins.evalCmp(args, _ > _, pos)
      case "="  => MathBuiltins.evalCmp(args, _ == _, pos)
      case "<=" => MathBuiltins.evalCmp(args, _ <= _, pos)
      case ">=" => MathBuiltins.evalCmp(args, _ >= _, pos)
      case "cons" =>
        args match
          case a :: b :: Nil => Value.PairVal(a, b)
          case _ =>
            throw new EvalError("cons requires exactly 2 arguments")
      case "car" =>
        args match
          case Value.PairVal(h, _, _) :: Nil => h
          case _ =>
            throw new EvalError("car requires a pair argument")
      case "cdr" =>
        args match
          case Value.PairVal(_, t, _) :: Nil => t
          case _ =>
            throw new EvalError("cdr requires a pair argument")
      case "null?" =>
        args match
          case Value.NilVal :: Nil => Value.BoolVal(true)
          case _ :: Nil            => Value.BoolVal(false)
          case _ =>
            throw new EvalError("null? requires exactly 1 argument")
      case "list" =>
        args.foldRight(Value.NilVal: Value)(Value.PairVal(_, _))
      case "length" =>
        args match
          case head :: Nil => Value.IntVal(listLength(head))
          case _ =>
            throw new EvalError("length requires exactly 1 argument")
      case "string?"  => typePred(args, v => v.isInstanceOf[Value.StringVal] || v.isInstanceOf[Value.MutableStringVal])
      case "number?"  => typePred(args, _.isInstanceOf[Value.IntVal])
      case "boolean?" => typePred(args, _.isInstanceOf[Value.BoolVal])
      case "pair?"    => typePred(args, _.isInstanceOf[Value.PairVal])
      case "symbol?"  => typePred(args, _.isInstanceOf[Value.Symbol])
      case "char?"    => typePred(args, _.isInstanceOf[Value.CharVal])
      case "string-append"    => StringBuiltins.evalStringAppend(args)
      case "string-length"    => StringBuiltins.evalStringLength(args)
      case "substring"        => StringBuiltins.evalSubstring(args)
      case "string->number"   => StringBuiltins.evalStringToNumber(args)
      case "number->string"   => StringBuiltins.evalNumberToString(args)
      case "symbol->string"   => StringBuiltins.evalSymbolToString(args)
      case "string->symbol"   => StringBuiltins.evalStringToSymbol(args)
      case "string-ref"       => StringBuiltins.evalStringRef(args)
      case "string-copy"      => StringBuiltins.evalStringCopy(args)
      case "string-set!"      => StringBuiltins.evalStringSet(args)
      case "string->list"     => StringBuiltins.evalStringToList(args)
      case "list->string"     => StringBuiltins.evalListToString(args)
      case "char->integer"    => StringBuiltins.evalCharToInteger(args)
      case "integer->char"    => StringBuiltins.evalIntegerToChar(args)
      case "eq?"              => StringBuiltins.evalEq(args)
      case "equal?"           => StringBuiltins.evalEqual(args)
      case "abs"              => MathBuiltins.evalAbs(args, pos)
      case "modulo"           => MathBuiltins.evalModulo(args, pos)
      case "remainder"        => MathBuiltins.evalRemainder(args, pos)
      case "quotient"         => MathBuiltins.evalQuotient(args, pos)
      case "min"              => MathBuiltins.evalMinMax(args, pos, _ min _)
      case "max"              => MathBuiltins.evalMinMax(args, pos, _ max _)
      case "expt"             => MathBuiltins.evalExpt(args, pos)
      case "zero?"            => MathBuiltins.numPred(args, pos, _ == 0L)
      case "positive?"        => MathBuiltins.numPred(args, pos, _ > 0L)
      case "negative?"        => MathBuiltins.numPred(args, pos, _ < 0L)
      case "odd?"             => MathBuiltins.numPred(args, pos, n => math.abs(n % 2) == 1L)
      case "even?"            => MathBuiltins.numPred(args, pos, _ % 2 == 0L)
      case "list-ref"         => evalListRef(args)
      case "list-tail"        => evalListTail(args)
      case "list?"            => evalListPred(args)
      case "assoc"            => evalAssoc(args)
      case "char-alphabetic?" => StringBuiltins.charPred(args, _.isLetter)
      case "char-numeric?"    => StringBuiltins.charPred(args, _.isDigit)
      case "char-upcase"      => StringBuiltins.charTransform(args, _.toUpper)
      case "char-downcase"    => StringBuiltins.charTransform(args, _.toLower)
      case "char=?"           => StringBuiltins.charCmp(args, _ == _)
      case "char<?"           => StringBuiltins.charCmp(args, _ < _)
      case "string=?"         => StringBuiltins.strCmp(args, _ == _)
      case "string<?"         => StringBuiltins.strCmp(args, _ < _)
      case "string-ci=?"      => StringBuiltins.strCmp(args, (a, b) => a.equalsIgnoreCase(b))
      case "string-upcase"    => StringBuiltins.strCase(args, _.toUpperCase)
      case "string-downcase"  => StringBuiltins.strCase(args, _.toLowerCase)
      case "integer?"         => typePred(args, _.isInstanceOf[Value.IntVal])
      case "procedure?"       => typePred(args, v => v.isInstanceOf[Value.LambdaVal] || v.isInstanceOf[Value.Symbol])
      case _                  => throw new EvalError(s"unknown procedure: $name")

  private def typePred(
    args: List[Value],
    pred: Value => Boolean
  ): Value =
    args match
      case v :: Nil => Value.BoolVal(pred(v))
      case _ =>
        throw new EvalError(
          "type predicate requires exactly 1 argument"
        )

  private def listLength(v: Value): Long = v match
    case Value.NilVal           => 0L
    case Value.PairVal(_, t, _) => 1L + listLength(t)
    case _                      => throw new EvalError("length: not a proper list")

  private def evalListRef(args: List[Value]): Value =
    args match
      case lst :: Value.IntVal(idx) :: Nil => listRef(lst, idx.toInt)
      case _                               => throw new EvalError("list-ref requires list and index")

  @scala.annotation.tailrec
  private def listRef(v: Value, idx: Int): Value =
    v match
      case Value.PairVal(car, cdr, _) =>
        if idx == 0 then car else listRef(cdr, idx - 1)
      case _ => throw new EvalError("list-ref: index out of range")

  private def evalListTail(args: List[Value]): Value =
    args match
      case lst :: Value.IntVal(idx) :: Nil => listTail(lst, idx.toInt)
      case _                               => throw new EvalError("list-tail requires list and index")

  @scala.annotation.tailrec
  private def listTail(v: Value, idx: Int): Value =
    if idx == 0 then v
    else
      v match
        case Value.PairVal(_, cdr, _) => listTail(cdr, idx - 1)
        case _                        => throw new EvalError("list-tail: index out of range")

  private def evalListPred(args: List[Value]): Value =
    args match
      case v :: Nil => Value.BoolVal(isProperList(v))
      case _        => throw new EvalError("list? requires exactly 1 argument")

  @scala.annotation.tailrec
  private def isProperList(v: Value): Boolean = v match
    case Value.NilVal           => true
    case Value.PairVal(_, t, _) => isProperList(t)
    case _                      => false

  private def evalAssoc(args: List[Value]): Value =
    args match
      case key :: lst :: Nil => assocSearch(key, lst)
      case _                 => throw new EvalError("assoc requires exactly 2 arguments")

  @scala.annotation.tailrec
  private def assocSearch(key: Value, lst: Value): Value =
    lst match
      case Value.NilVal => Value.BoolVal(false)
      case Value.PairVal(pair @ Value.PairVal(k, _, _), rest, _) =>
        if StringBuiltins.schemeEqual(key, k) then pair
        else assocSearch(key, rest)
      case _ => throw new EvalError("assoc: not a proper alist")

  val defaultEnv: Env = Env(Map.empty, None)
    .define("+", Value.Symbol("+"))
    .define("-", Value.Symbol("-"))
    .define("*", Value.Symbol("*"))
    .define("/", Value.Symbol("/"))
    .define("<", Value.Symbol("<"))
    .define(">", Value.Symbol(">"))
    .define("=", Value.Symbol("="))
    .define("<=", Value.Symbol("<="))
    .define(">=", Value.Symbol(">="))
    .define("cons", Value.Symbol("cons"))
    .define("car", Value.Symbol("car"))
    .define("cdr", Value.Symbol("cdr"))
    .define("null?", Value.Symbol("null?"))
    .define("list", Value.Symbol("list"))
    .define("length", Value.Symbol("length"))
    .define("string?", Value.Symbol("string?"))
    .define("number?", Value.Symbol("number?"))
    .define("boolean?", Value.Symbol("boolean?"))
    .define("pair?", Value.Symbol("pair?"))
    .define("symbol?", Value.Symbol("symbol?"))
    .define("char?", Value.Symbol("char?"))
    .define("display", Value.Symbol("display"))
    .define("write", Value.Symbol("write"))
    .define("newline", Value.Symbol("newline"))
    .define("string-append", Value.Symbol("string-append"))
    .define("string-length", Value.Symbol("string-length"))
    .define("substring", Value.Symbol("substring"))
    .define("string->number", Value.Symbol("string->number"))
    .define("number->string", Value.Symbol("number->string"))
    .define("symbol->string", Value.Symbol("symbol->string"))
    .define("string->symbol", Value.Symbol("string->symbol"))
    .define("string-ref", Value.Symbol("string-ref"))
    .define("string-copy", Value.Symbol("string-copy"))
    .define("string-set!", Value.Symbol("string-set!"))
    .define("string->list", Value.Symbol("string->list"))
    .define("list->string", Value.Symbol("list->string"))
    .define("char->integer", Value.Symbol("char->integer"))
    .define("integer->char", Value.Symbol("integer->char"))
    .define("eq?", Value.Symbol("eq?"))
    .define("equal?", Value.Symbol("equal?"))
    .define("abs", Value.Symbol("abs"))
    .define("modulo", Value.Symbol("modulo"))
    .define("remainder", Value.Symbol("remainder"))
    .define("quotient", Value.Symbol("quotient"))
    .define("min", Value.Symbol("min"))
    .define("max", Value.Symbol("max"))
    .define("expt", Value.Symbol("expt"))
    .define("zero?", Value.Symbol("zero?"))
    .define("positive?", Value.Symbol("positive?"))
    .define("negative?", Value.Symbol("negative?"))
    .define("odd?", Value.Symbol("odd?"))
    .define("even?", Value.Symbol("even?"))
    .define("list-ref", Value.Symbol("list-ref"))
    .define("list-tail", Value.Symbol("list-tail"))
    .define("list?", Value.Symbol("list?"))
    .define("assoc", Value.Symbol("assoc"))
    .define("map", Value.Symbol("map"))
    .define("char-alphabetic?", Value.Symbol("char-alphabetic?"))
    .define("char-numeric?", Value.Symbol("char-numeric?"))
    .define("char-upcase", Value.Symbol("char-upcase"))
    .define("char-downcase", Value.Symbol("char-downcase"))
    .define("char=?", Value.Symbol("char=?"))
    .define("char<?", Value.Symbol("char<?"))
    .define("string=?", Value.Symbol("string=?"))
    .define("string<?", Value.Symbol("string<?"))
    .define("string-ci=?", Value.Symbol("string-ci=?"))
    .define("string-upcase", Value.Symbol("string-upcase"))
    .define("string-downcase", Value.Symbol("string-downcase"))
    .define("integer?", Value.Symbol("integer?"))
    .define("procedure?", Value.Symbol("procedure?"))
    .define("apply", Value.Symbol("apply"))
    .define("call/cc", Value.Symbol("call/cc"))
    .define("call-with-current-continuation", Value.Symbol("call/cc"))
