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
      case "exact?" =>
        typePred(args, MathBuiltins.isExact)
      case "inexact?" =>
        typePred(args, v => MathBuiltins.isNumeric(v) && !MathBuiltins.isExact(v))
      case "exact->inexact" =>
        args match
          case v :: Nil => MathBuiltins.exactToInexact(v, pos)
          case _        => throw new EvalError("exact->inexact requires 1 argument")
      case "inexact->exact" =>
        args match
          case v :: Nil => MathBuiltins.inexactToExact(v, pos)
          case _        => throw new EvalError("inexact->exact requires 1 argument")
      case "numerator" =>
        args match
          case Value.IntVal(n) :: Nil         => Value.IntVal(n)
          case Value.RationalVal(n, _) :: Nil => Value.IntVal(n)
          case _                              => throw new EvalError("numerator requires 1 exact argument")
      case "denominator" =>
        args match
          case Value.IntVal(_) :: Nil         => Value.IntVal(1L)
          case Value.RationalVal(_, d) :: Nil => Value.IntVal(d)
          case _                              => throw new EvalError("denominator requires 1 exact argument")
      case "rational?" =>
        typePred(args, v => MathBuiltins.isExact(v))
      case "cons" =>
        args match
          case a :: b :: Nil => Value.MutablePairVal(Array(a, b))
          case _             => throw new EvalError("cons requires exactly 2 arguments")
      case "car"  => singleArg(args, "car", pairCar)
      case "cdr"  => singleArg(args, "cdr", pairCdr)
      case "caar" => singleArg(args, "caar", v => pairCar(pairCar(v)))
      case "cadr" => singleArg(args, "cadr", v => pairCar(pairCdr(v)))
      case "cdar" => singleArg(args, "cdar", v => pairCdr(pairCar(v)))
      case "cddr" => singleArg(args, "cddr", v => pairCdr(pairCdr(v)))
      case "null?" =>
        args match
          case Value.NilVal :: Nil => Value.BoolVal(true)
          case _ :: Nil            => Value.BoolVal(false)
          case _ =>
            throw new EvalError("null? requires exactly 1 argument")
      case "list" =>
        args.foldRight(Value.NilVal: Value)((a, b) => Value.MutablePairVal(Array(a, b)))
      case "length" =>
        args match
          case head :: Nil => Value.IntVal(ListBuiltins.listLength(head))
          case _ =>
            throw new EvalError("length requires exactly 1 argument")
      case "string?"  => typePred(args, v => v.isInstanceOf[Value.StringVal] || v.isInstanceOf[Value.MutableStringVal])
      case "number?"  => typePred(args, MathBuiltins.isNumeric)
      case "boolean?" => typePred(args, _.isInstanceOf[Value.BoolVal])
      case "pair?" =>
        typePred(args, v => v.isInstanceOf[Value.PairVal] || v.isInstanceOf[Value.MutablePairVal])
      case "symbol?" => typePred(args, _.isInstanceOf[Value.Symbol])
      case "char?"   => typePred(args, _.isInstanceOf[Value.CharVal])
      case _         => applyBuiltinExtended(name, args, pos)

  private def applyBuiltinExtended(
    name: String,
    args: List[Value],
    pos: Option[(Int, Int)]
  ): Value =
    name match
      case "string-append"  => StringBuiltins.evalStringAppend(args)
      case "string-length"  => StringBuiltins.evalStringLength(args)
      case "substring"      => StringBuiltins.evalSubstring(args)
      case "string->number" => StringBuiltins.evalStringToNumber(args)
      case "number->string" => StringBuiltins.evalNumberToString(args)
      case "symbol->string" => StringBuiltins.evalSymbolToString(args)
      case "string->symbol" => StringBuiltins.evalStringToSymbol(args)
      case "string-ref"     => StringBuiltins.evalStringRef(args)
      case "string-copy"    => StringBuiltins.evalStringCopy(args)
      case "string-set!"    => StringBuiltins.evalStringSet(args)
      case "string->list"   => StringBuiltins.evalStringToList(args)
      case "list->string"   => StringBuiltins.evalListToString(args)
      case "char->integer"  => StringBuiltins.evalCharToInteger(args)
      case "integer->char"  => StringBuiltins.evalIntegerToChar(args)
      case "eq?"            => StringBuiltins.evalEq(args)
      case "equal?"         => StringBuiltins.evalEqual(args)
      case "abs"            => MathBuiltins.evalAbs(args, pos)
      case "modulo"         => MathBuiltins.evalModulo(args, pos)
      case "remainder"      => MathBuiltins.evalRemainder(args, pos)
      case "quotient"       => MathBuiltins.evalQuotient(args, pos)
      case "min"            => MathBuiltins.evalMinMax(args, pos, _ min _)
      case "max"            => MathBuiltins.evalMinMax(args, pos, _ max _)
      case "exact" =>
        args match
          case v :: Nil => MathBuiltins.inexactToExact(v, pos)
          case _        => throw new EvalError("exact requires 1 argument")
      case "inexact" =>
        args match
          case v :: Nil => MathBuiltins.exactToInexact(v, pos)
          case _        => throw new EvalError("inexact requires 1 argument")
      case "expt"             => MathBuiltins.evalExpt(args, pos)
      case "zero?"            => MathBuiltins.numPred(args, pos, _ == 0L)
      case "positive?"        => MathBuiltins.numPred(args, pos, _ > 0L)
      case "negative?"        => MathBuiltins.numPred(args, pos, _ < 0L)
      case "odd?"             => MathBuiltins.numPred(args, pos, n => math.abs(n % 2) == 1L)
      case "even?"            => MathBuiltins.numPred(args, pos, _ % 2 == 0L)
      case "list-ref"         => ListBuiltins.evalListRef(args)
      case "list-tail"        => ListBuiltins.evalListTail(args)
      case "list?"            => ListBuiltins.evalListPred(args)
      case "assoc"            => ListBuiltins.evalAssoc(args)
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
      case "integer?"         => typePred(args, MathBuiltins.isInteger)
      case "procedure?" =>
        typePred(
          args,
          v => v.isInstanceOf[Value.LambdaVal] || v.isInstanceOf[Value.CaseLambdaVal] || v.isInstanceOf[Value.Symbol]
        )
      case "eqv?"          => StringBuiltins.evalEq(args)
      case "vector"        => VectorBuiltins.evalVector(args)
      case "make-vector"   => VectorBuiltins.evalMakeVector(args)
      case "vector-ref"    => VectorBuiltins.evalVectorRef(args)
      case "vector-set!"   => VectorBuiltins.evalVectorSet(args)
      case "vector-length" => VectorBuiltins.evalVectorLength(args)
      case "vector?"       => typePred(args, _.isInstanceOf[Value.VectorVal])
      case "vector->list"  => VectorBuiltins.evalVectorToList(args)
      case "list->vector"  => VectorBuiltins.evalListToVector(args)
      case "reverse"       => ListBuiltins.evalReverse(args)
      case "set-car!" =>
        args match
          case Value.MutablePairVal(cell) :: v :: Nil => cell(0) = v; Value.VoidVal
          case _ => throw new EvalError("set-car! requires a mutable pair and a value")
      case "set-cdr!" =>
        args match
          case Value.MutablePairVal(cell) :: v :: Nil => cell(1) = v; Value.VoidVal
          case _ => throw new EvalError("set-cdr! requires a mutable pair and a value")
      case "syntax->datum" => SyntaxCase.syntaxToDatum(args)
      case "datum->syntax" => SyntaxCase.datumToSyntax(args)
      case _               => throw new EvalError(s"unknown procedure: $name")

  private def pairCar(v: Value): Value = v match
    case Value.PairVal(h, _, _)     => h
    case Value.MutablePairVal(cell) => cell(0)
    case _                          => throw new EvalError("car: not a pair")

  private def pairCdr(v: Value): Value = v match
    case Value.PairVal(_, t, _)     => t
    case Value.MutablePairVal(cell) => cell(1)
    case _                          => throw new EvalError("cdr: not a pair")

  private def singleArg(args: List[Value], name: String, f: Value => Value): Value =
    args match
      case v :: Nil => f(v)
      case _        => throw new EvalError(s"$name requires exactly 1 argument")

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
    .define("eqv?", Value.Symbol("eqv?"))
    .define("vector", Value.Symbol("vector"))
    .define("make-vector", Value.Symbol("make-vector"))
    .define("vector-ref", Value.Symbol("vector-ref"))
    .define("vector-set!", Value.Symbol("vector-set!"))
    .define("vector-length", Value.Symbol("vector-length"))
    .define("vector?", Value.Symbol("vector?"))
    .define("vector->list", Value.Symbol("vector->list"))
    .define("list->vector", Value.Symbol("list->vector"))
    .define("apply", Value.Symbol("apply"))
    .define("call/cc", Value.Symbol("call/cc"))
    .define("call-with-current-continuation", Value.Symbol("call/cc"))
    .define("dynamic-wind", Value.Symbol("dynamic-wind"))
    .define("reverse", Value.Symbol("reverse"))
    .define("set-car!", Value.Symbol("set-car!"))
    .define("set-cdr!", Value.Symbol("set-cdr!"))
    .define("caar", Value.Symbol("caar"))
    .define("cadr", Value.Symbol("cadr"))
    .define("cdar", Value.Symbol("cdar"))
    .define("cddr", Value.Symbol("cddr"))
    .define("values", Value.Symbol("values"))
    .define("call-with-values", Value.Symbol("call-with-values"))
    .define("exact?", Value.Symbol("exact?"))
    .define("inexact?", Value.Symbol("inexact?"))
    .define("exact->inexact", Value.Symbol("exact->inexact"))
    .define("inexact->exact", Value.Symbol("inexact->exact"))
    .define("exact", Value.Symbol("exact"))
    .define("inexact", Value.Symbol("inexact"))
    .define("numerator", Value.Symbol("numerator"))
    .define("denominator", Value.Symbol("denominator"))
    .define("rational?", Value.Symbol("rational?"))
    .define("syntax->datum", Value.Symbol("syntax->datum"))
    .define("datum->syntax", Value.Symbol("datum->syntax"))
