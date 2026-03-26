package ming

import Display.{display, displayStr}
import SchemeTypes.{errAt, isNumeric, isTruthy, pairToScalaList, schemeList, Env, PairCell, Pos, Value}

object Builtins:

  def apply(
    name: String,
    args: List[Value],
    pos: Pos,
    env: Env
  ): Value = name match
    case "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" =>
      ArithmeticBuiltins.applyArithmetic(name, args, pos)
    case "abs" | "modulo" | "remainder" | "quotient" | "min" | "max" | "expt" =>
      ArithmeticBuiltins.applyNumericUtils(name, args, pos)
    case "zero?" | "positive?" | "negative?" | "odd?" | "even?" =>
      ArithmeticBuiltins.applyNumericPredicates(name, args, pos)
    case "gcd" | "lcm" | "truncate" | "round" =>
      ArithmeticBuiltins.applyExtraNumeric(name, args, pos)
    case "not" | "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append" | "reverse" =>
      applyListOps(name, args, pos)
    case "set-car!" | "set-cdr!" =>
      applyPairMutation(name, args, pos)
    case "cddr" =>
      if args.length != 1 then throw errAt(pos, "cddr requires 1 argument")
      applyCdr(applyCdr(args.head, pos), pos)
    case "caddr" =>
      if args.length != 1 then throw errAt(pos, "caddr requires 1 argument")
      applyCar(applyCdr(applyCdr(args.head, pos), pos), pos)
    case "list-ref" | "list-tail" | "list?" | "assoc" | "assv" | "member" | "map" | "for-each" | "equal?" | "eq?" |
        "eqv?" =>
      ListUtilBuiltins.applyListUtils(name, args, pos, env)
    case "vector" | "make-vector" | "vector-ref" | "vector-set!" | "vector-length" | "vector?" | "vector->list" |
        "list->vector" =>
      VectorBuiltins.applyVectorOps(name, args, pos)
    case "number?" | "string?" | "boolean?" | "pair?" | "symbol?" | "char?" | "integer?" | "rational?" | "procedure?" =>
      applyTypeCheck(name, args, pos)
    case "exact?" | "inexact?" | "exact->inexact" | "inexact->exact" | "numerator" | "denominator" =>
      ArithmeticBuiltins.applyExactOps(name, args, pos)
    case "display" | "write" | "newline" =>
      applyIO(name, args, pos, env)
    case "apply" =>
      applyApply(args, pos, env)
    case "error" =>
      val msg = args.map(a => displayStr(a)).mkString(" ")
      throw EvalError(msg)
    case "string-append" | "string-length" | "substring" | "string->number" | "number->string" | "symbol->string" |
        "string->symbol" | "string-ref" | "string-copy" | "string-set!" | "string->list" | "list->string" |
        "make-string" | "string" =>
      StringCharBuiltins.applyStringOps(name, args, pos)
    case "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase" | "char=?" | "char<?" |
        "char->integer" | "integer->char" =>
      StringCharBuiltins.applyCharOps(name, args, pos)
    case "string=?" | "string<?" | "string>?" | "string<=?" | "string>=?" | "string-ci=?" | "string-upcase" |
        "string-downcase" =>
      StringCharBuiltins.applyStringCompare(name, args, pos)
    case s if s.startsWith("__record-") => Records.applyRecordOp(s, args, pos)
    case "call/cc" | "call-with-current-continuation" =>
      Evaluator.applyFunc(Value.VBuiltin(name), args, pos, env)
    case _ => throw errAt(pos, s"unknown builtin: $name")

  private def applyListOps(
    name: String,
    args: List[Value],
    pos: Pos
  ): Value = name match
    case "not" =>
      if args.length != 1 then throw errAt(pos, "not requires 1 argument")
      Value.VBool(!isTruthy(args.head))
    case "cons" =>
      if args.length != 2 then throw errAt(pos, "cons requires 2 arguments")
      Value.VPair(new PairCell(args(0), args(1)))
    case "car" =>
      if args.length != 1 then throw errAt(pos, "car requires 1 argument")
      args.head match
        case Value.VPair(cell)            => cell.car
        case Value.VList(h :: _)          => h
        case Value.VDottedList(h :: _, _) => h
        case _                            => throw errAt(pos, "car: not a pair")
    case "cdr" =>
      if args.length != 1 then throw errAt(pos, "cdr requires 1 argument")
      args.head match
        case Value.VPair(cell)                 => cell.cdr
        case Value.VList(_ :: t)               => Value.VList(t)
        case Value.VDottedList(_ :: Nil, last) => last
        case Value.VDottedList(_ :: rest, last) =>
          Value.VDottedList(rest, last)
        case _ => throw errAt(pos, "cdr: not a pair")
    case "null?" =>
      if args.length != 1 then throw errAt(pos, "null? requires 1 argument")
      Value.VBool(args.head == Value.VList(Nil))
    case "list" =>
      schemeList(args)
    case "length" =>
      if args.length != 1 then throw errAt(pos, "length requires 1 argument")
      args.head match
        case Value.VList(elems) => Value.VNum(elems.length.toLong)
        case Value.VPair(_) =>
          val elems = pairToScalaList(args.head, pos)
          Value.VNum(elems.length.toLong)
        case _ => throw errAt(pos, "length: not a list")
    case "append" =>
      if args.isEmpty then Value.VList(Nil)
      else if args.length == 1 then args.head
      else
        val last   = args.last
        val prefix = args.init.flatMap(a => pairToScalaList(a, pos))
        prefix.foldRight(last)((h, t) => Value.VPair(new PairCell(h, t)))
    case "reverse" =>
      if args.length != 1 then throw errAt(pos, "reverse requires 1 argument")
      val elems = pairToScalaList(args.head, pos)
      schemeList(elems.reverse)
    case _ => throw errAt(pos, s"unknown list op: $name")

  private def applyCar(v: Value, pos: Pos): Value = v match
    case Value.VPair(cell)            => cell.car
    case Value.VList(h :: _)          => h
    case Value.VDottedList(h :: _, _) => h
    case _                            => throw errAt(pos, "car: not a pair")

  private def applyCdr(v: Value, pos: Pos): Value = v match
    case Value.VPair(cell)                  => cell.cdr
    case Value.VList(_ :: t)                => Value.VList(t)
    case Value.VDottedList(_ :: Nil, last)  => last
    case Value.VDottedList(_ :: rest, last) => Value.VDottedList(rest, last)
    case _                                  => throw errAt(pos, "cdr: not a pair")

  private def applyPairMutation(
    name: String,
    args: List[Value],
    pos: Pos
  ): Value = name match
    case "set-car!" =>
      if args.length != 2 then throw errAt(pos, "set-car! requires 2 arguments")
      args(0) match
        case Value.VPair(cell) =>
          cell.car = args(1)
          Value.VVoid
        case _ => throw errAt(pos, "set-car!: not a mutable pair")
    case "set-cdr!" =>
      if args.length != 2 then throw errAt(pos, "set-cdr! requires 2 arguments")
      args(0) match
        case Value.VPair(cell) =>
          cell.cdr = args(1)
          Value.VVoid
        case _ => throw errAt(pos, "set-cdr!: not a mutable pair")
    case _ => throw errAt(pos, s"unknown pair mutation: $name")

  private def applyTypeCheck(
    name: String,
    args: List[Value],
    pos: Pos
  ): Value =
    if args.length != 1 then throw errAt(pos, s"$name requires 1 argument")
    val arg = args.head
    val result = (name, arg) match
      case ("number?", v)                      => isNumeric(v)
      case ("integer?", Value.VNum(_))         => true
      case ("integer?", Value.VRational(_, _)) => false
      case ("integer?", Value.VFloat(d))       => d == d.toLong.toDouble && !d.isInfinite
      case ("integer?", _)                     => false
      case ("rational?", v) =>
        v match
          case _: Value.VNum | _: Value.VRational => true
          case _                                  => false
      case ("string?", _: Value.VStr)             => true
      case ("string?", _)                         => false
      case ("boolean?", _: Value.VBool)           => true
      case ("boolean?", _)                        => false
      case ("pair?", _: Value.VPair)              => true
      case ("pair?", Value.VList(_ :: _))         => true
      case ("pair?", _: Value.VDottedList)        => true
      case ("pair?", _)                           => false
      case ("symbol?", _: Value.VSymbol)          => true
      case ("symbol?", _)                         => false
      case ("char?", _: Value.VChar)              => true
      case ("char?", _)                           => false
      case ("procedure?", _: Value.VBuiltin)      => true
      case ("procedure?", _: Value.VLambda)       => true
      case ("procedure?", _: Value.VCaseLambda)   => true
      case ("procedure?", _: Value.VContinuation) => true
      case ("procedure?", _)                      => false
      case _                                      => throw errAt(pos, s"unknown type check: $name")
    Value.VBool(result)

  private def applyIO(
    name: String,
    args: List[Value],
    pos: Pos,
    env: Env
  ): Value = name match
    case "display" =>
      if args.length != 1 then throw errAt(pos, "display requires 1 argument")
      env.output.append(displayStr(args.head))
      Value.VVoid
    case "write" =>
      if args.length != 1 then throw errAt(pos, "write requires 1 argument")
      env.output.append(display(args.head))
      Value.VVoid
    case "newline" =>
      if args.nonEmpty then throw errAt(pos, "newline requires 0 arguments")
      env.output.append("\n")
      Value.VVoid
    case _ => throw errAt(pos, s"unknown IO op: $name")

  private def applyApply(
    args: List[Value],
    pos: Pos,
    env: Env
  ): Value =
    if args.length < 2 then throw errAt(pos, "apply requires at least 2 arguments")
    val func = args.head
    val lastArg = args.last match
      case Value.VList(elems) => elems
      case Value.VPair(_)     => pairToScalaList(args.last, pos)
      case _ =>
        throw errAt(pos, "apply: last argument must be a list")
    val prefixArgs = args.slice(1, args.length - 1)
    val allArgs    = prefixArgs ++ lastArg
    Evaluator.applyFunc(func, allArgs, pos, env)
