package ming

import SchemeTypes.{display, displayStr, errAt, isNumeric, isTruthy, valuesEqual, Env, Pos, Value}

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
    case "not" | "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append" =>
      applyListOps(name, args, pos)
    case "list-ref" | "list-tail" | "list?" | "assoc" | "map" | "equal?" | "eq?" =>
      applyListUtils(name, args, pos, env)
    case "number?" | "string?" | "boolean?" | "pair?" | "symbol?" | "char?" | "integer?" | "rational?" =>
      applyTypeCheck(name, args, pos)
    case "exact?" | "inexact?" | "exact->inexact" | "inexact->exact" | "numerator" | "denominator" =>
      ArithmeticBuiltins.applyExactOps(name, args, pos)
    case "display" | "write" | "newline" =>
      applyIO(name, args, pos, env)
    case "apply" =>
      applyApply(args, pos, env)
    case "string-append" | "string-length" | "substring" | "string->number" | "number->string" | "symbol->string" |
        "string->symbol" | "string-ref" | "string-copy" | "string-set!" =>
      StringCharBuiltins.applyStringOps(name, args, pos)
    case "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase" | "char=?" | "char<?" =>
      StringCharBuiltins.applyCharOps(name, args, pos)
    case "string=?" | "string<?" | "string-ci=?" | "string-upcase" | "string-downcase" =>
      StringCharBuiltins.applyStringCompare(name, args, pos)
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
      args(1) match
        case Value.VList(elems) => Value.VList(args(0) :: elems)
        case Value.VDottedList(elems, last) =>
          Value.VDottedList(args(0) :: elems, last)
        case _ => Value.VDottedList(List(args(0)), args(1))
    case "car" =>
      if args.length != 1 then throw errAt(pos, "car requires 1 argument")
      args.head match
        case Value.VList(h :: _)          => h
        case Value.VDottedList(h :: _, _) => h
        case _                            => throw errAt(pos, "car: not a pair")
    case "cdr" =>
      if args.length != 1 then throw errAt(pos, "cdr requires 1 argument")
      args.head match
        case Value.VList(_ :: t)               => Value.VList(t)
        case Value.VDottedList(_ :: Nil, last) => last
        case Value.VDottedList(_ :: rest, last) =>
          Value.VDottedList(rest, last)
        case _ => throw errAt(pos, "cdr: not a pair")
    case "null?" =>
      if args.length != 1 then throw errAt(pos, "null? requires 1 argument")
      Value.VBool(args.head == Value.VList(Nil))
    case "list" => Value.VList(args)
    case "length" =>
      if args.length != 1 then throw errAt(pos, "length requires 1 argument")
      args.head match
        case Value.VList(elems) =>
          Value.VNum(elems.length.toLong)
        case _ => throw errAt(pos, "length: not a list")
    case "append" =>
      args.foldRight(Value.VList(Nil): Value) { (arg, acc) =>
        (arg, acc) match
          case (Value.VList(elems), Value.VList(accElems)) =>
            Value.VList(elems ++ accElems)
          case _ =>
            throw errAt(pos, "append: not a list")
      }
    case _ => throw errAt(pos, s"unknown list op: $name")

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
      case ("string?", _: Value.VStr)      => true
      case ("string?", _)                  => false
      case ("boolean?", _: Value.VBool)    => true
      case ("boolean?", _)                 => false
      case ("pair?", Value.VList(_ :: _))  => true
      case ("pair?", _: Value.VDottedList) => true
      case ("pair?", _)                    => false
      case ("symbol?", _: Value.VSymbol)   => true
      case ("symbol?", _)                  => false
      case ("char?", _: Value.VChar)       => true
      case ("char?", _)                    => false
      case _                               => throw errAt(pos, s"unknown type check: $name")
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

  private def applyListUtils(
    name: String,
    args: List[Value],
    pos: Pos,
    env: Env
  ): Value = name match
    case "list-ref" =>
      if args.length != 2 then throw errAt(pos, "list-ref requires 2 arguments")
      (args(0), args(1)) match
        case (Value.VList(elems), Value.VNum(i)) =>
          elems(i.toInt)
        case _ =>
          throw errAt(pos, "list-ref: invalid arguments")
    case "list-tail" =>
      if args.length != 2 then throw errAt(pos, "list-tail requires 2 arguments")
      (args(0), args(1)) match
        case (Value.VList(elems), Value.VNum(i)) =>
          Value.VList(elems.drop(i.toInt))
        case _ =>
          throw errAt(pos, "list-tail: invalid arguments")
    case "list?" =>
      if args.length != 1 then throw errAt(pos, "list? requires 1 argument")
      args.head match
        case Value.VList(_) => Value.VBool(true)
        case _              => Value.VBool(false)
    case "assoc" =>
      if args.length != 2 then throw errAt(pos, "assoc requires 2 arguments")
      args(1) match
        case Value.VList(elems) =>
          elems
            .collectFirst {
              case v @ Value.VList(key :: _) if valuesEqual(key, args(0)) =>
                v
            }
            .getOrElse(Value.VBool(false))
        case _ => throw errAt(pos, "assoc: not a list")
    case "map" =>
      if args.length < 2 then throw errAt(pos, "map requires at least 2 arguments")
      val func = args.head
      val lists = args.tail.map {
        case Value.VList(elems) => elems
        case _                  => throw errAt(pos, "map: not a list")
      }
      val len = lists.head.length
      val result = (0 until len).toList.map { i =>
        val mapArgs = lists.map(_(i))
        Evaluator.applyFunc(func, mapArgs, pos, env)
      }
      Value.VList(result)
    case "equal?" =>
      if args.length != 2 then throw errAt(pos, "equal? requires 2 arguments")
      Value.VBool(valuesEqual(args(0), args(1)))
    case "eq?" =>
      if args.length != 2 then throw errAt(pos, "eq? requires 2 arguments")
      val result = (args(0), args(1)) match
        case (Value.VSymbol(a), Value.VSymbol(b)) => a == b
        case (Value.VNum(a), Value.VNum(b))       => a == b
        case (Value.VBool(a), Value.VBool(b))     => a == b
        case (Value.VChar(a), Value.VChar(b))     => a == b
        case (Value.VList(Nil), Value.VList(Nil)) => true
        case (a, b)                               => a eq b
      Value.VBool(result)
    case _ => throw errAt(pos, s"unknown list util: $name")

  private def applyApply(
    args: List[Value],
    pos: Pos,
    env: Env
  ): Value =
    if args.length < 2 then throw errAt(pos, "apply requires at least 2 arguments")
    val func = args.head
    val lastArg = args.last match
      case Value.VList(elems) => elems
      case _ =>
        throw errAt(pos, "apply: last argument must be a list")
    val prefixArgs = args.slice(1, args.length - 1)
    val allArgs    = prefixArgs ++ lastArg
    Evaluator.applyFunc(func, allArgs, pos, env)
