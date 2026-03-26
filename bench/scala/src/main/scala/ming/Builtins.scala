package ming

import SchemeTypes.{asNum, display, displayStr, errAt, isTruthy, valuesEqual, Env, Pos, Value}

object Builtins:

  def apply(
    name: String,
    args: List[Value],
    pos: Pos,
    env: Env
  ): Value = name match
    case "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" =>
      applyArithmetic(name, args, pos)
    case "abs" | "modulo" | "remainder" | "quotient" | "min" | "max" | "expt" =>
      applyNumericUtils(name, args, pos)
    case "zero?" | "positive?" | "negative?" | "odd?" | "even?" =>
      applyNumericPredicates(name, args, pos)
    case "not" | "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append" =>
      applyListOps(name, args, pos)
    case "list-ref" | "list-tail" | "list?" | "assoc" | "map" | "equal?" | "eq?" =>
      applyListUtils(name, args, pos, env)
    case "number?" | "string?" | "boolean?" | "pair?" | "symbol?" | "char?" =>
      applyTypeCheck(name, args, pos)
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

  private def applyArithmetic(
    name: String,
    args: List[Value],
    pos: Pos
  ): Value = name match
    case "+" => Value.VNum(args.map(v => asNum(v, pos)).sum)
    case "*" => Value.VNum(args.map(v => asNum(v, pos)).product)
    case "-" =>
      if args.isEmpty then throw errAt(pos, "- requires at least 1 argument")
      else if args.length == 1 then Value.VNum(-asNum(args.head, pos))
      else Value.VNum(args.map(v => asNum(v, pos)).reduceLeft(_ - _))
    case "/" =>
      if args.isEmpty then throw errAt(pos, "/ requires at least 1 argument")
      else if args.length == 1 then Value.VNum(1 / asNum(args.head, pos))
      else
        val nums = args.map(v => asNum(v, pos))
        if nums.tail.contains(0L) then throw errAt(pos, "division by zero")
        Value.VNum(nums.reduceLeft(_ / _))
    case "<" =>
      val nums = args.map(v => asNum(v, pos))
      Value.VBool(nums.zip(nums.tail).forall((a, b) => a < b))
    case ">" =>
      val nums = args.map(v => asNum(v, pos))
      Value.VBool(nums.zip(nums.tail).forall((a, b) => a > b))
    case "=" =>
      val nums = args.map(v => asNum(v, pos))
      Value.VBool(nums.zip(nums.tail).forall((a, b) => a == b))
    case "<=" =>
      val nums = args.map(v => asNum(v, pos))
      Value.VBool(nums.zip(nums.tail).forall((a, b) => a <= b))
    case ">=" =>
      val nums = args.map(v => asNum(v, pos))
      Value.VBool(nums.zip(nums.tail).forall((a, b) => a >= b))
    case _ => throw errAt(pos, s"unknown arithmetic op: $name")

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
      case ("number?", _: Value.VNum)      => true
      case ("number?", _)                  => false
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

  private def applyNumericUtils(
    name: String,
    args: List[Value],
    pos: Pos
  ): Value = name match
    case "abs" =>
      if args.length != 1 then throw errAt(pos, "abs requires 1 argument")
      Value.VNum(math.abs(asNum(args.head, pos)))
    case "modulo" =>
      if args.length != 2 then throw errAt(pos, "modulo requires 2 arguments")
      val a = asNum(args(0), pos)
      val b = asNum(args(1), pos)
      Value.VNum(java.lang.Math.floorMod(a, b))
    case "remainder" =>
      if args.length != 2 then throw errAt(pos, "remainder requires 2 arguments")
      Value.VNum(asNum(args(0), pos) % asNum(args(1), pos))
    case "quotient" =>
      if args.length != 2 then throw errAt(pos, "quotient requires 2 arguments")
      val a = asNum(args(0), pos)
      val b = asNum(args(1), pos)
      Value.VNum((a.toDouble / b.toDouble).toLong)
    case "min" =>
      if args.isEmpty then throw errAt(pos, "min requires at least 1 argument")
      Value.VNum(args.map(v => asNum(v, pos)).min)
    case "max" =>
      if args.isEmpty then throw errAt(pos, "max requires at least 1 argument")
      Value.VNum(args.map(v => asNum(v, pos)).max)
    case "expt" =>
      if args.length != 2 then throw errAt(pos, "expt requires 2 arguments")
      val base = asNum(args(0), pos)
      val exp  = asNum(args(1), pos)
      Value.VNum(math.pow(base.toDouble, exp.toDouble).toLong)
    case _ => throw errAt(pos, s"unknown numeric op: $name")

  private def applyNumericPredicates(
    name: String,
    args: List[Value],
    pos: Pos
  ): Value =
    if args.length != 1 then throw errAt(pos, s"$name requires 1 argument")
    val n = asNum(args.head, pos)
    val result = name match
      case "zero?"     => n == 0
      case "positive?" => n > 0
      case "negative?" => n < 0
      case "odd?"      => n % 2 != 0
      case "even?"     => n % 2 == 0
      case _           => throw errAt(pos, s"unknown predicate: $name")
    Value.VBool(result)

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
