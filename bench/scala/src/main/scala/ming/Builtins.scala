package ming

import Evaluator.{asNum, display, displayStr, errAt, isTruthy, Env, Pos, Value}

object Builtins:

  def apply(name: String, args: List[Value], pos: Pos, env: Env): Value = name match
    case "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" =>
      applyArithmetic(name, args, pos)
    case "not" | "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append" =>
      applyListOps(name, args, pos)
    case "number?" | "string?" | "boolean?" | "pair?" | "symbol?" | "char?" =>
      applyTypeCheck(name, args, pos)
    case "display" | "write" | "newline" =>
      applyIO(name, args, pos, env)
    case "apply" =>
      applyApply(args, pos, env)
    case "string-append" | "string-length" | "substring" | "string->number" | "number->string" | "symbol->string" |
        "string->symbol" | "string-ref" | "string-copy" | "string-set!" =>
      applyStringOps(name, args, pos)
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
        case _                  => Value.VList(List(args(0), args(1)))
    case "car" =>
      if args.length != 1 then throw errAt(pos, "car requires 1 argument")
      args.head match
        case Value.VList(h :: _) => h
        case _                   => throw errAt(pos, "car: not a pair")
    case "cdr" =>
      if args.length != 1 then throw errAt(pos, "cdr requires 1 argument")
      args.head match
        case Value.VList(_ :: t) => Value.VList(t)
        case _                   => throw errAt(pos, "cdr: not a pair")
    case "null?" =>
      if args.length != 1 then throw errAt(pos, "null? requires 1 argument")
      Value.VBool(args.head == Value.VList(Nil))
    case "list" => Value.VList(args)
    case "length" =>
      if args.length != 1 then throw errAt(pos, "length requires 1 argument")
      args.head match
        case Value.VList(elems) => Value.VNum(elems.length.toLong)
        case _                  => throw errAt(pos, "length: not a list")
    case "append" =>
      args.foldRight(Value.VList(Nil): Value) { (arg, acc) =>
        (arg, acc) match
          case (Value.VList(elems), Value.VList(accElems)) =>
            Value.VList(elems ++ accElems)
          case _ => throw errAt(pos, "append: not a list")
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
      case ("number?", _: Value.VNum)     => true
      case ("number?", _)                 => false
      case ("string?", _: Value.VStr)     => true
      case ("string?", _)                 => false
      case ("boolean?", _: Value.VBool)   => true
      case ("boolean?", _)                => false
      case ("pair?", Value.VList(_ :: _)) => true
      case ("pair?", _)                   => false
      case ("symbol?", _: Value.VSymbol)  => true
      case ("symbol?", _)                 => false
      case ("char?", _: Value.VChar)      => true
      case ("char?", _)                   => false
      case _                              => throw errAt(pos, s"unknown type check: $name")
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

  private def applyStringOps(
    name: String,
    args: List[Value],
    pos: Pos
  ): Value = name match
    case "string-append" =>
      val strs = args.map {
        case Value.VStr(chars) => new String(chars)
        case _                 => throw errAt(pos, "string-append: not a string")
      }
      Value.VStr(strs.mkString.toCharArray)
    case "string-length" =>
      if args.length != 1 then throw errAt(pos, "string-length requires 1 argument")
      args.head match
        case Value.VStr(chars) => Value.VNum(chars.length.toLong)
        case _                 => throw errAt(pos, "string-length: not a string")
    case "substring" =>
      if args.length != 3 then throw errAt(pos, "substring requires 3 arguments")
      (args(0), args(1), args(2)) match
        case (Value.VStr(chars), Value.VNum(start), Value.VNum(end)) =>
          Value.VStr(new String(chars).substring(start.toInt, end.toInt).toCharArray)
        case _ => throw errAt(pos, "substring: invalid arguments")
    case "string->number" =>
      if args.length != 1 then throw errAt(pos, "string->number requires 1 argument")
      args.head match
        case Value.VStr(chars) =>
          val s = new String(chars)
          try Value.VNum(s.toLong)
          catch case _: NumberFormatException => Value.VBool(false)
        case _ => throw errAt(pos, "string->number: not a string")
    case "number->string" =>
      if args.length != 1 then throw errAt(pos, "number->string requires 1 argument")
      args.head match
        case Value.VNum(n) => Value.VStr(n.toString.toCharArray)
        case _             => throw errAt(pos, "number->string: not a number")
    case "symbol->string" =>
      if args.length != 1 then throw errAt(pos, "symbol->string requires 1 argument")
      args.head match
        case Value.VSymbol(n) => Value.VStr(n.toCharArray)
        case _                => throw errAt(pos, "symbol->string: not a symbol")
    case "string->symbol" =>
      if args.length != 1 then throw errAt(pos, "string->symbol requires 1 argument")
      args.head match
        case Value.VStr(chars) => Value.VSymbol(new String(chars))
        case _                 => throw errAt(pos, "string->symbol: not a string")
    case "string-ref" =>
      if args.length != 2 then throw errAt(pos, "string-ref requires 2 arguments")
      (args(0), args(1)) match
        case (Value.VStr(chars), Value.VNum(i)) => Value.VChar(chars(i.toInt))
        case _                                  => throw errAt(pos, "string-ref: invalid arguments")
    case "string-copy" =>
      if args.length != 1 then throw errAt(pos, "string-copy requires 1 argument")
      args.head match
        case Value.VStr(chars) => Value.VStr(chars.clone())
        case _                 => throw errAt(pos, "string-copy: not a string")
    case "string-set!" =>
      if args.length != 3 then throw errAt(pos, "string-set! requires 3 arguments")
      (args(0), args(1), args(2)) match
        case (Value.VStr(chars), Value.VNum(i), Value.VChar(c)) =>
          chars(i.toInt) = c
          Value.VVoid
        case _ => throw errAt(pos, "string-set!: invalid arguments")
    case _ => throw errAt(pos, s"unknown string op: $name")

  private def applyApply(args: List[Value], pos: Pos, env: Env): Value =
    if args.length < 2 then throw errAt(pos, "apply requires at least 2 arguments")
    val func = args.head
    val lastArg = args.last match
      case Value.VList(elems) => elems
      case _                  => throw errAt(pos, "apply: last argument must be a list")
    val prefixArgs = args.slice(1, args.length - 1)
    val allArgs    = prefixArgs ++ lastArg
    Evaluator.applyFunc(func, allArgs, pos, env)
