package ming

import scala.collection.mutable

object Builtins:

  private def requireNums(name: String, args: List[SchemeVal]): List[Long] =
    args.map {
      case SchemeVal.IntVal(n) => n
      case other               => throw new EvalError(s"$name: expected number, got ${other.display}")
    }

  private def isTruthy(v: SchemeVal): Boolean = v match
    case SchemeVal.BoolVal(false) => false
    case _                        => true

  private def numericCmp(
    name: String,
    op: (Long, Long) => Boolean
  ): (String, SchemeVal) =
    name -> SchemeVal.BuiltinProc(
      name,
      args =>
        val nums = requireNums(name, args)
        if nums.length < 2 then throw new EvalError(s"$name: expected at least 2 arguments")
        SchemeVal.BoolVal(nums.sliding(2).forall(w => op(w(0), w(1))))
    )

  private def arithmeticBuiltins: List[(String, SchemeVal)] = List(
    "+" -> SchemeVal.BuiltinProc(
      "+",
      args => SchemeVal.IntVal(requireNums("+", args).sum)
    ),
    "-" -> SchemeVal.BuiltinProc(
      "-",
      args =>
        val nums = requireNums("-", args)
        if nums.isEmpty then throw new EvalError("-: expected at least 1 argument")
        if nums.length == 1 then SchemeVal.IntVal(-nums.head)
        else SchemeVal.IntVal(nums.reduce(_ - _))
    ),
    "*" -> SchemeVal.BuiltinProc(
      "*",
      args => SchemeVal.IntVal(requireNums("*", args).product)
    ),
    "/" -> SchemeVal.BuiltinProc(
      "/",
      args =>
        val nums = requireNums("/", args)
        if nums.isEmpty then throw new EvalError("/: expected at least 1 argument")
        if nums.length == 1 then SchemeVal.IntVal(1 / nums.head)
        else
          nums.tail.foreach(d => if d == 0 then throw new EvalError("division by zero"))
          SchemeVal.IntVal(nums.reduce(_ / _))
    )
  )

  private def comparisonBuiltins: List[(String, SchemeVal)] = List(
    numericCmp("<", _ < _),
    numericCmp(">", _ > _),
    numericCmp("=", _ == _),
    numericCmp("<=", _ <= _),
    numericCmp(">=", _ >= _)
  )

  private def logicBuiltins: List[(String, SchemeVal)] = List(
    "not" -> SchemeVal.BuiltinProc(
      "not",
      {
        case List(arg) => SchemeVal.BoolVal(!isTruthy(arg))
        case args =>
          throw new EvalError(s"not: expected 1 argument, got ${args.length}")
      }
    )
  )

  private def listBuiltins: List[(String, SchemeVal)] = List(
    "cons" -> SchemeVal.BuiltinProc(
      "cons",
      {
        case List(a, SchemeVal.ListVal(elems)) => SchemeVal.ListVal(a :: elems)
        case List(a, b)                        => SchemeVal.ListVal(List(a, b))
        case args =>
          throw new EvalError(s"cons: expected 2 arguments, got ${args.length}")
      }
    ),
    "car" -> SchemeVal.BuiltinProc(
      "car",
      {
        case List(SchemeVal.ListVal(h :: _)) => h
        case List(SchemeVal.ListVal(Nil)) =>
          throw new EvalError("car: empty list")
        case List(other) =>
          throw new EvalError(s"car: expected pair, got ${other.display}")
        case args =>
          throw new EvalError(s"car: expected 1 argument, got ${args.length}")
      }
    ),
    "cdr" -> SchemeVal.BuiltinProc(
      "cdr",
      {
        case List(SchemeVal.ListVal(_ :: t)) => SchemeVal.ListVal(t)
        case List(SchemeVal.ListVal(Nil)) =>
          throw new EvalError("cdr: empty list")
        case List(other) =>
          throw new EvalError(s"cdr: expected pair, got ${other.display}")
        case args =>
          throw new EvalError(s"cdr: expected 1 argument, got ${args.length}")
      }
    ),
    "null?" -> SchemeVal.BuiltinProc(
      "null?",
      {
        case List(SchemeVal.ListVal(Nil)) => SchemeVal.BoolVal(true)
        case List(_)                      => SchemeVal.BoolVal(false)
        case args =>
          throw new EvalError(s"null?: expected 1 argument, got ${args.length}")
      }
    ),
    "list" -> SchemeVal.BuiltinProc("list", args => SchemeVal.ListVal(args)),
    "length" -> SchemeVal.BuiltinProc(
      "length",
      {
        case List(SchemeVal.ListVal(elems)) =>
          SchemeVal.IntVal(elems.length.toLong)
        case List(other) =>
          throw new EvalError(s"length: expected list, got ${other.display}")
        case args =>
          throw new EvalError(
            s"length: expected 1 argument, got ${args.length}"
          )
      }
    ),
    "append" -> SchemeVal.BuiltinProc(
      "append",
      args =>
        val lists = args.map {
          case SchemeVal.ListVal(elems) => elems
          case other =>
            throw new EvalError(s"append: expected list, got ${other.display}")
        }
        SchemeVal.ListVal(lists.flatten)
    )
  )

  private def typePredicate(
    name: String,
    test: SchemeVal => Boolean
  ): (String, SchemeVal) =
    name -> SchemeVal.BuiltinProc(
      name,
      {
        case List(v) => SchemeVal.BoolVal(test(v))
        case args =>
          throw new EvalError(s"$name: expected 1 argument, got ${args.length}")
      }
    )

  private def typePredicateBuiltins: List[(String, SchemeVal)] = List(
    typePredicate("boolean?", _.isInstanceOf[SchemeVal.BoolVal]),
    typePredicate("number?", _.isInstanceOf[SchemeVal.IntVal]),
    typePredicate(
      "pair?",
      {
        case SchemeVal.ListVal(_ :: _) => true
        case _                         => false
      }
    ),
    typePredicate("string?", _.isInstanceOf[SchemeVal.StrVal]),
    typePredicate("symbol?", _.isInstanceOf[SchemeVal.SymVal])
  )

  private def ioBuiltins: List[(String, SchemeVal)] = List(
    "display" -> SchemeVal.BuiltinProc(
      "display",
      {
        case List(v) =>
          Evaluator.outputBuffer.get().append(v.displayStr)
          SchemeVal.Void
        case args => throw new EvalError(s"display: expected 1 argument, got ${args.length}")
      }
    ),
    "write" -> SchemeVal.BuiltinProc(
      "write",
      {
        case List(v) =>
          Evaluator.outputBuffer.get().append(v.writeStr)
          SchemeVal.Void
        case args => throw new EvalError(s"write: expected 1 argument, got ${args.length}")
      }
    ),
    "newline" -> SchemeVal.BuiltinProc(
      "newline",
      {
        case Nil =>
          Evaluator.outputBuffer.get().append("\n")
          SchemeVal.Void
        case args => throw new EvalError(s"newline: expected 0 arguments, got ${args.length}")
      }
    )
  )

  private def stringBuiltins: List[(String, SchemeVal)] = List(
    "string-append" -> SchemeVal.BuiltinProc(
      "string-append",
      args =>
        val strs = args.map {
          case SchemeVal.StrVal(s) => s
          case other               => throw new EvalError(s"string-append: expected string, got ${other.display}")
        }
        SchemeVal.StrVal(strs.mkString)
    ),
    "string-length" -> SchemeVal.BuiltinProc(
      "string-length",
      {
        case List(SchemeVal.StrVal(s)) => SchemeVal.IntVal(s.length.toLong)
        case List(other)               => throw new EvalError(s"string-length: expected string, got ${other.display}")
        case args                      => throw new EvalError(s"string-length: expected 1 argument, got ${args.length}")
      }
    ),
    "substring" -> SchemeVal.BuiltinProc(
      "substring",
      {
        case List(SchemeVal.StrVal(s), SchemeVal.IntVal(start), SchemeVal.IntVal(end)) =>
          SchemeVal.StrVal(s.substring(start.toInt, end.toInt))
        case _ => throw new EvalError("substring: expected (string, start, end)")
      }
    ),
    "string->number" -> SchemeVal.BuiltinProc(
      "string->number",
      {
        case List(SchemeVal.StrVal(s)) =>
          try SchemeVal.IntVal(s.toLong)
          catch case _: NumberFormatException => SchemeVal.BoolVal(false)
        case List(other) => throw new EvalError(s"string->number: expected string, got ${other.display}")
        case args        => throw new EvalError(s"string->number: expected 1 argument, got ${args.length}")
      }
    ),
    "number->string" -> SchemeVal.BuiltinProc(
      "number->string",
      {
        case List(SchemeVal.IntVal(n)) => SchemeVal.StrVal(n.toString)
        case List(other)               => throw new EvalError(s"number->string: expected number, got ${other.display}")
        case args => throw new EvalError(s"number->string: expected 1 argument, got ${args.length}")
      }
    ),
    "string-ref" -> SchemeVal.BuiltinProc(
      "string-ref",
      {
        case List(SchemeVal.StrVal(s), SchemeVal.IntVal(i)) =>
          SchemeVal.CharVal(s.charAt(i.toInt))
        case _ => throw new EvalError("string-ref: expected (string, index)")
      }
    ),
    "symbol->string" -> SchemeVal.BuiltinProc(
      "symbol->string",
      {
        case List(SchemeVal.SymVal(n)) => SchemeVal.StrVal(n)
        case List(other)               => throw new EvalError(s"symbol->string: expected symbol, got ${other.display}")
        case args => throw new EvalError(s"symbol->string: expected 1 argument, got ${args.length}")
      }
    ),
    "string->symbol" -> SchemeVal.BuiltinProc(
      "string->symbol",
      {
        case List(SchemeVal.StrVal(s)) => SchemeVal.SymVal(s)
        case List(other)               => throw new EvalError(s"string->symbol: expected string, got ${other.display}")
        case args => throw new EvalError(s"string->symbol: expected 1 argument, got ${args.length}")
      }
    ),
    typePredicate("char?", _.isInstanceOf[SchemeVal.CharVal])
  )

  def makeGlobalEnv(): Env =
    val env = new Env(mutable.Map.empty, None)
    val allBuiltins = arithmeticBuiltins
      ++ comparisonBuiltins
      ++ logicBuiltins
      ++ listBuiltins
      ++ typePredicateBuiltins
      ++ ioBuiltins
      ++ stringBuiltins
    for (name, proc) <- allBuiltins do env.define(name, proc)
    env
