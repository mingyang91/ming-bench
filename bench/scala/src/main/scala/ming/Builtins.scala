package ming

object Builtins:

  private def requireNums(args: List[SchemeVal], name: String): List[Long] =
    args.map {
      case SchemeVal.IntVal(n) => n
      case other               => throw new EvalError(s"$name: expected number, got ${SchemeVal.display(other)}")
    }

  def register(env: Env, output: StringBuilder = new StringBuilder): Unit =
    registerArithmetic(env)
    registerComparison(env)
    registerListOps(env)
    registerPredicates(env)
    registerApply(env)
    StringBuiltins.register(env, output)

  private def registerApply(env: Env): Unit =
    env.define(
      "apply",
      SchemeVal.BuiltinProc(
        "apply",
        args =>
          if args.size < 2 then throw new EvalError("apply: expected at least 2 arguments")
          val proc = args.head
          val lastArg = args.last match
            case SchemeVal.SList(elems) => elems
            case other => throw new EvalError(s"apply: last argument must be a list, got ${SchemeVal.display(other)}")
          val prefixArgs = args.slice(1, args.size - 1)
          Evaluator.apply(proc, prefixArgs ++ lastArg)
      )
    )

  private def registerArithmetic(env: Env): Unit =
    env.define(
      "+",
      SchemeVal.BuiltinProc(
        "+",
        args =>
          val nums = requireNums(args, "+")
          SchemeVal.IntVal(nums.sum)
      )
    )
    env.define(
      "-",
      SchemeVal.BuiltinProc(
        "-",
        args =>
          val nums = requireNums(args, "-")
          if nums.isEmpty then throw new EvalError("-: expected at least 1 argument")
          else if nums.size == 1 then SchemeVal.IntVal(-nums.head)
          else SchemeVal.IntVal(nums.tail.foldLeft(nums.head)(_ - _))
      )
    )
    env.define(
      "*",
      SchemeVal.BuiltinProc(
        "*",
        args =>
          val nums = requireNums(args, "*")
          SchemeVal.IntVal(nums.product)
      )
    )
    env.define(
      "/",
      SchemeVal.BuiltinProc(
        "/",
        args =>
          val nums = requireNums(args, "/")
          if nums.size < 2 then throw new EvalError("/: expected at least 2 arguments")
          if nums.tail.contains(0L) then throw new EvalError("division by zero")
          SchemeVal.IntVal(nums.tail.foldLeft(nums.head)(_ / _))
      )
    )

  private def registerComparison(env: Env): Unit =
    env.define(
      "<",
      SchemeVal.BuiltinProc(
        "<",
        args =>
          val nums = requireNums(args, "<")
          SchemeVal.BoolVal(nums.zip(nums.tail).forall((a, b) => a < b))
      )
    )
    env.define(
      ">",
      SchemeVal.BuiltinProc(
        ">",
        args =>
          val nums = requireNums(args, ">")
          SchemeVal.BoolVal(nums.zip(nums.tail).forall((a, b) => a > b))
      )
    )
    env.define(
      "=",
      SchemeVal.BuiltinProc(
        "=",
        args =>
          val nums = requireNums(args, "=")
          SchemeVal.BoolVal(nums.zip(nums.tail).forall((a, b) => a == b))
      )
    )
    env.define(
      "<=",
      SchemeVal.BuiltinProc(
        "<=",
        args =>
          val nums = requireNums(args, "<=")
          SchemeVal.BoolVal(nums.zip(nums.tail).forall((a, b) => a <= b))
      )
    )
    env.define(
      ">=",
      SchemeVal.BuiltinProc(
        ">=",
        args =>
          val nums = requireNums(args, ">=")
          SchemeVal.BoolVal(nums.zip(nums.tail).forall((a, b) => a >= b))
      )
    )

  private def registerListOps(env: Env): Unit =
    env.define(
      "cons",
      SchemeVal.BuiltinProc(
        "cons",
        args =>
          if args.size != 2 then throw new EvalError("cons: expected 2 arguments")
          args(1) match
            case SchemeVal.SList(elems) => SchemeVal.SList(args(0) :: elems)
            case _                      => SchemeVal.SList(List(args(0), args(1)))
      )
    )
    env.define(
      "car",
      SchemeVal.BuiltinProc(
        "car",
        args =>
          if args.size != 1 then throw new EvalError("car: expected 1 argument")
          args.head match
            case SchemeVal.SList(elems) if elems.nonEmpty => elems.head
            case _                                        => throw new EvalError("car: expected pair")
      )
    )
    env.define(
      "cdr",
      SchemeVal.BuiltinProc(
        "cdr",
        args =>
          if args.size != 1 then throw new EvalError("cdr: expected 1 argument")
          args.head match
            case SchemeVal.SList(elems) if elems.nonEmpty => SchemeVal.SList(elems.tail)
            case _                                        => throw new EvalError("cdr: expected pair")
      )
    )
    env.define("list", SchemeVal.BuiltinProc("list", args => SchemeVal.SList(args)))
    env.define(
      "length",
      SchemeVal.BuiltinProc(
        "length",
        args =>
          if args.size != 1 then throw new EvalError("length: expected 1 argument")
          args.head match
            case SchemeVal.SList(elems) => SchemeVal.IntVal(elems.size.toLong)
            case _                      => throw new EvalError("length: expected list")
      )
    )
    env.define(
      "null?",
      SchemeVal.BuiltinProc(
        "null?",
        args =>
          if args.size != 1 then throw new EvalError("null?: expected 1 argument")
          args.head match
            case SchemeVal.SList(elems) => SchemeVal.BoolVal(elems.isEmpty)
            case _                      => SchemeVal.BoolVal(false)
      )
    )
    env.define(
      "append",
      SchemeVal.BuiltinProc(
        "append",
        args =>
          if args.isEmpty then SchemeVal.SList(Nil)
          else
            val lists = args.map {
              case SchemeVal.SList(elems) => elems
              case other =>
                throw new EvalError(s"append: expected list, got ${SchemeVal.display(other)}")
            }
            SchemeVal.SList(lists.flatten)
      )
    )

  private def registerPredicates(env: Env): Unit =
    env.define(
      "number?",
      SchemeVal.BuiltinProc(
        "number?",
        args =>
          if args.size != 1 then throw new EvalError("number?: expected 1 argument")
          args.head match
            case SchemeVal.IntVal(_) => SchemeVal.BoolVal(true)
            case _                   => SchemeVal.BoolVal(false)
      )
    )
    env.define(
      "string?",
      SchemeVal.BuiltinProc(
        "string?",
        args =>
          if args.size != 1 then throw new EvalError("string?: expected 1 argument")
          args.head match
            case SchemeVal.StringVal(_) => SchemeVal.BoolVal(true)
            case _                      => SchemeVal.BoolVal(false)
      )
    )
    env.define(
      "boolean?",
      SchemeVal.BuiltinProc(
        "boolean?",
        args =>
          if args.size != 1 then throw new EvalError("boolean?: expected 1 argument")
          args.head match
            case SchemeVal.BoolVal(_) => SchemeVal.BoolVal(true)
            case _                    => SchemeVal.BoolVal(false)
      )
    )
    env.define(
      "pair?",
      SchemeVal.BuiltinProc(
        "pair?",
        args =>
          if args.size != 1 then throw new EvalError("pair?: expected 1 argument")
          args.head match
            case SchemeVal.SList(elems) => SchemeVal.BoolVal(elems.nonEmpty)
            case _                      => SchemeVal.BoolVal(false)
      )
    )
    env.define(
      "symbol?",
      SchemeVal.BuiltinProc(
        "symbol?",
        args =>
          if args.size != 1 then throw new EvalError("symbol?: expected 1 argument")
          args.head match
            case SchemeVal.Symbol(_) => SchemeVal.BoolVal(true)
            case _                   => SchemeVal.BoolVal(false)
      )
    )
    env.define(
      "not",
      SchemeVal.BuiltinProc(
        "not",
        args =>
          if args.size != 1 then throw new EvalError("not: expected 1 argument")
          args.head match
            case SchemeVal.BoolVal(false) => SchemeVal.BoolVal(true)
            case _                        => SchemeVal.BoolVal(false)
      )
    )
    env.define(
      "char?",
      SchemeVal.BuiltinProc(
        "char?",
        args =>
          if args.size != 1 then throw new EvalError("char?: expected 1 argument")
          args.head match
            case SchemeVal.CharVal(_) => SchemeVal.BoolVal(true)
            case _                    => SchemeVal.BoolVal(false)
      )
    )
