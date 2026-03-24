package ming

import scala.collection.mutable

object Builtins:

  private def requireNumVals(name: String, args: List[SchemeVal]): List[SchemeVal] =
    args.map(v => SchemeNum.requireNum(name, v))

  private def isTruthy(v: SchemeVal): Boolean = v match
    case SchemeVal.BoolVal(false) => false
    case _                        => true

  private def numericCmp(
    name: String,
    op: (SchemeVal, SchemeVal) => Boolean
  ): (String, SchemeVal) =
    name -> SchemeVal.BuiltinProc(
      name,
      args =>
        val nums = requireNumVals(name, args)
        if nums.length < 2 then throw new EvalError(s"$name: expected at least 2 arguments")
        SchemeVal.BoolVal(nums.sliding(2).forall(w => op(w(0), w(1))))
    )

  private def arithmeticBuiltins: List[(String, SchemeVal)] = List(
    "+" -> SchemeVal.BuiltinProc(
      "+",
      args =>
        val nums = requireNumVals("+", args)
        if nums.isEmpty then SchemeVal.IntVal(0)
        else nums.reduce(SchemeNum.add)
    ),
    "-" -> SchemeVal.BuiltinProc(
      "-",
      args =>
        val nums = requireNumVals("-", args)
        if nums.isEmpty then throw new EvalError("-: expected at least 1 argument")
        if nums.length == 1 then SchemeNum.negate(nums.head)
        else nums.reduce(SchemeNum.sub)
    ),
    "*" -> SchemeVal.BuiltinProc(
      "*",
      args =>
        val nums = requireNumVals("*", args)
        if nums.isEmpty then SchemeVal.IntVal(1)
        else nums.reduce(SchemeNum.mul)
    ),
    "/" -> SchemeVal.BuiltinProc(
      "/",
      args =>
        val nums = requireNumVals("/", args)
        if nums.isEmpty then throw new EvalError("/: expected at least 1 argument")
        if nums.length == 1 then SchemeNum.div(SchemeVal.IntVal(1), nums.head)
        else nums.reduce(SchemeNum.div)
    )
  )

  private def comparisonBuiltins: List[(String, SchemeVal)] = List(
    numericCmp("<", SchemeNum.numLt),
    numericCmp(">", SchemeNum.numGt),
    numericCmp("=", SchemeNum.numEq),
    numericCmp("<=", SchemeNum.numLe),
    numericCmp(">=", SchemeNum.numGe)
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
        case List(a, b)                        => SchemeVal.PairVal(a, b)
        case args =>
          throw new EvalError(s"cons: expected 2 arguments, got ${args.length}")
      }
    ),
    "car" -> SchemeVal.BuiltinProc(
      "car",
      {
        case List(SchemeVal.ListVal(h :: _)) => h
        case List(SchemeVal.PairVal(h, _))   => h
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
        case List(SchemeVal.PairVal(_, t))   => t
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
    typePredicate("number?", _.isNumber),
    typePredicate(
      "integer?",
      {
        case SchemeVal.IntVal(_)   => true
        case SchemeVal.FloatVal(d) => d == d.toLong.toDouble && !d.isInfinite
        case _                     => false
      }
    ),
    typePredicate(
      "rational?",
      {
        case SchemeVal.IntVal(_) | SchemeVal.RatVal(_, _) => true
        case _                                            => false
      }
    ),
    typePredicate(
      "pair?",
      {
        case SchemeVal.ListVal(_ :: _) => true
        case SchemeVal.PairVal(_, _)   => true
        case _                         => false
      }
    ),
    typePredicate("string?", _.isInstanceOf[SchemeVal.StrVal]),
    typePredicate("symbol?", _.isInstanceOf[SchemeVal.SymVal]),
    typePredicate(
      "procedure?",
      {
        case _: SchemeVal.Procedure   => true
        case _: SchemeVal.BuiltinProc => true
        case _: SchemeVal.CaseLambda  => true
        case _                        => false
      }
    )
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

  private def applyBuiltin: List[(String, SchemeVal)] = List(
    "apply" -> SchemeVal.BuiltinProc(
      "apply",
      args =>
        if args.length < 2 then throw new EvalError("apply: expected at least 2 arguments")
        val fn = args.head
        val lastArg = args.last match
          case SchemeVal.ListVal(elems) => elems
          case other => throw new EvalError(s"apply: last argument must be a list, got ${other.display}")
        val prefixArgs = args.slice(1, args.length - 1)
        val allArgs    = prefixArgs ++ lastArg
        Evaluator.applyProc(fn, allArgs)
    )
  )

  def makeGlobalEnv(): Env =
    val env = new Env(mutable.Map.empty, None)
    val allBuiltins = arithmeticBuiltins
      ++ comparisonBuiltins
      ++ logicBuiltins
      ++ listBuiltins
      ++ typePredicateBuiltins
      ++ ioBuiltins
      ++ StringBuiltins.all
      ++ applyBuiltin
      ++ NumericBuiltins.numericBuiltins
      ++ NumericBuiltins.listBuiltins
      ++ NumericBuiltins.exactnessBuiltins
    for (name, proc) <- allBuiltins do env.define(name, proc)
    env
