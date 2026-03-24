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
        case List(a, b) => SchemeVal.PairVal(new MutablePair(a, b))
        case args =>
          throw new EvalError(s"cons: expected 2 arguments, got ${args.length}")
      }
    ),
    "car" -> SchemeVal.BuiltinProc(
      "car",
      {
        case List(SchemeVal.PairVal(p))      => p.car
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
        case List(SchemeVal.PairVal(p))      => p.cdr
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
    "list" -> SchemeVal.BuiltinProc("list", args => SchemeVal.schemeList(args)),
    "length" -> SchemeVal.BuiltinProc(
      "length",
      {
        case List(v @ (SchemeVal.PairVal(_) | SchemeVal.ListVal(_))) =>
          SchemeVal.IntVal(SchemeVal.toScalaList(v).length.toLong)
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
        if args.isEmpty then SchemeVal.ListVal(Nil)
        else
          val lists = args.map {
            case v @ (SchemeVal.PairVal(_) | SchemeVal.ListVal(_)) => SchemeVal.toScalaList(v)
            case other =>
              throw new EvalError(s"append: expected list, got ${other.display}")
          }
          SchemeVal.schemeList(lists.flatten)
    ),
    "set-car!" -> SchemeVal.BuiltinProc(
      "set-car!",
      {
        case List(SchemeVal.PairVal(p), v) =>
          p.car = v
          SchemeVal.Void
        case List(other, _) =>
          throw new EvalError(s"set-car!: expected pair, got ${other.display}")
        case args =>
          throw new EvalError(s"set-car!: expected 2 arguments, got ${args.length}")
      }
    ),
    "set-cdr!" -> SchemeVal.BuiltinProc(
      "set-cdr!",
      {
        case List(SchemeVal.PairVal(p), v) =>
          p.cdr = v
          SchemeVal.Void
        case List(other, _) =>
          throw new EvalError(s"set-cdr!: expected pair, got ${other.display}")
        case args =>
          throw new EvalError(s"set-cdr!: expected 2 arguments, got ${args.length}")
      }
    )
  )

  private[ming] def typePredicate(
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
        case SchemeVal.PairVal(_)      => true
        case SchemeVal.ListVal(_ :: _) => true
        case _                         => false
      }
    ),
    typePredicate("string?", _.isInstanceOf[SchemeVal.StrVal]),
    typePredicate("symbol?", _.isInstanceOf[SchemeVal.SymVal]),
    typePredicate(
      "procedure?",
      {
        case _: SchemeVal.Procedure       => true
        case _: SchemeVal.BuiltinProc     => true
        case _: SchemeVal.CaseLambda      => true
        case _: SchemeVal.ContinuationVal => true
        case _                            => false
      }
    )
  )

  def makeGlobalEnv(): Env =
    val env = new Env(mutable.Map.empty, None)
    val allBuiltins = arithmeticBuiltins
      ++ comparisonBuiltins
      ++ logicBuiltins
      ++ listBuiltins
      ++ PairAccessorBuiltins.all
      ++ typePredicateBuiltins
      ++ IOBuiltins.all
      ++ StringBuiltins.all
      ++ ApplyBuiltins.applyBuiltin
      ++ NumericBuiltins.numericBuiltins
      ++ NumericBuiltins.mathBuiltins
      ++ NumericBuiltins.exactnessBuiltins
      ++ ListSearchBuiltins.all
      ++ VectorBuiltins.vectorBuiltins
      ++ VectorBuiltins.equalityBuiltins
      ++ ApplyBuiltins.callccBuiltin
      ++ ApplyBuiltins.valuesBuiltins
    val syntaxBuiltins: List[(String, SchemeVal)] = List(
      "syntax->datum" -> SchemeVal.BuiltinProc(
        "syntax->datum",
        {
          case List(SchemeVal.SyntaxObj(e)) => EvalHelpers.exprToVal(e)
          case List(other) => throw new EvalError(s"syntax->datum: expected syntax object, got ${other.display}")
          case args        => throw new EvalError(s"syntax->datum: expected 1 argument, got ${args.length}")
        }
      ),
      "datum->syntax" -> SchemeVal.BuiltinProc(
        "datum->syntax",
        {
          case List(_, datum) => SchemeVal.SyntaxObj(EvalHelpers.valToExpr(datum))
          case args           => throw new EvalError(s"datum->syntax: expected 2 arguments, got ${args.length}")
        }
      )
    )
    for (name, proc) <- allBuiltins ++ syntaxBuiltins do env.define(name, proc)
    env
