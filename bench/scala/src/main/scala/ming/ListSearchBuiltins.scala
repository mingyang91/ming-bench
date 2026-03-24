package ming

object ListSearchBuiltins:

  private def schemeEqual(a: SchemeVal, b: SchemeVal): Boolean =
    (a, b) match
      case (SchemeVal.IntVal(x), SchemeVal.IntVal(y))   => x == y
      case (x, y) if x.isNumber && y.isNumber           => SchemeNum.numEq(x, y)
      case (SchemeVal.BoolVal(x), SchemeVal.BoolVal(y)) => x == y
      case (SchemeVal.StrVal(x), SchemeVal.StrVal(y)) =>
        java.util.Arrays.equals(x, y)
      case (SchemeVal.SymVal(x), SchemeVal.SymVal(y))   => x == y
      case (SchemeVal.CharVal(x), SchemeVal.CharVal(y)) => x == y
      case (SchemeVal.ListVal(xs), SchemeVal.ListVal(ys)) =>
        xs.length == ys.length && xs
          .zip(ys)
          .forall((a, b) => schemeEqual(a, b))
      case (SchemeVal.PairVal(p1), SchemeVal.PairVal(p2)) =>
        schemeEqual(p1.car, p2.car) && schemeEqual(p1.cdr, p2.cdr)
      case (SchemeVal.VectorVal(xs), SchemeVal.VectorVal(ys)) =>
        xs.length == ys.length && xs
          .zip(ys)
          .forall((a, b) => schemeEqual(a, b))
      case (SchemeVal.Void, SchemeVal.Void) => true
      case _                                => false

  @annotation.tailrec
  private def memqHelper(
    key: SchemeVal,
    cur: SchemeVal,
    cmp: (SchemeVal, SchemeVal) => Boolean
  ): SchemeVal =
    cur match
      case SchemeVal.PairVal(p) =>
        if cmp(key, p.car) then cur
        else memqHelper(key, p.cdr, cmp)
      case SchemeVal.ListVal(elems) =>
        val idx = elems.indexWhere(e => cmp(key, e))
        if idx >= 0 then SchemeVal.schemeList(elems.drop(idx))
        else SchemeVal.BoolVal(false)
      case _ => SchemeVal.BoolVal(false)

  private def searchBuiltins: List[(String, SchemeVal)] = List(
    "assoc" -> SchemeVal.BuiltinProc(
      "assoc",
      {
        case List(
              key,
              alist @ (SchemeVal.PairVal(_) | SchemeVal.ListVal(_))
            ) =>
          val list = SchemeVal.toScalaList(alist)
          list
            .collectFirst {
              case v @ SchemeVal.PairVal(p) if schemeEqual(key, p.car)  => v
              case v @ SchemeVal.ListVal(k :: _) if schemeEqual(key, k) => v
            }
            .getOrElse(SchemeVal.BoolVal(false))
        case _ => throw new EvalError("assoc: expected (key, alist)")
      }
    ),
    "assv" -> SchemeVal.BuiltinProc(
      "assv",
      {
        case List(
              key,
              alist @ (SchemeVal.PairVal(_) | SchemeVal.ListVal(_))
            ) =>
          val list = SchemeVal.toScalaList(alist)
          list
            .collectFirst {
              case v @ SchemeVal.PairVal(p) if Equality.eqvCheck(key, p.car) =>
                v
              case v @ SchemeVal.ListVal(k :: _) if Equality.eqvCheck(key, k) =>
                v
            }
            .getOrElse(SchemeVal.BoolVal(false))
        case _ => throw new EvalError("assv: expected (key, alist)")
      }
    ),
    "assq" -> SchemeVal.BuiltinProc(
      "assq",
      {
        case List(
              key,
              alist @ (SchemeVal.PairVal(_) | SchemeVal.ListVal(_))
            ) =>
          val list = SchemeVal.toScalaList(alist)
          list
            .collectFirst {
              case v @ SchemeVal.PairVal(p) if SchemeVal.schemeEq(key, p.car) =>
                v
              case v @ SchemeVal.ListVal(k :: _) if SchemeVal.schemeEq(key, k) =>
                v
            }
            .getOrElse(SchemeVal.BoolVal(false))
        case _ => throw new EvalError("assq: expected (key, alist)")
      }
    ),
    "memq" -> SchemeVal.BuiltinProc(
      "memq",
      {
        case List(key, lst) => memqHelper(key, lst, SchemeVal.schemeEq)
        case _              => throw new EvalError("memq: expected (key, list)")
      }
    ),
    "memv" -> SchemeVal.BuiltinProc(
      "memv",
      {
        case List(key, lst) => memqHelper(key, lst, Equality.eqvCheck)
        case _              => throw new EvalError("memv: expected (key, list)")
      }
    ),
    "member" -> SchemeVal.BuiltinProc(
      "member",
      {
        case List(key, lst) => memqHelper(key, lst, schemeEqual)
        case _              => throw new EvalError("member: expected (key, list)")
      }
    )
  )

  private def iterationBuiltins: List[(String, SchemeVal)] = List(
    "map" -> SchemeVal.BuiltinProc(
      "map",
      args =>
        if args.length < 2 then throw new EvalError("map: expected at least 2 arguments")
        val fn = args.head
        val lists = args.tail.map {
          case v @ (SchemeVal.PairVal(_) | SchemeVal.ListVal(_)) =>
            SchemeVal.toScalaList(v)
          case other =>
            throw new EvalError(s"map: expected list, got ${other.display}")
        }
        val len = lists.head.length
        val result = (0 until len).map { i =>
          val callArgs = lists.map(_(i))
          Evaluator.applyProc(fn, callArgs)
        }.toList
        SchemeVal.schemeList(result)
    ),
    "for-each" -> SchemeVal.BuiltinProc(
      "for-each",
      args =>
        if args.length < 2 then throw new EvalError("for-each: expected at least 2 arguments")
        val fn = args.head
        val lists = args.tail.map {
          case v @ (SchemeVal.PairVal(_) | SchemeVal.ListVal(_)) =>
            SchemeVal.toScalaList(v)
          case other =>
            throw new EvalError(
              s"for-each: expected list, got ${other.display}"
            )
        }
        val len = lists.head.length
        for i <- 0 until len do
          val callArgs = lists.map(_(i))
          Evaluator.applyProc(fn, callArgs)
        SchemeVal.Void
    ),
    "reverse" -> SchemeVal.BuiltinProc(
      "reverse",
      {
        case List(v @ (SchemeVal.PairVal(_) | SchemeVal.ListVal(_))) =>
          SchemeVal.schemeList(SchemeVal.toScalaList(v).reverse)
        case List(other) =>
          throw new EvalError(
            s"reverse: expected list, got ${other.display}"
          )
        case args =>
          throw new EvalError(
            s"reverse: expected 1 argument, got ${args.length}"
          )
      }
    )
  )

  private def accessBuiltins: List[(String, SchemeVal)] = List(
    "list-ref" -> SchemeVal.BuiltinProc(
      "list-ref",
      {
        case List(
              v @ (SchemeVal.PairVal(_) | SchemeVal.ListVal(_)),
              SchemeVal.IntVal(i)
            ) =>
          SchemeVal.toScalaList(v)(i.toInt)
        case _ => throw new EvalError("list-ref: expected (list, index)")
      }
    ),
    "list-tail" -> SchemeVal.BuiltinProc(
      "list-tail",
      {
        case List(v, SchemeVal.IntVal(i)) =>
          var cur: SchemeVal = v
          for _ <- 0 until i.toInt do
            cur match
              case SchemeVal.PairVal(p)      => cur = p.cdr
              case SchemeVal.ListVal(_ :: t) => cur = SchemeVal.ListVal(t)
              case _ =>
                throw new EvalError("list-tail: index out of range")
          cur
        case _ => throw new EvalError("list-tail: expected (list, index)")
      }
    ),
    Builtins.typePredicate("list?", v => SchemeVal.isProperList(v))
  )

  private def equalityBuiltins: List[(String, SchemeVal)] = List(
    "equal?" -> SchemeVal.BuiltinProc(
      "equal?",
      {
        case List(a, b) => SchemeVal.BoolVal(schemeEqual(a, b))
        case args =>
          throw new EvalError(
            s"equal?: expected 2 arguments, got ${args.length}"
          )
      }
    ),
    "eq?" -> SchemeVal.BuiltinProc(
      "eq?",
      {
        case List(a, b) => SchemeVal.BoolVal(SchemeVal.schemeEq(a, b))
        case args =>
          throw new EvalError(
            s"eq?: expected 2 arguments, got ${args.length}"
          )
      }
    )
  )

  def all: List[(String, SchemeVal)] =
    accessBuiltins ++ searchBuiltins ++ equalityBuiltins ++ iterationBuiltins
