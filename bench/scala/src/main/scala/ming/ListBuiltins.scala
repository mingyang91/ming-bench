package ming

import scala.annotation.tailrec

/** List utilities, pair mutation, and association list builtins. */
object ListBuiltins:

  def register(env: Env): Unit =
    registerPairMutation(env)
    registerListUtils(env)

  private def registerPairMutation(env: Env): Unit =
    env.define(
      "set-car!",
      SchemeVal.BuiltinProc(
        "set-car!",
        args =>
          if args.size != 2 then throw new EvalError("set-car!: expected 2 arguments")
          args.head match
            case SchemeVal.Pair(cell) =>
              cell.car = args(1)
              SchemeVal.Void
            case _ => throw new EvalError("set-car!: expected mutable pair")
      )
    )
    env.define(
      "set-cdr!",
      SchemeVal.BuiltinProc(
        "set-cdr!",
        args =>
          if args.size != 2 then throw new EvalError("set-cdr!: expected 2 arguments")
          args.head match
            case SchemeVal.Pair(cell) =>
              cell.cdr = args(1)
              SchemeVal.Void
            case _ => throw new EvalError("set-cdr!: expected mutable pair")
      )
    )
    env.define(
      "for-each",
      SchemeVal.BuiltinProc(
        "for-each",
        args =>
          if args.size < 2 then throw new EvalError("for-each: expected at least 2 arguments")
          val proc  = args.head
          val lists = args.tail.map(a => SchemeVal.toScalaList(a))
          val len   = lists.head.size
          for i <- 0 until len do
            val mapArgs = lists.map(_(i))
            Evaluator.apply(proc, mapArgs)
          SchemeVal.Void
      )
    )
    env.define(
      "error",
      SchemeVal.BuiltinProc(
        "error",
        args =>
          if args.isEmpty then throw new EvalError("error")
          val msg = args.map(a => SchemeVal.displayOutput(a)).mkString(" ")
          throw new EvalError(msg)
      )
    )

  private def registerListUtils(env: Env): Unit =
    registerListAccess(env)
    registerMembership(env)
    registerAssociation(env)
    env.define(
      "reverse",
      SchemeVal.BuiltinProc(
        "reverse",
        args =>
          if args.size != 1 then throw new EvalError("reverse: expected 1 argument")
          val elems = SchemeVal.toScalaList(args.head)
          elems.reverse.foldRight(SchemeVal.SList(Nil): SchemeVal)((a, acc) => SchemeVal.makePair(a, acc))
      )
    )
    env.define(
      "list?",
      SchemeVal.BuiltinProc(
        "list?",
        args =>
          if args.size != 1 then throw new EvalError("list?: expected 1 argument")
          SchemeVal.BoolVal(SchemeVal.isList(args.head))
      )
    )

  private def registerListAccess(env: Env): Unit =
    env.define(
      "list-ref",
      SchemeVal.BuiltinProc(
        "list-ref",
        args =>
          if args.size != 2 then throw new EvalError("list-ref: expected 2 arguments")
          val idx = args(1) match
            case SchemeVal.IntVal(i) => i.toInt
            case _                   => throw new EvalError("list-ref: expected integer index")
          args(0) match
            case SchemeVal.SList(elems) =>
              if idx < 0 || idx >= elems.size then throw new EvalError("list-ref: index out of bounds")
              elems(idx)
            case SchemeVal.Pair(_) => pairListRef(args(0), idx)
            case _                 => throw new EvalError("list-ref: expected list and integer")
      )
    )
    env.define(
      "list-tail",
      SchemeVal.BuiltinProc(
        "list-tail",
        args =>
          if args.size != 2 then throw new EvalError("list-tail: expected 2 arguments")
          val idx = args(1) match
            case SchemeVal.IntVal(i) => i.toInt
            case _                   => throw new EvalError("list-tail: expected integer index")
          args(0) match
            case SchemeVal.SList(elems) =>
              if idx < 0 || idx > elems.size then throw new EvalError("list-tail: index out of bounds")
              SchemeVal.SList(elems.drop(idx))
            case SchemeVal.Pair(_) => pairListTail(args(0), idx)
            case _                 => throw new EvalError("list-tail: expected list and integer")
      )
    )

  @tailrec
  private def pairListRef(cur: SchemeVal, idx: Int): SchemeVal =
    if idx == 0 then
      cur match
        case SchemeVal.Pair(c) => c.car
        case _                 => throw new EvalError("list-ref: index out of bounds")
    else
      cur match
        case SchemeVal.Pair(c) => pairListRef(c.cdr, idx - 1)
        case _                 => throw new EvalError("list-ref: index out of bounds")

  @tailrec
  private def pairListTail(cur: SchemeVal, idx: Int): SchemeVal =
    if idx == 0 then cur
    else
      cur match
        case SchemeVal.Pair(c) => pairListTail(c.cdr, idx - 1)
        case _                 => throw new EvalError("list-tail: index out of bounds")

  private def registerMembership(env: Env): Unit =
    env.define(
      "memq",
      SchemeVal.BuiltinProc(
        "memq",
        args =>
          if args.size != 2 then throw new EvalError("memq: expected 2 arguments")
          searchList(args(0), args(1), isEq)
      )
    )
    env.define(
      "memv",
      SchemeVal.BuiltinProc(
        "memv",
        args =>
          if args.size != 2 then throw new EvalError("memv: expected 2 arguments")
          searchList(args(0), args(1), isEqv)
      )
    )
    env.define(
      "member",
      SchemeVal.BuiltinProc(
        "member",
        args =>
          if args.size != 2 then throw new EvalError("member: expected 2 arguments")
          searchList(args(0), args(1), SchemeVal.schemeEqual)
      )
    )

  @tailrec
  private def searchList(
    key: SchemeVal,
    cur: SchemeVal,
    eq: (SchemeVal, SchemeVal) => Boolean
  ): SchemeVal =
    cur match
      case SchemeVal.SList(Nil) => SchemeVal.BoolVal(false)
      case SchemeVal.Pair(c) =>
        if eq(key, c.car) then cur
        else searchList(key, c.cdr, eq)
      case SchemeVal.SList(elems) if elems.nonEmpty =>
        if eq(key, elems.head) then cur
        else searchList(key, SchemeVal.SList(elems.tail), eq)
      case _ => SchemeVal.BoolVal(false)

  private def registerAssociation(env: Env): Unit =
    env.define(
      "assoc",
      SchemeVal.BuiltinProc(
        "assoc",
        args =>
          if args.size != 2 then throw new EvalError("assoc: expected 2 arguments")
          assocSearch(args(0), args(1), SchemeVal.schemeEqual)
      )
    )
    env.define(
      "assq",
      SchemeVal.BuiltinProc(
        "assq",
        args =>
          if args.size != 2 then throw new EvalError("assq: expected 2 arguments")
          assocSearch(args(0), args(1), isEq)
      )
    )
    env.define(
      "assv",
      SchemeVal.BuiltinProc(
        "assv",
        args =>
          if args.size != 2 then throw new EvalError("assv: expected 2 arguments")
          assocSearch(args(0), args(1), isEqv)
      )
    )

  private def assocSearch(
    key: SchemeVal,
    alist: SchemeVal,
    eq: (SchemeVal, SchemeVal) => Boolean
  ): SchemeVal =
    val elems = SchemeVal.toScalaList(alist)
    elems
      .collectFirst {
        case found if isPairLike(found) && eq(pairCar(found), key) => found
      }
      .getOrElse(SchemeVal.BoolVal(false))

  private def pairCar(v: SchemeVal): SchemeVal = v match
    case SchemeVal.Pair(c)                                => c.car
    case SchemeVal.SList(elems) if elems.nonEmpty         => elems.head
    case SchemeVal.DottedList(elems, _) if elems.nonEmpty => elems.head
    case _                                                => throw new EvalError("car: not a pair")

  private def isEq(a: SchemeVal, b: SchemeVal): Boolean = (a, b) match
    case (SchemeVal.Symbol(x), SchemeVal.Symbol(y))   => x == y
    case (SchemeVal.IntVal(x), SchemeVal.IntVal(y))   => x == y
    case (SchemeVal.BoolVal(x), SchemeVal.BoolVal(y)) => x == y
    case (SchemeVal.CharVal(x), SchemeVal.CharVal(y)) => x == y
    case _                                            => a eq b

  private def isEqv(a: SchemeVal, b: SchemeVal): Boolean = (a, b) match
    case (SchemeVal.Symbol(x), SchemeVal.Symbol(y))                     => x == y
    case (SchemeVal.IntVal(x), SchemeVal.IntVal(y))                     => x == y
    case (SchemeVal.RationalVal(n1, d1), SchemeVal.RationalVal(n2, d2)) => n1 == n2 && d1 == d2
    case (SchemeVal.FloatVal(x), SchemeVal.FloatVal(y))                 => x == y
    case (SchemeVal.BoolVal(x), SchemeVal.BoolVal(y))                   => x == y
    case (SchemeVal.CharVal(x), SchemeVal.CharVal(y))                   => x == y
    case _                                                              => a eq b

  private def isPairLike(v: SchemeVal): Boolean = v match
    case SchemeVal.Pair(_)                        => true
    case SchemeVal.SList(elems) if elems.nonEmpty => true
    case SchemeVal.DottedList(_, _)               => true
    case _                                        => false
