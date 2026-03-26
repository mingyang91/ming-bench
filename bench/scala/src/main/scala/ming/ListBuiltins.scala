package ming

private[ming] object ListBuiltins:

  import Builtins.typeCheck

  private def requireList(v: SchemeVal, name: String): List[SchemeVal] =
    SchemeListOps.toScalaList(v) match
      case Some(elems) => elems
      case None        => throw new EvalError(s"$name: expected list")

  private[ming] def schemeEqual(a: SchemeVal, b: SchemeVal): Boolean =
    schemeEqualRec(a, b, 0)

  private def schemeEqualRec(a: SchemeVal, b: SchemeVal, depth: Int): Boolean =
    if depth > 100000 then return false // safety limit for cycles
    (a, b) match
      case (SchemeInt(x), SchemeInt(y))       => x == y
      case (SchemeBool(x), SchemeBool(y))     => x == y
      case (SchemeString(x), SchemeString(y)) => x == y
      case (SchemeChar(x), SchemeChar(y))     => x == y
      case (SchemeSymbol(x), SchemeSymbol(y)) => x == y
      case (SchemeList(Nil), SchemeList(Nil)) => true
      case (va: SchemeVector, vb: SchemeVector) =>
        va.elems.length == vb.elems.length && va.elems
          .zip(vb.elems)
          .forall((a, b) => schemeEqualRec(a, b, depth + 1))
      case (SchemeVoid, SchemeVoid) => true
      case _                        =>
        // Handle pair chains / SchemeList equivalently
        val aList = SchemeListOps.toScalaList(a)
        val bList = SchemeListOps.toScalaList(b)
        (aList, bList) match
          case (Some(as), Some(bs)) =>
            as.length == bs.length && as.zip(bs).forall((x, y) => schemeEqualRec(x, y, depth + 1))
          case _ =>
            // Try as pairs
            (a, b) match
              case (SchemePair(a1, d1), SchemePair(a2, d2)) =>
                schemeEqualRec(a1, a2, depth + 1) && schemeEqualRec(d1, d2, depth + 1)
              case _ => a eq b

  def install(env: Env): Unit =
    installCore(env)
    installUtils(env)
    installEquality(env)
    VectorBuiltins.install(env)
    installMap(env)
    PairBuiltins.install(env)

  private def installCore(env: Env): Unit =
    env.set(
      "cons",
      SchemeBuiltin(
        "cons",
        args =>
          if args.size != 2 then throw new EvalError("cons: expected 2 arguments")
          new SchemePair(args(0), args(1))
      )
    )

    env.set(
      "car",
      SchemeBuiltin(
        "car",
        args =>
          if args.size != 1 then throw new EvalError("car: expected 1 argument")
          args.head match
            case SchemeList(h :: _) => h
            case SchemePair(h, _)   => h
            case SchemeList(Nil)    => throw new EvalError("car: empty list")
            case _                  => throw new EvalError("car: expected pair")
      )
    )

    env.set(
      "cdr",
      SchemeBuiltin(
        "cdr",
        args =>
          if args.size != 1 then throw new EvalError("cdr: expected 1 argument")
          args.head match
            case SchemeList(_ :: t) => SchemeListOps.makeList(t)
            case SchemePair(_, d)   => d
            case SchemeList(Nil)    => throw new EvalError("cdr: empty list")
            case _                  => throw new EvalError("cdr: expected pair")
      )
    )

    env.set(
      "null?",
      typeCheck("null?") {
        case SchemeList(Nil) => true
        case _               => false
      }
    )

    env.set("list", SchemeBuiltin("list", args => SchemeListOps.makeList(args)))

    env.set(
      "length",
      SchemeBuiltin(
        "length",
        args =>
          if args.size != 1 then throw new EvalError("length: expected 1 argument")
          val elems = requireList(args.head, "length")
          SchemeInt(elems.size.toLong)
      )
    )

    env.set(
      "append",
      SchemeBuiltin(
        "append",
        args =>
          if args.isEmpty then SchemeList(Nil)
          else if args.size == 1 then args.head
          else
            val allButLast = args.init.flatMap(v => requireList(v, "append"))
            // Last arg can be anything (for append with improper tail)
            args.last match
              case _ if allButLast.isEmpty => args.last
              case _ =>
                val lastElems = SchemeListOps.toScalaList(args.last)
                lastElems match
                  case Some(elems) => SchemeListOps.makeList(allButLast ++ elems)
                  case None        =>
                    // Improper list: build pair chain ending with last arg
                    allButLast.foldRight(args.last)((e, acc) => new SchemePair(e, acc))
      )
    )

  private def installUtils(env: Env): Unit =
    env.set(
      "list?",
      typeCheck("list?")(v => SchemeListOps.isList(v))
    )
    env.set(
      "list-ref",
      SchemeBuiltin(
        "list-ref",
        args =>
          if args.size != 2 then throw new EvalError("list-ref: expected 2 arguments")
          args(1) match
            case SchemeInt(idx) =>
              var curr = args(0)
              var i    = idx
              while i > 0 do
                curr match
                  case SchemePair(_, d)   => curr = d; i -= 1
                  case SchemeList(_ :: t) => curr = SchemeListOps.makeList(t); i -= 1
                  case _                  => throw new EvalError("list-ref: index out of bounds")
              curr match
                case SchemePair(h, _)   => h
                case SchemeList(h :: _) => h
                case _                  => throw new EvalError("list-ref: index out of bounds")
            case _ => throw new EvalError("list-ref: expected integer index")
      )
    )
    env.set(
      "list-tail",
      SchemeBuiltin(
        "list-tail",
        args =>
          if args.size != 2 then throw new EvalError("list-tail: expected 2 arguments")
          args(1) match
            case SchemeInt(idx) =>
              var curr = args(0)
              var i    = idx
              while i > 0 do
                curr match
                  case SchemePair(_, d)   => curr = d; i -= 1
                  case SchemeList(_ :: t) => curr = SchemeListOps.makeList(t); i -= 1
                  case _                  => throw new EvalError("list-tail: index out of bounds")
              curr
            case _ => throw new EvalError("list-tail: expected integer index")
      )
    )
    env.set(
      "assoc",
      SchemeBuiltin(
        "assoc",
        args =>
          if args.size != 2 then throw new EvalError("assoc: expected 2 arguments")
          val key   = args(0)
          val elems = requireList(args(1), "assoc")
          elems
            .collectFirst {
              case entry if SchemeListOps.toScalaList(entry).exists(l => l.nonEmpty && schemeEqual(l.head, key)) =>
                entry
            }
            .getOrElse(SchemeBool(false))
      )
    )

  private[ming] def schemeEqv(a: SchemeVal, b: SchemeVal): Boolean =
    (a, b) match
      case (SchemeInt(x), SchemeInt(y))     => x == y
      case (SchemeFloat(x), SchemeFloat(y)) => x == y
      case (SchemeRational(n1, d1), SchemeRational(n2, d2)) =>
        n1 == n2 && d1 == d2
      case (SchemeBool(x), SchemeBool(y))     => x == y
      case (SchemeChar(x), SchemeChar(y))     => x == y
      case (SchemeSymbol(x), SchemeSymbol(y)) => x == y
      case (SchemeList(Nil), SchemeList(Nil)) => true
      case (SchemeVoid, SchemeVoid)           => true
      case (a, b)                             => a eq b

  private def installEquality(env: Env): Unit =
    env.set(
      "equal?",
      SchemeBuiltin(
        "equal?",
        args =>
          if args.size != 2 then throw new EvalError("equal?: expected 2 arguments")
          SchemeBool(schemeEqual(args(0), args(1)))
      )
    )
    env.set(
      "eq?",
      SchemeBuiltin(
        "eq?",
        args =>
          if args.size != 2 then throw new EvalError("eq?: expected 2 arguments")
          val result = (args(0), args(1)) match
            case (SchemeSymbol(a), SchemeSymbol(b)) => a == b
            case (SchemeBool(a), SchemeBool(b))     => a == b
            case (SchemeInt(a), SchemeInt(b))       => a == b
            case (SchemeList(Nil), SchemeList(Nil)) => true
            case (a, b)                             => a eq b
          SchemeBool(result)
      )
    )
    env.set(
      "eqv?",
      SchemeBuiltin(
        "eqv?",
        args =>
          if args.size != 2 then throw new EvalError("eqv?: expected 2 arguments")
          SchemeBool(schemeEqv(args(0), args(1)))
      )
    )

  private def installMap(env: Env): Unit =
    env.set(
      "map",
      SchemeBuiltin(
        "map",
        args =>
          if args.size < 2 then throw new EvalError("map: expected at least 2 arguments")
          val proc  = args.head
          val lists = args.tail.map(v => requireList(v, "map"))
          val len   = lists.head.size
          val result = (0 until len).map { i =>
            val elems = lists.map(_(i))
            Evaluator.applyProcSafe(proc, elems)
          }.toList
          SchemeListOps.makeList(result)
      )
    )
    env.set(
      "for-each",
      SchemeBuiltin(
        "for-each",
        args =>
          if args.size < 2 then throw new EvalError("for-each: expected at least 2 arguments")
          val proc  = args.head
          val lists = args.tail.map(v => requireList(v, "for-each"))
          val len   = lists.head.size
          for i <- 0 until len do
            val elems = lists.map(_(i))
            Evaluator.applyProcSafe(proc, elems)
          SchemeVoid
      )
    )

  private def installMutation(env: Env): Unit =
    env.set(
      "set-car!",
      SchemeBuiltin(
        "set-car!",
        args =>
          if args.size != 2 then throw new EvalError("set-car!: expected 2 arguments")
          args(0) match
            case p: SchemePair => p.car = args(1); SchemeVoid
            case _             => throw new EvalError("set-car!: expected mutable pair")
      )
    )
    env.set(
      "set-cdr!",
      SchemeBuiltin(
        "set-cdr!",
        args =>
          if args.size != 2 then throw new EvalError("set-cdr!: expected 2 arguments")
          args(0) match
            case p: SchemePair => p.cdr = args(1); SchemeVoid
            case _             => throw new EvalError("set-cdr!: expected mutable pair")
      )
    )
