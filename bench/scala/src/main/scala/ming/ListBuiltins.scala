package ming

private[ming] object ListBuiltins:

  import Builtins.typeCheck

  private[ming] def schemeEqual(a: SchemeVal, b: SchemeVal): Boolean =
    (a, b) match
      case (SchemeInt(x), SchemeInt(y))       => x == y
      case (SchemeBool(x), SchemeBool(y))     => x == y
      case (SchemeString(x), SchemeString(y)) => x == y
      case (SchemeChar(x), SchemeChar(y))     => x == y
      case (SchemeSymbol(x), SchemeSymbol(y)) => x == y
      case (SchemeList(xs), SchemeList(ys)) =>
        xs.length == ys.length && xs
          .zip(ys)
          .forall((a, b) => schemeEqual(a, b))
      case (va: SchemeVector, vb: SchemeVector) =>
        va.elems.length == vb.elems.length && va.elems
          .zip(vb.elems)
          .forall((a, b) => schemeEqual(a, b))
      case (SchemePair(a1, d1), SchemePair(a2, d2)) =>
        schemeEqual(a1, a2) && schemeEqual(d1, d2)
      case (SchemeVoid, SchemeVoid) => true
      case _                        => a eq b

  def install(env: Env): Unit =
    installCore(env)
    installUtils(env)
    installEquality(env)
    VectorBuiltins.install(env)
    installMap(env)

  private def installCore(env: Env): Unit =
    env.set(
      "cons",
      SchemeBuiltin(
        "cons",
        args =>
          if args.size != 2 then throw new EvalError("cons: expected 2 arguments")
          args(1) match
            case SchemeList(elems) => SchemeList(args(0) :: elems)
            case _                 => SchemePair(args(0), args(1))
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
            case SchemeList(_ :: t) => SchemeList(t)
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

    env.set("list", SchemeBuiltin("list", args => SchemeList(args)))

    env.set(
      "length",
      SchemeBuiltin(
        "length",
        args =>
          if args.size != 1 then throw new EvalError("length: expected 1 argument")
          args.head match
            case SchemeList(elems) => SchemeInt(elems.size.toLong)
            case _                 => throw new EvalError("length: expected list")
      )
    )

    env.set(
      "append",
      SchemeBuiltin(
        "append",
        args =>
          val result = args.foldLeft(List.empty[SchemeVal]) { (acc, v) =>
            v match
              case SchemeList(elems) => acc ++ elems
              case _                 => throw new EvalError("append: expected list")
          }
          SchemeList(result)
      )
    )

  private def installUtils(env: Env): Unit =
    env.set(
      "list?",
      typeCheck("list?") {
        case SchemeList(_) => true
        case _             => false
      }
    )
    env.set(
      "list-ref",
      SchemeBuiltin(
        "list-ref",
        args =>
          if args.size != 2 then throw new EvalError("list-ref: expected 2 arguments")
          (args(0), args(1)) match
            case (SchemeList(elems), SchemeInt(i)) =>
              if i < 0 || i >= elems.size then throw new EvalError("list-ref: index out of bounds")
              elems(i.toInt)
            case _ =>
              throw new EvalError("list-ref: expected list and integer")
      )
    )
    env.set(
      "list-tail",
      SchemeBuiltin(
        "list-tail",
        args =>
          if args.size != 2 then throw new EvalError("list-tail: expected 2 arguments")
          (args(0), args(1)) match
            case (SchemeList(elems), SchemeInt(i)) =>
              if i < 0 || i > elems.size then throw new EvalError("list-tail: index out of bounds")
              SchemeList(elems.drop(i.toInt))
            case _ =>
              throw new EvalError("list-tail: expected list and integer")
      )
    )
    env.set(
      "assoc",
      SchemeBuiltin(
        "assoc",
        args =>
          if args.size != 2 then throw new EvalError("assoc: expected 2 arguments")
          val key = args(0)
          args(1) match
            case SchemeList(elems) =>
              elems
                .collectFirst {
                  case entry @ SchemeList(k :: _) if schemeEqual(k, key) =>
                    entry
                }
                .getOrElse(SchemeBool(false))
            case _ => throw new EvalError("assoc: expected list")
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
          val proc = args.head
          val lists = args.tail.map {
            case SchemeList(elems) => elems
            case _                 => throw new EvalError("map: expected list")
          }
          val len = lists.head.size
          val result = (0 until len).map { i =>
            val elems = lists.map(_(i))
            Evaluator.applyProc(proc, elems)
          }.toList
          SchemeList(result)
      )
    )
