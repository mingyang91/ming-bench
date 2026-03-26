package ming

/** Compound accessor (caar, cadr, …), pair mutation (set-car!, set-cdr!), member, reverse, assv. */
private[ming] object PairBuiltins:

  import Builtins.typeCheck

  private def requireList(v: SchemeVal, name: String): List[SchemeVal] =
    SchemeListOps.toScalaList(v) match
      case Some(elems) => elems
      case None        => throw new EvalError(s"$name: expected list")

  def install(env: Env): Unit =
    installCxr(env)
    installMutation(env)
    installAssoc(env)
    installSearch(env)
    installMemq(env)

  private def installCxr(env: Env): Unit =
    def car(v: SchemeVal): SchemeVal = v match
      case SchemePair(h, _)   => h
      case SchemeList(h :: _) => h
      case _                  => throw new EvalError("car: expected pair")
    def cdr(v: SchemeVal): SchemeVal = v match
      case SchemePair(_, d)   => d
      case SchemeList(_ :: t) => SchemeListOps.makeList(t)
      case _                  => throw new EvalError("cdr: expected pair")

    env.set(
      "caar",
      SchemeBuiltin(
        "caar",
        { case a :: Nil => car(car(a)); case _ => throw new EvalError("caar: expected 1 argument") }
      )
    )
    env.set(
      "cadr",
      SchemeBuiltin(
        "cadr",
        { case a :: Nil => car(cdr(a)); case _ => throw new EvalError("cadr: expected 1 argument") }
      )
    )
    env.set(
      "cdar",
      SchemeBuiltin(
        "cdar",
        { case a :: Nil => cdr(car(a)); case _ => throw new EvalError("cdar: expected 1 argument") }
      )
    )
    env.set(
      "cddr",
      SchemeBuiltin(
        "cddr",
        { case a :: Nil => cdr(cdr(a)); case _ => throw new EvalError("cddr: expected 1 argument") }
      )
    )
    env.set(
      "caddr",
      SchemeBuiltin(
        "caddr",
        { case a :: Nil => car(cdr(cdr(a))); case _ => throw new EvalError("caddr: expected 1 argument") }
      )
    )
    env.set(
      "cadddr",
      SchemeBuiltin(
        "cadddr",
        { case a :: Nil => car(cdr(cdr(cdr(a)))); case _ => throw new EvalError("cadddr: expected 1 argument") }
      )
    )

  private[ming] def pairCar(v: SchemeVal): Option[SchemeVal] = v match
    case SchemePair(h, _)   => Some(h)
    case SchemeList(h :: _) => Some(h)
    case _                  => None

  private def installAssoc(env: Env): Unit =
    env.set(
      "assv",
      SchemeBuiltin(
        "assv",
        args =>
          if args.size != 2 then throw new EvalError("assv: expected 2 arguments")
          val key   = args(0)
          val elems = requireList(args(1), "assv")
          elems
            .collectFirst {
              case entry if pairCar(entry).exists(h => ListBuiltins.schemeEqv(h, key)) => entry
            }
            .getOrElse(SchemeBool(false))
      )
    )

  private def installSearch(env: Env): Unit =
    env.set(
      "member",
      SchemeBuiltin(
        "member",
        args =>
          if args.size != 2 then throw new EvalError("member: expected 2 arguments")
          val key              = args(0)
          var curr             = args(1)
          var found: SchemeVal = SchemeBool(false)
          var going            = true
          while going do
            curr match
              case SchemeList(Nil) => going = false
              case p: SchemePair =>
                if ListBuiltins.schemeEqual(p.car, key) then
                  found = p
                  going = false
                else curr = p.cdr
              case SchemeList(elems) =>
                elems.indexWhere(e => ListBuiltins.schemeEqual(e, key)) match
                  case -1 => going = false
                  case i  => found = SchemeListOps.makeList(elems.drop(i)); going = false
              case _ => throw new EvalError("member: expected list")
          found
      )
    )

    env.set(
      "reverse",
      SchemeBuiltin(
        "reverse",
        args =>
          if args.size != 1 then throw new EvalError("reverse: expected 1 argument")
          val elems = requireList(args.head, "reverse")
          SchemeListOps.makeList(elems.reverse)
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

  private def schemeEq(a: SchemeVal, b: SchemeVal): Boolean =
    (a, b) match
      case (SchemeSymbol(x), SchemeSymbol(y)) => x == y
      case (SchemeBool(x), SchemeBool(y))     => x == y
      case (SchemeInt(x), SchemeInt(y))       => x == y
      case (SchemeList(Nil), SchemeList(Nil)) => true
      case (a, b)                             => a eq b

  private def searchList(args: List[SchemeVal], name: String, cmp: (SchemeVal, SchemeVal) => Boolean): SchemeVal =
    if args.size != 2 then throw new EvalError(s"$name: expected 2 arguments")
    val key              = args(0)
    var curr             = args(1)
    var found: SchemeVal = SchemeBool(false)
    var going            = true
    while going do
      curr match
        case SchemeList(Nil) => going = false
        case p: SchemePair =>
          if cmp(p.car, key) then
            found = p
            going = false
          else curr = p.cdr
        case SchemeList(elems) =>
          elems.indexWhere(e => cmp(e, key)) match
            case -1 => going = false
            case i  => found = SchemeListOps.makeList(elems.drop(i)); going = false
        case _ => throw new EvalError(s"$name: expected list")
    found

  private def installMemq(env: Env): Unit =
    env.set(
      "memq",
      SchemeBuiltin("memq", args => searchList(args, "memq", schemeEq))
    )
    env.set(
      "memv",
      SchemeBuiltin("memv", args => searchList(args, "memv", ListBuiltins.schemeEqv))
    )
    env.set(
      "assq",
      SchemeBuiltin(
        "assq",
        args =>
          if args.size != 2 then throw new EvalError("assq: expected 2 arguments")
          val key   = args(0)
          val elems = requireList(args(1), "assq")
          elems
            .collectFirst {
              case entry if pairCar(entry).exists(h => schemeEq(h, key)) => entry
            }
            .getOrElse(SchemeBool(false))
      )
    )
