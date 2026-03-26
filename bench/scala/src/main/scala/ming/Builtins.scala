package ming

private[ming] object Builtins:

  private def gcd(a: Long, b: Long): Long =
    if b == 0 then a else gcd(b, a % b)

  /** Normalize a rational: simplify and keep den > 0. May return SchemeInt if den == 1. */
  private[ming] def makeRational(n: Long, d: Long): SchemeVal =
    if d == 0 then throw new EvalError("/: division by zero")
    val g    = gcd(math.abs(n), math.abs(d))
    val sign = if d < 0 then -1L else 1L
    val sn   = sign * n / g
    val sd   = sign * d / g
    if sd == 1L then SchemeInt(sn) else SchemeRational(sn, sd)

  /** Convert any numeric SchemeVal to Double */
  private def toDouble(v: SchemeVal, op: String): Double = v match
    case SchemeInt(n)         => n.toDouble
    case SchemeRational(n, d) => n.toDouble / d.toDouble
    case SchemeFloat(f)       => f
    case _                    => throw new EvalError(s"$op: expected number, got ${v.display}")

  /** Convert to (numerator, denominator) — exact representation */
  private def toRat(v: SchemeVal, op: String): (Long, Long) = v match
    case SchemeInt(n)         => (n, 1L)
    case SchemeRational(n, d) => (n, d)
    case _                    => throw new EvalError(s"$op: expected exact number, got ${v.display}")

  private def isExact(v: SchemeVal): Boolean = v match
    case _: SchemeInt      => true
    case _: SchemeRational => true
    case _                 => false

  private def isInexact(v: SchemeVal): Boolean = v match
    case _: SchemeFloat => true
    case _              => false

  private def isNumber(v: SchemeVal): Boolean = v match
    case _: SchemeInt | _: SchemeRational | _: SchemeFloat => true
    case _                                                 => false

  private def hasInexact(args: List[SchemeVal]): Boolean = args.exists(isInexact)

  private def addExact(a: (Long, Long), b: (Long, Long)): SchemeVal =
    val (an, ad) = a; val (bn, bd) = b
    makeRational(an * bd + bn * ad, ad * bd)

  private def subExact(a: (Long, Long), b: (Long, Long)): SchemeVal =
    val (an, ad) = a; val (bn, bd) = b
    makeRational(an * bd - bn * ad, ad * bd)

  private def mulExact(a: (Long, Long), b: (Long, Long)): SchemeVal =
    val (an, ad) = a; val (bn, bd) = b
    makeRational(an * bn, ad * bd)

  private def divExact(a: (Long, Long), b: (Long, Long)): SchemeVal =
    val (an, ad) = a; val (bn, bd) = b
    if bn == 0 then throw new EvalError("/: division by zero")
    makeRational(an * bd, ad * bn)

  private[ming] def asLong(v: SchemeVal, op: String): Long = v match
    case SchemeInt(n) => n
    case _            => throw new EvalError(s"$op: expected number, got ${v.display}")

  private def cmp(name: String, op: (Double, Double) => Boolean): SchemeBuiltin =
    SchemeBuiltin(
      name,
      args =>
        if args.size < 2 then throw new EvalError(s"$name: expected at least 2 arguments")
        val nums = args.map(a => toDouble(a, name))
        SchemeBool(nums.sliding(2).forall(w => op(w(0), w(1))))
    )

  private[ming] def typeCheck(name: String)(pred: SchemeVal => Boolean): SchemeBuiltin =
    SchemeBuiltin(
      name,
      args =>
        if args.size != 1 then throw new EvalError(s"$name: expected 1 argument")
        SchemeBool(pred(args.head))
    )

  def install(env: Env, output: StringBuilder = new StringBuilder): Unit =
    env.set(
      "+",
      SchemeBuiltin(
        "+",
        args =>
          if args.isEmpty then SchemeInt(0)
          else if hasInexact(args) then SchemeFloat(args.map(a => toDouble(a, "+")).sum)
          else
            args.map(a => toRat(a, "+")).reduce { (a, b) =>
              val r = addExact(a, b); toRat(r, "+")
            } match
              case (n, d) => makeRational(n, d)
      )
    )
    env.set(
      "-",
      SchemeBuiltin(
        "-",
        args =>
          if args.isEmpty then throw new EvalError("-: expected at least 1 argument")
          else if hasInexact(args) then
            if args.size == 1 then SchemeFloat(-toDouble(args.head, "-"))
            else SchemeFloat(args.map(a => toDouble(a, "-")).reduce(_ - _))
          else if args.size == 1 then
            val (n, d) = toRat(args.head, "-")
            makeRational(-n, d)
          else
            args.map(a => toRat(a, "-")).reduce { (a, b) =>
              val r = subExact(a, b); toRat(r, "-")
            } match
              case (n, d) => makeRational(n, d)
      )
    )
    env.set(
      "*",
      SchemeBuiltin(
        "*",
        args =>
          if args.isEmpty then SchemeInt(1)
          else if hasInexact(args) then SchemeFloat(args.map(a => toDouble(a, "*")).product)
          else
            args.map(a => toRat(a, "*")).reduce { (a, b) =>
              val r = mulExact(a, b); toRat(r, "*")
            } match
              case (n, d) => makeRational(n, d)
      )
    )
    env.set(
      "/",
      SchemeBuiltin(
        "/",
        args =>
          if args.size < 2 then throw new EvalError("/: expected at least 2 arguments")
          else if hasInexact(args) then
            val nums = args.map(a => toDouble(a, "/"))
            SchemeFloat(nums.reduce(_ / _))
          else
            args.map(a => toRat(a, "/")).reduce { (a, b) =>
              val r = divExact(a, b); toRat(r, "/")
            } match
              case (n, d) => makeRational(n, d)
      )
    )

    env.set("<", cmp("<", _ < _))
    env.set(">", cmp(">", _ > _))
    env.set("=", cmp("=", _ == _))
    env.set("<=", cmp("<=", _ <= _))
    env.set(">=", cmp(">=", _ >= _))

    env.set(
      "not",
      SchemeBuiltin(
        "not",
        args =>
          if args.size != 1 then throw new EvalError("not: expected 1 argument")
          SchemeBool(args.head match
            case SchemeBool(false) => true
            case _                 => false)
      )
    )

    installPredicates(env)
    installIO(env, output)
    installApply(env)
    installError(env)
    ListBuiltins.install(env)
    StringBuiltins.install(env)
    NumericBuiltins.install(env)
    CharStringBuiltins.install(env)

  private def installApply(env: Env): Unit =
    env.set(
      "apply",
      SchemeBuiltin(
        "apply",
        args =>
          if args.size < 2 then throw new EvalError("apply: expected at least 2 arguments")
          val proc    = args.head
          val lastArg = args.last
          val lastList = SchemeListOps.toScalaList(lastArg) match
            case Some(elems) => elems
            case None        => throw new EvalError("apply: last argument must be a list")
          val prefixArgs = args.slice(1, args.size - 1)
          val allArgs    = prefixArgs ++ lastList
          Evaluator.applyProc(proc, allArgs)
      )
    )

  private def installError(env: Env): Unit =
    env.set(
      "error",
      SchemeBuiltin(
        "error",
        args =>
          if args.isEmpty then throw new EvalError("error")
          val msg = args.head match
            case SchemeString(s) => s
            case other           => other.display
          val irritants = args.tail.map(_.display).mkString(" ")
          throw new EvalError(if irritants.isEmpty then msg else s"$msg $irritants")
      )
    )

  private def installPredicates(env: Env): Unit =
    env.set("number?", typeCheck("number?")(v => isNumber(v)))
    env.set(
      "integer?",
      typeCheck("integer?") {
        case _: SchemeInt         => true
        case SchemeRational(n, d) => n % d == 0 // e.g. 4/2
        case SchemeFloat(f)       => f == math.floor(f) && !f.isInfinite
        case _                    => false
      }
    )
    env.set(
      "rational?",
      typeCheck("rational?") {
        case _: SchemeInt | _: SchemeRational => true
        case _                                => false
      }
    )
    env.set("exact?", typeCheck("exact?")(v => isExact(v)))
    env.set("inexact?", typeCheck("inexact?")(v => isInexact(v)))
    env.set("string?", typeCheck("string?") { case _: SchemeString => true; case _ => false })
    env.set("boolean?", typeCheck("boolean?") { case _: SchemeBool => true; case _ => false })
    env.set(
      "pair?",
      typeCheck("pair?") {
        case SchemeList(_ :: _) => true
        case _: SchemePair      => true
        case _                  => false
      }
    )
    env.set("symbol?", typeCheck("symbol?") { case _: SchemeSymbol => true; case _ => false })
    env.set("char?", typeCheck("char?") { case _: SchemeChar => true; case _ => false })
    env.set(
      "procedure?",
      typeCheck("procedure?") {
        case _: SchemeBuiltin      => true
        case _: SchemeLambda       => true
        case _: SchemeCaseLambda   => true
        case _: SchemeContinuation => true
        case SchemeCallCC          => true
        case _                     => false
      }
    )

  private[ming] def displayVal(v: SchemeVal): String = v match
    case SchemeString(s) => s
    case _ =>
      SchemeListOps.toScalaList(v) match
        case Some(elems) =>
          "(" + elems.map(displayVal).mkString(" ") + ")"
        case None =>
          v match
            case p: SchemePair =>
              val sb = new StringBuilder("(")
              sb.append(displayVal(p.car))
              sb.append(" . ").append(displayVal(p.cdr))
              sb.append(")").toString
            case other => other.display

  private def installIO(env: Env, output: StringBuilder): Unit =
    env.set(
      "display",
      SchemeBuiltin(
        "display",
        args =>
          if args.size != 1 then throw new EvalError("display: expected 1 argument")
          output.append(displayVal(args.head))
          SchemeVoid
      )
    )
    env.set(
      "write",
      SchemeBuiltin(
        "write",
        args =>
          if args.size != 1 then throw new EvalError("write: expected 1 argument")
          output.append(args.head.display)
          SchemeVoid
      )
    )
    env.set(
      "newline",
      SchemeBuiltin(
        "newline",
        args =>
          if args.nonEmpty then throw new EvalError("newline: expected 0 arguments")
          output.append("\n")
          SchemeVoid
      )
    )
