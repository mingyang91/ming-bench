package ming

private[ming] object OutputBuiltins:

  def displayVal(v: SchemeVal): String = v match
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

  def valToExpr(v: SchemeVal): Expr =
    v match
      case SchemeInt(n)         => IntLit(n)
      case SchemeFloat(f)       => FloatLit(f)
      case SchemeRational(n, d) => RationalLit(n, d)
      case SchemeBool(b)        => BoolLit(b)
      case SchemeString(s)      => StringLit(s)
      case SchemeChar(c)        => CharLit(c)
      case SchemeSymbol(name)   => Symbol(name)
      case SchemeList(elems)    => SList(elems.map(valToExpr))
      case p: SchemePair =>
        SchemeListOps.toScalaList(p) match
          case Some(elems) => SList(elems.map(valToExpr))
          case None        => throw new EvalError("datum->syntax: cannot convert improper list")
      case _ => throw new EvalError(s"datum->syntax: cannot convert ${v.display}")

  def installSyntax(env: Env): Unit =
    env.set(
      "syntax->datum",
      SchemeBuiltin(
        "syntax->datum",
        args =>
          if args.size != 1 then throw new EvalError("syntax->datum: expected 1 argument")
          args.head match
            case SchemeSyntax(expr) => EvalHelpers.exprToVal(expr)
            case _                  => throw new EvalError("syntax->datum: expected syntax object")
      )
    )
    env.set(
      "datum->syntax",
      SchemeBuiltin(
        "datum->syntax",
        args =>
          if args.size != 2 then throw new EvalError("datum->syntax: expected 2 arguments")
          args.head match
            case _: SchemeSyntax => ()
            case _               => throw new EvalError("datum->syntax: first argument must be syntax object")
          val datum = args(1)
          SchemeSyntax(valToExpr(datum))
      )
    )

  def installIO(env: Env, output: StringBuilder): Unit =
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
