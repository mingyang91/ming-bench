package ming

import SchemeTypes.{errAt, Env, Pos, Value}

/** Extracted special-form evaluators and helpers that do not depend on `eval`. */
private[ming] object EvalForms:

  def quoteToValue(expr: Expr): Value = expr match
    case Expr.Num(n, _)       => Value.VNum(n)
    case Expr.Flt(d, _)       => Value.VFloat(d)
    case Expr.Rat(n, d, _)    => Value.VRational(n, d)
    case Expr.Bool(b, _)      => Value.VBool(b)
    case Expr.Str(s, _)       => Value.VStr(s.toCharArray)
    case Expr.Chr(c, _)       => Value.VChar(c)
    case Expr.Symbol(name, _) => Value.VSymbol(name)
    case Expr.SList(elems, _) =>
      Value.VList(elems.map(quoteToValue))

  def parseParams(
    params: List[Expr],
    pos: Pos
  ): (List[String], Option[String]) =
    val dotIdx = params.indexWhere {
      case Expr.Symbol(".", _) => true; case _ => false
    }
    if dotIdx < 0 then
      val names = params.map {
        case Expr.Symbol(n, _) => n
        case _                 => throw errAt(pos, "invalid parameter")
      }
      (names, None)
    else
      val fixed = params.take(dotIdx).map {
        case Expr.Symbol(n, _) => n
        case _                 => throw errAt(pos, "invalid parameter")
      }
      params.drop(dotIdx + 1) match
        case Expr.Symbol(rest, _) :: Nil => (fixed, Some(rest))
        case _                           => throw errAt(pos, "invalid rest parameter")

  def evalDefineSyntax(
    rest: List[Expr],
    env: Env,
    pos: Pos
  ): Value =
    rest match
      case Expr.Symbol(name, _) :: Expr.SList(
            Expr.Symbol("syntax-rules", _) :: Expr.SList(lits, _) :: rules,
            _
          ) :: Nil =>
        val literals = lits.map {
          case Expr.Symbol(s, _) => s
          case _                 => throw errAt(pos, "invalid syntax-rules")
        }
        val parsedRules = rules.map {
          case Expr.SList(pat :: tmpl :: Nil, _) => (pat, tmpl)
          case _                                 => throw errAt(pos, "invalid syntax-rules")
        }
        env.define(name, Value.VMacro(literals, parsedRules, env))
        Value.VVoid
      case _ => throw errAt(pos, "invalid define-syntax")

  def evalCaseLambda(
    clauses: List[Expr],
    env: Env,
    pos: Pos
  ): Value =
    val parsed = clauses.map {
      case Expr.SList(Expr.SList(params, _) :: body, _) =>
        val (paramNames, restParam) = parseParams(params, pos)
        (paramNames, restParam, body, env)
      case Expr.SList(Expr.Symbol(name, _) :: body, _) =>
        (Nil, Some(name), body, env)
      case _ => throw errAt(pos, "invalid case-lambda clause")
    }
    Value.VCaseLambda(parsed)

  def evalDefineRecordType(
    rest: List[Expr],
    env: Env,
    pos: Pos
  ): Value =
    Records.evalDefineRecordType(rest, env, pos)
