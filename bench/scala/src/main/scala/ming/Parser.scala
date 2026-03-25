package ming

object Parser:

  def parseAll(tokens: List[Token]): List[SchemeVal] =
    val exprs     = scala.collection.mutable.ListBuffer[SchemeVal]()
    var remaining = tokens
    while remaining.nonEmpty do
      val (expr, rest) = parseExpr(remaining)
      exprs += expr
      remaining = rest
    exprs.toList

  private def parseExpr(tokens: List[Token]): (SchemeVal, List[Token]) =
    tokens match
      case Nil => throw new EvalError("unexpected end of input")
      case Token.LParen :: rest =>
        val (elems, remaining) = parseList(rest)
        (SchemeVal.SList(elems), remaining)
      case Token.RParen :: _ =>
        throw new EvalError("unexpected )")
      case Token.Str(s) :: rest =>
        (SchemeVal.StringVal(s), rest)
      case Token.Atom("quote-sugar") :: rest =>
        val (expr, remaining) = parseExpr(rest)
        (SchemeVal.SList(List(SchemeVal.Symbol("quote"), expr)), remaining)
      case Token.Atom(s) :: rest =>
        (parseAtom(s), rest)

  private def parseList(tokens: List[Token]): (List[SchemeVal], List[Token]) =
    val elems     = scala.collection.mutable.ListBuffer[SchemeVal]()
    var remaining = tokens
    while remaining.nonEmpty && remaining.head != Token.RParen do
      val (expr, rest) = parseExpr(remaining)
      elems += expr
      remaining = rest
    remaining match
      case Token.RParen :: rest => (elems.toList, rest)
      case _                    => throw new EvalError("missing )")

  private def parseAtom(s: String): SchemeVal =
    if s == "#t" then SchemeVal.BoolVal(true)
    else if s == "#f" then SchemeVal.BoolVal(false)
    else
      s.toLongOption match
        case Some(n) => SchemeVal.IntVal(n)
        case None    => SchemeVal.Symbol(s)
