package ming

object Parser:

  private def withPos(sv: SchemeVal, pos: (Int, Int)): SchemeVal =
    sv.pos = pos
    sv

  def parseAll(tokens: List[Token]): List[SchemeVal] =
    val exprs     = scala.collection.mutable.ListBuffer[SchemeVal]()
    var remaining = tokens
    while remaining.nonEmpty do
      val (expr, rest) = parseExpr(remaining)
      exprs += expr
      remaining = rest
    exprs.toList

  private def tokenPos(t: Token): (Int, Int) = t match
    case Token.LParen(p)  => p
    case Token.RParen(p)  => p
    case Token.Str(_, p)  => p
    case Token.Atom(_, p) => p

  private def parseExpr(tokens: List[Token]): (SchemeVal, List[Token]) =
    tokens match
      case Nil => throw new EvalError("unexpected end of input")
      case Token.LParen(p) :: rest =>
        val (elems, remaining) = parseList(rest)
        (withPos(SchemeVal.SList(elems), p), remaining)
      case Token.RParen(_) :: _ =>
        throw new EvalError("unexpected )")
      case Token.Str(s, p) :: rest =>
        (withPos(SchemeVal.StringVal(s), p), rest)
      case Token.Atom("quote-sugar", p) :: rest =>
        val (expr, remaining) = parseExpr(rest)
        (withPos(SchemeVal.SList(List(withPos(SchemeVal.Symbol("quote"), p), expr)), p), remaining)
      case Token.Atom(s, p) :: rest =>
        (withPos(parseAtom(s), p), rest)

  private def parseList(tokens: List[Token]): (List[SchemeVal], List[Token]) =
    val elems     = scala.collection.mutable.ListBuffer[SchemeVal]()
    var remaining = tokens
    while remaining.nonEmpty && !remaining.head.isInstanceOf[Token.RParen] do
      val (expr, rest) = parseExpr(remaining)
      elems += expr
      remaining = rest
    remaining match
      case Token.RParen(_) :: rest => (elems.toList, rest)
      case _                       => throw new EvalError("missing )")

  private def parseAtom(s: String): SchemeVal =
    if s == "#t" then SchemeVal.BoolVal(true)
    else if s == "#f" then SchemeVal.BoolVal(false)
    else
      s.toLongOption match
        case Some(n) => SchemeVal.IntVal(n)
        case None    => SchemeVal.Symbol(s)
