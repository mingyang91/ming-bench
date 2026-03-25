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
        val (elems, dotTail, remaining) = parseList(rest)
        val sv = dotTail match
          case Some(tail) => SchemeVal.DottedList(elems, tail)
          case None       => SchemeVal.SList(elems)
        (withPos(sv, p), remaining)
      case Token.RParen(_) :: _ =>
        throw new EvalError("unexpected )")
      case Token.Str(s, p) :: rest =>
        (withPos(SchemeVal.str(s), p), rest)
      case Token.Atom("quote-sugar", p) :: rest =>
        val (expr, remaining) = parseExpr(rest)
        (withPos(SchemeVal.SList(List(withPos(SchemeVal.Symbol("quote"), p), expr)), p), remaining)
      case Token.Atom(s, p) :: rest =>
        (withPos(parseAtom(s), p), rest)

  private def parseList(tokens: List[Token]): (List[SchemeVal], Option[SchemeVal], List[Token]) =
    val elems                      = scala.collection.mutable.ListBuffer[SchemeVal]()
    var remaining                  = tokens
    var dotTail: Option[SchemeVal] = None
    while remaining.nonEmpty && !remaining.head.isInstanceOf[Token.RParen] do
      remaining match
        case Token.Atom(".", _) :: rest =>
          remaining = rest
          val (expr, rest2) = parseExpr(remaining)
          dotTail = Some(expr)
          remaining = rest2
        case _ =>
          val (expr, rest) = parseExpr(remaining)
          elems += expr
          remaining = rest
    remaining match
      case Token.RParen(_) :: rest => (elems.toList, dotTail, rest)
      case _                       => throw new EvalError("missing )")

  private def parseAtom(s: String): SchemeVal =
    if s == "#t" then SchemeVal.BoolVal(true)
    else if s == "#f" then SchemeVal.BoolVal(false)
    else if s.startsWith("#\\") then
      val rest = s.substring(2)
      rest match
        case "space"            => SchemeVal.CharVal(' ')
        case "newline"          => SchemeVal.CharVal('\n')
        case "tab"              => SchemeVal.CharVal('\t')
        case c if c.length == 1 => SchemeVal.CharVal(c.charAt(0))
        case _                  => throw new EvalError(s"invalid character literal: $s")
    else
      s.toLongOption match
        case Some(n) => SchemeVal.IntVal(n)
        case None    => SchemeVal.Symbol(s)
