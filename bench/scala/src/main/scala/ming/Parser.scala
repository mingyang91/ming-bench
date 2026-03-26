package ming

private[ming] enum Expr:
  case Num(value: Long)
  case Bool(value: Boolean)
  case Str(value: String)
  case Symbol(name: String)
  case SList(elems: List[Expr])

private[ming] object Parser:

  def tokenize(input: String): List[String] =
    val tokens = scala.collection.mutable.ListBuffer[String]()
    var i      = 0
    while i < input.length do
      input(i) match
        case c if c.isWhitespace => i += 1
        case ';'                 => i = skipComment(input, i)
        case '(' | ')' | '\'' =>
          tokens += input(i).toString; i += 1
        case '"' =>
          val (tok, next) = readString(input, i)
          tokens += tok; i = next
        case '#' =>
          val (tok, next) = readHash(input, i)
          tokens += tok; i = next
        case _ =>
          val (tok, next) = readWord(input, i)
          tokens += tok; i = next
    tokens.toList

  def parseAll(tokens: List[String]): List[Expr] =
    val exprs = scala.collection.mutable.ListBuffer[Expr]()
    var rest  = tokens
    while rest.nonEmpty do
      val (expr, remaining) = parseExpr(rest)
      exprs += expr
      rest = remaining
    exprs.toList

  private def parseExpr(tokens: List[String]): (Expr, List[String]) = tokens match
    case Nil => throw EvalError("unexpected end of input")
    case "(" :: rest =>
      val (elems, remaining) = parseList(rest)
      (Expr.SList(elems), remaining)
    case "'" :: rest =>
      val (expr, remaining) = parseExpr(rest)
      (Expr.SList(List(Expr.Symbol("quote"), expr)), remaining)
    case ")" :: _      => throw EvalError("unexpected )")
    case token :: rest => (parseAtom(token), rest)

  private def parseList(tokens: List[String]): (List[Expr], List[String]) =
    val elems = scala.collection.mutable.ListBuffer[Expr]()
    var rest  = tokens
    while rest.nonEmpty && rest.head != ")" do
      val (expr, remaining) = parseExpr(rest)
      elems += expr
      rest = remaining
    if rest.isEmpty then throw EvalError("missing )")
    (elems.toList, rest.tail)

  private def parseAtom(token: String): Expr =
    if token == "#t" then Expr.Bool(true)
    else if token == "#f" then Expr.Bool(false)
    else if token.startsWith("\"") then Expr.Str(token.substring(1, token.length - 1))
    else
      token.toLongOption match
        case Some(n) => Expr.Num(n)
        case None    => Expr.Symbol(token)

  private def readWord(input: String, start: Int): (String, Int) =
    val sb = new StringBuilder
    var i  = start
    while i < input.length && !input(i).isWhitespace && input(i) != '(' && input(i) != ')' do
      sb += input(i); i += 1
    (sb.toString, i)

  private def readString(input: String, start: Int): (String, Int) =
    val sb = new StringBuilder("\"")
    var i  = start + 1
    while i < input.length && input(i) != '"' do
      if input(i) == '\\' then
        sb += input(i); i += 1
        if i < input.length then
          sb += input(i); i += 1
      else
        sb += input(i); i += 1
    if i < input.length then
      sb += '"'; i += 1
    (sb.toString, i)

  private def readHash(input: String, start: Int): (String, Int) =
    if start + 1 >= input.length then ("#", start + 1)
    else
      val next = input(start + 1)
      if next == 't' || next == 'f' then (input.substring(start, start + 2), start + 2)
      else readWord(input, start)

  private def skipComment(input: String, start: Int): Int =
    var i = start
    while i < input.length && input(i) != '\n' do i += 1
    i
