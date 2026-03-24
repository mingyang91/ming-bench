package ming

import scala.collection.mutable

object Parser:

  def parse(input: String): List[Expr] =
    val tokens     = tokenize(input)
    val (exprs, _) = parseAll(tokens, 0)
    exprs

  private def tokenizeString(input: String, start: Int): (String, Int) =
    val sb = new StringBuilder("\"")
    var i  = start
    while i < input.length && input(i) != '"' do
      if input(i) == '\\' then
        i += 1
        if i < input.length then
          input(i) match
            case 'n'   => sb.append('\n')
            case 't'   => sb.append('\t')
            case '\\'  => sb.append('\\')
            case '"'   => sb.append('"')
            case other => sb.append('\\'); sb.append(other)
          i += 1
        end if
      else
        sb.append(input(i))
        i += 1
    end while
    if i < input.length then i += 1 // closing quote
    sb.append('"')
    (sb.toString, i)

  private def isDelimiter(c: Char): Boolean =
    c.isWhitespace || c == '(' || c == ')' || c == '"' || c == ';'

  private def tokenizeBare(input: String, start: Int): (String, Int) =
    val sb = new StringBuilder
    var i  = start
    while i < input.length && !isDelimiter(input(i)) do
      sb.append(input(i))
      i += 1
    end while
    (sb.toString, i)

  private def tokenize(input: String): Array[String] =
    val buf = mutable.ArrayBuffer[String]()
    var i   = 0
    while i < input.length do
      input(i) match
        case c if c.isWhitespace => i += 1
        case ';'                 => while i < input.length && input(i) != '\n' do i += 1
        case '('                 => buf += "("; i += 1
        case ')'                 => buf += ")"; i += 1
        case '\''                => buf += "'"; i += 1
        case '"' =>
          val (tok, next) = tokenizeString(input, i + 1)
          buf += tok
          i = next
        case _ =>
          val (tok, next) = tokenizeBare(input, i)
          buf += tok
          i = next
    end while
    buf.toArray

  private def parseAll(tokens: Array[String], pos: Int): (List[Expr], Int) =
    val buf = mutable.ListBuffer[Expr]()
    var i   = pos
    while i < tokens.length do
      val (expr, next) = parseExpr(tokens, i)
      buf += expr
      i = next
    end while
    (buf.toList, i)

  private def parseExpr(tokens: Array[String], pos: Int): (Expr, Int) =
    if pos >= tokens.length then throw new EvalError("unexpected end of input")
    val tok = tokens(pos)
    tok match
      case "(" =>
        val buf = mutable.ListBuffer[Expr]()
        var i   = pos + 1
        while i < tokens.length && tokens(i) != ")" do
          val (expr, next) = parseExpr(tokens, i)
          buf += expr
          i = next
        end while
        if i >= tokens.length then throw new EvalError("missing closing parenthesis")
        (Expr.SList(buf.toList), i + 1)
      case ")" =>
        throw new EvalError("unexpected )")
      case "'" =>
        val (expr, next) = parseExpr(tokens, pos + 1)
        (Expr.SList(List(Expr.Symbol("quote"), expr)), next)
      case _ =>
        (parseAtom(tok), pos + 1)

  private def parseAtom(tok: String): Expr =
    if tok == "#t" then Expr.BoolLit(true)
    else if tok == "#f" then Expr.BoolLit(false)
    else if tok.startsWith("\"") && tok.endsWith("\"") then Expr.StrLit(tok.substring(1, tok.length - 1))
    else
      tok.toLongOption match
        case Some(n) => Expr.IntLit(n)
        case None    => Expr.Symbol(tok)
