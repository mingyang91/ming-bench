package ming

import scala.collection.mutable

object Parser:

  // Maps Expr instances (by reference identity) to (line, col) from source
  val positions: java.util.IdentityHashMap[Expr, (Int, Int)] =
    new java.util.IdentityHashMap()

  private case class Token(text: String, line: Int, col: Int)

  def parse(input: String): List[Expr] =
    positions.clear()
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

  private def tokenize(input: String): Array[Token] =
    val buf  = mutable.ArrayBuffer[Token]()
    var i    = 0
    var line = 1
    var col  = 1
    while i < input.length do
      input(i) match
        case '\n' =>
          i += 1; line += 1; col = 1
        case c if c.isWhitespace =>
          i += 1; col += 1
        case ';' =>
          while i < input.length && input(i) != '\n' do
            i += 1; col += 1
        case '(' =>
          buf += Token("(", line, col)
          i += 1; col += 1
        case ')' =>
          buf += Token(")", line, col)
          i += 1; col += 1
        case '\'' =>
          buf += Token("'", line, col)
          i += 1; col += 1
        case '`' =>
          buf += Token("`", line, col)
          i += 1; col += 1
        case ',' =>
          if i + 1 < input.length && input(i + 1) == '@' then
            buf += Token(",@", line, col)
            i += 2; col += 2
          else
            buf += Token(",", line, col)
            i += 1; col += 1
        case '#' if i + 1 < input.length && input(i + 1) == '\'' =>
          buf += Token("#'", line, col)
          i += 2; col += 2
        case '"' =>
          val startCol    = col
          val (tok, next) = tokenizeString(input, i + 1)
          col += (next - i)
          i = next
          buf += Token(tok, line, startCol)
        case _ =>
          val startCol    = col
          val (tok, next) = tokenizeBare(input, i)
          col += (next - i)
          i = next
          buf += Token(tok, line, startCol)
    end while
    buf.toArray

  private def parseAll(tokens: Array[Token], pos: Int): (List[Expr], Int) =
    val buf = mutable.ListBuffer[Expr]()
    var i   = pos
    while i < tokens.length do
      val (expr, next) = parseExpr(tokens, i)
      buf += expr
      i = next
    end while
    (buf.toList, i)

  private def parseExpr(tokens: Array[Token], pos: Int): (Expr, Int) =
    if pos >= tokens.length then throw new EvalError("unexpected end of input")
    val tok = tokens(pos)
    tok.text match
      case "(" =>
        val buf = mutable.ListBuffer[Expr]()
        var i   = pos + 1
        while i < tokens.length && tokens(i).text != ")" do
          val (expr, next) = parseExpr(tokens, i)
          buf += expr
          i = next
        end while
        if i >= tokens.length then throw new EvalError(s"missing closing parenthesis at ${tok.line}:${tok.col}")
        val expr = Expr.SList(buf.toList)
        positions.put(expr, (tok.line, tok.col))
        (expr, i + 1)
      case ")" =>
        throw new EvalError(s"unexpected ) at ${tok.line}:${tok.col}")
      case "'" =>
        val (inner, next) = parseExpr(tokens, pos + 1)
        val expr          = Expr.SList(List(Expr.Symbol("quote"), inner))
        positions.put(expr, (tok.line, tok.col))
        (expr, next)
      case "#'" =>
        val (inner, next) = parseExpr(tokens, pos + 1)
        val expr          = Expr.SList(List(Expr.Symbol("syntax"), inner))
        positions.put(expr, (tok.line, tok.col))
        (expr, next)
      case "`" =>
        val (inner, next) = parseExpr(tokens, pos + 1)
        val expr          = Expr.SList(List(Expr.Symbol("quasiquote"), inner))
        positions.put(expr, (tok.line, tok.col))
        (expr, next)
      case "," =>
        val (inner, next) = parseExpr(tokens, pos + 1)
        val expr          = Expr.SList(List(Expr.Symbol("unquote"), inner))
        positions.put(expr, (tok.line, tok.col))
        (expr, next)
      case ",@" =>
        val (inner, next) = parseExpr(tokens, pos + 1)
        val expr          = Expr.SList(List(Expr.Symbol("unquote-splicing"), inner))
        positions.put(expr, (tok.line, tok.col))
        (expr, next)
      case _ =>
        val expr = parseAtom(tok.text)
        positions.put(expr, (tok.line, tok.col))
        (expr, pos + 1)

  private def parseRationalOrSymbol(tok: String): Expr =
    val parts = tok.split('/')
    if parts.length == 2 then
      (parts(0).toLongOption, parts(1).toLongOption) match
        case (Some(n), Some(d)) if d != 0 => Expr.RatLit(n, d)
        case _                            => Expr.Symbol(tok)
    else Expr.Symbol(tok)

  private def parseNumericOrSymbol(tok: String): Expr =
    tok.toLongOption match
      case Some(n) => Expr.IntLit(n)
      case None =>
        tok.toDoubleOption match
          case Some(d) => Expr.FloatLit(d)
          case None    => Expr.Symbol(tok)

  private def parseAtom(tok: String): Expr =
    if tok == "#t" then Expr.BoolLit(true)
    else if tok == "#f" then Expr.BoolLit(false)
    else if tok.startsWith("#\\") then
      val charName = tok.substring(2)
      val c = charName match
        case "space"            => ' '
        case "newline"          => '\n'
        case "tab"              => '\t'
        case s if s.length == 1 => s.charAt(0)
        case _                  => throw new EvalError(s"unknown character name: $charName")
      Expr.CharLit(c)
    else if tok.startsWith("\"") && tok.endsWith("\"") then Expr.StrLit(tok.substring(1, tok.length - 1))
    else if tok.contains('/') && !tok.startsWith("/") then parseRationalOrSymbol(tok)
    else parseNumericOrSymbol(tok)
