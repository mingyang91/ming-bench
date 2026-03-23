package ming

import Expr.*

/** Recursive-descent S-expression parser with source position tracking. */
object Parser:

  /** A token with its source position. */
  private case class Token(text: String, pos: Pos)

  /** Parse all top-level expressions from input string. */
  def parseAll(input: String): List[Expr] =
    val tokens = tokenize(input)
    parseTokens(tokens, Nil).reverse

  // --- Tokenizer ---

  /** Scan a string literal starting after the opening quote. Returns (token, newPos). */
  private def scanString(input: String, start: Int): (String, Int) =
    val sb = new StringBuilder("\"")
    var i  = start
    while i < input.length && input(i) != '"' do
      if input(i) == '\\' then
        sb += input(i)
        i += 1
        if i < input.length then
          sb += input(i)
          i += 1
      else
        sb += input(i)
        i += 1
    if i < input.length then
      sb += '"'
      i += 1
    (sb.toString, i)

  private def isAtomChar(ch: Char): Boolean =
    !ch.isWhitespace && ch != '(' && ch != ')' && ch != '"' && ch != ';' && ch != '\''

  private def tokenize(input: String): List[Token] =
    val result    = scala.collection.mutable.ListBuffer.empty[Token]
    var i         = 0
    var line      = 1
    var lineStart = 0
    while i < input.length do
      input(i) match
        case '\n' =>
          i += 1
          line += 1
          lineStart = i
        case ch if ch.isWhitespace =>
          i += 1
        case ';' =>
          while i < input.length && input(i) != '\n' do i += 1
        case '(' =>
          result += Token("(", Pos(line, i - lineStart + 1))
          i += 1
        case ')' =>
          result += Token(")", Pos(line, i - lineStart + 1))
          i += 1
        case '\'' =>
          result += Token("'", Pos(line, i - lineStart + 1))
          i += 1
        case '"' =>
          val col           = i - lineStart + 1
          val (tok, newPos) = scanString(input, i + 1)
          result += Token(tok, Pos(line, col))
          i = newPos
        case _ =>
          val col   = i - lineStart + 1
          val start = i
          while i < input.length && isAtomChar(input(i)) do i += 1
          result += Token(input.substring(start, i), Pos(line, col))
    result.toList

  // --- Parser ---

  private def parseTokens(tokens: List[Token], acc: List[Expr]): List[Expr] =
    tokens match
      case Nil => acc
      case Token("(", pos) :: rest =>
        val (expr, remaining) = parseList(rest, Nil, pos)
        parseTokens(remaining, expr :: acc)
      case Token(")", _) :: _ =>
        throw new EvalError("unexpected )")
      case Token("'", pos) :: rest =>
        val (quoted, remaining) = parseOne(rest)
        val quoteExpr           = SList(Sym("quote", Some(pos)) :: quoted :: Nil, Some(pos))
        parseTokens(remaining, quoteExpr :: acc)
      case Token(text, pos) :: rest =>
        parseTokens(rest, parseAtom(text, pos) :: acc)

  private def parseOne(tokens: List[Token]): (Expr, List[Token]) =
    tokens match
      case Nil => throw new EvalError("unexpected end of input")
      case Token("(", pos) :: rest =>
        parseList(rest, Nil, pos)
      case Token("'", pos) :: rest =>
        val (quoted, remaining) = parseOne(rest)
        (SList(Sym("quote", Some(pos)) :: quoted :: Nil, Some(pos)), remaining)
      case Token(text, pos) :: rest =>
        (parseAtom(text, pos), rest)

  private def parseList(tokens: List[Token], acc: List[Expr], listPos: Pos): (Expr, List[Token]) =
    tokens match
      case Nil                   => throw new EvalError("unexpected end of input")
      case Token(")", _) :: rest => (SList(acc.reverse, Some(listPos)), rest)
      case _ =>
        val (expr, remaining) = parseOne(tokens)
        parseList(remaining, expr :: acc, listPos)

  private def parseAtom(token: String, pos: Pos): Expr =
    if token == "#t" then Bool(true, Some(pos))
    else if token == "#f" then Bool(false, Some(pos))
    else if token.startsWith("#\\") then parseCharLiteral(token, pos)
    else if token.startsWith("\"") && token.endsWith("\"") then Str(token.substring(1, token.length - 1), Some(pos))
    else
      token.toLongOption match
        case Some(n) => Num(n, Some(pos))
        case None    => Sym(token, Some(pos))

  private def parseCharLiteral(token: String, pos: Pos): Expr =
    val name = token.substring(2)
    val ch = name match
      case "space"            => ' '
      case "newline"          => '\n'
      case "tab"              => '\t'
      case s if s.length == 1 => s.charAt(0)
      case _                  => throw new EvalError(s"unknown character name: $name")
    Chr(ch, Some(pos))
