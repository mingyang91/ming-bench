package ming

import Expr.*

/** Recursive-descent S-expression parser. */
object Parser:

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

  private def tokenize(input: String): List[String] =
    val result = scala.collection.mutable.ListBuffer.empty[String]
    var i      = 0
    while i < input.length do
      input(i) match
        case ch if ch.isWhitespace =>
          i += 1
        case ';' =>
          while i < input.length && input(i) != '\n' do i += 1
        case '(' =>
          result += "("
          i += 1
        case ')' =>
          result += ")"
          i += 1
        case '\'' =>
          result += "'"
          i += 1
        case '"' =>
          val (tok, newPos) = scanString(input, i + 1)
          result += tok
          i = newPos
        case _ =>
          val start = i
          while i < input.length && isAtomChar(input(i)) do i += 1
          result += input.substring(start, i)
    result.toList

  // --- Parser ---

  private def parseTokens(tokens: List[String], acc: List[Expr]): List[Expr] =
    tokens match
      case Nil => acc
      case "(" :: rest =>
        val (expr, remaining) = parseList(rest, Nil)
        parseTokens(remaining, expr :: acc)
      case ")" :: _ =>
        throw new EvalError("unexpected )")
      case "'" :: rest =>
        val (quoted, remaining) = parseOne(rest)
        parseTokens(remaining, SList(Sym("quote") :: quoted :: Nil) :: acc)
      case token :: rest =>
        parseTokens(rest, parseAtom(token) :: acc)

  private def parseOne(tokens: List[String]): (Expr, List[String]) =
    tokens match
      case Nil => throw new EvalError("unexpected end of input")
      case "(" :: rest =>
        parseList(rest, Nil)
      case "'" :: rest =>
        val (quoted, remaining) = parseOne(rest)
        (SList(Sym("quote") :: quoted :: Nil), remaining)
      case token :: rest =>
        (parseAtom(token), rest)

  private def parseList(tokens: List[String], acc: List[Expr]): (Expr, List[String]) =
    tokens match
      case Nil         => throw new EvalError("unexpected end of input")
      case ")" :: rest => (SList(acc.reverse), rest)
      case _ =>
        val (expr, remaining) = parseOne(tokens)
        parseList(remaining, expr :: acc)

  private def parseAtom(token: String): Expr =
    if token == "#t" then Bool(true)
    else if token == "#f" then Bool(false)
    else if token.startsWith("\"") && token.endsWith("\"") then Str(token.substring(1, token.length - 1))
    else
      token.toLongOption match
        case Some(n) => Num(n)
        case None    => Sym(token)
