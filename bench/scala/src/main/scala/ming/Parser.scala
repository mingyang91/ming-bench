package ming

import SchemeValue.*

/** Token with source position. */
case class Token(text: String, pos: SourcePos)

/** Recursive-descent parser for Scheme expressions. */
object Parser:

  def parse(input: String): List[SchemeValue] =
    val tokens             = tokenize(input)
    val (exprs, remaining) = parseAll(tokens)
    if remaining.nonEmpty then throw new EvalError("unexpected tokens after expression")
    exprs

  private def scanString(input: String, start: Int): (String, Int) =
    val sb = new StringBuilder("\"")
    var i  = start
    while i < input.length && input(i) != '"' do
      if input(i) == '\\' then
        i += 1
        if i < input.length then
          input(i) match
            case 'n'  => sb += '\n'
            case 't'  => sb += '\t'
            case '\\' => sb += '\\'
            case '"'  => sb += '"'
            case c    => sb += '\\'; sb += c
          i += 1
        end if
      else
        sb += input(i)
        i += 1
      end if
    end while
    if i < input.length then i += 1 // skip closing quote
    sb += '"'
    (sb.toString, i)

  /** Compute (line, col) from a character offset. Both 1-based. */
  private def offsetToPos(input: String, offset: Int): SourcePos =
    var line = 1
    var col  = 1
    var i    = 0
    while i < offset && i < input.length do
      if input(i) == '\n' then
        line += 1
        col = 1
      else col += 1
      i += 1
    SourcePos(line, col)

  private def tokenize(input: String): List[Token] =
    val result = scala.collection.mutable.ListBuffer.empty[Token]
    var i      = 0
    while i < input.length do
      input(i) match
        case ch if ch.isWhitespace =>
          i += 1
        case ';' =>
          while i < input.length && input(i) != '\n' do i += 1
        case '(' =>
          result += Token("(", offsetToPos(input, i))
          i += 1
        case ')' =>
          result += Token(")", offsetToPos(input, i))
          i += 1
        case '#' if i + 1 < input.length && input(i + 1) == '\'' =>
          result += Token("#'", offsetToPos(input, i))
          i += 2
        case '\'' =>
          result += Token("'", offsetToPos(input, i))
          i += 1
        case '"' =>
          val pos         = offsetToPos(input, i)
          val (tok, next) = scanString(input, i + 1)
          result += Token(tok, pos)
          i = next
        case _ =>
          val pos = offsetToPos(input, i)
          val sb  = new StringBuilder
          while i < input.length && !input(i).isWhitespace &&
            input(i) != '(' && input(i) != ')' && input(i) != '"' && input(i) != ';'
          do
            sb += input(i)
            i += 1
          result += Token(sb.toString, pos)
    end while
    result.toList

  private def parseAll(
    tokens: List[Token]
  ): (List[SchemeValue], List[Token]) =
    tokens match
      case Nil                => (Nil, Nil)
      case Token(")", _) :: _ => (Nil, tokens)
      case _ =>
        val (expr, rest)      = parseExpr(tokens)
        val (more, remaining) = parseAll(rest)
        (expr :: more, remaining)

  private def parseExpr(
    tokens: List[Token]
  ): (SchemeValue, List[Token]) =
    tokens match
      case Nil => throw new EvalError("unexpected end of input")
      case Token("(", pos) :: rest =>
        val (elements, afterList) = parseAll(rest)
        afterList match
          case Token(")", _) :: remaining => (ListVal(elements, Some(pos)), remaining)
          case _                          => throw new EvalError("missing closing parenthesis")
      case Token(")", _) :: _ =>
        throw new EvalError("unexpected )")
      case Token("'", pos) :: rest =>
        val (quoted, remaining) = parseExpr(rest)
        (ListVal(List(SymbolVal("quote", Some(pos)), quoted), Some(pos)), remaining)
      case Token("#'", pos) :: rest =>
        val (quoted, remaining) = parseExpr(rest)
        (ListVal(List(SymbolVal("syntax-quote", Some(pos)), quoted), Some(pos)), remaining)
      case Token(text, pos) :: rest =>
        (parseAtom(text, pos), rest)

  private def parseAtom(token: String, pos: SourcePos): SchemeValue =
    if token == "#t" then BoolVal(true, Some(pos))
    else if token == "#f" then BoolVal(false, Some(pos))
    else if token.startsWith("#\\") then parseCharLiteral(token, pos)
    else if token.startsWith("\"") && token.endsWith("\"") then
      StringVal(token.substring(1, token.length - 1), Some(pos))
    else
      token.toLongOption match
        case Some(n) => IntVal(n, Some(pos))
        case None    => parseRationalOrFloat(token, pos)

  private def parseRationalOrFloat(token: String, pos: SourcePos): SchemeValue =
    val slashIdx = token.indexOf('/')
    if slashIdx > 0 then
      val numStr = token.substring(0, slashIdx)
      val denStr = token.substring(slashIdx + 1)
      (numStr.toLongOption, denStr.toLongOption) match
        case (Some(n), Some(d)) => Rational.make(n, d, Some(pos))
        case _                  => SymbolVal(token, Some(pos))
    else if token.contains('.') then
      token.toDoubleOption match
        case Some(d) => DoubleVal(d, Some(pos))
        case None    => SymbolVal(token, Some(pos))
    else SymbolVal(token, Some(pos))

  private def parseCharLiteral(token: String, pos: SourcePos): SchemeValue =
    val name = token.substring(2)
    if name.length == 1 then CharVal(name.charAt(0), Some(pos))
    else
      name.toLowerCase match
        case "space"   => CharVal(' ', Some(pos))
        case "newline" => CharVal('\n', Some(pos))
        case "tab"     => CharVal('\t', Some(pos))
        case _         => throw new EvalError(s"unknown character name: $token")
