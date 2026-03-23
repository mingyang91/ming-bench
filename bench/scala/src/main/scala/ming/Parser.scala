package ming

import SchemeValue.*

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
          val (tok, next) = scanString(input, i + 1)
          result += tok
          i = next
        case _ =>
          val sb = new StringBuilder
          while i < input.length && !input(i).isWhitespace &&
            input(i) != '(' && input(i) != ')' && input(i) != '"' && input(i) != ';'
          do
            sb += input(i)
            i += 1
          result += sb.toString
    end while
    result.toList

  private def parseAll(
    tokens: List[String]
  ): (List[SchemeValue], List[String]) =
    tokens match
      case Nil      => (Nil, Nil)
      case ")" :: _ => (Nil, tokens)
      case _ =>
        val (expr, rest)      = parseExpr(tokens)
        val (more, remaining) = parseAll(rest)
        (expr :: more, remaining)

  private def parseExpr(
    tokens: List[String]
  ): (SchemeValue, List[String]) =
    tokens match
      case Nil => throw new EvalError("unexpected end of input")
      case "(" :: rest =>
        val (elements, afterList) = parseAll(rest)
        afterList match
          case ")" :: remaining => (ListVal(elements), remaining)
          case _                => throw new EvalError("missing closing parenthesis")
      case ")" :: _ =>
        throw new EvalError("unexpected )")
      case "'" :: rest =>
        val (quoted, remaining) = parseExpr(rest)
        (ListVal(List(SymbolVal("quote"), quoted)), remaining)
      case token :: rest =>
        (parseAtom(token), rest)

  private def parseAtom(token: String): SchemeValue =
    if token == "#t" then BoolVal(true)
    else if token == "#f" then BoolVal(false)
    else if token.startsWith("\"") && token.endsWith("\"") then StringVal(token.substring(1, token.length - 1))
    else
      token.toLongOption match
        case Some(n) => IntVal(n)
        case None    => SymbolVal(token)
