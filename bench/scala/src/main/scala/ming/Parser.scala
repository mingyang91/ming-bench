package ming

import Evaluator.Val
import Evaluator.Val.*

/** S-expression parser for Scheme source code. */
private[ming] class Parser(input: String):
  private var pos = 0

  def parseAll(): List[Val] =
    val exprs = scala.collection.mutable.ListBuffer[Val]()
    while
      skipWhitespace()
      pos < input.length
    do exprs += parseExpr()
    exprs.toList

  private def skipWhitespace(): Unit =
    while pos < input.length && (input(pos).isWhitespace || input(pos) == ';') do
      if input(pos) == ';' then while pos < input.length && input(pos) != '\n' do pos += 1
      else pos += 1

  private def parseExpr(): Val =
    skipWhitespace()
    if pos >= input.length then throw new EvalError("unexpected end of input")
    input(pos) match
      case '(' =>
        pos += 1
        parseList()
      case '\'' =>
        pos += 1
        val e = parseExpr()
        Pair(Symbol("quote"), Pair(e, Nil))
      case '"' =>
        parseString()
      case '#' =>
        pos += 1
        if pos >= input.length then throw new EvalError("unexpected end of input after #")
        input(pos) match
          case 't'   => pos += 1; Bool(true)
          case 'f'   => pos += 1; Bool(false)
          case other => throw new EvalError(s"unexpected character after #: $other")
      case _ =>
        parseAtom()

  private def parseList(): Val =
    skipWhitespace()
    if pos >= input.length then throw new EvalError("unexpected end of input in list")
    if input(pos) == ')' then
      pos += 1
      Nil
    else
      val first = parseExpr()
      skipWhitespace()
      if pos < input.length && input(pos) == '.' then
        pos += 1
        val rest = parseExpr()
        skipWhitespace()
        if pos >= input.length || input(pos) != ')' then throw new EvalError("expected ) after dotted pair")
        pos += 1
        Pair(first, rest)
      else
        val rest = parseList()
        Pair(first, rest)

  private def parseString(): Val =
    pos += 1 // skip opening "
    val sb = new StringBuilder
    while pos < input.length && input(pos) != '"' do
      if input(pos) == '\\' then
        pos += 1
        if pos >= input.length then throw new EvalError("unterminated string")
        input(pos) match
          case 'n'  => sb += '\n'
          case 't'  => sb += '\t'
          case '\\' => sb += '\\'
          case '"'  => sb += '"'
          case c    => sb += '\\'; sb += c
      else sb += input(pos)
      pos += 1
    if pos >= input.length then throw new EvalError("unterminated string")
    pos += 1 // skip closing "
    Str(sb.toString)

  private def parseAtom(): Val =
    val start = pos
    while pos < input.length && !input(pos).isWhitespace && !"()\"';".contains(input(pos)) do pos += 1
    val token = input.substring(start, pos)
    if token.isEmpty then throw new EvalError(s"unexpected character: ${input(pos)}")
    token.toLongOption match
      case Some(n) => Num(n)
      case None    => Symbol(token)
