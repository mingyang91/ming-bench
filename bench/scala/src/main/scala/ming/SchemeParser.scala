package ming

import scala.collection.mutable.ListBuffer

import SchemeModel.*

private[ming] object SchemeParser:

  def parseProgram(input: String): List[Expr] =
    Parser(input).parseProgram()

  final private class Parser(input: String):
    private var index = 0

    def parseProgram(): List[Expr] =
      val expressions = ListBuffer.empty[Expr]
      skipTrivia()
      while !isAtEnd do
        expressions += parseExpr()
        skipTrivia()
      expressions.toList

    private def parseExpr(): Expr =
      skipTrivia()
      if isAtEnd then parseError("unexpected end of input")

      input.charAt(index) match
        case '(' =>
          index += 1
          parseList()
        case '\'' =>
          index += 1
          Expr.ListExpr(List(Expr.Symbol("quote"), parseExpr()))
        case ')' =>
          parseError("unexpected ')'")
        case '"' =>
          parseString()
        case '#' =>
          parseBoolean()
        case _ =>
          parseAtom()

    private def parseList(): Expr =
      val items = ListBuffer.empty[Expr]
      skipTrivia()
      while !isAtEnd && input.charAt(index) != ')' do
        items += parseExpr()
        skipTrivia()

      if isAtEnd then parseError("unterminated list")
      index += 1
      Expr.ListExpr(items.toList)

    private def parseString(): Expr =
      index += 1
      val builder = new StringBuilder

      while !isAtEnd && input.charAt(index) != '"' do
        val char = input.charAt(index)
        if char == '\\' then
          index += 1
          if isAtEnd then parseError("unterminated string escape")
          val escaped = input.charAt(index)
          builder +=
            (escaped match
              case '"'   => '"'
              case '\\'  => '\\'
              case 'n'   => '\n'
              case 'r'   => '\r'
              case 't'   => '\t'
              case other => other)
        else builder += char
        index += 1

      if isAtEnd then parseError("unterminated string literal")
      index += 1
      Expr.StringLiteral(builder.result())

    private def parseBoolean(): Expr =
      if startsWith("#t") && tokenBoundary(index + 2) then
        index += 2
        Expr.BooleanLiteral(true)
      else if startsWith("#f") && tokenBoundary(index + 2) then
        index += 2
        Expr.BooleanLiteral(false)
      else parseError("invalid boolean literal")

    private def parseAtom(): Expr =
      val start = index
      while !isAtEnd && !isDelimiter(input.charAt(index)) do index += 1

      val token = input.substring(start, index)
      if token.matches("-?\\d+") then Expr.IntegerLiteral(BigInt(token))
      else Expr.Symbol(token)

    private def skipTrivia(): Unit =
      var keepSkipping = true
      while keepSkipping && !isAtEnd do
        while !isAtEnd && input.charAt(index).isWhitespace do index += 1

        if !isAtEnd && input.charAt(index) == ';' then while !isAtEnd && input.charAt(index) != '\n' do index += 1
        else keepSkipping = false

    private def startsWith(prefix: String): Boolean =
      input.regionMatches(index, prefix, 0, prefix.length)

    private def tokenBoundary(boundary: Int): Boolean =
      boundary >= input.length || isDelimiter(input.charAt(boundary))

    private def isDelimiter(char: Char): Boolean =
      char.isWhitespace || char == '(' || char == ')' || char == ';'

    private def isAtEnd: Boolean =
      index >= input.length

    private def parseError(message: String): Nothing =
      throw new EvalError(s"$message at offset $index")
