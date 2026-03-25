package ming

private[ming] class SchemeParser(input: String):
  private var pos: Int = 0

  private def peek: Char =
    if pos < input.length then input(pos) else '\u0000'

  private def advance(): Char =
    val c = input(pos)
    pos += 1
    c

  private def lineColAt(offset: Int): (Int, Int) =
    var line = 1
    var col  = 1
    for i <- 0 until offset do
      if input(i) == '\n' then
        line += 1
        col = 1
      else col += 1
    (line, col)

  private def skipWhitespaceAndComments(): Unit =
    while pos < input.length do
      val c = input(pos)
      if c.isWhitespace then pos += 1
      else if c == ';' then while pos < input.length && input(pos) != '\n' do pos += 1
      else return

  def parseAll(): List[Expr] =
    val exprs = List.newBuilder[Expr]
    skipWhitespaceAndComments()
    while pos < input.length do
      exprs += parseExpr()
      skipWhitespaceAndComments()
    exprs.result()

  private def parseExpr(): Expr =
    skipWhitespaceAndComments()
    if pos >= input.length then throw EvalError("unexpected end of input")
    val startPos = pos
    val result = peek match
      case '(' => parseList()
      case '"' => parseString()
      case '\'' =>
        advance()
        val inner = parseExpr()
        Expr.Lst(List(Expr.Sym("quote"), inner))
      case '#' => parseHash()
      case _   => parseAtom()
    val (l, c) = lineColAt(startPos)
    result.withPos(l, c)

  private def parseList(): Expr =
    advance() // skip '('
    val elems = List.newBuilder[Expr]
    skipWhitespaceAndComments()
    while pos < input.length && peek != ')' do
      elems += parseExpr()
      skipWhitespaceAndComments()
    if pos >= input.length then throw EvalError("unmatched (")
    advance() // skip ')'
    Expr.Lst(elems.result())

  private def parseString(): Expr =
    advance() // skip opening "
    val sb = StringBuilder()
    while pos < input.length && peek != '"' do
      val c = advance()
      if c == '\\' && pos < input.length then
        val esc = advance()
        esc match
          case 'n'   => sb.append('\n')
          case 't'   => sb.append('\t')
          case '\\'  => sb.append('\\')
          case '"'   => sb.append('"')
          case other => sb.append('\\'); sb.append(other)
      else sb.append(c)
    if pos >= input.length then throw EvalError("unterminated string")
    advance() // skip closing "
    Expr.Str(sb.result().toCharArray)

  private def parseHash(): Expr =
    advance() // skip '#'
    if pos >= input.length then throw EvalError("unexpected end after #")
    val c = advance()
    c match
      case 't' =>
        if pos < input.length && !isDelimiter(peek) then throw EvalError(s"unexpected character after #t")
        Expr.Bool(true)
      case 'f' =>
        if pos < input.length && !isDelimiter(peek) then throw EvalError(s"unexpected character after #f")
        Expr.Bool(false)
      case '\\' =>
        if pos >= input.length then throw EvalError("unexpected end after #\\")
        // Read the character name or single character
        val start = pos
        while pos < input.length && !isDelimiter(peek) do pos += 1
        val token = input.substring(start, pos)
        if token.isEmpty then throw EvalError("unexpected end after #\\")
        token match
          case "space"            => Expr.Chr(' ')
          case "newline"          => Expr.Chr('\n')
          case "tab"              => Expr.Chr('\t')
          case s if s.length == 1 => Expr.Chr(s.charAt(0))
          case _                  => throw EvalError(s"unknown character name: $token")
      case _ => throw EvalError(s"unexpected #$c")

  private def isDelimiter(c: Char): Boolean =
    c.isWhitespace || c == '(' || c == ')' || c == '"' || c == ';'

  private def parseAtom(): Expr =
    val start = pos
    while pos < input.length && !isDelimiter(peek) do pos += 1
    val token = input.substring(start, pos)
    if token.isEmpty then throw EvalError(s"unexpected character: ${peek}")
    token.toLongOption match
      case Some(n) => Expr.Num(n)
      case None    => Expr.Sym(token)
