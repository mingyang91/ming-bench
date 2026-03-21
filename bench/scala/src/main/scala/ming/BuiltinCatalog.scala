package ming

object BuiltinCatalog:

  private val builtinNames = Set(
    "+",
    "-",
    "*",
    "/",
    "<",
    ">",
    "=",
    "<=",
    "not",
    "cons",
    "car",
    "cdr",
    "null?",
    "list",
    "length",
    "string?",
    "number?",
    "boolean?",
    "pair?",
    "symbol?",
    "display",
    "write",
    "newline",
    "string-append",
    "string-length",
    "substring",
    "string->number",
    "number->string",
    "symbol->string",
    "string->symbol",
    "string-ref",
    "string-copy",
    "string-set!",
    "char?"
  )

  def resolve(name: String): Option[Value] =
    Option.when(builtinNames.contains(name))(Value.Builtin(name))
