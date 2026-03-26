package ming

import SchemeModel.*
import SchemeBuiltinSupport.*
import SchemeRuntime.*

private[ming] object SchemeBuiltins:

  def bindings(output: StringBuilder): List[(String, Value)] =
    numericBindings ++
      coreBindings ++
      outputBindings(output) ++
      stringBindings ++
      predicateBindings

  private val numericBindings: List[(String, Value)] = List(
    "+" -> Value.Builtin(
      "+",
      args => Value.IntegerValue(numericArgs("+", args).foldLeft(BigInt(0))(_ + _))
    ),
    "-" -> Value.Builtin(
      "-",
      args =>
        val numbers = numericArgs("-", args)
        requireMinArgCount("-", args, 1)
        val result =
          if numbers.length == 1 then -numbers.head
          else numbers.tail.foldLeft(numbers.head)(_ - _)
        Value.IntegerValue(result)
    ),
    "*" -> Value.Builtin(
      "*",
      args => Value.IntegerValue(numericArgs("*", args).foldLeft(BigInt(1))(_ * _))
    ),
    "/" -> Value.Builtin(
      "/",
      args =>
        val numbers = numericArgs("/", args)
        requireMinArgCount("/", args, 2)
        val result = numbers.tail.foldLeft(numbers.head) { (left, right) =>
          if right == 0 then throw new EvalError("division by zero")
          left / right
        }
        Value.IntegerValue(result)
    ),
    "<"  -> numericComparator("<")(_ < _),
    ">"  -> numericComparator(">")(_ > _),
    "="  -> numericComparator("=")(_ == _),
    "<=" -> numericComparator("<=")(_ <= _)
  )

  private val coreBindings: List[(String, Value)] = List(
    "not" -> Value.Builtin(
      "not",
      args =>
        requireArgCount("not", args, 1)
        Value.BooleanValue(!isTruthy(args.head))
    ),
    "cons" -> Value.Builtin(
      "cons",
      args =>
        requireArgCount("cons", args, 2)
        Value.PairValue(args.head, args(1))
    ),
    "car" -> Value.Builtin(
      "car",
      args =>
        requireArgCount("car", args, 1)
        requirePair("car", args.head).car
    ),
    "cdr" -> Value.Builtin(
      "cdr",
      args =>
        requireArgCount("cdr", args, 1)
        requirePair("cdr", args.head).cdr
    ),
    "null?" -> predicateBuiltin("null?") {
      case Value.NilValue => true
      case _              => false
    },
    "list" -> Value.Builtin(
      "list",
      args => makeList(args)
    ),
    "length" -> Value.Builtin(
      "length",
      args =>
        requireArgCount("length", args, 1)
        Value.IntegerValue(BigInt(properListLength("length", args.head)))
    ),
    "append" -> Value.Builtin(
      "append",
      args => appendValues(args)
    )
  )

  private def outputBindings(output: StringBuilder): List[(String, Value)] = List(
    "display" -> Value.Builtin(
      "display",
      args =>
        requireArgCount("display", args, 1)
        output.append(renderForDisplay(args.head))
        Value.VoidValue
    ),
    "write" -> Value.Builtin(
      "write",
      args =>
        requireArgCount("write", args, 1)
        output.append(render(args.head))
        Value.VoidValue
    ),
    "newline" -> Value.Builtin(
      "newline",
      args =>
        requireArgCount("newline", args, 0)
        output.append('\n')
        Value.VoidValue
    )
  )

  private val stringBindings: List[(String, Value)] = List(
    "string-append" -> Value.Builtin(
      "string-append",
      args =>
        Value.StringValue(
          SchemeString.fromText(args.map(arg => requireString("string-append", arg).text).mkString)
        )
    ),
    "string-length" -> unaryStringBuiltin("string-length") { text =>
      Value.IntegerValue(BigInt(text.length))
    },
    "substring" -> Value.Builtin(
      "substring",
      args =>
        requireArgCount("substring", args, 3)
        val text = requireString("substring", args.head)
        buildSubstring(text, args(1), args(2))
    ),
    "string->number" -> unaryStringBuiltin("string->number") { text =>
      parseInteger(text)
    },
    "number->string" -> Value.Builtin(
      "number->string",
      args =>
        requireArgCount("number->string", args, 1)
        Value.StringValue(SchemeString.fromText(requireInteger("number->string", args.head).toString))
    ),
    "symbol->string" -> Value.Builtin(
      "symbol->string",
      args =>
        requireArgCount("symbol->string", args, 1)
        Value.StringValue(SchemeString.fromText(requireSymbol("symbol->string", args.head)))
    ),
    "string->symbol" -> Value.Builtin(
      "string->symbol",
      args =>
        requireArgCount("string->symbol", args, 1)
        Value.SymbolValue(requireString("string->symbol", args.head).text)
    ),
    "string-ref" -> Value.Builtin(
      "string-ref",
      args =>
        requireArgCount("string-ref", args, 2)
        val text  = requireString("string-ref", args.head)
        val index = requireIndex("string-ref", args(1), text.length)
        Value.CharValue(text.codePointAt(index))
    ),
    "string-set!" -> Value.Builtin(
      "string-set!",
      args =>
        requireArgCount("string-set!", args, 3)
        val text  = requireString("string-set!", args.head)
        val index = requireIndex("string-set!", args(1), text.length)
        text.setCodePoint(index, requireChar("string-set!", args(2)))
        Value.VoidValue
    ),
    "string-copy" -> Value.Builtin(
      "string-copy",
      args =>
        requireArgCount("string-copy", args, 1)
        Value.StringValue(requireString("string-copy", args.head).copyString())
    )
  )

  private val predicateBindings: List[(String, Value)] = List(
    "string?" -> predicateBuiltin("string?") {
      case Value.StringValue(_) => true
      case _                    => false
    },
    "char?" -> predicateBuiltin("char?") {
      case Value.CharValue(_) => true
      case _                  => false
    },
    "number?" -> predicateBuiltin("number?") {
      case Value.IntegerValue(_) => true
      case _                     => false
    },
    "boolean?" -> predicateBuiltin("boolean?") {
      case Value.BooleanValue(_) => true
      case _                     => false
    },
    "pair?" -> predicateBuiltin("pair?") {
      case Value.PairValue(_, _) => true
      case _                     => false
    },
    "symbol?" -> predicateBuiltin("symbol?") {
      case Value.SymbolValue(_) => true
      case _                    => false
    }
  )
