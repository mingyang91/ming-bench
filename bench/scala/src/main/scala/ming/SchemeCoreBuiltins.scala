package ming

import scala.annotation.tailrec

import SchemeBuiltinSupport.*
import SchemeModel.*
import SchemeRuntime.*

private[ming] object SchemeCoreBuiltins:

  val bindings: List[(String, Value)] = List(
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
    "cddr" -> Value.Builtin(
      "cddr",
      args =>
        requireArgCount("cddr", args, 1)
        requirePair("cddr", requirePair("cddr", args.head).cdr).cdr
    ),
    "set-car!" -> Value.Builtin(
      "set-car!",
      args =>
        requireArgCount("set-car!", args, 2)
        val pair = requirePair("set-car!", args.head)
        pair.car = args(1)
        Value.VoidValue
    ),
    "set-cdr!" -> Value.Builtin(
      "set-cdr!",
      args =>
        requireArgCount("set-cdr!", args, 2)
        val pair = requirePair("set-cdr!", args.head)
        pair.cdr = args(1)
        Value.VoidValue
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
    ),
    "reverse" -> Value.Builtin(
      "reverse",
      args =>
        requireArgCount("reverse", args, 1)
        makeList(properListElements("reverse", args.head).reverse)
    ),
    "apply" -> Value.Builtin(
      "apply",
      args =>
        requireMinArgCount("apply", args, 2)
        val procedure  = args.head
        val prefixArgs = args.slice(1, args.length - 1)
        val listArgs   = properListElements("apply", args.last)
        SchemeInterpreter.applyProcedure(procedure, prefixArgs ++ listArgs)
    ),
    "dynamic-wind"                   -> controlPlaceholder("dynamic-wind"),
    "call/cc"                        -> controlPlaceholder("call/cc"),
    "call-with-current-continuation" -> controlPlaceholder("call-with-current-continuation"),
    "eq?" -> Value.Builtin(
      "eq?",
      args =>
        requireArgCount("eq?", args, 2)
        Value.BooleanValue(eqValues(args.head, args(1)))
    ),
    "eqv?" -> Value.Builtin(
      "eqv?",
      args =>
        requireArgCount("eqv?", args, 2)
        Value.BooleanValue(eqvValues(args.head, args(1)))
    ),
    "equal?" -> Value.Builtin(
      "equal?",
      args =>
        requireArgCount("equal?", args, 2)
        Value.BooleanValue(equalValues(args.head, args(1)))
    ),
    "list-ref" -> Value.Builtin(
      "list-ref",
      args =>
        requireArgCount("list-ref", args, 2)
        listRef(args.head, requireNonNegativeIndex("list-ref", args(1)))
    ),
    "list-tail" -> Value.Builtin(
      "list-tail",
      args =>
        requireArgCount("list-tail", args, 2)
        listTail(args.head, requireNonNegativeIndex("list-tail", args(1)))
    ),
    "memq" -> Value.Builtin(
      "memq",
      args =>
        requireArgCount("memq", args, 2)
        member(args.head, args(1), eqValues, "memq")
    ),
    "memv" -> Value.Builtin(
      "memv",
      args =>
        requireArgCount("memv", args, 2)
        member(args.head, args(1), eqvValues, "memv")
    ),
    "member" -> Value.Builtin(
      "member",
      args =>
        requireArgCount("member", args, 2)
        member(args.head, args(1), equalValues, "member")
    ),
    "assq" -> Value.Builtin(
      "assq",
      args =>
        requireArgCount("assq", args, 2)
        assoc(args.head, args(1), eqValues, "assq")
    ),
    "assv" -> Value.Builtin(
      "assv",
      args =>
        requireArgCount("assv", args, 2)
        assoc(args.head, args(1), eqvValues, "assv")
    ),
    "assoc" -> Value.Builtin(
      "assoc",
      args =>
        requireArgCount("assoc", args, 2)
        assoc(args.head, args(1), equalValues, "assoc")
    ),
    "map" -> Value.Builtin(
      "map",
      args =>
        requireMinArgCount("map", args, 2)
        mapValues(args.head, args.tail)
    ),
    "for-each" -> Value.Builtin(
      "for-each",
      args =>
        requireMinArgCount("for-each", args, 2)
        forEachValues(args.head, args.tail)
    )
  )

  private def listRef(list: Value, index: Int): Value =
    @tailrec
    def loop(current: Value, remaining: Int): Value =
      current match
        case Value.PairValue(car, cdr) =>
          if remaining == 0 then car else loop(cdr, remaining - 1)
        case _ =>
          throw new EvalError("list-ref index out of range")

    loop(list, index)

  private def listTail(list: Value, index: Int): Value =
    @tailrec
    def loop(current: Value, remaining: Int): Value =
      if remaining == 0 then current
      else
        current match
          case Value.PairValue(_, cdr) => loop(cdr, remaining - 1)
          case _                       => throw new EvalError("list-tail index out of range")

    loop(list, index)

  private def member(
    key: Value,
    list: Value,
    predicate: (Value, Value) => Boolean,
    name: String
  ): Value =
    @tailrec
    def loop(current: Value): Value =
      current match
        case Value.NilValue =>
          Value.BooleanValue(false)
        case pair @ Value.PairValue(car, cdr) =>
          if predicate(key, car) then pair else loop(cdr)
        case _ =>
          throw new EvalError(s"$name expected a proper list")

    loop(list)

  private def assoc(
    key: Value,
    alist: Value,
    predicate: (Value, Value) => Boolean,
    name: String
  ): Value =
    @tailrec
    def loop(current: Value): Value =
      current match
        case Value.NilValue =>
          Value.BooleanValue(false)
        case Value.PairValue(entry, rest) =>
          entry match
            case pair @ Value.PairValue(car, _) =>
              if predicate(key, car) then pair else loop(rest)
            case _ =>
              throw new EvalError(s"$name expected an association list")
        case _ =>
          throw new EvalError(s"$name expected an association list")

    loop(alist)

  private def controlPlaceholder(name: String): Value =
    Value.Builtin(
      name,
      _ => throw new IllegalStateException(s"$name should be handled by the evaluator")
    )

  private def mapValues(procedure: Value, lists: List[Value]): Value =
    @tailrec
    def loop(cursors: List[Value], reversedResults: List[Value]): Value =
      if cursors.forall(_ == Value.NilValue) then makeList(reversedResults.reverse)
      else if cursors.exists(_ == Value.NilValue) then throw new EvalError("map expected lists of equal length")
      else
        val (callArgs, nextCursors) = cursors.map {
          case Value.PairValue(car, cdr) => (car, cdr)
          case _                         => throw new EvalError("map expected a proper list")
        }.unzip
        val mapped = SchemeInterpreter.applyProcedure(procedure, callArgs)
        loop(nextCursors, mapped :: reversedResults)

    loop(lists, Nil)

  private def forEachValues(procedure: Value, lists: List[Value]): Value =
    @tailrec
    def loop(cursors: List[Value]): Value =
      if cursors.forall(_ == Value.NilValue) then Value.VoidValue
      else if cursors.exists(_ == Value.NilValue) then throw new EvalError("for-each expected lists of equal length")
      else
        val (callArgs, nextCursors) = cursors.map {
          case Value.PairValue(car, cdr) => (car, cdr)
          case _                         => throw new EvalError("for-each expected a proper list")
        }.unzip
        SchemeInterpreter.applyProcedure(procedure, callArgs)
        loop(nextCursors)

    loop(lists)
