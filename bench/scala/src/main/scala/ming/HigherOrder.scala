package ming

/** Higher-order procedures and output operations. */
object HigherOrder:

  /** Thread-local output buffer for display/write/newline. */
  val outputBuffer: ThreadLocal[StringBuilder] =
    ThreadLocal.withInitial(() => new StringBuilder())

  def apply(
    name: String,
    args: List[SchemeVal],
    applyProc: (SchemeVal, List[SchemeVal]) => SchemeVal
  ): SchemeVal =
    name match
      case "display" =>
        if args.length != 1 then throw new EvalError("display: expected 1 argument")
        outputBuffer.get().append(args.head.displayRepr)
        SchemeVal.SVoid
      case "write" =>
        if args.length != 1 then throw new EvalError("write: expected 1 argument")
        outputBuffer.get().append(args.head.display)
        SchemeVal.SVoid
      case "newline" =>
        if args.nonEmpty then throw new EvalError("newline: expected 0 arguments")
        outputBuffer.get().append("\n")
        SchemeVal.SVoid
      case "apply" =>
        if args.length < 2 then throw new EvalError("apply: expected at least 2 arguments")
        val proc = args.head
        val lastArg = args.last match
          case SchemeVal.SList(elems) => elems
          case other =>
            throw new EvalError(
              s"apply: last argument must be a list, got ${other.display}"
            )
        val prefixArgs = args.slice(1, args.length - 1)
        applyProc(proc, prefixArgs ++ lastArg)
      case "map" =>
        if args.length < 2 then throw new EvalError("map: expected at least 2 arguments")
        val proc = args.head
        val lists = args.tail.map {
          case SchemeVal.SList(elems) => elems
          case other =>
            throw new EvalError(s"map: expected list, got ${other.display}")
        }
        val len = lists.head.length
        val result = (0 until len).map { i =>
          val mapArgs = lists.map(_(i))
          applyProc(proc, mapArgs)
        }.toList
        SchemeVal.SList(result)
      case "for-each" =>
        if args.length < 2 then throw new EvalError("for-each: expected at least 2 arguments")
        val proc = args.head
        val lists = args.tail.map {
          case SchemeVal.SList(elems) => elems
          case other =>
            throw new EvalError(s"for-each: expected list, got ${other.display}")
        }
        val len = lists.head.length
        (0 until len).foreach { i =>
          val feArgs = lists.map(_(i))
          applyProc(proc, feArgs)
        }
        SchemeVal.SVoid
      case _ => throw new EvalError(s"unknown higher-order op: $name")
