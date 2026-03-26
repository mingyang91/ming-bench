package ming

import SchemeTypes.{PairCell, Value}

object Display:

  def display(v: Value): String =
    displaySafe(v, new java.util.IdentityHashMap[PairCell, java.lang.Boolean]())

  private def displaySafe(
    v: Value,
    seen: java.util.IdentityHashMap[PairCell, java.lang.Boolean]
  ): String = v match
    case Value.VNum(n)         => n.toString
    case Value.VFloat(d)       => displayFloat(d)
    case Value.VRational(n, d) => s"$n/$d"
    case Value.VBool(true)     => "#t"
    case Value.VBool(false)    => "#f"
    case Value.VStr(chars, _)  => s"\"${new String(chars)}\""
    case Value.VChar(c)        => displayChar(c)
    case Value.VList(elems) =>
      "(" + elems.map(e => displaySafe(e, seen)).mkString(" ") + ")"
    case Value.VDottedList(elems, last) =>
      "(" + elems.map(e => displaySafe(e, seen)).mkString(" ") + " . " + displaySafe(
        last,
        seen
      ) + ")"
    case Value.VPair(cell)         => displayPairChain(cell, seen)
    case Value.VSymbol(n)          => n
    case Value.VBuiltin(n)         => s"#<procedure $n>"
    case Value.VLambda(_, _, _, _) => "#<procedure>"
    case Value.VCaseLambda(_)      => "#<procedure>"
    case Value.VMacro(_, _, _)     => "#<macro>"
    case Value.VVector(elems) =>
      "#(" + elems.map(e => displaySafe(e, seen)).mkString(" ") + ")"
    case Value.VRecord(typeName, _) => s"#<record:$typeName>"
    case Value.VVoid                => ""

  private def displayPairChain(
    cell: PairCell,
    seen: java.util.IdentityHashMap[PairCell, java.lang.Boolean]
  ): String =
    if seen.containsKey(cell) then return "#<cycle>"
    seen.put(cell, java.lang.Boolean.TRUE)
    val sb = new StringBuilder("(")
    sb.append(displaySafe(cell.car, seen))
    var cur  = cell.cdr
    var done = false
    while !done do
      cur match
        case Value.VList(Nil) => done = true
        case Value.VPair(c) =>
          if seen.containsKey(c) then
            sb.append(" . #<cycle>")
            done = true
          else
            seen.put(c, java.lang.Boolean.TRUE)
            sb.append(" ").append(displaySafe(c.car, seen))
            cur = c.cdr
        case Value.VList(elems) =>
          elems.foreach(e => sb.append(" ").append(displaySafe(e, seen)))
          done = true
        case Value.VDottedList(elems, last) =>
          elems.foreach(e => sb.append(" ").append(displaySafe(e, seen)))
          sb.append(" . ").append(displaySafe(last, seen))
          done = true
        case other =>
          sb.append(" . ").append(displaySafe(other, seen))
          done = true
    sb.append(")").toString

  private def displayFloat(d: Double): String =
    if d == d.toLong.toDouble && !d.isInfinite then s"${d.toLong}.0"
    else d.toString

  private def displayChar(c: Char): String = c match
    case ' '  => "#\\space"
    case '\n' => "#\\newline"
    case '\t' => "#\\tab"
    case _    => s"#\\$c"

  def displayStr(v: Value): String =
    displayStrSafe(v, new java.util.IdentityHashMap[PairCell, java.lang.Boolean]())

  private def displayStrSafe(
    v: Value,
    seen: java.util.IdentityHashMap[PairCell, java.lang.Boolean]
  ): String = v match
    case Value.VStr(chars, _) => new String(chars)
    case Value.VChar(c)       => c.toString
    case Value.VList(elems) =>
      "(" + elems.map(e => displayStrSafe(e, seen)).mkString(" ") + ")"
    case Value.VDottedList(elems, last) =>
      "(" + elems.map(e => displayStrSafe(e, seen)).mkString(" ") + " . " + displayStrSafe(
        last,
        seen
      ) + ")"
    case Value.VPair(cell) => displayStrPairChain(cell, seen)
    case Value.VVector(elems) =>
      "#(" + elems.map(e => displayStrSafe(e, seen)).mkString(" ") + ")"
    case other => displaySafe(other, seen)

  private def displayStrPairChain(
    cell: PairCell,
    seen: java.util.IdentityHashMap[PairCell, java.lang.Boolean]
  ): String =
    if seen.containsKey(cell) then return "#<cycle>"
    seen.put(cell, java.lang.Boolean.TRUE)
    val sb = new StringBuilder("(")
    sb.append(displayStrSafe(cell.car, seen))
    var cur  = cell.cdr
    var done = false
    while !done do
      cur match
        case Value.VList(Nil) => done = true
        case Value.VPair(c) =>
          if seen.containsKey(c) then
            sb.append(" . #<cycle>")
            done = true
          else
            seen.put(c, java.lang.Boolean.TRUE)
            sb.append(" ").append(displayStrSafe(c.car, seen))
            cur = c.cdr
        case Value.VList(elems) =>
          elems.foreach(e => sb.append(" ").append(displayStrSafe(e, seen)))
          done = true
        case other =>
          sb.append(" . ").append(displayStrSafe(other, seen))
          done = true
    sb.append(")").toString
