package ming

import scala.collection.mutable

/** Pattern matching for syntax-rules / syntax-case macros.
  *
  * Extracted from Macro to keep file sizes under the 300-line limit. All methods are package-private so that Macro and
  * SyntaxCase can call them.
  */
private[ming] object MacroMatch:
  import Macro.Bindings

  /** Flatten a value into (list-elements, optional-tail). */
  def flattenVal(v: SchemeVal): (List[SchemeVal], Option[SchemeVal]) =
    v match
      case SchemeVal.SList(elems) => (elems, None)
      case SchemeVal.SPair(cell) =>
        val (rest, tail) = flattenVal(cell.cdr)
        (cell.car :: rest, tail)
      case other => (Nil, Some(other))

  /** Match a single pattern element against a single input element. */
  def matchOne(
    pattern: SchemeVal,
    input: SchemeVal,
    literals: Set[String]
  ): Option[Bindings] =
    pattern match
      case SchemeVal.SSymbol("_") => Some(Map.empty)
      case SchemeVal.SSymbol(name) if literals.contains(name) =>
        input match
          case SchemeVal.SSymbol(n) if n == name => Some(Map.empty)
          case _                                 => None
      case SchemeVal.SSymbol(name) =>
        Some(Map(name -> Left(input)))
      case SchemeVal.SList(pElems) =>
        val (iElems, iTail) = flattenVal(input)
        tryMatchDotted(pElems, None, iElems, iTail, literals)
      case SchemeVal.SPair(_) =>
        val (pElems, pTail) = flattenVal(pattern)
        val (iElems, iTail) = flattenVal(input)
        tryMatchDotted(pElems, pTail, iElems, iTail, literals)
      case SchemeVal.SBool(a) =>
        input match
          case SchemeVal.SBool(b) if a == b => Some(Map.empty)
          case _                            => None
      case SchemeVal.SInt(a) =>
        input match
          case SchemeVal.SInt(b) if a == b => Some(Map.empty)
          case _                           => None
      case _ => None

  /** Try to match input elements against pattern elements, with optional dotted tail. */
  def tryMatchDotted(
    patElems: List[SchemeVal],
    patTail: Option[SchemeVal],
    inElems: List[SchemeVal],
    inTail: Option[SchemeVal],
    literals: Set[String]
  ): Option[Bindings] =
    val ellipsisIdx = patElems.indexWhere {
      case SchemeVal.SSymbol("...") => true
      case _                        => false
    }

    if ellipsisIdx < 0 then matchFixed(patElems, patTail, inElems, inTail, literals)
    else matchEllipsis(patElems, patTail, inElems, inTail, literals, ellipsisIdx)

  /** Backward-compatible wrapper for proper list matching. */
  def tryMatch(
    patElems: List[SchemeVal],
    inElems: List[SchemeVal],
    literals: Set[String]
  ): Option[Bindings] =
    tryMatchDotted(patElems, None, inElems, None, literals)

  /** Collect all pattern variable names from a pattern element. */
  def collectPatternVars(pattern: SchemeVal, literals: Set[String]): Set[String] =
    pattern match
      case SchemeVal.SSymbol(name) if name != "_" && name != "..." && !literals.contains(name) =>
        Set(name)
      case SchemeVal.SList(elems) => elems.flatMap(collectPatternVars(_, literals)).toSet
      case SchemeVal.SPair(cell) =>
        collectPatternVars(cell.car, literals) ++ collectPatternVars(cell.cdr, literals)
      case _ => Set.empty

  // ── helpers ──────────────────────────────────────────────────────────

  private def matchFixed(
    patElems: List[SchemeVal],
    patTail: Option[SchemeVal],
    inElems: List[SchemeVal],
    inTail: Option[SchemeVal],
    literals: Set[String]
  ): Option[Bindings] =
    if patTail.isDefined then matchFixedDotted(patElems, patTail.get, inElems, inTail, literals)
    else matchFixedProper(patElems, inElems, inTail, literals)

  private def matchFixedProper(
    patElems: List[SchemeVal],
    inElems: List[SchemeVal],
    inTail: Option[SchemeVal],
    literals: Set[String]
  ): Option[Bindings] =
    if patElems.length != inElems.length || inTail.isDefined then return None
    val bindings = mutable.HashMap[String, Either[SchemeVal, List[SchemeVal]]]()
    for (p, i) <- patElems.zip(inElems) do
      matchOne(p, i, literals) match
        case Some(b) => bindings ++= b
        case None    => return None
    Some(bindings.toMap)

  private def matchFixedDotted(
    patElems: List[SchemeVal],
    patTailVal: SchemeVal,
    inElems: List[SchemeVal],
    inTail: Option[SchemeVal],
    literals: Set[String]
  ): Option[Bindings] =
    if inElems.length < patElems.length then return None
    val bindings = mutable.HashMap[String, Either[SchemeVal, List[SchemeVal]]]()
    for (p, i) <- patElems.zip(inElems) do
      matchOne(p, i, literals) match
        case Some(b) => bindings ++= b
        case None    => return None
    val remaining = inElems.drop(patElems.length)
    val restVal = (remaining, inTail) match
      case (Nil, Some(t)) => t
      case (Nil, None)    => SchemeVal.SList(Nil)
      case _ =>
        val tail = inTail.getOrElse(SchemeVal.SList(Nil))
        remaining.foldRight(tail)((e, acc) => SchemeVal.SPair(new MutableCell(e, acc)))
    patTailVal match
      case SchemeVal.SSymbol(name) if !literals.contains(name) && name != "_" =>
        bindings(name) = Left(restVal)
        Some(bindings.toMap)
      case _ =>
        matchOne(patTailVal, restVal, literals) match
          case Some(b) => Some(bindings.toMap ++ b)
          case None    => None

  private def matchEllipsis(
    patElems: List[SchemeVal],
    patTail: Option[SchemeVal],
    inElems: List[SchemeVal],
    inTail: Option[SchemeVal],
    literals: Set[String],
    ellipsisIdx: Int
  ): Option[Bindings] =
    if ellipsisIdx == 0 then return None
    val fixedBefore = patElems.take(ellipsisIdx - 1)
    val ellipsisPat = patElems(ellipsisIdx - 1)
    val fixedAfter  = patElems.drop(ellipsisIdx + 1)
    val minRequired = fixedBefore.length + fixedAfter.length
    if inElems.length < minRequired then return None

    val bindings = mutable.HashMap[String, Either[SchemeVal, List[SchemeVal]]]()

    for (p, i) <- fixedBefore.zip(inElems) do
      matchOne(p, i, literals) match
        case Some(b) => bindings ++= b
        case None    => return None

    val ellipsisInputs = inElems.slice(fixedBefore.length, inElems.length - fixedAfter.length)
    val ellipsisVars   = collectPatternVars(ellipsisPat, literals)
    val accum          = mutable.HashMap[String, mutable.ListBuffer[SchemeVal]]()
    for v <- ellipsisVars do accum(v) = mutable.ListBuffer()

    for inp <- ellipsisInputs do
      matchOne(ellipsisPat, inp, literals) match
        case Some(b) =>
          for (name, value) <- b do
            value match
              case Left(v) => accum.getOrElseUpdate(name, mutable.ListBuffer()) += v
              case _       => ()
        case None => return None

    for (name, values) <- accum do bindings(name) = Right(values.toList)

    for (p, i) <- fixedAfter.zip(inElems.takeRight(fixedAfter.length)) do
      matchOne(p, i, literals) match
        case Some(b) => bindings ++= b
        case None    => return None

    if patTail.isDefined then
      val restVal = inTail.getOrElse(SchemeVal.SList(Nil))
      matchOne(patTail.get, restVal, literals) match
        case Some(b) => bindings ++= b
        case None    => return None

    Some(bindings.toMap)
