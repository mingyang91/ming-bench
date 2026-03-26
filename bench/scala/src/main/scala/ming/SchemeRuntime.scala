package ming

import java.util.IdentityHashMap

import scala.annotation.tailrec
import scala.collection.mutable

import SchemeModel.*
import SchemeNumbers.*

private[ming] object SchemeRuntime:

  final private case class PairRenderState(partsReversed: List[String], current: Value, first: Boolean)

  private enum RenderMode:
    case Write
    case Display

  def baseEnv(output: StringBuilder = new StringBuilder): Env =
    val env = new Env(None)
    SchemeBuiltins.bindings(output).foreach { case (name, value) =>
      env.define(name, value)
    }
    env

  def isTruthy(value: Value): Boolean =
    value match
      case Value.BooleanValue(false) => false
      case _                         => true

  def isProcedure(value: Value): Boolean =
    value match
      case Value.Builtin(_, _) => true
      case Value.Closure(_, _, _, _, _) =>
        true
      case Value.CaseClosure(_) => true
      case _                    => false

  def requireSingleValue(value: Value): Value =
    value match
      case Value.MultiValues(Nil) =>
        throw new EvalError("expected 1 value, got 0")
      case Value.MultiValues(single :: Nil) =>
        single
      case Value.MultiValues(values) =>
        throw new EvalError(s"expected 1 value, got ${values.length}")
      case other =>
        other

  def toValueList(value: Value): List[Value] =
    value match
      case Value.MultiValues(values) => values
      case other                     => List(other)

  def render(value: Value): String =
    renderValue(value, RenderMode.Write, new IdentityHashMap[AnyRef, java.lang.Boolean]())

  def renderForDisplay(value: Value): String =
    renderValue(value, RenderMode.Display, new IdentityHashMap[AnyRef, java.lang.Boolean]())

  def makeList(values: List[Value]): Value =
    values.foldRight(Value.NilValue: Value) { (car, cdr) =>
      Value.PairValue(car, cdr)
    }

  def ensureDistinct(names: List[String], context: String): Unit =
    if names.distinct.length != names.length then throw new EvalError(s"$context must be distinct")

  def requireArgCount(name: String, args: List[Value], exact: Int): Unit =
    if args.length != exact then throw new EvalError(s"$name expected $exact argument(s), got ${args.length}")

  private def renderValue(
    value: Value,
    mode: RenderMode,
    active: IdentityHashMap[AnyRef, java.lang.Boolean]
  ): String =
    value match
      case number @ (Value.IntegerValue(_) | Value.RationalValue(_, _) | Value.InexactValue(_)) =>
        SchemeNumbers.render(number)
      case Value.BooleanValue(flag) =>
        if flag then "#t" else "#f"
      case Value.StringValue(text) =>
        mode match
          case RenderMode.Write   => s""""${escapeString(text.text)}""""
          case RenderMode.Display => text.text
      case Value.CharValue(codePoint) =>
        mode match
          case RenderMode.Write   => renderChar(codePoint)
          case RenderMode.Display => codePointToString(codePoint)
      case Value.SymbolValue(name) =>
        name
      case Value.NilValue =>
        "()"
      case pair: Value.PairValue =>
        renderPair(pair, mode, active)
      case Value.RecordValue(recordType, _) =>
        s"#<record ${recordType.name}>"
      case Value.VectorValue(elements) =>
        renderVector(elements, mode, active)
      case Value.Builtin(name, _) =>
        s"#<procedure:$name>"
      case Value.Closure(Some(name), _, _, _, _) =>
        s"#<procedure:$name>"
      case Value.Closure(None, _, _, _, _) =>
        "#<procedure>"
      case Value.CaseClosure(_) =>
        "#<procedure>"
      case Value.SyntaxObject(_, _) =>
        "#<syntax>"
      case Value.SyntaxContextValue(_) =>
        "#<syntax-context>"
      case Value.MultiValues(_) =>
        "#<values>"
      case Value.UninitializedValue(name) =>
        s"#<uninitialized:$name>"
      case Value.VoidValue =>
        "#<void>"

  private def renderPair(
    pair: Value.PairValue,
    mode: RenderMode,
    active: IdentityHashMap[AnyRef, java.lang.Boolean]
  ): String =
    val rootRef: AnyRef = pair
    if active.containsKey(rootRef) then "#<cycle>"
    else
      val added = mutable.ArrayBuffer[AnyRef](rootRef)
      active.put(rootRef, java.lang.Boolean.TRUE)

      try
        renderPairLoop(PairRenderState(Nil, pair, first = true), mode, active, added)
      finally
        added.foreach(ref => active.remove(ref))

  @tailrec
  private def renderPairLoop(
    state: PairRenderState,
    mode: RenderMode,
    active: IdentityHashMap[AnyRef, java.lang.Boolean],
    added: mutable.ArrayBuffer[AnyRef]
  ): String =
    state.current match
      case currentPair: Value.PairValue =>
        registerPair(currentPair, state.first, active, added) match
          case Some(cycleMarker) =>
            pairText(state.partsReversed, s" . $cycleMarker")
          case None =>
            renderPairLoop(
              PairRenderState(
                renderValue(currentPair.car, mode, active) :: state.partsReversed,
                currentPair.cdr,
                first = false
              ),
              mode,
              active,
              added
            )
      case Value.NilValue =>
        pairText(state.partsReversed, "")
      case other =>
        pairText(state.partsReversed, s" . ${renderValue(other, mode, active)}")

  private def registerPair(
    pair: Value.PairValue,
    first: Boolean,
    active: IdentityHashMap[AnyRef, java.lang.Boolean],
    added: mutable.ArrayBuffer[AnyRef]
  ): Option[String] =
    if first then None
    else
      val ref: AnyRef = pair
      if active.containsKey(ref) then Some("#<cycle>")
      else
        active.put(ref, java.lang.Boolean.TRUE)
        added += ref
        None

  private def pairText(partsReversed: List[String], tail: String): String =
    partsReversed.reverse.mkString("(", " ", s"$tail)")

  private def renderVector(
    elements: mutable.ArrayBuffer[Value],
    mode: RenderMode,
    active: IdentityHashMap[AnyRef, java.lang.Boolean]
  ): String =
    val ref: AnyRef = elements
    if active.containsKey(ref) then "#<cycle>"
    else
      active.put(ref, java.lang.Boolean.TRUE)
      try elements.iterator.map(renderValue(_, mode, active)).mkString("#(", " ", ")")
      finally active.remove(ref)

  private def escapeString(text: String): String =
    text.flatMap {
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c    => c.toString
    }

  private def renderChar(codePoint: Int): String =
    codePoint match
      case 32 => "#\\space"
      case 10 => "#\\newline"
      case _  => s"#\\${codePointToString(codePoint)}"

  private def codePointToString(codePoint: Int): String =
    new String(Character.toChars(codePoint))
