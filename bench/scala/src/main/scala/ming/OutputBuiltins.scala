package ming

import BuiltinSupport.*

private[ming] object OutputBuiltins:

  val names: Set[String] = Set(
    "display",
    "write",
    "newline"
  )

  def handles(name: String): Boolean =
    names.contains(name)

  def invoke(name: String, args: List[Value], pos: SourcePos, context: EvalContext): Value =
    name match
      case "display" =>
        context.emit(SchemeRenderer.renderForDisplay(requireSingleArg(name, args, pos)))
        Value.Void
      case "write" =>
        context.emit(SchemeRenderer.render(requireSingleArg(name, args, pos)))
        Value.Void
      case "newline" =>
        requireArgCount(name, args, expected = 0, pos)
        context.emit("\n")
        Value.Void
      case _ =>
        unknownProcedure(name, pos)
