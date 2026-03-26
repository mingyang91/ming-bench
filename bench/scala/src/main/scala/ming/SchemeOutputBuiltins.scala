package ming

import SchemeModel.*
import SchemeRuntime.*

private[ming] object SchemeOutputBuiltins:

  def bindings(output: StringBuilder): List[(String, Value)] = List(
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
