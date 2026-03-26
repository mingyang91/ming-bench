package ming

import SchemeBuiltinSupport.*
import SchemeEvaluatorSupport.*
import SchemeModel.*
import SchemeRuntime.*
import SchemeSyntaxSupport.*

private[ming] object SchemeMacroBuiltins:

  val bindings: List[(String, Value)] = List(
    "syntax->datum" -> Value.Builtin(
      "syntax->datum",
      args =>
        requireArgCount("syntax->datum", args, 1)
        val syntaxObject = requireSyntaxObject("syntax->datum", args.head)
        quoteExpr(syntaxObject.expr)
    ),
    "datum->syntax" -> Value.Builtin(
      "datum->syntax",
      args =>
        requireArgCount("datum->syntax", args, 2)
        val context = requireSyntaxObject("datum->syntax", args.head)
        Value.SyntaxObject(
          datumToExpr(args(1), context.expr.pos, "datum->syntax"),
          context.contextEnv
        )
    ),
    "identifier?" -> Value.Builtin(
      "identifier?",
      args =>
        requireArgCount("identifier?", args, 1)
        Value.BooleanValue(
          args.head match
            case Value.SyntaxObject(Expr.Symbol(_, _), _) => true
            case _                                        => false
        )
    )
  )
