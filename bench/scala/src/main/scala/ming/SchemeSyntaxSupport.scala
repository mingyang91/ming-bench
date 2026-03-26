package ming

import SchemeBuiltinSupport.*
import SchemeMacros.*
import SchemeModel.*
import SchemeRuntime.*

private[ming] object SchemeSyntaxSupport:

  type RuntimeSyntaxBindings = Map[String, Vector[Value.SyntaxObject]]

  private val MacroContextBinding = "%ming:macro-context%"

  def injectMacroContext(parent: Env, context: SyntaxContext): Env =
    val env = new Env(Some(parent))
    env.define(MacroContextBinding, Value.SyntaxContextValue(context))
    env

  def extendMacroContext(parent: Env, context: SyntaxContext): Env =
    injectMacroContext(parent, context)

  def requireMacroContext(env: Env, formName: String): SyntaxContext =
    env.lookupValueCell(MacroContextBinding).map(_.value) match
      case Some(Value.SyntaxContextValue(context)) =>
        context
      case _ =>
        throw new EvalError(s"$formName can only be used in a syntax transformer")

  def extendContext(
    context: SyntaxContext,
    bindings: PatternBindings,
    patternVariables: Set[String]
  ): SyntaxContext =
    context.copy(
      patternBindings = context.patternBindings ++ bindings,
      patternVariables = context.patternVariables ++ patternVariables
    )

  def runtimeSyntaxBindings(
    bindings: PatternBindings,
    contextEnv: Env
  ): RuntimeSyntaxBindings =
    bindings.map { case (name, exprs) =>
      val syntaxObjects: Vector[Value.SyntaxObject] =
        exprs.map(expr => Value.SyntaxObject(expr, contextEnv))
      name -> syntaxObjects
    }

  def bindRuntimeSyntaxVariables(env: Env, bindings: RuntimeSyntaxBindings): Unit =
    bindings.foreach { case (name, syntaxObjects) =>
      val value: Value =
        if syntaxObjects.length == 1 then syntaxObjects.head
        else makeList(syntaxObjects.toList)
      env.define(name, value)
    }

  def datumToExpr(value: Value, pos: SourcePos, context: String): Expr =
    value match
      case Value.IntegerValue(number) =>
        Expr.IntegerLiteral(number, pos)
      case Value.RationalValue(numerator, denominator) =>
        Expr.RationalLiteral(numerator, denominator, pos)
      case Value.InexactValue(number) =>
        Expr.InexactLiteral(number, pos)
      case Value.BooleanValue(flag) =>
        Expr.BooleanLiteral(flag, pos)
      case Value.StringValue(text) =>
        Expr.StringLiteral(text.text, pos)
      case Value.CharValue(codePoint) =>
        Expr.CharLiteral(codePoint, pos)
      case Value.SymbolValue(name) =>
        Expr.Symbol(name, pos)
      case Value.NilValue =>
        Expr.ListExpr(Nil, pos)
      case list @ Value.PairValue(_, _) =>
        pairToExpr(list, pos, context)
      case Value.VectorValue(elements) =>
        Expr.VectorExpr(elements.toList.map(datumToExpr(_, pos, context)), pos)
      case Value.SyntaxObject(expr, _) =>
        expr
      case other =>
        throw new EvalError(s"$context expected a datum, got ${render(other)}")

  private def pairToExpr(pair: Value.PairValue, pos: SourcePos, context: String): Expr =
    def loop(current: Value, reversedItems: List[Expr]): Expr =
      current match
        case Value.PairValue(car, cdr) =>
          loop(cdr, datumToExpr(car, pos, context) :: reversedItems)
        case Value.NilValue =>
          Expr.ListExpr(reversedItems.reverse, pos)
        case tail =>
          Expr.ListExpr(
            reversedItems.reverse ::: List(Expr.Symbol(".", pos), datumToExpr(tail, pos, context)),
            pos
          )

    loop(pair, Nil)
