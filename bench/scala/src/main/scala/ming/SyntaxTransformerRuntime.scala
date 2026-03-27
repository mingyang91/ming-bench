package ming

import BuiltinSupport.*

private[ming] object SyntaxTransformerRuntime:

  enum TransformerValue:
    case Datum(value: Value)
    case Syntax(expr: Expr)
    case RepeatedSyntax(values: Vector[Expr])

  final case class TransformerContext(
    bindings: Map[String, TransformerValue],
    definitionEnv: Env
  ):

    def extend(newBindings: Map[String, TransformerValue]): TransformerContext =
      copy(bindings = bindings ++ newBindings)

    def syntaxBindings: PatternBindings =
      PatternBindings(
        single = bindings.collect { case (name, TransformerValue.Syntax(expr)) => name -> expr },
        repeated = bindings.collect { case (name, TransformerValue.RepeatedSyntax(values)) => name -> values }
      )

  def initialContext(
    parameter: String,
    invocation: Expr.ListExpr,
    definitionEnv: Env
  ): TransformerContext =
    TransformerContext(
      bindings = Map(parameter -> TransformerValue.Syntax(invocation)),
      definitionEnv = definitionEnv
    )

  def evalBody(body: List[Expr], context: TransformerContext): TransformerValue =
    body.foldLeft[TransformerValue](TransformerValue.Datum(Value.Void)) { (_, expr) =>
      evalExpr(expr, context)
    }

  def evalExpr(expr: Expr, context: TransformerContext): TransformerValue =
    expr match
      case Expr.IntLit(_, _) | Expr.RationalLit(_, _, _) | Expr.InexactLit(_, _) | Expr.BoolLit(_, _) |
          Expr.StringLit(_, _) | Expr.CharLit(_, _) | Expr.VectorExpr(_, _) =>
        TransformerValue.Datum(ValueSemantics.quote(expr))
      case Expr.Symbol(name, pos) =>
        evalSymbol(name, pos, context)
      case Expr.ListExpr(Nil, pos) =>
        throw EvalError.at(pos, "cannot evaluate empty list")
      case Expr.ListExpr(Expr.Symbol("quote", formPos) :: args, _) =>
        evalQuote(args, formPos)
      case Expr.ListExpr(Expr.Symbol("syntax", formPos) :: args, _) =>
        evalSyntax(args, context, formPos)
      case Expr.ListExpr(Expr.Symbol("syntax-case", formPos) :: args, _) =>
        SyntaxTransformerForms.evalSyntaxCase(args, context, formPos)
      case Expr.ListExpr(Expr.Symbol("with-syntax", formPos) :: args, _) =>
        SyntaxTransformerForms.evalWithSyntax(args, context, formPos)
      case Expr.ListExpr(Expr.Symbol("if", formPos) :: args, _) =>
        SyntaxTransformerForms.evalIf(args, context, formPos)
      case Expr.ListExpr(Expr.Symbol("begin", _) :: args, _) =>
        if args.isEmpty then TransformerValue.Datum(Value.Void)
        else evalBody(args, context)
      case Expr.ListExpr(Expr.Symbol("let", formPos) :: args, _) =>
        SyntaxTransformerForms.evalLet(args, context, formPos, sequential = false)
      case Expr.ListExpr(Expr.Symbol("let*", formPos) :: args, _) =>
        SyntaxTransformerForms.evalLet(args, context, formPos, sequential = true)
      case Expr.ListExpr(Expr.Symbol("and", _) :: args, _) =>
        SyntaxTransformerForms.evalAnd(args, context)
      case Expr.ListExpr(Expr.Symbol("or", _) :: args, _) =>
        SyntaxTransformerForms.evalOr(args, context)
      case Expr.ListExpr(Expr.Symbol("syntax->datum", formPos) :: args, _) =>
        SyntaxTransformerForms.evalSyntaxToDatum(args, context, formPos)
      case Expr.ListExpr(Expr.Symbol("datum->syntax", formPos) :: args, _) =>
        SyntaxTransformerForms.evalDatumToSyntax(args, context, formPos)
      case Expr.ListExpr(Expr.Symbol("identifier?", formPos) :: args, _) =>
        SyntaxTransformerForms.evalIdentifierPredicate(args, context, formPos)
      case Expr.ListExpr(Expr.Symbol("free-identifier=?", formPos) :: args, _) =>
        SyntaxTransformerForms.evalIdentifierEquality(args, context, formPos)
      case Expr.ListExpr(Expr.Symbol("bound-identifier=?", formPos) :: args, _) =>
        SyntaxTransformerForms.evalIdentifierEquality(args, context, formPos)
      case Expr.ListExpr(head :: args, pos) =>
        evalApplication(head, args, context, pos)

  def evalSymbol(
    name: String,
    pos: SourcePos,
    context: TransformerContext
  ): TransformerValue =
    context.bindings.get(name) match
      case Some(value @ TransformerValue.Datum(_)) =>
        value
      case Some(value @ TransformerValue.Syntax(_)) =>
        value
      case Some(TransformerValue.RepeatedSyntax(_)) =>
        throw EvalError.at(pos, s"repeated pattern variable used outside syntax template: $name")
      case None =>
        context.definitionEnv.lookup(name) match
          case Some(value) =>
            TransformerValue.Datum(value)
          case None =>
            throw EvalError.at(pos, s"unbound variable: $name")

  def evalQuote(args: List[Expr], pos: SourcePos): TransformerValue =
    args match
      case expr :: Nil =>
        TransformerValue.Datum(ValueSemantics.quote(expr))
      case _ =>
        throw EvalError.at(pos, "quote expects exactly 1 argument")

  def evalSyntax(
    args: List[Expr],
    context: TransformerContext,
    pos: SourcePos
  ): TransformerValue =
    args match
      case template :: Nil =>
        TransformerValue.Syntax(
          SyntaxRuleTemplateExpander.expand(template, context.syntaxBindings, context.definitionEnv)
        )
      case _ =>
        throw EvalError.at(pos, "syntax expects exactly 1 argument")

  def evalApplication(
    head: Expr,
    args: List[Expr],
    context: TransformerContext,
    pos: SourcePos
  ): TransformerValue =
    val procedure = requireDatum(evalExpr(head, context), head.pos, "procedure application")
    val arguments = args.map(arg => requireDatum(evalExpr(arg, context), arg.pos, "procedure argument"))
    TransformerValue.Datum(
      ExpressionEvaluator.run(EvalStep.InvokeProcedure(procedure, arguments, pos), new EvalContext)
    )

  def patternBindings(bindings: PatternBindings): Map[String, TransformerValue] =
    bindings.single.view.mapValues(TransformerValue.Syntax.apply).toMap ++
      bindings.repeated.view.mapValues(TransformerValue.RepeatedSyntax.apply).toMap

  def requireDatum(result: TransformerValue, pos: SourcePos, context: String): Value =
    result match
      case TransformerValue.Datum(value) =>
        value
      case TransformerValue.Syntax(_) | TransformerValue.RepeatedSyntax(_) =>
        throw EvalError.at(pos, s"$context expected a datum")

  def requireSyntax(result: TransformerValue, pos: SourcePos, context: String): Expr =
    result match
      case TransformerValue.Syntax(expr) =>
        expr
      case TransformerValue.Datum(_) | TransformerValue.RepeatedSyntax(_) =>
        throw EvalError.at(pos, s"$context expected a syntax object")

  def datumToSyntax(value: Value, pos: SourcePos): Expr =
    value match
      case Value.IntVal(number) =>
        Expr.IntLit(number, pos)
      case Value.RationalVal(numerator, denominator) =>
        Expr.RationalLit(numerator, denominator, pos)
      case Value.InexactVal(number) =>
        Expr.InexactLit(number, pos)
      case Value.BoolVal(boolean) =>
        Expr.BoolLit(boolean, pos)
      case Value.StringVal(text) =>
        Expr.StringLit(text.text, pos)
      case Value.CharVal(ch) =>
        Expr.CharLit(ch, pos)
      case Value.SymbolVal(symbol) =>
        Expr.Symbol(symbol, pos)
      case Value.EmptyList =>
        Expr.ListExpr(Nil, pos)
      case _: Value.PairVal =>
        val listParts = ExprListSupport.parseValueList(value, pos, "datum->syntax")
        ExprListSupport.buildExpr(
          items = listParts.items.map(item => datumToSyntax(item, pos)),
          tail = listParts.tail.map(tailValue => datumToSyntax(tailValue, pos)),
          pos = pos
        )
      case Value.VectorVal(vector) =>
        Expr.VectorExpr(vector.elements.map(element => datumToSyntax(element, pos)), pos)
      case other =>
        throw EvalError.at(pos, s"datum->syntax expected a datum, got ${ValueSemantics.typeName(other)}")
