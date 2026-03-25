package ming

private[ming] object SchemeInterpreterListDispatch:

  import SchemeInterpreter.{EvalState, Expr, Resume, Value}

  private type EvalExprState =
    (Expr, Env, MacroScope, Resume) => EvalState

  private type EvalSequenceState =
    (List[Expr], Env, MacroScope, Resume) => EvalState

  private type ApplyProcedureState =
    (Value, List[Value], SourcePos, Resume) => EvalState

  def evalNamedFormState(
    name: String,
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume,
    evalExprState: EvalExprState,
    evalSequenceState: EvalSequenceState,
    applyProcedureState: ApplyProcedureState
  ): Option[EvalState] =
    name match
      case "define-record-type" | "define-syntax" | "define" | "set!" | "begin" | "if" | "quote" | "syntax" |
          "syntax-case" | "lambda" | "with-syntax" | "case-lambda" | "do" | "and" | "or" | "quasiquote" =>
        Some(evalCoreFormState(name, args, env, macros, pos, cont, evalExprState, evalSequenceState))
      case "let" | "let*" | "letrec" | "letrec*" =>
        Some(
          evalBindingFormState(
            name,
            args,
            env,
            macros,
            pos,
            cont,
            evalExprState,
            evalSequenceState,
            applyProcedureState
          )
        )
      case "cond" | "guard" | "case" =>
        Some(
          evalBranchingFormState(
            name,
            args,
            env,
            macros,
            pos,
            cont,
            evalExprState,
            evalSequenceState,
            applyProcedureState
          )
        )
      case _ =>
        None

  private def evalCoreFormState(
    name: String,
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume,
    evalExprState: EvalExprState,
    evalSequenceState: EvalSequenceState
  ): EvalState =
    name match
      case "define-record-type" =>
        cont(SchemeRecords.evalDefineRecordType(args, env, pos))
      case "define-syntax" =>
        SchemeInterpreterSpecialForms.evalDefineSyntaxState(args, env, macros, pos, cont)
      case "define" =>
        SchemeInterpreterSpecialForms.evalDefineState(args, env, macros, pos, cont, evalExprState)
      case "set!" =>
        SchemeInterpreterSpecialForms.evalSetState(args, env, macros, pos, cont, evalExprState)
      case "begin" =>
        evalSequenceState(args, env, macros, cont)
      case "if" =>
        SchemeInterpreterConditionalForms.evalIfState(args, env, macros, pos, cont, evalExprState)
      case "quote" =>
        SchemeInterpreterSpecialForms.evalQuoteState(args, pos, cont)
      case "quasiquote" =>
        SchemeInterpreterSpecialForms.evalQuasiquoteState(args, env, macros, pos, cont)
      case "syntax" =>
        SchemeInterpreterSpecialForms.evalSyntaxState(args, env, macros, pos, cont)
      case "syntax-case" =>
        SchemeInterpreterSpecialForms.evalSyntaxCaseState(args, env, macros, pos, cont)
      case "lambda" =>
        SchemeInterpreterSpecialForms.evalLambdaState(args, env, macros, pos, cont)
      case "with-syntax" =>
        SchemeInterpreterSpecialForms.evalWithSyntaxState(args, env, macros, pos, cont)
      case "case-lambda" =>
        cont(SchemeProcedures.evalCaseLambda(args, env, macros, pos))
      case "do" =>
        evalExprState(SchemeInterpreterDoSupport.expand(args, pos), env, macros, cont)
      case "and" =>
        SchemeInterpreterConditionalForms.evalAndState(args, env, macros, cont, evalExprState)
      case "or" =>
        SchemeInterpreterConditionalForms.evalOrState(args, env, macros, cont, evalExprState)
      case _ =>
        throw new IllegalStateException("unreachable")

  private def evalBindingFormState(
    name: String,
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume,
    evalExprState: EvalExprState,
    evalSequenceState: EvalSequenceState,
    applyProcedureState: ApplyProcedureState
  ): EvalState =
    name match
      case "let" =>
        SchemeInterpreterBindingForms.evalLetState(
          args,
          env,
          macros,
          pos,
          cont,
          evalExprState,
          evalSequenceState,
          applyProcedureState
        )
      case "let*" =>
        SchemeInterpreterBindingForms.evalLetStarState(
          args,
          env,
          macros,
          pos,
          cont,
          evalExprState,
          evalSequenceState
        )
      case "letrec" =>
        SchemeInterpreterBindingForms.evalLetrecState(
          args,
          env,
          macros,
          pos,
          cont,
          evalExprState,
          evalSequenceState
        )
      case "letrec*" =>
        SchemeInterpreterBindingForms.evalLetrecStarState(
          args,
          env,
          macros,
          pos,
          cont,
          evalExprState,
          evalSequenceState
        )
      case _ =>
        throw new IllegalStateException("unreachable")

  private def evalBranchingFormState(
    name: String,
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume,
    evalExprState: EvalExprState,
    evalSequenceState: EvalSequenceState,
    applyProcedureState: ApplyProcedureState
  ): EvalState =
    name match
      case "cond" =>
        SchemeInterpreterConditionalForms.evalCondState(
          args,
          env,
          macros,
          pos,
          cont,
          evalExprState,
          evalSequenceState,
          applyProcedureState
        )
      case "guard" =>
        SchemeInterpreterConditionalForms.evalGuardState(
          args,
          env,
          macros,
          pos,
          cont,
          evalExprState
        )
      case "case" =>
        SchemeInterpreterConditionalForms.evalCaseState(
          args,
          env,
          macros,
          pos,
          cont,
          evalExprState,
          evalSequenceState
        )
      case _ =>
        throw new IllegalStateException("unreachable")
