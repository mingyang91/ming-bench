package ming

import SchemeModel.*

private[ming] object SchemeEvaluatorState:

  enum EvalState:
    case ExprState(expr: Expr, env: Env)
    case SequenceState(expressions: List[Expr], env: Env)
    case CallState(procedure: Value, args: List[Value], pos: Option[SourcePos])

  enum StepResult:
    case Final(value: Value)
    case Continue(state: EvalState)
