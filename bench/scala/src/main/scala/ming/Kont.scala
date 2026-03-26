package ming

import SchemeTypes.{Env, Pos, Value}

/** A dynamic-wind extent entry (identity-based for wind stack sharing). */
class WindEntry(val inThunk: Value, val outThunk: Value)

/** Explicit continuation frames for the CEK machine. call/cc captures these as first-class values.
  */
enum Kont:
  case Halt
  case Seq(remaining: List[Expr], env: Env, next: Kont)
  case Define(name: String, env: Env, next: Kont)
  case Set(name: String, env: Env, pos: Pos, next: Kont)
  case If(thenExpr: Expr, elseExpr: Option[Expr], env: Env, next: Kont)
  case And(remaining: List[Expr], env: Env, next: Kont)
  case Or(remaining: List[Expr], env: Env, next: Kont)
  case AppHead(argExprs: List[Expr], env: Env, pos: Pos, next: Kont)

  case AppArg(
    func: Value,
    done: List[Value],
    remaining: List[Expr],
    env: Env,
    pos: Pos,
    next: Kont
  )

  case LetBind(
    varName: String,
    remaining: List[(String, Expr)],
    letEnv: Env,
    initEnv: Env,
    body: List[Expr],
    next: Kont
  )

  case LetStarBind(
    varName: String,
    remaining: List[(String, Expr)],
    letEnv: Env,
    body: List[Expr],
    next: Kont
  )

  case NamedLetArgs(
    params: List[String],
    doneVals: List[Value],
    remainingExprs: List[Expr],
    initEnv: Env,
    loopEnv: Env,
    body: List[Expr],
    next: Kont
  )

  case CondTest(
    body: List[Expr],
    remainingClauses: List[Expr],
    env: Env,
    next: Kont
  )

  case CondArrow(
    testValue: Value,
    env: Env,
    pos: Pos,
    next: Kont
  )

  // dynamic-wind continuation frames
  case DynWindAfterIn(bodyThunk: Value, entry: WindEntry, env: Env, pos: Pos, next: Kont)
  case DynWindAfterBody(entry: WindEntry, env: Env, pos: Pos, next: Kont)
  case DynWindAfterOut(bodyResult: Value, next: Kont)

  // Wind transition: list of (isIn, entry) pairs to process before applying savedK
  case DynWindTransition(
    ops: List[(Boolean, WindEntry)],
    value: Value,
    savedK: Kont,
    env: Env,
    pos: Pos
  )

  // Exception handling
  case PopHandler(next: Kont)
  case GuardTest(variable: String, clauses: List[Expr], env: Env, next: Kont)
  case RaiseReturn(next: Kont)
  case CaseKey(clauses: List[Expr], env: Env, next: Kont)
  case CallWithValues(consumer: Value, env: Env, pos: Pos, next: Kont)
  case MacroTransformerResult(callEnv: Env, defEnv: Env, pos: Pos, next: Kont)

/** Exception for continuation invocation from non-CEK code paths (builtins). */
class ContinuationInvoke(val value: Value, val kont: Kont) extends RuntimeException(null, null, true, false)

/** Exception for Scheme raise — caught by the CEK loop to dispatch to exception handlers. */
class SchemeRaise(val value: Value) extends RuntimeException(null, null, true, false)

/** Exception handler installed by guard or with-exception-handler. */
enum ExceptionHandler:
  case Guard(variable: String, clauses: List[Expr], env: Env, kont: Kont, savedWindStack: List[WindEntry])
  case Proc(handler: Value, env: Env, savedWindStack: List[WindEntry])
