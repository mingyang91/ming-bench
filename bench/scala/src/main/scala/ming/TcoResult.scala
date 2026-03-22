package ming

import Interpreter.{Env, Output}

private[ming] enum TcoResult:
  case Value(value: SchemeValue, output: Output)

  case TailCall(
    func: SchemeValue,
    args: List[SchemeValue],
    callingEnv: Env,
    output: Output
  )
