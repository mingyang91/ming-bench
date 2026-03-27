package ming

import ContinuationFrame.*

private[ming] object MachineContinuations:

  def continueWith(machine: Machine, frame: ContinuationFrame, value: Value): Unit =
    frame match
      case Sequence(remaining, env) =>
        MachineExpressions.startSequence(machine, remaining, env)
      case IfBranch(thenExpr, elseExpr, env) =>
        if ValueSemantics.isTruthy(value) then machine.setExpr(thenExpr, env)
        else
          elseExpr match
            case Some(expr) =>
              machine.setExpr(expr, env)
            case None =>
              machine.setValue(Value.Void)
      case DefineValue(name, env) =>
        env.define(name, value)
        machine.setValue(Value.Void)
      case SetValue(name, symbolPos, env) =>
        if env.assign(name, value) then machine.setValue(Value.Void)
        else throw EvalError.at(symbolPos, s"unbound variable: $name")
      case CallHead(args, env, pos) =>
        args match
          case Nil =>
            machine.setInvoke(value, Nil, pos)
          case _ =>
            machine.push(CallArg(value, Nil, args.dropRight(1).reverse, env, pos))
            machine.setExpr(args.last, env)
      case CallArg(procedure, evaluatedRev, remaining, env, pos) =>
        val nextEvaluated = value :: evaluatedRev
        remaining match
          case Nil =>
            machine.setInvoke(procedure, nextEvaluated, pos)
          case head :: tail =>
            machine.push(CallArg(procedure, nextEvaluated, tail, env, pos))
            machine.setExpr(head, env)
      case And(remaining, env) =>
        if !ValueSemantics.isTruthy(value) then machine.setValue(value)
        else
          remaining match
            case Nil =>
              machine.setValue(value)
            case head :: Nil =>
              machine.setExpr(head, env)
            case head :: tail =>
              machine.push(And(tail, env))
              machine.setExpr(head, env)
      case Or(remaining, env) =>
        if ValueSemantics.isTruthy(value) then machine.setValue(value)
        else
          remaining match
            case Nil =>
              machine.setValue(value)
            case head :: Nil =>
              machine.setExpr(head, env)
            case head :: tail =>
              machine.push(Or(tail, env))
              machine.setExpr(head, env)
      case CondClause(body, remainingClauses, env, pos) =>
        if ValueSemantics.isTruthy(value) then
          if body.isEmpty then machine.setValue(value)
          else MachineExpressions.startSequence(machine, body, env)
        else MachineExpressions.startCond(machine, remainingClauses, env, pos)
      case CaseKey(clauses, env, pos) =>
        MachineExpressions.startCaseClauses(machine, value, clauses, env, pos)
      case LetrecSequentialValue(currentCell, remaining, recursiveEnv, body) =>
        currentCell.value = value
        remaining match
          case Nil =>
            MachineExpressions.startSequence(machine, body, recursiveEnv)
          case (binding, nextCell) :: tail =>
            machine.push(LetrecSequentialValue(nextCell, tail, recursiveEnv, body))
            machine.setExpr(binding.valueExpr, recursiveEnv)
      case LetrecParallelValue(currentCell, remaining, evaluatedRev, recursiveEnv, body) =>
        val nextEvaluated = (currentCell, value) :: evaluatedRev
        remaining match
          case Nil =>
            nextEvaluated.reverse.foreach { case (cell, bindingValue) =>
              cell.value = bindingValue
            }
            MachineExpressions.startSequence(machine, body, recursiveEnv)
          case (binding, nextCell) :: tail =>
            machine.push(LetrecParallelValue(nextCell, tail, nextEvaluated, recursiveEnv, body))
            machine.setExpr(binding.valueExpr, recursiveEnv)
      case MapResult(procedure, remainingRows, accRev, pos) =>
        val nextAcc = value :: accRev
        remainingRows match
          case Nil =>
            machine.setValue(ValueSemantics.listFrom(nextAcc.reverse))
          case row :: tail =>
            machine.push(MapResult(procedure, tail, nextAcc, pos))
            machine.setInvoke(procedure, row, pos)
      case ForEachResult(procedure, remainingRows, pos) =>
        remainingRows match
          case Nil =>
            machine.setValue(Value.Void)
          case row :: tail =>
            machine.push(ForEachResult(procedure, tail, pos))
            machine.setInvoke(procedure, row, pos)
