package ming

/** Scheme interpreter entry point. */
object Evaluator:

  def evalStr(input: String): String =
    val exprs = Parser.parse(input)
    if exprs.isEmpty then throw new EvalError("no expressions")
    val (lastVal, _) = evalAll(exprs, defaultEnv)
    lastVal.display

  def evalStrWithOutput(input: String): (String, String) =
    val result = evalStr(input)
    (result, "")

  private def evalAll(
    exprs: List[Value],
    env: Env
  ): (Value, Env) =
    exprs match
      case Nil         => (Value.VoidVal, env)
      case head :: Nil => eval(head, env)
      case head :: tail =>
        val (_, newEnv) = eval(head, env)
        evalAll(tail, newEnv)

  private def eval(expr: Value, env: Env): (Value, Env) =
    expr match
      case Value.IntVal(_)    => (expr, env)
      case Value.BoolVal(_)   => (expr, env)
      case Value.StringVal(_) => (expr, env)
      case Value.NilVal       => (expr, env)
      case Value.VoidVal      => (expr, env)
      case _: Value.LambdaVal => (expr, env)
      case Value.Symbol(name) => (env.lookup(name), env)
      case Value.PairVal(car, _) =>
        val args = toList(expr).tail
        evalForm(car, args, env)

  private def evalForm(
    op: Value,
    args: List[Value],
    env: Env
  ): (Value, Env) =
    op match
      case Value.Symbol("define") => evalDefine(args, env)
      case Value.Symbol("if")     => evalIf(args, env)
      case Value.Symbol("quote")  => evalQuote(args, env)
      case Value.Symbol("lambda") => evalLambda(args, env)
      case Value.Symbol("let")    => evalLet(args, env)
      case Value.Symbol("begin")  => evalBegin(args, env)
      case Value.Symbol("cond")   => evalCond(args, env)
      case Value.Symbol("and")    => (evalAnd(args, env), env)
      case Value.Symbol("or")     => (evalOr(args, env), env)
      case Value.Symbol("not")    => (evalNot(args, env), env)
      case _ =>
        val (proc, _)  = eval(op, env)
        val evaledArgs = args.map(a => eval(a, env)._1)
        (applyProc(proc, evaledArgs), env)

  private def evalDefine(
    args: List[Value],
    env: Env
  ): (Value, Env) =
    args match
      case Value.PairVal(Value.Symbol(name), paramsList) :: body =>
        val params = toList(paramsList).map {
          case Value.Symbol(s) => s
          case other           => throw new EvalError(s"expected symbol in parameter list, got: ${other.display}")
        }
        val lambda = Value.LambdaVal(params, body, env, Some(name))
        (Value.VoidVal, env.define(name, lambda))
      case Value.Symbol(name) :: valueExpr :: Nil =>
        val (v, _) = eval(valueExpr, env)
        (Value.VoidVal, env.define(name, v))
      case _ =>
        throw new EvalError("bad define syntax")

  private def evalIf(args: List[Value], env: Env): (Value, Env) =
    args match
      case cond :: thenBranch :: elseBranch =>
        val (condVal, _) = eval(cond, env)
        if !isFalsy(condVal) then eval(thenBranch, env)
        else
          elseBranch match
            case eb :: Nil => eval(eb, env)
            case Nil       => (Value.VoidVal, env)
            case _         => throw new EvalError("bad if syntax")
      case _ => throw new EvalError("bad if syntax")

  private def evalQuote(
    args: List[Value],
    env: Env
  ): (Value, Env) =
    args match
      case datum :: Nil => (datum, env)
      case _            => throw new EvalError("quote requires exactly 1 argument")

  private def evalLambda(
    args: List[Value],
    env: Env
  ): (Value, Env) =
    args match
      case paramExpr :: body if body.nonEmpty =>
        val params = toList(paramExpr).map {
          case Value.Symbol(s) => s
          case other           => throw new EvalError(s"expected symbol in parameter list, got: ${other.display}")
        }
        (Value.LambdaVal(params, body, env, None), env)
      case _ => throw new EvalError("bad lambda syntax")

  private def applyProc(
    proc: Value,
    args: List[Value]
  ): Value =
    proc match
      case lam @ Value.LambdaVal(params, body, closure, nameOpt) =>
        val closureWithSelf = nameOpt match
          case Some(n) => closure.define(n, lam)
          case None    => closure
        val localEnv    = closureWithSelf.extend(params, args)
        val (result, _) = evalAll(body, localEnv)
        result
      case Value.Symbol(name) => applyBuiltin(name, args)
      case _ =>
        throw new EvalError(s"not a procedure: ${proc.display}")

  private def applyBuiltin(name: String, args: List[Value]): Value =
    name match
      case "+"  => evalAdd(args)
      case "-"  => evalSub(args)
      case "*"  => evalMul(args)
      case "/"  => evalDiv(args)
      case "<"  => evalCmp(args, _ < _)
      case ">"  => evalCmp(args, _ > _)
      case "="  => evalCmp(args, _ == _)
      case "<=" => evalCmp(args, _ <= _)
      case ">=" => evalCmp(args, _ >= _)
      case "cons" =>
        args match
          case a :: b :: Nil => Value.PairVal(a, b)
          case _             => throw new EvalError("cons requires exactly 2 arguments")
      case "car" =>
        args match
          case Value.PairVal(h, _) :: Nil => h
          case _                          => throw new EvalError("car requires a pair argument")
      case "cdr" =>
        args match
          case Value.PairVal(_, t) :: Nil => t
          case _                          => throw new EvalError("cdr requires a pair argument")
      case "null?" =>
        args match
          case Value.NilVal :: Nil => Value.BoolVal(true)
          case _ :: Nil            => Value.BoolVal(false)
          case _                   => throw new EvalError("null? requires exactly 1 argument")
      case "list" => args.foldRight(Value.NilVal: Value)(Value.PairVal(_, _))
      case "length" =>
        args match
          case head :: Nil => Value.IntVal(listLength(head))
          case _           => throw new EvalError("length requires exactly 1 argument")
      case "string?"  => typePred(args, _.isInstanceOf[Value.StringVal])
      case "number?"  => typePred(args, _.isInstanceOf[Value.IntVal])
      case "boolean?" => typePred(args, _.isInstanceOf[Value.BoolVal])
      case "pair?"    => typePred(args, _.isInstanceOf[Value.PairVal])
      case "symbol?"  => typePred(args, _.isInstanceOf[Value.Symbol])
      case _          => throw new EvalError(s"unknown procedure: $name")

  private def evalAnd(args: List[Value], env: Env): Value =
    args match
      case Nil         => Value.BoolVal(true)
      case head :: Nil => eval(head, env)._1
      case head :: tail =>
        val v = eval(head, env)._1
        if isFalsy(v) then v else evalAnd(tail, env)

  private def evalOr(args: List[Value], env: Env): Value =
    args match
      case Nil         => Value.BoolVal(false)
      case head :: Nil => eval(head, env)._1
      case head :: tail =>
        val v = eval(head, env)._1
        if !isFalsy(v) then v else evalOr(tail, env)

  private def evalNot(args: List[Value], env: Env): Value =
    args match
      case head :: Nil => Value.BoolVal(isFalsy(eval(head, env)._1))
      case _           => throw new EvalError("not requires exactly 1 argument")

  private def evalLet(args: List[Value], env: Env): (Value, Env) =
    args match
      case bindings :: body if body.nonEmpty =>
        val bindingList = toList(bindings)
        val localEnv = bindingList.foldLeft(env) { (acc, binding) =>
          val pair = toList(binding)
          pair match
            case Value.Symbol(name) :: valExpr :: Nil =>
              val (v, _) = eval(valExpr, env)
              acc.define(name, v)
            case _ => throw new EvalError("bad let binding")
        }
        val (result, _) = evalAll(body, localEnv)
        (result, env)
      case _ => throw new EvalError("bad let syntax")

  private def evalBegin(args: List[Value], env: Env): (Value, Env) =
    evalAll(args, env)

  private def evalCond(clauses: List[Value], env: Env): (Value, Env) =
    clauses match
      case Nil => (Value.VoidVal, env)
      case clause :: rest =>
        val parts = toList(clause)
        parts match
          case Value.Symbol("else") :: body =>
            evalAll(body, env)
          case test :: body =>
            val (testVal, _) = eval(test, env)
            if !isFalsy(testVal) then evalAll(body, env)
            else evalCond(rest, env)
          case _ => throw new EvalError("bad cond clause")

  private def typePred(args: List[Value], pred: Value => Boolean): Value =
    args match
      case v :: Nil => Value.BoolVal(pred(v))
      case _        => throw new EvalError("type predicate requires exactly 1 argument")

  private def listLength(v: Value): Long = v match
    case Value.NilVal        => 0L
    case Value.PairVal(_, t) => 1L + listLength(t)
    case _                   => throw new EvalError("length: not a proper list")

  private def isFalsy(v: Value): Boolean = v match
    case Value.BoolVal(false) => true
    case _                    => false

  private def asInt(v: Value): Long = v match
    case Value.IntVal(n) => n
    case other           => throw new EvalError(s"expected integer, got: ${other.display}")

  private def toList(v: Value): List[Value] = v match
    case Value.NilVal        => Nil
    case Value.PairVal(h, t) => h :: toList(t)
    case other               => List(other)

  private def evalAdd(args: List[Value]): Value =
    Value.IntVal(args.foldLeft(0L)((acc, v) => acc + asInt(v)))

  private def evalSub(args: List[Value]): Value = args match
    case Nil          => throw new EvalError("- requires at least 1 argument")
    case head :: Nil  => Value.IntVal(-asInt(head))
    case head :: tail => Value.IntVal(tail.foldLeft(asInt(head))((a, v) => a - asInt(v)))

  private def evalMul(args: List[Value]): Value =
    Value.IntVal(args.foldLeft(1L)((acc, v) => acc * asInt(v)))

  private def evalDiv(args: List[Value]): Value = args match
    case Nil          => throw new EvalError("/ requires at least 1 argument")
    case head :: Nil  => Value.IntVal(1 / asInt(head))
    case head :: tail => Value.IntVal(tail.foldLeft(asInt(head))((a, v) => a / asInt(v)))

  private def evalCmp(
    args: List[Value],
    op: (Long, Long) => Boolean
  ): Value =
    args match
      case a :: b :: Nil => Value.BoolVal(op(asInt(a), asInt(b)))
      case _             => throw new EvalError("comparison requires exactly 2 arguments")

  private val defaultEnv: Env = Env(Map.empty, None)
    .define("+", Value.Symbol("+"))
    .define("-", Value.Symbol("-"))
    .define("*", Value.Symbol("*"))
    .define("/", Value.Symbol("/"))
    .define("<", Value.Symbol("<"))
    .define(">", Value.Symbol(">"))
    .define("=", Value.Symbol("="))
    .define("<=", Value.Symbol("<="))
    .define(">=", Value.Symbol(">="))
    .define("cons", Value.Symbol("cons"))
    .define("car", Value.Symbol("car"))
    .define("cdr", Value.Symbol("cdr"))
    .define("null?", Value.Symbol("null?"))
    .define("list", Value.Symbol("list"))
    .define("length", Value.Symbol("length"))
    .define("string?", Value.Symbol("string?"))
    .define("number?", Value.Symbol("number?"))
    .define("boolean?", Value.Symbol("boolean?"))
    .define("pair?", Value.Symbol("pair?"))
    .define("symbol?", Value.Symbol("symbol?"))
