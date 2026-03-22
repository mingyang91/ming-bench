package ming

import SchemeValue.*

/** List, pair, vector, and equality builtins. */
private[ming] object CollectionBuiltins:

  def evalCons(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("cons: expected 2 arguments")
    SchemePair(args.head, args(1))

  def evalCar(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("car: expected 1 argument")
    args.head match
      case SchemeList(h :: _) => h
      case SchemePair(h, _)   => h
      case other              => throw new EvalError(s"car: not a pair: ${other.display}")

  def evalCdr(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("cdr: expected 1 argument")
    args.head match
      case SchemeList(_ :: t) => SchemeList(t)
      case SchemePair(_, t)   => t
      case other              => throw new EvalError(s"cdr: not a pair: ${other.display}")

  def evalNullQ(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("null?: expected 1 argument")
    SchemeBool(args.head == SchemeList(Nil))

  def evalAppend(args: List[SchemeValue]): SchemeValue =
    val combined = args.foldLeft(List.empty[SchemeValue]) { (acc, arg) =>
      acc ++ Builtins.asList(arg)
    }
    Builtins.toPairChain(combined)

  def evalLength(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("length: expected 1 argument")
    SchemeInt(Builtins.asList(args.head).length.toLong)

  def evalPairQ(args: List[SchemeValue]): SchemeValue =
    SchemeBool(args.length == 1 && (args.head match
      case SchemeList(_ :: _) => true
      case _: SchemePair      => true
      case _                  => false))

  def evalNot(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("not: expected 1 argument")
    SchemeBool(Evaluator.isFalsy(args.head))

  def evalListRef(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("list-ref: expected 2 arguments")
    args(1) match
      case SchemeInt(idx) =>
        val es = Builtins.asList(args.head)
        if idx < 0 || idx >= es.length then throw new EvalError("list-ref: index out of bounds")
        es(idx.toInt)
      case _ => throw new EvalError("list-ref: invalid arguments")

  def evalListTail(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("list-tail: expected 2 arguments")
    args(1) match
      case SchemeInt(idx) =>
        @scala.annotation.tailrec
        def drop(v: SchemeValue, n: Long): SchemeValue =
          if n == 0 then v
          else
            v match
              case SchemeList(_ :: t) => drop(SchemeList(t), n - 1)
              case p: SchemePair      => drop(p.cdr, n - 1)
              case _                  => throw new EvalError("list-tail: index out of bounds")
        if idx < 0 then throw new EvalError("list-tail: index out of bounds")
        drop(args.head, idx)
      case _ => throw new EvalError("list-tail: invalid arguments")

  def evalListPred(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("list?: expected 1 argument")
    SchemeBool(isList(args.head))

  def evalAssoc(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("assoc: expected 2 arguments")
    val key   = args.head
    val alist = Builtins.asList(args(1))
    alist
      .collectFirst { case pair if schemeEqual(listHead(pair), key) => pair }
      .getOrElse(SchemeBool(false))

  def evalEqQ(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("eq?: expected 2 arguments")
    SchemeBool(schemeEq(args.head, args(1)))

  def evalEqualQ(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("equal?: expected 2 arguments")
    SchemeBool(schemeEqual(args.head, args(1)))

  def evalCddr(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("cddr: expected 1 argument")
    evalCdr(List(evalCdr(args).asInstanceOf[SchemeValue]))

  def evalMemq(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("memq: expected 2 arguments")
    val key = args.head
    @scala.annotation.tailrec
    def loop(v: SchemeValue): SchemeValue = v match
      case SchemeList(Nil) => SchemeBool(false)
      case SchemeList(h :: t) =>
        if schemeEq(h, key) then v else loop(SchemeList(t))
      case p: SchemePair =>
        if schemeEq(p.car, key) then v else loop(p.cdr)
      case _ => SchemeBool(false)
    loop(args(1))

  def evalAssq(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("assq: expected 2 arguments")
    val key   = args.head
    val alist = Builtins.asList(args(1))
    alist
      .collectFirst { case pair if schemeEq(listHead(pair), key) => pair }
      .getOrElse(SchemeBool(false))

  def evalSetCar(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("set-car!: expected 2 arguments")
    args.head match
      case p: SchemePair => p.setCar(args(1)); SchemeVoid
      case other         => throw new EvalError(s"set-car!: not a mutable pair: ${other.display}")

  def evalSetCdr(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("set-cdr!: expected 2 arguments")
    args.head match
      case p: SchemePair => p.setCdr(args(1)); SchemeVoid
      case other         => throw new EvalError(s"set-cdr!: not a mutable pair: ${other.display}")

  def evalReverse(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("reverse: expected 1 argument")
    Builtins.toPairChain(Builtins.asList(args.head).reverse)

  def evalError(args: List[SchemeValue]): SchemeValue =
    val msg = args.map(_.display).mkString(" ")
    throw new EvalError(s"error: $msg")

  def evalMakeVector(args: List[SchemeValue]): SchemeValue =
    if args.isEmpty || args.length > 2 then throw new EvalError("make-vector: expected 1-2 arguments")
    val size = Builtins.asInt(args.head).toInt
    val fill = if args.length == 2 then args(1) else SchemeInt(0)
    new SchemeVector(Array.fill(size)(fill))

  def evalVectorRef(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("vector-ref: expected 2 arguments")
    args.head match
      case v: SchemeVector =>
        val idx = Builtins.asInt(args(1)).toInt
        if idx < 0 || idx >= v.elements.length then throw new EvalError("vector-ref: index out of bounds")
        v.elements(idx)
      case other => throw new EvalError(s"vector-ref: not a vector: ${other.display}")

  def evalVectorSet(args: List[SchemeValue]): SchemeValue =
    if args.length != 3 then throw new EvalError("vector-set!: expected 3 arguments")
    args.head match
      case v: SchemeVector =>
        val idx = Builtins.asInt(args(1)).toInt
        if idx < 0 || idx >= v.elements.length then throw new EvalError("vector-set!: index out of bounds")
        v.elements(idx) = args(2)
        SchemeVoid
      case other => throw new EvalError(s"vector-set!: not a vector: ${other.display}")

  def evalVectorLength(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("vector-length: expected 1 argument")
    args.head match
      case v: SchemeVector => SchemeInt(v.elements.length.toLong)
      case other           => throw new EvalError(s"vector-length: not a vector: ${other.display}")

  def evalVectorToList(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("vector->list: expected 1 argument")
    args.head match
      case v: SchemeVector => Builtins.toPairChain(v.elements.toList)
      case other           => throw new EvalError(s"vector->list: not a vector: ${other.display}")

  def evalListToVector(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("list->vector: expected 1 argument")
    new SchemeVector(Builtins.asList(args.head).toArray)

  def evalAssv(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("assv: expected 2 arguments")
    val key   = args.head
    val alist = Builtins.asList(args(1))
    alist
      .collectFirst { case pair if schemeEqv(listHead(pair), key) => pair }
      .getOrElse(SchemeBool(false))

  def evalTruncate(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("truncate: expected 1 argument")
    args.head match
      case n: SchemeInt         => n
      case SchemeFloat(f)       => SchemeFloat(f.toLong.toDouble)
      case SchemeRational(n, d) => SchemeInt(n / d)
      case other                => throw new EvalError(s"truncate: not a number: ${other.display}")

  def evalProcedureQ(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("procedure?: expected 1 argument")
    SchemeBool(args.head match
      case _: SchemeLambda       => true
      case _: SchemeCaseLambda   => true
      case _: SchemeBuiltinProc  => true
      case _: SchemeContinuation => true
      case _                     => false)

  def evalMember(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("member: expected 2 arguments")
    val key = args.head
    @scala.annotation.tailrec
    def loop(v: SchemeValue): SchemeValue = v match
      case SchemeList(Nil) => SchemeBool(false)
      case SchemeList(h :: t) =>
        if schemeEqual(h, key) then v else loop(SchemeList(t))
      case p: SchemePair =>
        if schemeEqual(p.car, key) then v else loop(p.cdr)
      case _ => SchemeBool(false)
    loop(args(1))

  // --- Equality helpers ---

  def schemeEq(a: SchemeValue, b: SchemeValue): Boolean = (a, b) match
    case (SchemeInt(x), SchemeInt(y))                     => x == y
    case (SchemeRational(n1, d1), SchemeRational(n2, d2)) => n1 == n2 && d1 == d2
    case (SchemeFloat(x), SchemeFloat(y))                 => x == y
    case (SchemeBool(x), SchemeBool(y))                   => x == y
    case (SchemeSymbol(x), SchemeSymbol(y))               => x == y
    case (SchemeChar(x), SchemeChar(y))                   => x == y
    case (SchemeList(Nil), SchemeList(Nil))               => true
    case _                                                => a eq b

  def schemeEqual(a: SchemeValue, b: SchemeValue): Boolean = (a, b) match
    case (SchemeInt(x), SchemeInt(y))                     => x == y
    case (SchemeRational(n1, d1), SchemeRational(n2, d2)) => n1 == n2 && d1 == d2
    case (SchemeFloat(x), SchemeFloat(y))                 => x == y
    case (SchemeBool(x), SchemeBool(y))                   => x == y
    case (SchemeSymbol(x), SchemeSymbol(y))               => x == y
    case (SchemeChar(x), SchemeChar(y))                   => x == y
    case (SchemeString(x), SchemeString(y))               => x == y
    case (v1: SchemeVector, v2: SchemeVector) =>
      v1.elements.length == v2.elements.length &&
      v1.elements.zip(v2.elements).forall(schemeEqual(_, _))
    case _ =>
      (toListOpt(a), toListOpt(b)) match
        case (Some(as), Some(bs)) =>
          as.length == bs.length && as.zip(bs).forall(schemeEqual(_, _))
        case _ => a eq b

  def schemeEqv(a: SchemeValue, b: SchemeValue): Boolean = schemeEq(a, b)

  // --- Internal helpers ---

  private def isList(v: SchemeValue): Boolean =
    val visited = new java.util.IdentityHashMap[SchemePair, java.lang.Boolean]()
    @scala.annotation.tailrec
    def loop(v: SchemeValue): Boolean = v match
      case SchemeList(_) => true
      case p: SchemePair =>
        if visited.containsKey(p) then false
        else
          visited.put(p, java.lang.Boolean.TRUE)
          loop(p.cdr)
      case _ => false
    loop(v)

  private def toListOpt(v: SchemeValue): Option[List[SchemeValue]] =
    val visited = new java.util.IdentityHashMap[SchemePair, java.lang.Boolean]()
    @scala.annotation.tailrec
    def loop(v: SchemeValue, acc: List[SchemeValue]): Option[List[SchemeValue]] = v match
      case SchemeList(es) => Some(acc.reverse ++ es)
      case p: SchemePair =>
        if visited.containsKey(p) then None
        else
          visited.put(p, java.lang.Boolean.TRUE)
          loop(p.cdr, p.car :: acc)
      case _ => None
    loop(v, Nil)

  private def listHead(v: SchemeValue): SchemeValue = v match
    case SchemeList(h :: _) => h
    case p: SchemePair      => p.car
    case _                  => throw new EvalError(s"not a pair: ${v.display}")
