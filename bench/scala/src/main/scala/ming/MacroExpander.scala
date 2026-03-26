package ming

import SchemeTypes.{errAt, Pos, Value}

object MacroExpander:

  private val specialForms: Set[String] = Set(
    "quote",
    "define",
    "if",
    "lambda",
    "and",
    "or",
    "let",
    "begin",
    "cond",
    "set!",
    "define-syntax",
    "syntax-rules",
    "else"
  )

  private val gensymCounter = java.util.concurrent.atomic.AtomicLong(0L)

  private def gensym(base: String): String =
    val id = gensymCounter.incrementAndGet()
    s"$base##$id"

  type PublicBindings   = Map[String, Either[Expr, List[Expr]]]
  private type Bindings = PublicBindings

  def expand(
    mac: Value.VMacro,
    input: Expr,
    pos: Pos
  ): (Expr, Map[String, Value]) =
    val inputElems = input match
      case Expr.SList(elems, _) => elems
      case _                    => throw errAt(pos, "invalid macro use")
    val inputArgs = inputElems.tail

    val result = mac.rules.iterator
      .map { (pattern, template) =>
        val patElems = pattern match
          case Expr.SList(elems, _) => elems.tail
          case _                    => throw errAt(pos, "invalid macro pattern")
        matchPatternList(patElems, inputArgs, mac.literals).map { bindings =>
          val patVarNames = bindings.keySet
          val allSyms     = findAllSymbols(template)
          val introduced  = allSyms -- patVarNames -- specialForms - "..."
          val gsMap       = introduced.map(n => n -> gensym(n)).toMap
          val expanded    = expandTemplate(template, bindings, gsMap)
          val injections = gsMap.flatMap { (orig, gs) =>
            mac.defEnv.lookupOpt(orig).map(v => gs -> v)
          }
          (expanded, injections)
        }
      }
      .collectFirst { case Some(r) => r }

    result.getOrElse(throw errAt(pos, "no matching macro rule"))

  private def matchPatternList(
    pats: List[Expr],
    inputs: List[Expr],
    literals: List[String]
  ): Option[Bindings] = pats match
    case Nil => if inputs.isEmpty then Some(Map.empty) else None
    case pat :: Expr.Symbol("...", _) :: restPats =>
      val minRemaining = countNonEllipsis(restPats)
      if inputs.length < minRemaining then None
      else
        val maxTake = inputs.length - minRemaining
        pat match
          case Expr.Symbol(name, _) if !literals.contains(name) =>
            matchPatternList(restPats, inputs.drop(maxTake), literals).map { restB =>
              restB + (name -> Right(inputs.take(maxTake)))
            }
          case _ => None
    case pat :: restPats =>
      if inputs.isEmpty then None
      else
        for
          headB <- matchPatternSingle(pat, inputs.head, literals)
          tailB <- matchPatternList(restPats, inputs.tail, literals)
        yield headB ++ tailB

  private def matchPatternSingle(
    pat: Expr,
    input: Expr,
    literals: List[String]
  ): Option[Bindings] = pat match
    case Expr.Symbol(name, _) if literals.contains(name) =>
      input match
        case Expr.Symbol(n, _) if n == name => Some(Map.empty)
        case _                              => None
    case Expr.Symbol(name, _) =>
      Some(Map(name -> Left(input)))
    case Expr.SList(patElems, _) =>
      input match
        case Expr.SList(inputElems, _) =>
          matchPatternList(patElems, inputElems, literals)
        case _ => None
    case Expr.Bool(b1, _) =>
      input match
        case Expr.Bool(b2, _) if b1 == b2 => Some(Map.empty)
        case _                            => None
    case Expr.Num(n1, _) =>
      input match
        case Expr.Num(n2, _) if n1 == n2 => Some(Map.empty)
        case _                           => None
    case _ => None

  private def countNonEllipsis(pats: List[Expr]): Int = pats match
    case Nil                                => 0
    case _ :: Expr.Symbol("...", _) :: rest => countNonEllipsis(rest)
    case _ :: rest                          => 1 + countNonEllipsis(rest)

  private def findAllSymbols(expr: Expr): Set[String] = expr match
    case Expr.Symbol(name, _) => Set(name)
    case Expr.SList(elems, _) => elems.flatMap(findAllSymbols).toSet
    case _                    => Set.empty

  private def expandTemplate(
    template: Expr,
    bindings: Bindings,
    gsMap: Map[String, String]
  ): Expr = template match
    case Expr.Symbol(name, p) =>
      bindings.get(name) match
        case Some(Left(expr)) => expr
        case _ =>
          gsMap.get(name) match
            case Some(gs) => Expr.Symbol(gs, p)
            case None     => template
    case Expr.SList(elems, p) =>
      Expr.SList(expandListTemplate(elems, bindings, gsMap), p)
    case other => other

  private def expandListTemplate(
    elems: List[Expr],
    bindings: Bindings,
    gsMap: Map[String, String]
  ): List[Expr] = elems match
    case Nil => Nil
    case elem :: Expr.Symbol("...", _) :: rest =>
      val ellipsisVars = findAllSymbols(elem).filter(v => bindings.get(v).exists(_.isRight))
      if ellipsisVars.isEmpty then expandListTemplate(rest, bindings, gsMap)
      else
        val count = ellipsisVars
          .map(v =>
            bindings(v) match
              case Right(lst) => lst.length
              case _          => 0
          )
          .min
        val expanded = (0 until count).toList.map { i =>
          val singleBindings = bindings ++ ellipsisVars.map { v =>
            val Right(lst) = bindings(v): @unchecked
            v -> Left(lst(i))
          }
          expandTemplate(elem, singleBindings, gsMap)
        }
        expanded ++ expandListTemplate(rest, bindings, gsMap)
    case elem :: rest =>
      expandTemplate(elem, bindings, gsMap) :: expandListTemplate(
        rest,
        bindings,
        gsMap
      )

  /** Match a syntax-case pattern against input elements. Public for SyntaxCaseSupport. */
  def matchSyntaxCase(
    patElems: List[Expr],
    inputElems: List[Expr],
    literals: List[String]
  ): Option[PublicBindings] =
    matchPatternList(patElems, inputElems, literals)
