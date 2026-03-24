package ming

import Evaluator.Val
import Evaluator.Val.*

/** Syntax-rules macro expansion engine. */
private[ming] object Macros:

  private var gensymCounter = 0L

  private def gensym(prefix: String): String =
    gensymCounter += 1
    s"${prefix}__${gensymCounter}"

  private val specialFormNames = Set(
    "quote",
    "define",
    "if",
    "lambda",
    "begin",
    "cond",
    "let",
    "set!",
    "and",
    "or",
    "define-syntax",
    "syntax-rules",
    "syntax-case",
    "syntax",
    "with-syntax",
    "let*",
    "letrec",
    "letrec*",
    "case",
    "case-lambda",
    "do",
    "define-record-type",
    "dynamic-wind",
    "guard",
    "with-exception-handler",
    "call-with-values"
  )

  private def error(msg: String): Nothing =
    throw new EvalError(msg)

  def evalDefineSyntax(name: String, syntaxRules: Val, env: Env): Val =
    syntaxRules match
      case Pair(Symbol("syntax-rules"), Pair(literalsList, rulesList)) =>
        val literals = Evaluator.toList(literalsList).map {
          case Symbol(s) => s
          case _         => error("syntax-rules: literals must be identifiers")
        }
        val rules = Evaluator.toList(rulesList).map {
          case Pair(pattern, Pair(template, Nil)) => (pattern, template)
          case _                                  => error("syntax-rules: bad rule")
        }
        val defEnv                  = env
        val defNames                = env.boundNames
        val transformer: Val => Val = (form: Val) => expandSyntaxRules(form, literals, rules, defEnv, defNames)
        env.define(name, MacroTransformer(transformer))
        Void
      case Pair(Symbol("lambda"), _) =>
        val closure     = Interpreter.eval(syntaxRules, env)
        val defEnv      = env
        val defNames    = env.boundNames
        val transformer = SyntaxCase.makeTransformer(closure, defEnv, defNames)
        env.define(name, MacroTransformer(transformer))
        Void
      case _ => error("bad define-syntax")

  private def expandSyntaxRules(
    form: Val,
    literals: List[String],
    rules: List[(Val, Val)],
    defEnv: Env,
    defNames: Set[String] = Set.empty
  ): Val =
    def tryRules(remaining: List[(Val, Val)]): Val = remaining match
      case scala.Nil => error(s"no matching syntax-rules pattern for ${Display.write(form)}")
      case (pattern, template) :: rest =>
        matchSyntaxPattern(pattern, form, literals) match
          case Some(bindings) =>
            val patVars = collectPatternVars(pattern, literals)
            expandTemplate(template, bindings, patVars, defEnv, Some(defNames))
          case None => tryRules(rest)
    tryRules(rules)

  // --- Pattern matching ---

  private[ming] def toElements(v: Val): List[Val] = v match
    case Nil            => List.empty
    case Pair(car, cdr) => car :: toElements(cdr)
    case _              => List(v)

  private def matchSyntaxPattern(
    pattern: Val,
    input: Val,
    literals: List[String]
  ): Option[Map[String, Either[Val, List[Val]]]] =
    (pattern, input) match
      case (Pair(_, patRest), Pair(_, inpRest)) =>
        matchPatternList(toElements(patRest), Evaluator.toList(inpRest), literals)
      case _ => None

  private[ming] def matchPatternList(
    patElems: List[Val],
    inputs: List[Val],
    literals: List[String]
  ): Option[Map[String, Either[Val, List[Val]]]] =
    patElems match
      case scala.Nil =>
        if inputs.isEmpty then Some(Map.empty) else None
      case pat :: Symbol("...") :: restPat =>
        val fixedCount = countFixed(restPat)
        if inputs.length < fixedCount then None
        else
          val ellipsisCount  = inputs.length - fixedCount
          val ellipsisInputs = inputs.take(ellipsisCount)
          val restInputs     = inputs.drop(ellipsisCount)
          val matched        = ellipsisInputs.map(inp => matchPatternSingle(inp, literals, pat))
          if matched.exists(_.isEmpty) then None
          else
            val merged = mergeEllipsis(pat, matched.map(_.get), literals)
            matchPatternList(restPat, restInputs, literals).map(rest => merged ++ rest)
      case pat :: restPat =>
        if inputs.isEmpty then None
        else
          for
            b1 <- matchPatternSingle(inputs.head, literals, pat)
            b2 <- matchPatternList(restPat, inputs.tail, literals)
          yield b1 ++ b2

  private def countFixed(elems: List[Val]): Int = elems match
    case scala.Nil                  => 0
    case _ :: Symbol("...") :: rest => countFixed(rest)
    case _ :: rest                  => 1 + countFixed(rest)

  private[ming] def matchPatternSingle(
    input: Val,
    literals: List[String],
    pattern: Val
  ): Option[Map[String, Either[Val, List[Val]]]] =
    pattern match
      case Symbol("_") => Some(Map.empty)
      case Symbol(name) if literals.contains(name) =>
        input match
          case Symbol(n) if n == name => Some(Map.empty)
          case _                      => None
      case Symbol(name) => Some(Map(name -> Left(input)))
      case Pair(_, _) =>
        input match
          case Pair(_, _) =>
            matchPatternList(toElements(pattern), Evaluator.toList(input), literals)
          case _ => None
      case Nil =>
        if input == Nil then Some(Map.empty) else None
      case Bool(b) =>
        input match
          case Bool(b2) if b == b2 => Some(Map.empty)
          case _                   => None
      case Num(n) =>
        input match
          case Num(n2) if n == n2 => Some(Map.empty)
          case _                  => None
      case _ => None

  private def mergeEllipsis(
    pat: Val,
    bindings: List[Map[String, Either[Val, List[Val]]]],
    literals: List[String]
  ): Map[String, Either[Val, List[Val]]] =
    val vars = collectPatternVars(pat, literals)
    vars.map { v =>
      val values = bindings.map(b =>
        b(v) match
          case Left(x)  => x
          case Right(_) => error("nested ellipsis not supported")
      )
      v -> Right(values)
    }.toMap

  private[ming] def collectPatternVars(pattern: Val, literals: List[String]): Set[String] =
    pattern match
      case Symbol(name) if name != "_" && name != "..." && !literals.contains(name) => Set(name)
      case Pair(car, cdr) => collectPatternVars(car, literals) ++ collectPatternVars(cdr, literals)
      case _              => Set.empty

  // --- Template expansion ---

  private def expandTemplate(
    template: Val,
    bindings: Map[String, Either[Val, List[Val]]],
    patVars: Set[String],
    defEnv: Env,
    defBoundNames: Option[Set[String]] = None
  ): Val =
    val allSyms    = collectAllSymbols(template)
    val freeSyms   = allSyms -- patVars -- specialFormNames - "..."
    val knownNames = defEnv.boundNames
    val renameMap = freeSyms.flatMap { sym =>
      if knownNames.contains(sym) then None
      else Some(sym -> gensym(sym))
    }.toMap
    expandInner(template, bindings, defEnv, renameMap, patVars)

  private def collectAllSymbols(v: Val): Set[String] = v match
    case Symbol(name)             => Set(name)
    case Pair(Symbol("quote"), _) => Set.empty
    case Pair(car, cdr)           => collectAllSymbols(car) ++ collectAllSymbols(cdr)
    case _                        => Set.empty

  private def expandInner(
    template: Val,
    bindings: Map[String, Either[Val, List[Val]]],
    defEnv: Env,
    renameMap: Map[String, String],
    patVars: Set[String]
  ): Val =
    template match
      case Symbol(name) if bindings.contains(name) =>
        bindings(name) match
          case Left(v)  => v
          case Right(_) => error(s"ellipsis variable $name used without ...")
      case Symbol(name) if renameMap.contains(name) =>
        Symbol(renameMap(name))
      case Symbol(name) if !specialFormNames.contains(name) && !patVars.contains(name) && name != "..." =>
        defEnv.lookup(name) match
          case Some(MacroTransformer(_)) => Symbol(name)
          case Some(v)                   => v
          case None                      => Symbol(name)
      case Symbol(_)                => template
      case Pair(Symbol("quote"), _) => template
      case Pair(_, _) =>
        val elems    = toElements(template)
        val expanded = expandElements(elems, bindings, defEnv, renameMap, patVars)
        expanded.foldRight(Nil: Val)((a, acc) => Pair(a, acc))
      case _ => template

  private def expandElements(
    elems: List[Val],
    bindings: Map[String, Either[Val, List[Val]]],
    defEnv: Env,
    renameMap: Map[String, String],
    patVars: Set[String]
  ): List[Val] =
    elems match
      case scala.Nil => scala.Nil
      case tmpl :: Symbol("...") :: rest =>
        val ellipsisVars = findEllipsisVars(tmpl, bindings)
        if ellipsisVars.isEmpty then expandElements(rest, bindings, defEnv, renameMap, patVars)
        else
          val count = bindings(ellipsisVars.head) match
            case Right(vs) => vs.length
            case _         => 0
          val expanded = (0 until count).toList.map { i =>
            val singleBindings = bindings.map { (k, v) =>
              if ellipsisVars.contains(k) then
                v match
                  case Right(vs) => (k, Left(vs(i)))
                  case other     => (k, other)
              else (k, v)
            }
            expandInner(tmpl, singleBindings, defEnv, renameMap, patVars)
          }
          expanded ++ expandElements(rest, bindings, defEnv, renameMap, patVars)
      case tmpl :: rest =>
        expandInner(tmpl, bindings, defEnv, renameMap, patVars) ::
          expandElements(rest, bindings, defEnv, renameMap, patVars)

  private[ming] def findEllipsisVars(
    tmpl: Val,
    bindings: Map[String, Either[Val, List[Val]]]
  ): Set[String] =
    tmpl match
      case Symbol(name) =>
        bindings.get(name) match
          case Some(Right(_)) => Set(name)
          case _              => Set.empty
      case Pair(car, cdr) => findEllipsisVars(car, bindings) ++ findEllipsisVars(cdr, bindings)
      case _              => Set.empty
