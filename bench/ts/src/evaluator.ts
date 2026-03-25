import { EvalError, type SourcePosition } from './evalError.js';
import {
  absNumber,
  addNumbers,
  compareNumbers,
  denominatorPart,
  divideNumbers,
  exactInteger,
  exactToInexact,
  exptNumber,
  formatNumber,
  inexactNumber,
  inexactToExact,
  integerToJs,
  isExactNumber,
  isInexactNumber,
  isIntegerNumber,
  isNegativeNumber,
  isPositiveNumber,
  isRationalNumber,
  isZeroNumber,
  maxNumber,
  minNumber,
  multiplyNumbers,
  numeratorPart,
  parseNumberLiteral,
  subtractNumbers,
  type SchemeNumber,
} from './numbers.js';

type Token =
  | { kind: 'paren'; value: '(' | ')'; position: SourcePosition }
  | { kind: 'atom'; value: string; position: SourcePosition }
  | { kind: 'string'; value: string; position: SourcePosition }
  | { kind: 'quote'; position: SourcePosition };

type Expr =
  | { type: 'number'; value: SchemeNumber; position: SourcePosition; introduced?: boolean }
  | { type: 'boolean'; value: boolean; position: SourcePosition; introduced?: boolean }
  | { type: 'string'; value: string; position: SourcePosition; introduced?: boolean }
  | { type: 'char'; value: string; position: SourcePosition; introduced?: boolean }
  | {
      type: 'symbol';
      name: string;
      position: SourcePosition;
      introduced?: boolean;
      resolvedName?: string;
      capturedCell?: BindingCell;
      capturedSyntax?: SyntaxRulesMacro;
    }
  | { type: 'list'; elements: Expr[]; position: SourcePosition; introduced?: boolean };

type EvaluatedArg = {
  value: SchemeValue;
  position: SourcePosition;
};

type BuiltinProcedure = {
  type: 'builtin';
  name: string;
  invoke: (args: EvaluatedArg[], callPosition: SourcePosition) => SchemeValue;
};

type Closure = {
  type: 'closure';
  params: string[];
  restParam?: string;
  body: Expr[];
  env: Environment;
};

type BindingCell = {
  value: SchemeValue;
};

type SyntaxRule = {
  pattern: Expr;
  template: Expr;
};

type SyntaxRulesMacro = {
  type: 'syntax-rules';
  name: string;
  ellipsis: string;
  literals: ReadonlySet<string>;
  rules: SyntaxRule[];
  env: Environment;
};

type EvaluationContext = {
  output: string[];
};

type SchemeValue =
  | { type: 'number'; value: SchemeNumber }
  | { type: 'boolean'; value: boolean }
  | { type: 'string'; value: string }
  | { type: 'char'; value: string }
  | { type: 'symbol'; name: string }
  | { type: 'list'; elements: SchemeValue[]; tail?: SchemeValue }
  | BuiltinProcedure
  | Closure
  | { type: 'void' };

type ListValue = Extract<SchemeValue, { type: 'list' }>;
type SymbolExpr = Extract<Expr, { type: 'symbol' }>;
type MatchBinding = Expr | MatchBinding[];
type MatchBindings = Map<string, MatchBinding>;

const START_POSITION: SourcePosition = { line: 1, column: 1 };
const VOID_VALUE: SchemeValue = { type: 'void' };
const SPECIAL_FORM_NAMES = new Set([
  'define',
  'define-syntax',
  'set!',
  'if',
  'quote',
  'lambda',
  'and',
  'or',
  'begin',
  'let',
  'cond',
]);

let freshIdentifierCounter = 0;

class Environment {
  private readonly bindings = new Map<string, BindingCell>();
  private readonly syntaxBindings = new Map<string, SyntaxRulesMacro>();

  constructor(private readonly parent?: Environment) {}

  define(name: string, value: SchemeValue): void {
    this.bindings.set(name, { value });
  }

  set(name: string, value: SchemeValue, position: SourcePosition): void {
    this.lookupCell(name, position).value = value;
  }

  lookup(name: string, position: SourcePosition): SchemeValue {
    return this.lookupCell(name, position).value;
  }

  lookupCell(name: string, position: SourcePosition): BindingCell {
    const cell = this.tryLookupCell(name);
    if (cell !== undefined) {
      return cell;
    }

    throw new EvalError(`unbound variable: ${name}`, position);
  }

  tryLookupCell(name: string): BindingCell | undefined {
    const cell = this.bindings.get(name);
    if (cell !== undefined) {
      return cell;
    }

    return this.parent?.tryLookupCell(name);
  }

  defineSyntax(name: string, macro: SyntaxRulesMacro): void {
    this.syntaxBindings.set(name, macro);
  }

  tryLookupSyntax(name: string): SyntaxRulesMacro | undefined {
    const macro = this.syntaxBindings.get(name);
    if (macro !== undefined) {
      return macro;
    }

    return this.parent?.tryLookupSyntax(name);
  }
}

/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input: string): string {
  return formatValue(evaluateProgram(input).result);
}

/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input: string): { result: string; output: string } {
  const { result, output } = evaluateProgram(input);
  return { result: formatValue(result), output };
}

function evaluateProgram(input: string): { result: SchemeValue; output: string } {
  const expressions = parseProgram(input);

  if (expressions.length === 0) {
    throw new EvalError('empty input', START_POSITION);
  }

  const context: EvaluationContext = { output: [] };
  const env = createGlobalEnv(context);
  let result: SchemeValue = VOID_VALUE;

  for (const expr of expressions) {
    result = evaluate(expr, env);
  }

  return { result, output: context.output.join('') };
}

function parseProgram(input: string): Expr[] {
  const { tokens, eofPosition } = tokenize(input);
  let index = 0;
  const expressions: Expr[] = [];

  while (index < tokens.length) {
    expressions.push(parseExpr());
  }

  return expressions;

  function parseExpr(): Expr {
    const token = tokens[index];
    if (!token) {
      throw new EvalError('unexpected end of input', eofPosition);
    }

    index += 1;

    if (token.kind === 'paren') {
      if (token.value === ')') {
        throw new EvalError('unexpected )', token.position);
      }

      const elements: Expr[] = [];
      while (index < tokens.length) {
        const next = tokens[index];
        if (next.kind === 'paren' && next.value === ')') {
          index += 1;
          return { type: 'list', elements, position: token.position };
        }

        elements.push(parseExpr());
      }

      throw new EvalError('missing )', eofPosition);
    }

    if (token.kind === 'quote') {
      return {
        type: 'list',
        elements: [
          { type: 'symbol', name: 'quote', position: token.position },
          parseExpr(),
        ],
        position: token.position,
      };
    }

    if (token.kind === 'string') {
      return { type: 'string', value: token.value, position: token.position };
    }

    if (token.value === '#t') {
      return { type: 'boolean', value: true, position: token.position };
    }

    if (token.value === '#f') {
      return { type: 'boolean', value: false, position: token.position };
    }

    const character = parseCharLiteral(token.value, token.position);
    if (character !== null) {
      return { type: 'char', value: character, position: token.position };
    }

    const numericValue = parseNumberLiteral(token.value, token.position);
    if (numericValue !== null) {
      return { type: 'number', value: numericValue, position: token.position };
    }

    return { type: 'symbol', name: token.value, position: token.position };
  }
}

function tokenize(input: string): { tokens: Token[]; eofPosition: SourcePosition } {
  const tokens: Token[] = [];
  let index = 0;
  let line = 1;
  let column = 1;

  const currentPosition = (): SourcePosition => ({ line, column });

  const advanceChar = (ch: string): void => {
    index += 1;
    if (ch === '\n') {
      line += 1;
      column = 1;
      return;
    }

    column += 1;
  };

  while (index < input.length) {
    const ch = input[index];

    if (/\s/.test(ch)) {
      advanceChar(ch);
      continue;
    }

    if (ch === ';') {
      while (index < input.length && input[index] !== '\n') {
        advanceChar(input[index]);
      }
      continue;
    }

    const position = currentPosition();

    if (ch === '(' || ch === ')') {
      tokens.push({ kind: 'paren', value: ch, position });
      advanceChar(ch);
      continue;
    }

    if (ch === "'") {
      tokens.push({ kind: 'quote', position });
      advanceChar(ch);
      continue;
    }

    if (ch === '"') {
      advanceChar(ch);
      let value = '';
      let terminated = false;

      while (index < input.length) {
        const current = input[index];

        if (current === '"') {
          advanceChar(current);
          tokens.push({ kind: 'string', value, position });
          terminated = true;
          break;
        }

        if (current === '\\') {
          advanceChar(current);
          if (index >= input.length) {
            throw new EvalError('unterminated string literal', position);
          }

          const escaped = input[index];
          switch (escaped) {
            case 'n':
              value += '\n';
              break;
            case 'r':
              value += '\r';
              break;
            case 't':
              value += '\t';
              break;
            case '"':
              value += '"';
              break;
            case '\\':
              value += '\\';
              break;
            default:
              value += escaped;
              break;
          }

          advanceChar(escaped);
          continue;
        }

        value += current;
        advanceChar(current);
      }

      if (!terminated) {
        throw new EvalError('unterminated string literal', position);
      }

      continue;
    }

    let value = '';
    while (index < input.length) {
      const current = input[index];
      if (/\s/.test(current) || current === '(' || current === ')' || current === ';') {
        break;
      }

      value += current;
      advanceChar(current);
    }

    tokens.push({ kind: 'atom', value, position });
  }

  return { tokens, eofPosition: currentPosition() };
}

function evaluate(expr: Expr, env: Environment): SchemeValue {
  try {
    switch (expr.type) {
      case 'number':
      case 'boolean':
        return expr;
      case 'string':
        return stringValue(expr.value);
      case 'char':
        return charValue(expr.value, expr.position);
      case 'symbol':
        if (expr.capturedCell) {
          return expr.capturedCell.value;
        }

        if (expr.capturedSyntax) {
          throw new EvalError(`syntax identifier used as value: ${expr.name}`, expr.position);
        }

        return env.lookup(symbolKey(expr), expr.position);
      case 'list':
        return evaluateList(expr, env);
    }
  } catch (error) {
    throw attachPosition(error, expr.position);
  }
}

function evaluateList(expr: Extract<Expr, { type: 'list' }>, env: Environment): SchemeValue {
  if (expr.elements.length === 0) {
    throw new EvalError('cannot evaluate empty list', expr.position);
  }

  const operator = expr.elements[0];
  const args = expr.elements.slice(1);

  if (operator.type === 'symbol') {
    switch (operator.name) {
      case 'define':
        return evaluateDefine(args, env, operator.position);
      case 'define-syntax':
        return evaluateDefineSyntax(args, env, operator.position);
      case 'set!':
        return evaluateSet(args, env, operator.position);
      case 'if':
        return evaluateIf(args, env, operator.position);
      case 'quote':
        return evaluateQuote(args, operator.position);
      case 'lambda':
        return evaluateLambda(args, env, operator.position);
      case 'and':
        return evaluateAnd(args, env);
      case 'or':
        return evaluateOr(args, env);
      case 'begin':
        return evaluateBegin(args, env);
      case 'let':
        return evaluateLet(args, env, operator.position);
      case 'cond':
        return evaluateCond(args, env);
    }

    const macro = operator.capturedSyntax ?? env.tryLookupSyntax(symbolKey(operator));
    if (macro) {
      return evaluate(expandMacroCall(macro, expr), env);
    }
  }

  const procedure = evaluate(operator, env);
  const evaluatedArgs = args.map((arg) => ({ value: evaluate(arg, env), position: arg.position }));
  return applyProcedure(procedure, evaluatedArgs, operator.position);
}

function evaluateDefine(args: Expr[], env: Environment, position: SourcePosition): SchemeValue {
  if (args.length < 2) {
    throw new EvalError(`define: expected at least 2 argument(s), got ${args.length}`, position);
  }

  const target = args[0];

  if (target.type === 'symbol') {
    requireArgCount('define', args.length, 2, position);
    const value = evaluate(args[1], env);
    env.define(symbolKey(target), value);
    return VOID_VALUE;
  }

  if (target.type !== 'list' || target.elements.length === 0) {
    throw new EvalError('define: invalid binding target', target.position);
  }

  const nameExpr = target.elements[0];
  if (nameExpr.type !== 'symbol') {
    throw new EvalError('define: invalid function name', nameExpr.position);
  }

  const { params, restParam } = parseParameterList(target.elements.slice(1));
  const body = args.slice(1);
  const closure: Closure = { type: 'closure', params, restParam, body, env };
  env.define(symbolKey(nameExpr), closure);
  return VOID_VALUE;
}

function evaluateDefineSyntax(args: Expr[], env: Environment, position: SourcePosition): SchemeValue {
  requireArgCount('define-syntax', args.length, 2, position);

  const target = args[0];
  if (target.type !== 'symbol') {
    throw new EvalError('define-syntax: expected identifier', target.position);
  }

  env.defineSyntax(symbolKey(target), parseSyntaxRules(target.name, args[1], env));
  return VOID_VALUE;
}

function evaluateSet(args: Expr[], env: Environment, position: SourcePosition): SchemeValue {
  requireArgCount('set!', args.length, 2, position);

  const target = args[0];
  if (target.type !== 'symbol') {
    throw new EvalError('set!: invalid binding target', target.position);
  }

  const value = evaluate(args[1], env);
  if (target.capturedCell) {
    target.capturedCell.value = value;
    return VOID_VALUE;
  }

  env.set(symbolKey(target), value, target.position);
  return VOID_VALUE;
}

function evaluateIf(args: Expr[], env: Environment, position: SourcePosition): SchemeValue {
  requireArgCount('if', args.length, 3, position);
  return isTruthy(evaluate(args[0], env)) ? evaluate(args[1], env) : evaluate(args[2], env);
}

function evaluateQuote(args: Expr[], position: SourcePosition): SchemeValue {
  requireArgCount('quote', args.length, 1, position);
  return quoteExpr(args[0]);
}

function evaluateLambda(args: Expr[], env: Environment, position: SourcePosition): SchemeValue {
  requireArgCountAtLeast('lambda', args.length, 2, position);
  const paramsExpr = args[0];
  if (paramsExpr.type === 'symbol') {
    return {
      type: 'closure',
      params: [],
      restParam: symbolKey(paramsExpr),
      body: args.slice(1),
      env,
    };
  }

  if (paramsExpr.type !== 'list') {
    throw new EvalError('lambda: parameter list must be a list', paramsExpr.position);
  }

  const { params, restParam } = parseParameterList(paramsExpr.elements);

  return {
    type: 'closure',
    params,
    restParam,
    body: args.slice(1),
    env,
  };
}

function evaluateAnd(args: Expr[], env: Environment): SchemeValue {
  let result: SchemeValue = booleanValue(true);

  for (const arg of args) {
    result = evaluate(arg, env);
    if (!isTruthy(result)) {
      return result;
    }
  }

  return result;
}

function evaluateOr(args: Expr[], env: Environment): SchemeValue {
  let result: SchemeValue = booleanValue(false);

  for (const arg of args) {
    result = evaluate(arg, env);
    if (isTruthy(result)) {
      return result;
    }
  }

  return result;
}

function evaluateBegin(args: Expr[], env: Environment): SchemeValue {
  return evaluateSequence(args, env);
}

function evaluateLet(args: Expr[], env: Environment, position: SourcePosition): SchemeValue {
  requireArgCountAtLeast('let', args.length, 2, position);

  const firstArg = args[0];
  if (firstArg.type === 'symbol') {
    requireArgCountAtLeast('let', args.length, 3, position);

    const bindings = parseLetBindings(args[1]);
    const values = bindings.map((binding) => ({
      value: evaluate(binding.value, env),
      position: binding.value.position,
    }));
    const letEnv = new Environment(env);
    const closure: Closure = {
      type: 'closure',
      params: bindings.map((binding) => symbolKey(binding.name)),
      body: args.slice(2),
      env: letEnv,
    };

    letEnv.define(symbolKey(firstArg), closure);
    return applyProcedure(closure, values, firstArg.position);
  }

  const bindings = parseLetBindings(firstArg);
  const letEnv = new Environment(env);

  for (const binding of bindings) {
    letEnv.define(symbolKey(binding.name), evaluate(binding.value, env));
  }

  return evaluateSequence(args.slice(1), letEnv);
}

function evaluateCond(args: Expr[], env: Environment): SchemeValue {
  for (const clause of args) {
    if (clause.type !== 'list' || clause.elements.length === 0) {
      throw new EvalError('cond: expected non-empty clause', clause.position);
    }

    const [testExpr, ...body] = clause.elements;

    if (testExpr.type === 'symbol' && testExpr.name === 'else') {
      return body.length === 0 ? VOID_VALUE : evaluateSequence(body, env);
    }

    const testValue = evaluate(testExpr, env);
    if (isTruthy(testValue)) {
      return body.length === 0 ? testValue : evaluateSequence(body, env);
    }
  }

  return VOID_VALUE;
}

function quoteExpr(expr: Expr): SchemeValue {
  switch (expr.type) {
    case 'number':
      return expr;
    case 'boolean':
      return booleanValue(expr.value);
    case 'string':
      return stringValue(expr.value);
    case 'char':
      return charValue(expr.value, expr.position);
    case 'symbol':
      return { type: 'symbol', name: expr.name };
    case 'list':
      return { type: 'list', elements: expr.elements.map(quoteExpr) };
  }
}

function parseParameterList(params: Expr[]): { params: string[]; restParam?: string } {
  const names: string[] = [];

  for (let index = 0; index < params.length; index += 1) {
    const param = params[index];
    if (param.type !== 'symbol') {
      throw new EvalError('lambda: parameter names must be symbols', param.position);
    }

    if (param.name !== '.') {
      names.push(symbolKey(param));
      continue;
    }

    const restParam = params[index + 1];
    if (
      restParam === undefined ||
      restParam.type !== 'symbol' ||
      restParam.name === '.' ||
      index + 2 !== params.length
    ) {
      throw new EvalError('lambda: invalid rest parameter list', param.position);
    }

    return { params: names, restParam: symbolKey(restParam) };
  }

  return { params: names };
}

function parseLetBindings(bindingsExpr: Expr): Array<{ name: SymbolExpr; value: Expr }> {
  if (bindingsExpr.type !== 'list') {
    throw new EvalError('let: expected binding list', bindingsExpr.position);
  }

  return bindingsExpr.elements.map((bindingExpr) => {
    if (bindingExpr.type !== 'list' || bindingExpr.elements.length !== 2) {
      throw new EvalError('let: expected binding pair', bindingExpr.position);
    }

    const [nameExpr, valueExpr] = bindingExpr.elements;
    if (nameExpr.type !== 'symbol') {
      throw new EvalError('let: binding name must be a symbol', nameExpr.position);
    }

    return { name: nameExpr, value: valueExpr };
  });
}

function symbolKey(symbol: SymbolExpr): string {
  return symbol.resolvedName ?? symbol.name;
}

function parseSyntaxRules(
  keywordName: string,
  transformerExpr: Expr,
  env: Environment,
): SyntaxRulesMacro {
  if (transformerExpr.type !== 'list' || transformerExpr.elements.length < 2) {
    throw new EvalError('define-syntax: expected syntax-rules transformer', transformerExpr.position);
  }

  const [head, ...rest] = transformerExpr.elements;
  if (head.type !== 'symbol' || head.name !== 'syntax-rules') {
    throw new EvalError('define-syntax: expected syntax-rules transformer', transformerExpr.position);
  }

  let ellipsis = '...';
  let literalsExpr: Expr | undefined;
  let ruleExprs: Expr[] = [];

  if (rest[0]?.type === 'symbol') {
    ellipsis = rest[0].name;
    literalsExpr = rest[1];
    ruleExprs = rest.slice(2);
  } else {
    literalsExpr = rest[0];
    ruleExprs = rest.slice(1);
  }

  if (literalsExpr === undefined || literalsExpr.type !== 'list') {
    throw new EvalError('syntax-rules: expected literal identifier list', transformerExpr.position);
  }

  if (ruleExprs.length === 0) {
    throw new EvalError('syntax-rules: expected at least one rule', transformerExpr.position);
  }

  const literals = new Set<string>([keywordName]);
  for (const literal of literalsExpr.elements) {
    if (literal.type !== 'symbol') {
      throw new EvalError('syntax-rules: literal identifiers must be symbols', literal.position);
    }

    literals.add(literal.name);
  }

  const rules = ruleExprs.map((ruleExpr) => {
    if (ruleExpr.type !== 'list' || ruleExpr.elements.length !== 2) {
      throw new EvalError('syntax-rules: expected (pattern template) rule', ruleExpr.position);
    }

    const [pattern, template] = ruleExpr.elements;
    return { pattern, template };
  });

  return {
    type: 'syntax-rules',
    name: keywordName,
    ellipsis,
    literals,
    rules,
    env,
  };
}

function expandMacroCall(macro: SyntaxRulesMacro, expr: Extract<Expr, { type: 'list' }>): Expr {
  for (const rule of macro.rules) {
    const bindings = matchPattern(rule.pattern, expr, macro, new Map(), []);
    if (bindings === null) {
      continue;
    }

    const expanded = expandTemplate(rule.template, bindings, macro, []);
    return cloneExpr(hygienizeExpr(expanded, macro.env, new Map()));
  }

  throw new EvalError(`${macro.name}: no matching syntax-rules pattern`, expr.position);
}

function matchPattern(
  pattern: Expr,
  input: Expr,
  macro: SyntaxRulesMacro,
  bindings: MatchBindings,
  path: number[],
): MatchBindings | null {
  switch (pattern.type) {
    case 'number':
    case 'boolean':
    case 'string':
    case 'char':
      return exprSyntaxEqual(pattern, input) ? bindings : null;
    case 'symbol':
      if (macro.literals.has(pattern.name) || pattern.name === macro.ellipsis) {
        return input.type === 'symbol' && input.name === pattern.name ? bindings : null;
      }

      return bindPatternVariable(bindings, pattern.name, path, input);
    case 'list':
      if (input.type !== 'list') {
        return null;
      }

      return matchPatternSequence(pattern.elements, input.elements, macro, bindings, path, 0, 0);
  }
}

function matchPatternSequence(
  patterns: Expr[],
  inputs: Expr[],
  macro: SyntaxRulesMacro,
  bindings: MatchBindings,
  path: number[],
  patternIndex: number,
  inputIndex: number,
): MatchBindings | null {
  if (patternIndex === patterns.length) {
    return inputIndex === inputs.length ? bindings : null;
  }

  const pattern = patterns[patternIndex];
  if (patternIndex + 1 < patterns.length && isEllipsisExpr(patterns[patternIndex + 1], macro.ellipsis)) {
    const seededBindings = seedRepeatedPatternBindings(bindings, pattern, macro, path);
    const minimumRemainingLength = minimumPatternLength(patterns.slice(patternIndex + 2), macro);
    const maxRepeatCount = inputs.length - inputIndex - minimumRemainingLength;
    if (maxRepeatCount < 0) {
      return null;
    }

    for (let repeatCount = 0; repeatCount <= maxRepeatCount; repeatCount += 1) {
      let repeatedBindings = seededBindings;
      let matched = true;

      for (let repeatIndex = 0; repeatIndex < repeatCount; repeatIndex += 1) {
        const nextBindings = matchPattern(
          pattern,
          inputs[inputIndex + repeatIndex],
          macro,
          repeatedBindings,
          [...path, repeatIndex],
        );
        if (nextBindings === null) {
          matched = false;
          break;
        }

        repeatedBindings = nextBindings;
      }

      if (!matched) {
        continue;
      }

      const remainingBindings = matchPatternSequence(
        patterns,
        inputs,
        macro,
        repeatedBindings,
        path,
        patternIndex + 2,
        inputIndex + repeatCount,
      );
      if (remainingBindings !== null) {
        return remainingBindings;
      }
    }

    return null;
  }

  if (inputIndex >= inputs.length) {
    return null;
  }

  const nextBindings = matchPattern(pattern, inputs[inputIndex], macro, bindings, path);
  if (nextBindings === null) {
    return null;
  }

  return matchPatternSequence(
    patterns,
    inputs,
    macro,
    nextBindings,
    path,
    patternIndex + 1,
    inputIndex + 1,
  );
}

function minimumPatternLength(patterns: Expr[], macro: SyntaxRulesMacro): number {
  let length = 0;

  for (let index = 0; index < patterns.length; index += 1) {
    if (index + 1 < patterns.length && isEllipsisExpr(patterns[index + 1], macro.ellipsis)) {
      index += 1;
      continue;
    }

    length += 1;
  }

  return length;
}

function seedRepeatedPatternBindings(
  bindings: MatchBindings,
  pattern: Expr,
  macro: SyntaxRulesMacro,
  path: number[],
): MatchBindings {
  const variableNames = collectPatternVariables(pattern, macro, new Set<string>());
  if (variableNames.size === 0) {
    return bindings;
  }

  const nextBindings = new Map(bindings);
  for (const name of variableNames) {
    nextBindings.set(name, ensureArrayBinding(nextBindings.get(name), path));
  }

  return nextBindings;
}

function collectPatternVariables(
  pattern: Expr,
  macro: SyntaxRulesMacro,
  names: Set<string>,
): Set<string> {
  switch (pattern.type) {
    case 'symbol':
      if (!macro.literals.has(pattern.name) && pattern.name !== macro.ellipsis) {
        names.add(pattern.name);
      }
      return names;
    case 'list':
      for (const element of pattern.elements) {
        collectPatternVariables(element, macro, names);
      }
      return names;
    default:
      return names;
  }
}

function ensureArrayBinding(binding: MatchBinding | undefined, path: number[]): MatchBinding {
  if (path.length === 0) {
    return binding ?? [];
  }

  if (binding !== undefined && !Array.isArray(binding)) {
    return binding;
  }

  const [index, ...rest] = path;
  const values = binding === undefined ? [] : [...binding];
  values[index] = ensureArrayBinding(values[index], rest);
  return values;
}

function bindPatternVariable(
  bindings: MatchBindings,
  name: string,
  path: number[],
  input: Expr,
): MatchBindings | null {
  const nextBinding = setMatchBinding(bindings.get(name), path, input);
  if (nextBinding === null) {
    return null;
  }

  const nextBindings = new Map(bindings);
  nextBindings.set(name, nextBinding);
  return nextBindings;
}

function setMatchBinding(
  binding: MatchBinding | undefined,
  path: number[],
  input: Expr,
): MatchBinding | null {
  if (path.length === 0) {
    if (binding === undefined) {
      return input;
    }

    if (Array.isArray(binding)) {
      return null;
    }

    return exprSyntaxEqual(binding, input) ? binding : null;
  }

  if (binding !== undefined && !Array.isArray(binding)) {
    return null;
  }

  const [index, ...rest] = path;
  const values = binding === undefined ? [] : [...binding];
  const nextBinding = setMatchBinding(values[index], rest, input);
  if (nextBinding === null) {
    return null;
  }

  values[index] = nextBinding;
  return values;
}

function getMatchBinding(binding: MatchBinding | undefined, path: number[]): MatchBinding | undefined {
  let current = binding;

  for (const index of path) {
    if (!Array.isArray(current)) {
      return undefined;
    }

    current = current[index];
  }

  return current;
}

function expandTemplate(
  template: Expr,
  bindings: MatchBindings,
  macro: SyntaxRulesMacro,
  path: number[],
): Expr {
  switch (template.type) {
    case 'number':
    case 'boolean':
    case 'string':
    case 'char':
      return { ...cloneExpr(template), introduced: true };
    case 'symbol': {
      if (template.name === macro.ellipsis) {
        throw new EvalError('syntax-rules: invalid ellipsis in template', template.position);
      }

      const binding = bindings.get(template.name);
      if (binding === undefined) {
        return { ...cloneExpr(template), introduced: true };
      }

      const value = getMatchBinding(binding, path);
      if (value === undefined || Array.isArray(value)) {
        throw new EvalError('syntax-rules: invalid template ellipsis usage', template.position);
      }

      return cloneExpr(value);
    }
    case 'list': {
      const elements: Expr[] = [];

      for (let index = 0; index < template.elements.length; index += 1) {
        const element = template.elements[index];
        if (
          index + 1 < template.elements.length &&
          isEllipsisExpr(template.elements[index + 1], macro.ellipsis)
        ) {
          const repeatCount = findTemplateRepeatCount(element, bindings, path);
          if (repeatCount === null) {
            throw new EvalError('syntax-rules: template ellipsis has no repeated variable', element.position);
          }

          for (let repeatIndex = 0; repeatIndex < repeatCount; repeatIndex += 1) {
            elements.push(expandTemplate(element, bindings, macro, [...path, repeatIndex]));
          }

          index += 1;
          continue;
        }

        elements.push(expandTemplate(element, bindings, macro, path));
      }

      return { type: 'list', elements, position: template.position, introduced: true };
    }
  }
}

function findTemplateRepeatCount(
  template: Expr,
  bindings: MatchBindings,
  path: number[],
): number | null {
  switch (template.type) {
    case 'symbol': {
      const binding = bindings.get(template.name);
      if (binding === undefined) {
        return null;
      }

      const value = getMatchBinding(binding, path);
      return Array.isArray(value) ? value.length : null;
    }
    case 'list':
      for (const element of template.elements) {
        const repeatCount = findTemplateRepeatCount(element, bindings, path);
        if (repeatCount !== null) {
          return repeatCount;
        }
      }

      return null;
    default:
      return null;
  }
}

function hygienizeExpr(
  expr: Expr,
  definitionEnv: Environment,
  scope: Map<string, string>,
): Expr {
  if (expr.type === 'symbol') {
    return hygienizeSymbol(expr, definitionEnv, scope);
  }

  if (expr.type !== 'list' || !expr.introduced) {
    return expr;
  }

  const operator = expr.elements[0];
  if (operator?.type === 'symbol') {
    switch (operator.name) {
      case 'quote':
        return expr;
      case 'lambda':
        return hygienizeLambdaExpr(expr, definitionEnv, scope);
      case 'let':
        return hygienizeLetExpr(expr, definitionEnv, scope);
      case 'define':
        return hygienizeDefineExpr(expr, definitionEnv, scope);
    }
  }

  return {
    ...expr,
    elements: expr.elements.map((element) => hygienizeExpr(element, definitionEnv, scope)),
  };
}

function hygienizeSymbol(
  expr: SymbolExpr,
  definitionEnv: Environment,
  scope: Map<string, string>,
): Expr {
  if (!expr.introduced) {
    return expr;
  }

  const renamed = scope.get(expr.name);
  if (renamed !== undefined) {
    return { ...expr, resolvedName: renamed };
  }

  if (SPECIAL_FORM_NAMES.has(expr.name)) {
    return expr;
  }

  const syntax = definitionEnv.tryLookupSyntax(expr.name);
  if (syntax !== undefined) {
    return { ...expr, capturedSyntax: syntax };
  }

  const cell = definitionEnv.tryLookupCell(expr.name);
  if (cell !== undefined) {
    return { ...expr, capturedCell: cell };
  }

  return expr;
}

function hygienizeLambdaExpr(
  expr: Extract<Expr, { type: 'list' }>,
  definitionEnv: Environment,
  scope: Map<string, string>,
): Expr {
  if (expr.elements.length < 2) {
    return expr;
  }

  const transformedParams = hygienizeParameterSpec(expr.elements[1], scope);
  return {
    ...expr,
    elements: [
      expr.elements[0],
      transformedParams.paramsExpr,
      ...expr.elements
        .slice(2)
        .map((element) => hygienizeExpr(element, definitionEnv, transformedParams.scope)),
    ],
  };
}

function hygienizeLetExpr(
  expr: Extract<Expr, { type: 'list' }>,
  definitionEnv: Environment,
  scope: Map<string, string>,
): Expr {
  if (expr.elements.length < 3) {
    return expr;
  }

  const [operator, firstArg] = expr.elements;
  if (firstArg.type === 'symbol') {
    let bodyScope = new Map(scope);
    const renamedLet = freshenBinder(firstArg, bodyScope);
    bodyScope = renamedLet.scope;

    const transformedBindings = hygienizeLetBindings(expr.elements[2], definitionEnv, scope, bodyScope);
    return {
      ...expr,
      elements: [
        operator,
        renamedLet.symbol,
        transformedBindings.bindingsExpr,
        ...expr.elements
          .slice(3)
          .map((element) => hygienizeExpr(element, definitionEnv, transformedBindings.scope)),
      ],
    };
  }

  const transformedBindings = hygienizeLetBindings(firstArg, definitionEnv, scope, new Map(scope));
  return {
    ...expr,
    elements: [
      operator,
      transformedBindings.bindingsExpr,
      ...expr.elements
        .slice(2)
        .map((element) => hygienizeExpr(element, definitionEnv, transformedBindings.scope)),
    ],
  };
}

function hygienizeDefineExpr(
  expr: Extract<Expr, { type: 'list' }>,
  definitionEnv: Environment,
  scope: Map<string, string>,
): Expr {
  if (expr.elements.length < 3) {
    return expr;
  }

  const [operator, target, ...rest] = expr.elements;

  if (target.type === 'symbol') {
    return {
      ...expr,
      elements: [
        operator,
        freshenBinder(target, new Map(scope)).symbol,
        ...rest.map((element) => hygienizeExpr(element, definitionEnv, scope)),
      ],
    };
  }

  if (target.type !== 'list' || target.elements.length === 0 || target.elements[0].type !== 'symbol') {
    return {
      ...expr,
      elements: expr.elements.map((element) => hygienizeExpr(element, definitionEnv, scope)),
    };
  }

  let bodyScope = new Map(scope);
  const renamedTarget = freshenBinder(target.elements[0], bodyScope);
  bodyScope = renamedTarget.scope;

  const transformedParams = hygienizeParameterSpec(
    { type: 'list', elements: target.elements.slice(1), position: target.position, introduced: target.introduced },
    bodyScope,
  );

  return {
    ...expr,
    elements: [
      operator,
      {
        type: 'list',
        position: target.position,
        introduced: target.introduced,
        elements: [renamedTarget.symbol, ...(transformedParams.paramsExpr as Extract<Expr, { type: 'list' }>).elements],
      },
      ...rest.map((element) => hygienizeExpr(element, definitionEnv, transformedParams.scope)),
    ],
  };
}

function hygienizeParameterSpec(
  paramsExpr: Expr,
  scope: Map<string, string>,
): { paramsExpr: Expr; scope: Map<string, string> } {
  let nextScope = new Map(scope);

  if (paramsExpr.type === 'symbol') {
    const renamed = freshenBinder(paramsExpr, nextScope);
    return { paramsExpr: renamed.symbol, scope: renamed.scope };
  }

  if (paramsExpr.type !== 'list') {
    return { paramsExpr, scope: nextScope };
  }

  const params: Expr[] = [];
  for (const param of paramsExpr.elements) {
    if (param.type !== 'symbol' || param.name === '.') {
      params.push(param);
      continue;
    }

    const renamed = freshenBinder(param, nextScope);
    params.push(renamed.symbol);
    nextScope = renamed.scope;
  }

  return {
    paramsExpr: { ...paramsExpr, elements: params },
    scope: nextScope,
  };
}

function hygienizeLetBindings(
  bindingsExpr: Expr,
  definitionEnv: Environment,
  valueScope: Map<string, string>,
  bodyScope: Map<string, string>,
): { bindingsExpr: Expr; scope: Map<string, string> } {
  if (bindingsExpr.type !== 'list') {
    return { bindingsExpr, scope: bodyScope };
  }

  let nextScope = new Map(bodyScope);
  const bindings = bindingsExpr.elements.map((bindingExpr) => {
    if (bindingExpr.type !== 'list' || bindingExpr.elements.length !== 2) {
      return hygienizeExpr(bindingExpr, definitionEnv, valueScope);
    }

    const [nameExpr, valueExpr] = bindingExpr.elements;
    const transformedValue = hygienizeExpr(valueExpr, definitionEnv, valueScope);

    if (nameExpr.type !== 'symbol' || nameExpr.name === '.') {
      return {
        ...bindingExpr,
        elements: [nameExpr, transformedValue],
      };
    }

    const renamed = freshenBinder(nameExpr, nextScope);
    nextScope = renamed.scope;

    return {
      ...bindingExpr,
      elements: [renamed.symbol, transformedValue],
    };
  });

  return {
    bindingsExpr: { ...bindingsExpr, elements: bindings },
    scope: nextScope,
  };
}

function freshenBinder(
  symbol: SymbolExpr,
  scope: Map<string, string>,
): { symbol: SymbolExpr; scope: Map<string, string> } {
  if (!symbol.introduced || symbol.name === '.') {
    return { symbol, scope };
  }

  const freshName = freshResolvedName(symbol.name);
  const nextScope = new Map(scope);
  nextScope.set(symbol.name, freshName);
  return {
    symbol: { ...symbol, resolvedName: freshName },
    scope: nextScope,
  };
}

function freshResolvedName(name: string): string {
  freshIdentifierCounter += 1;
  return `__macro_${freshIdentifierCounter}_${name}`;
}

function isEllipsisExpr(expr: Expr, ellipsis: string): boolean {
  return expr.type === 'symbol' && expr.name === ellipsis;
}

function exprSyntaxEqual(left: Expr, right: Expr): boolean {
  if (left.type !== right.type) {
    return false;
  }

  switch (left.type) {
    case 'boolean':
    case 'string':
    case 'char':
      return left.value === (right as typeof left).value;
    case 'number':
      return compareNumbers(left.value, (right as typeof left).value) === 0;
    case 'symbol':
      return left.name === (right as typeof left).name;
    case 'list': {
      const rightList = right as Extract<Expr, { type: 'list' }>;
      if (left.elements.length !== rightList.elements.length) {
        return false;
      }

      for (let index = 0; index < left.elements.length; index += 1) {
        if (!exprSyntaxEqual(left.elements[index], rightList.elements[index])) {
          return false;
        }
      }

      return true;
    }
  }
}

function cloneExpr<T extends Expr>(expr: T): T {
  switch (expr.type) {
    case 'number':
    case 'boolean':
    case 'string':
    case 'char':
      return { ...expr, introduced: undefined };
    case 'symbol':
      return { ...expr, introduced: undefined };
    case 'list':
      return {
        ...expr,
        introduced: undefined,
        elements: expr.elements.map((element) => cloneExpr(element)),
      } as T;
  }
}

function applyProcedure(
  value: SchemeValue,
  args: EvaluatedArg[],
  callPosition: SourcePosition,
): SchemeValue {
  switch (value.type) {
    case 'builtin':
      return value.invoke(args, callPosition);
    case 'closure': {
      if (value.restParam === undefined) {
        requireArgCount('lambda', args.length, value.params.length, callPosition);
      } else {
        requireArgCountAtLeast('lambda', args.length, value.params.length, callPosition);
      }

      const callEnv = new Environment(value.env);
      for (let index = 0; index < value.params.length; index += 1) {
        callEnv.define(value.params[index], args[index].value);
      }

      if (value.restParam !== undefined) {
        callEnv.define(value.restParam, {
          type: 'list',
          elements: args.slice(value.params.length).map((arg) => arg.value),
        });
      }

      return evaluateSequence(value.body, callEnv);
    }
    default:
      throw new EvalError('attempted to call a non-procedure', callPosition);
  }
}

function evaluateSequence(expressions: Expr[], env: Environment): SchemeValue {
  let result: SchemeValue = VOID_VALUE;

  for (const expr of expressions) {
    result = evaluate(expr, env);
  }

  return result;
}

function createGlobalEnv(context: EvaluationContext): Environment {
  const env = new Environment();

  env.define(
    '+',
    builtin('+', (args, callPosition) =>
      numberValue(addNumbers(evaluateNumberArgs('+', args), callPosition), callPosition),
    ),
  );

  env.define(
    '*',
    builtin('*', (args, callPosition) =>
      numberValue(multiplyNumbers(evaluateNumberArgs('*', args), callPosition), callPosition),
    ),
  );

  env.define(
    '-',
    builtin('-', (args, callPosition) => {
      const values = evaluateNumberArgs('-', args);
      requireArgCountAtLeast('-', values.length, 1, callPosition);
      return numberValue(subtractNumbers(values, callPosition), callPosition);
    }),
  );

  env.define(
    '/',
    builtin('/', (args, callPosition) => {
      const values = evaluateNumberArgs('/', args);
      requireArgCountAtLeast('/', values.length, 1, callPosition);

      if (values.length === 1) {
        if (isZeroNumber(values[0])) {
          throw new EvalError('division by zero', args[0].position);
        }
      }

      for (let index = 1; index < values.length; index += 1) {
        if (isZeroNumber(values[index])) {
          throw new EvalError('division by zero', args[index].position);
        }
      }

      return numberValue(divideNumbers(values, callPosition), callPosition);
    }),
  );

  env.define(
    '<',
    builtin('<', (args, callPosition) =>
      booleanValue(compareNumberArgs('<', args, (comparison) => comparison < 0, callPosition)),
    ),
  );
  env.define(
    '>',
    builtin('>', (args, callPosition) =>
      booleanValue(compareNumberArgs('>', args, (comparison) => comparison > 0, callPosition)),
    ),
  );
  env.define(
    '=',
    builtin('=', (args, callPosition) =>
      booleanValue(compareNumberArgs('=', args, (comparison) => comparison === 0, callPosition)),
    ),
  );
  env.define(
    '<=',
    builtin('<=', (args, callPosition) =>
      booleanValue(compareNumberArgs('<=', args, (comparison) => comparison <= 0, callPosition)),
    ),
  );

  env.define(
    'abs',
    builtin('abs', (args, callPosition) => {
      requireArgCount('abs', args.length, 1, callPosition);
      return numberValue(absNumber(expectNumber('abs', args[0]), callPosition), callPosition);
    }),
  );

  env.define(
    'quotient',
    builtin('quotient', (args, callPosition) => {
      requireArgCount('quotient', args.length, 2, callPosition);
      const dividend = expectInteger('quotient', args[0]);
      const divisor = expectInteger('quotient', args[1]);
      if (divisor === 0) {
        throw new EvalError('division by zero', args[1].position);
      }

      return numberValue(Math.trunc(dividend / divisor), callPosition);
    }),
  );

  env.define(
    'remainder',
    builtin('remainder', (args, callPosition) => {
      requireArgCount('remainder', args.length, 2, callPosition);
      const dividend = expectInteger('remainder', args[0]);
      const divisor = expectInteger('remainder', args[1]);
      if (divisor === 0) {
        throw new EvalError('division by zero', args[1].position);
      }

      return numberValue(dividend % divisor, callPosition);
    }),
  );

  env.define(
    'modulo',
    builtin('modulo', (args, callPosition) => {
      requireArgCount('modulo', args.length, 2, callPosition);
      const dividend = expectInteger('modulo', args[0]);
      const divisor = expectInteger('modulo', args[1]);
      if (divisor === 0) {
        throw new EvalError('division by zero', args[1].position);
      }

      let result = dividend % divisor;
      if (result !== 0 && Math.sign(result) !== Math.sign(divisor)) {
        result += divisor;
      }

      return numberValue(result, callPosition);
    }),
  );

  env.define(
    'min',
    builtin('min', (args, callPosition) => {
      const values = evaluateNumberArgs('min', args);
      requireArgCountAtLeast('min', values.length, 1, callPosition);
      return numberValue(minNumber(values), callPosition);
    }),
  );

  env.define(
    'max',
    builtin('max', (args, callPosition) => {
      const values = evaluateNumberArgs('max', args);
      requireArgCountAtLeast('max', values.length, 1, callPosition);
      return numberValue(maxNumber(values), callPosition);
    }),
  );

  env.define(
    'expt',
    builtin('expt', (args, callPosition) => {
      requireArgCount('expt', args.length, 2, callPosition);
      const base = expectNumber('expt', args[0]);
      const exponent = expectInteger('expt', args[1]);
      return numberValue(exptNumber(base, exponent, args[1].position), callPosition);
    }),
  );

  env.define(
    'zero?',
    builtin('zero?', (args, callPosition) => {
      requireArgCount('zero?', args.length, 1, callPosition);
      return booleanValue(isZeroNumber(expectNumber('zero?', args[0])));
    }),
  );

  env.define(
    'positive?',
    builtin('positive?', (args, callPosition) => {
      requireArgCount('positive?', args.length, 1, callPosition);
      return booleanValue(isPositiveNumber(expectNumber('positive?', args[0])));
    }),
  );

  env.define(
    'negative?',
    builtin('negative?', (args, callPosition) => {
      requireArgCount('negative?', args.length, 1, callPosition);
      return booleanValue(isNegativeNumber(expectNumber('negative?', args[0])));
    }),
  );

  env.define(
    'odd?',
    builtin('odd?', (args, callPosition) => {
      requireArgCount('odd?', args.length, 1, callPosition);
      return booleanValue(Math.abs(expectInteger('odd?', args[0]) % 2) === 1);
    }),
  );

  env.define(
    'even?',
    builtin('even?', (args, callPosition) => {
      requireArgCount('even?', args.length, 1, callPosition);
      return booleanValue(expectInteger('even?', args[0]) % 2 === 0);
    }),
  );

  env.define(
    'not',
    builtin('not', (args, callPosition) => {
      requireArgCount('not', args.length, 1, callPosition);
      return booleanValue(!isTruthy(args[0].value));
    }),
  );

  env.define(
    'eq?',
    builtin('eq?', (args, callPosition) => {
      requireArgCount('eq?', args.length, 2, callPosition);
      return booleanValue(equalValues(args[0].value, args[1].value));
    }),
  );

  env.define(
    'equal?',
    builtin('equal?', (args, callPosition) => {
      requireArgCount('equal?', args.length, 2, callPosition);
      return booleanValue(equalValues(args[0].value, args[1].value));
    }),
  );

  env.define(
    'cons',
    builtin('cons', (args, callPosition) => {
      requireArgCount('cons', args.length, 2, callPosition);
      return consValue(args[0].value, args[1].value);
    }),
  );

  env.define(
    'car',
    builtin('car', (args, callPosition) => {
      requireArgCount('car', args.length, 1, callPosition);
      return expectPair('car', args[0]).elements[0];
    }),
  );

  env.define(
    'cdr',
    builtin('cdr', (args, callPosition) => {
      requireArgCount('cdr', args.length, 1, callPosition);
      return cdrValue(expectPair('cdr', args[0]));
    }),
  );

  env.define('list', builtin('list', (args) => listValue(args.map((arg) => arg.value))));

  env.define(
    'length',
    builtin('length', (args, callPosition) => {
      requireArgCount('length', args.length, 1, callPosition);
      return numberValue(expectList('length', args[0]).elements.length, callPosition);
    }),
  );

  env.define(
    'append',
    builtin('append', (args) => {
      const elements: SchemeValue[] = [];

      for (const arg of args) {
        elements.push(...expectList('append', arg).elements);
      }

      return listValue(elements);
    }),
  );

  env.define(
    'apply',
    builtin('apply', (args, callPosition) => {
      requireArgCountAtLeast('apply', args.length, 2, callPosition);

      const procedure = args[0].value;
      const finalListArg = args[args.length - 1];
      const trailingArgs = expectList('apply', finalListArg).elements.map((value) => ({
        value,
        position: finalListArg.position,
      }));

      return applyProcedure(
        procedure,
        [...args.slice(1, -1), ...trailingArgs],
        callPosition,
      );
    }),
  );

  env.define(
    'map',
    builtin('map', (args, callPosition) => {
      requireArgCountAtLeast('map', args.length, 2, callPosition);

      const procedure = args[0].value;
      const lists = args.slice(1).map((arg) => expectList('map', arg));
      const resultLength = Math.min(...lists.map((list) => list.elements.length));
      const results: SchemeValue[] = [];

      for (let index = 0; index < resultLength; index += 1) {
        const mappedArgs = lists.map((list, listIndex) => ({
          value: list.elements[index],
          position: args[listIndex + 1].position,
        }));
        results.push(applyProcedure(procedure, mappedArgs, callPosition));
      }

      return listValue(results);
    }),
  );

  env.define(
    'display',
    builtin('display', (args, callPosition) => {
      requireArgCount('display', args.length, 1, callPosition);
      context.output.push(formatDisplayValue(args[0].value));
      return VOID_VALUE;
    }),
  );

  env.define(
    'write',
    builtin('write', (args, callPosition) => {
      requireArgCount('write', args.length, 1, callPosition);
      context.output.push(formatValue(args[0].value));
      return VOID_VALUE;
    }),
  );

  env.define(
    'newline',
    builtin('newline', (args, callPosition) => {
      requireArgCount('newline', args.length, 0, callPosition);
      context.output.push('\n');
      return VOID_VALUE;
    }),
  );

  env.define(
    'string-append',
    builtin('string-append', (args) =>
      stringValue(args.map((arg) => expectString('string-append', arg)).join('')),
    ),
  );

  env.define(
    'string-length',
    builtin('string-length', (args, callPosition) => {
      requireArgCount('string-length', args.length, 1, callPosition);
      return numberValue(codePoints(expectString('string-length', args[0])).length, callPosition);
    }),
  );

  env.define(
    'substring',
    builtin('substring', (args, callPosition) => {
      requireArgCount('substring', args.length, 3, callPosition);
      const value = expectString('substring', args[0]);
      const start = expectNonNegativeInteger('substring', args[1]);
      const end = expectNonNegativeInteger('substring', args[2]);
      const characters = codePoints(value);

      if (start > end) {
        throw new EvalError('substring: start index exceeds end index', args[1].position);
      }

      if (end > characters.length) {
        throw new EvalError('substring: index out of range', args[2].position);
      }

      return { type: 'string', value: characters.slice(start, end).join('') };
    }),
  );

  env.define(
    'string->number',
    builtin('string->number', (args, callPosition) => {
      requireArgCount('string->number', args.length, 1, callPosition);
      const value = expectString('string->number', args[0]);
      const parsed = parseNumberLiteral(value, args[0].position);
      if (parsed === null) {
        return booleanValue(false);
      }

      return numberValue(parsed, args[0].position);
    }),
  );

  env.define(
    'number->string',
    builtin('number->string', (args, callPosition) => {
      requireArgCount('number->string', args.length, 1, callPosition);
      return stringValue(formatNumber(expectNumber('number->string', args[0])));
    }),
  );

  env.define(
    'symbol->string',
    builtin('symbol->string', (args, callPosition) => {
      requireArgCount('symbol->string', args.length, 1, callPosition);
      return stringValue(expectSymbol('symbol->string', args[0]).name);
    }),
  );

  env.define(
    'string->symbol',
    builtin('string->symbol', (args, callPosition) => {
      requireArgCount('string->symbol', args.length, 1, callPosition);
      return { type: 'symbol', name: expectString('string->symbol', args[0]) };
    }),
  );

  env.define(
    'string-ref',
    builtin('string-ref', (args, callPosition) => {
      requireArgCount('string-ref', args.length, 2, callPosition);
      const value = expectString('string-ref', args[0]);
      const index = expectNonNegativeInteger('string-ref', args[1]);
      const characters = codePoints(value);

      if (index >= characters.length) {
        throw new EvalError('string-ref: index out of range', args[1].position);
      }

      return charValue(characters[index], args[1].position);
    }),
  );

  env.define(
    'string-copy',
    builtin('string-copy', (args, callPosition) => {
      requireArgCount('string-copy', args.length, 1, callPosition);
      return stringValue(expectString('string-copy', args[0]));
    }),
  );

  env.define(
    'string-set!',
    builtin('string-set!', (args, callPosition) => {
      requireArgCount('string-set!', args.length, 3, callPosition);
      const target = expectStringValue('string-set!', args[0]);
      const index = expectNonNegativeInteger('string-set!', args[1]);
      const character = expectChar('string-set!', args[2]).value;
      const characters = codePoints(target.value);

      if (index >= characters.length) {
        throw new EvalError('string-set!: index out of range', args[1].position);
      }

      characters[index] = character;
      target.value = characters.join('');
      return VOID_VALUE;
    }),
  );

  env.define(
    'string=?',
    builtin('string=?', (args, callPosition) =>
      booleanValue(compareStringArgs('string=?', args, (a, b) => a === b, callPosition)),
    ),
  );

  env.define(
    'string<?',
    builtin('string<?', (args, callPosition) =>
      booleanValue(compareStringArgs('string<?', args, (a, b) => a < b, callPosition)),
    ),
  );

  env.define(
    'string-ci=?',
    builtin('string-ci=?', (args, callPosition) =>
      booleanValue(
        compareStringArgs(
          'string-ci=?',
          args,
          (a, b) => a.toLowerCase() === b.toLowerCase(),
          callPosition,
        ),
      ),
    ),
  );

  env.define(
    'string-upcase',
    builtin('string-upcase', (args, callPosition) => {
      requireArgCount('string-upcase', args.length, 1, callPosition);
      return stringValue(expectString('string-upcase', args[0]).toUpperCase());
    }),
  );

  env.define(
    'string-downcase',
    builtin('string-downcase', (args, callPosition) => {
      requireArgCount('string-downcase', args.length, 1, callPosition);
      return stringValue(expectString('string-downcase', args[0]).toLowerCase());
    }),
  );

  env.define(
    'null?',
    builtin('null?', (args, callPosition) => {
      requireArgCount('null?', args.length, 1, callPosition);
      return booleanValue(
        args[0].value.type === 'list' &&
          args[0].value.elements.length === 0 &&
          args[0].value.tail === undefined,
      );
    }),
  );

  env.define(
    'pair?',
    builtin('pair?', (args, callPosition) => {
      requireArgCount('pair?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'list' && args[0].value.elements.length > 0);
    }),
  );

  env.define(
    'string?',
    builtin('string?', (args, callPosition) => {
      requireArgCount('string?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'string');
    }),
  );

  env.define(
    'number?',
    builtin('number?', (args, callPosition) => {
      requireArgCount('number?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'number');
    }),
  );

  env.define(
    'integer?',
    builtin('integer?', (args, callPosition) => {
      requireArgCount('integer?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'number' && isIntegerNumber(args[0].value.value));
    }),
  );

  env.define(
    'rational?',
    builtin('rational?', (args, callPosition) => {
      requireArgCount('rational?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'number' && isRationalNumber(args[0].value.value));
    }),
  );

  env.define(
    'exact?',
    builtin('exact?', (args, callPosition) => {
      requireArgCount('exact?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'number' && isExactNumber(args[0].value.value));
    }),
  );

  env.define(
    'inexact?',
    builtin('inexact?', (args, callPosition) => {
      requireArgCount('inexact?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'number' && isInexactNumber(args[0].value.value));
    }),
  );

  env.define(
    'exact->inexact',
    builtin('exact->inexact', (args, callPosition) => {
      requireArgCount('exact->inexact', args.length, 1, callPosition);
      return numberValue(exactToInexact(expectNumber('exact->inexact', args[0]), args[0].position), callPosition);
    }),
  );

  env.define(
    'inexact->exact',
    builtin('inexact->exact', (args, callPosition) => {
      requireArgCount('inexact->exact', args.length, 1, callPosition);
      return numberValue(inexactToExact(expectNumber('inexact->exact', args[0]), args[0].position), callPosition);
    }),
  );

  env.define(
    'numerator',
    builtin('numerator', (args, callPosition) => {
      requireArgCount('numerator', args.length, 1, callPosition);
      return numberValue(numeratorPart(expectNumber('numerator', args[0]), args[0].position), callPosition);
    }),
  );

  env.define(
    'denominator',
    builtin('denominator', (args, callPosition) => {
      requireArgCount('denominator', args.length, 1, callPosition);
      return numberValue(denominatorPart(expectNumber('denominator', args[0]), args[0].position), callPosition);
    }),
  );

  env.define(
    'boolean?',
    builtin('boolean?', (args, callPosition) => {
      requireArgCount('boolean?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'boolean');
    }),
  );

  env.define(
    'symbol?',
    builtin('symbol?', (args, callPosition) => {
      requireArgCount('symbol?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'symbol');
    }),
  );

  env.define(
    'char?',
    builtin('char?', (args, callPosition) => {
      requireArgCount('char?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'char');
    }),
  );

  env.define(
    'list?',
    builtin('list?', (args, callPosition) => {
      requireArgCount('list?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'list' && isProperList(args[0].value));
    }),
  );

  env.define(
    'list-ref',
    builtin('list-ref', (args, callPosition) => {
      requireArgCount('list-ref', args.length, 2, callPosition);
      const list = expectList('list-ref', args[0]);
      const index = expectNonNegativeInteger('list-ref', args[1]);
      if (index >= list.elements.length) {
        throw new EvalError('list-ref: index out of range', args[1].position);
      }

      return list.elements[index];
    }),
  );

  env.define(
    'list-tail',
    builtin('list-tail', (args, callPosition) => {
      requireArgCount('list-tail', args.length, 2, callPosition);
      const list = expectList('list-tail', args[0]);
      const index = expectNonNegativeInteger('list-tail', args[1]);
      if (index > list.elements.length) {
        throw new EvalError('list-tail: index out of range', args[1].position);
      }

      return listValue(list.elements.slice(index));
    }),
  );

  env.define(
    'assoc',
    builtin('assoc', (args, callPosition) => {
      requireArgCount('assoc', args.length, 2, callPosition);
      const key = args[0].value;
      const alist = expectList('assoc', args[1]);

      for (const entry of alist.elements) {
        if (entry.type === 'list' && entry.elements.length > 0 && equalValues(entry.elements[0], key)) {
          return entry;
        }
      }

      return booleanValue(false);
    }),
  );

  env.define(
    'char-alphabetic?',
    builtin('char-alphabetic?', (args, callPosition) => {
      requireArgCount('char-alphabetic?', args.length, 1, callPosition);
      return booleanValue(/^[A-Za-z]$/u.test(expectChar('char-alphabetic?', args[0]).value));
    }),
  );

  env.define(
    'char-numeric?',
    builtin('char-numeric?', (args, callPosition) => {
      requireArgCount('char-numeric?', args.length, 1, callPosition);
      return booleanValue(/^[0-9]$/u.test(expectChar('char-numeric?', args[0]).value));
    }),
  );

  env.define(
    'char-upcase',
    builtin('char-upcase', (args, callPosition) => {
      requireArgCount('char-upcase', args.length, 1, callPosition);
      return charValue(expectChar('char-upcase', args[0]).value.toUpperCase(), args[0].position);
    }),
  );

  env.define(
    'char-downcase',
    builtin('char-downcase', (args, callPosition) => {
      requireArgCount('char-downcase', args.length, 1, callPosition);
      return charValue(expectChar('char-downcase', args[0]).value.toLowerCase(), args[0].position);
    }),
  );

  env.define(
    'char=?',
    builtin('char=?', (args, callPosition) =>
      booleanValue(compareCharArgs('char=?', args, (a, b) => a === b, callPosition)),
    ),
  );

  env.define(
    'char<?',
    builtin('char<?', (args, callPosition) =>
      booleanValue(
        compareCharArgs(
          'char<?',
          args,
          (a, b) => a.codePointAt(0)! < b.codePointAt(0)!,
          callPosition,
        ),
      ),
    ),
  );

  return env;
}

function builtin(
  name: string,
  invoke: (args: EvaluatedArg[], callPosition: SourcePosition) => SchemeValue,
): BuiltinProcedure {
  return { type: 'builtin', name, invoke };
}

function evaluateNumberArgs(name: string, args: EvaluatedArg[]): SchemeNumber[] {
  return args.map((arg) => expectNumber(name, arg));
}

function compareNumberArgs(
  name: string,
  args: EvaluatedArg[],
  predicate: (comparison: number) => boolean,
  position: SourcePosition,
): boolean {
  const values = evaluateNumberArgs(name, args);
  requireArgCountAtLeast(name, values.length, 1, position);

  for (let index = 0; index < values.length - 1; index += 1) {
    if (!predicate(compareNumbers(values[index], values[index + 1]))) {
      return false;
    }
  }

  return true;
}

function compareStringArgs(
  name: string,
  args: EvaluatedArg[],
  predicate: (left: string, right: string) => boolean,
  position: SourcePosition,
): boolean {
  const values = args.map((arg) => expectString(name, arg));
  requireArgCountAtLeast(name, values.length, 1, position);

  for (let index = 0; index < values.length - 1; index += 1) {
    if (!predicate(values[index], values[index + 1])) {
      return false;
    }
  }

  return true;
}

function compareCharArgs(
  name: string,
  args: EvaluatedArg[],
  predicate: (left: string, right: string) => boolean,
  position: SourcePosition,
): boolean {
  const values = args.map((arg) => expectChar(name, arg).value);
  requireArgCountAtLeast(name, values.length, 1, position);

  for (let index = 0; index < values.length - 1; index += 1) {
    if (!predicate(values[index], values[index + 1])) {
      return false;
    }
  }

  return true;
}

function requireArgCount(
  name: string,
  actual: number,
  expected: number,
  position: SourcePosition,
): void {
  if (actual !== expected) {
    throw new EvalError(`${name}: expected ${expected} argument(s), got ${actual}`, position);
  }
}

function requireArgCountAtLeast(
  name: string,
  actual: number,
  minimum: number,
  position: SourcePosition,
): void {
  if (actual < minimum) {
    throw new EvalError(`${name}: expected at least ${minimum} argument(s), got ${actual}`, position);
  }
}

function expectNumber(name: string, arg: EvaluatedArg): SchemeNumber {
  if (arg.value.type !== 'number') {
    throw new EvalError(`${name}: expected number`, arg.position);
  }

  return arg.value.value;
}

function expectInteger(name: string, arg: EvaluatedArg): number {
  const value = expectNumber(name, arg);
  if (!isIntegerNumber(value)) {
    throw new EvalError(`${name}: expected integer`, arg.position);
  }

  return integerToJs(value, arg.position);
}

function expectString(name: string, arg: EvaluatedArg): string {
  return expectStringValue(name, arg).value;
}

function expectStringValue(name: string, arg: EvaluatedArg): Extract<SchemeValue, { type: 'string' }> {
  if (arg.value.type !== 'string') {
    throw new EvalError(`${name}: expected string`, arg.position);
  }

  return arg.value;
}

function expectSymbol(name: string, arg: EvaluatedArg): Extract<SchemeValue, { type: 'symbol' }> {
  if (arg.value.type !== 'symbol') {
    throw new EvalError(`${name}: expected symbol`, arg.position);
  }

  return arg.value;
}

function expectNonNegativeInteger(name: string, arg: EvaluatedArg): number {
  const value = expectInteger(name, arg);
  if (value < 0) {
    throw new EvalError(`${name}: expected non-negative integer`, arg.position);
  }

  return value;
}

function expectList(name: string, arg: EvaluatedArg): ListValue {
  const list = expectListValue(name, arg);
  if (!isProperList(list)) {
    throw new EvalError(`${name}: expected list`, arg.position);
  }

  return list;
}

function expectListValue(name: string, arg: EvaluatedArg): ListValue {
  if (arg.value.type !== 'list') {
    throw new EvalError(`${name}: expected list`, arg.position);
  }

  return arg.value;
}

function expectPair(name: string, arg: EvaluatedArg): ListValue {
  const list = expectListValue(name, arg);
  if (list.elements.length === 0) {
    throw new EvalError(`${name}: expected non-empty list`, arg.position);
  }

  return list;
}

function expectChar(name: string, arg: EvaluatedArg): Extract<SchemeValue, { type: 'char' }> {
  if (arg.value.type !== 'char') {
    throw new EvalError(`${name}: expected character`, arg.position);
  }

  return arg.value;
}

function listValue(elements: SchemeValue[], tail?: SchemeValue): ListValue {
  return tail === undefined ? { type: 'list', elements } : { type: 'list', elements, tail };
}

function isProperList(value: ListValue): boolean {
  return value.tail === undefined;
}

function consValue(head: SchemeValue, tail: SchemeValue): ListValue {
  if (tail.type === 'list') {
    return listValue([head, ...tail.elements], tail.tail);
  }

  return listValue([head], tail);
}

function cdrValue(list: ListValue): SchemeValue {
  if (list.elements.length > 1) {
    return listValue(list.elements.slice(1), list.tail);
  }

  return list.tail ?? listValue([]);
}

function equalValues(left: SchemeValue, right: SchemeValue): boolean {
  if (left === right) {
    return true;
  }

  if (left.type !== right.type) {
    return false;
  }

  switch (left.type) {
    case 'number':
      return compareNumbers(left.value, (right as typeof left).value) === 0;
    case 'boolean':
    case 'string':
    case 'char':
      return left.value === (right as typeof left).value;
    case 'symbol':
      return left.name === (right as typeof left).name;
    case 'list': {
      const rightList = right as ListValue;
      if (left.elements.length !== rightList.elements.length) {
        return false;
      }

      for (let index = 0; index < left.elements.length; index += 1) {
        if (!equalValues(left.elements[index], rightList.elements[index])) {
          return false;
        }
      }

      if (left.tail === undefined || rightList.tail === undefined) {
        return left.tail === undefined && rightList.tail === undefined;
      }

      return equalValues(left.tail, rightList.tail);
    }
    case 'builtin':
    case 'closure':
      return left === right;
    case 'void':
      return true;
  }
}

function isTruthy(value: SchemeValue): boolean {
  return value.type !== 'boolean' || value.value;
}

function parseCharLiteral(value: string, position: SourcePosition): string | null {
  if (!value.startsWith('#\\')) {
    return null;
  }

  const literal = value.slice(2);
  switch (literal) {
    case 'space':
      return ' ';
    case 'newline':
      return '\n';
  }

  const characters = codePoints(literal);
  if (characters.length === 1) {
    return characters[0];
  }

  throw new EvalError('invalid character literal', position);
}

function codePoints(value: string): string[] {
  return Array.from(value);
}

function numberValue(value: number | SchemeNumber, position?: SourcePosition): SchemeValue {
  if (typeof value === 'number') {
    return {
      type: 'number',
      value: Number.isInteger(value) ? exactInteger(value) : inexactNumber(value, position),
    };
  }

  return { type: 'number', value };
}

function booleanValue(value: boolean): SchemeValue {
  return { type: 'boolean', value };
}

function stringValue(value: string): SchemeValue {
  return { type: 'string', value };
}

function charValue(value: string, position?: SourcePosition): SchemeValue {
  if (codePoints(value).length !== 1) {
    throw new EvalError('invalid character', position);
  }

  return { type: 'char', value };
}

function attachPosition(error: unknown, position: SourcePosition): EvalError {
  if (error instanceof EvalError) {
    return error.position ? error : new EvalError(error.rawMessage, position);
  }

  if (error instanceof Error) {
    return new EvalError(error.message, position);
  }

  return new EvalError(String(error), position);
}

function formatDisplayValue(value: SchemeValue): string {
  switch (value.type) {
    case 'string':
    case 'char':
      return value.value;
    case 'list':
      return formatListValue(value, formatDisplayValue);
    default:
      return formatValue(value);
  }
}

function formatValue(value: SchemeValue): string {
  switch (value.type) {
    case 'number':
      return formatNumber(value.value);
    case 'boolean':
      return value.value ? '#t' : '#f';
    case 'string':
      return JSON.stringify(value.value);
    case 'char':
      return formatChar(value.value);
    case 'symbol':
      return value.name;
    case 'list':
      return formatListValue(value, formatValue);
    case 'builtin':
    case 'closure':
      return '#<procedure>';
    case 'void':
      return '#<void>';
  }
}

function formatListValue(
  value: ListValue,
  formatter: (value: SchemeValue) => string,
): string {
  const elements = value.elements.map(formatter);
  if (value.tail === undefined) {
    return `(${elements.join(' ')})`;
  }

  return `(${elements.join(' ')} . ${formatter(value.tail)})`;
}

function formatChar(value: string): string {
  if (value === ' ') {
    return '#\\space';
  }

  if (value === '\n') {
    return '#\\newline';
  }

  return `#\\${value}`;
}
