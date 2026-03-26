import { EvalError } from './evalError.js';
const BUILTIN_NAMES = [
    '+',
    '-',
    '*',
    '/',
    'abs',
    'modulo',
    'remainder',
    'quotient',
    'min',
    'max',
    'expt',
    '<',
    '>',
    '=',
    '<=',
    'zero?',
    'positive?',
    'negative?',
    'odd?',
    'even?',
    'not',
    'cons',
    'car',
    'cdr',
    'null?',
    'list',
    'append',
    'length',
    'list-ref',
    'list-tail',
    'list?',
    'assoc',
    'map',
    'string?',
    'number?',
    'boolean?',
    'pair?',
    'symbol?',
    'eq?',
    'equal?',
    'display',
    'write',
    'newline',
    'string-append',
    'string-length',
    'substring',
    'string->number',
    'number->string',
    'symbol->string',
    'string->symbol',
    'string-ref',
    'string-copy',
    'string-set!',
    'string=?',
    'string<?',
    'string-ci=?',
    'string-upcase',
    'string-downcase',
    'char?',
    'char-alphabetic?',
    'char-numeric?',
    'char-upcase',
    'char-downcase',
    'char=?',
    'char<?',
    'apply',
];
const NIL_VALUE = { kind: 'nil' };
const VOID_VALUE = { kind: 'void' };
const DEFAULT_SOURCE_POS = { line: 1, col: 1 };
const ALPHABETIC_CHAR_RE = /^\p{L}$/u;
const NUMERIC_CHAR_RE = /^\p{N}$/u;
let macroIdentifierCounter = 0;
class Environment {
    parent;
    bindings = new Map();
    macros = new Map();
    constructor(parent) {
        this.parent = parent;
    }
    define(name, value) {
        this.bindings.set(name, value);
    }
    defineMacro(name, macroRules) {
        this.macros.set(name, macroRules);
    }
    lookupOptional(name) {
        if (this.bindings.has(name)) {
            return this.bindings.get(name);
        }
        return this.parent?.lookupOptional(name);
    }
    lookupMacro(name) {
        if (this.macros.has(name)) {
            return this.macros.get(name);
        }
        return this.parent?.lookupMacro(name);
    }
    lookup(name) {
        const value = this.lookupOptional(name);
        if (value !== undefined) {
            return value;
        }
        throw new EvalError(`unbound symbol: ${name}`);
    }
    assign(name, value) {
        if (this.bindings.has(name)) {
            this.bindings.set(name, value);
            return;
        }
        if (this.parent !== undefined) {
            this.parent.assign(name, value);
            return;
        }
        throw new EvalError(`unbound symbol: ${name}`);
    }
}
/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input) {
    return formatValue(evaluateProgram(input).result);
}
/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input) {
    const evaluation = evaluateProgram(input);
    return {
        result: formatValue(evaluation.result),
        output: evaluation.output,
    };
}
function evaluateProgram(input) {
    const expressions = parseProgram(input);
    if (expressions.length === 0) {
        throw new EvalError('expected at least one expression');
    }
    const env = createGlobalEnv();
    const context = { output: [] };
    let result = VOID_VALUE;
    for (const expr of expressions) {
        result = evaluateExpr(expr, env, context);
    }
    return {
        result,
        output: context.output.join(''),
    };
}
function createGlobalEnv() {
    const env = new Environment();
    for (const name of BUILTIN_NAMES) {
        env.define(name, { kind: 'builtin', name });
    }
    return env;
}
function parseProgram(input) {
    const tokens = tokenize(input);
    const expressions = [];
    let index = 0;
    while (index < tokens.length) {
        const parsed = parseExpr(tokens, index);
        expressions.push(parsed.expr);
        index = parsed.nextIndex;
    }
    return expressions;
}
function tokenize(input) {
    const tokens = [];
    let index = 0;
    let line = 1;
    let col = 1;
    const currentPos = () => ({ line, col });
    const advanceChar = (char) => {
        if (char === '\n') {
            line += 1;
            col = 1;
            return;
        }
        col += 1;
    };
    while (index < input.length) {
        const char = input[index];
        if (isWhitespace(char)) {
            advanceChar(char);
            index += 1;
            continue;
        }
        if (char === ';') {
            while (index < input.length && input[index] !== '\n') {
                advanceChar(input[index]);
                index += 1;
            }
            continue;
        }
        if (char === '(' || char === ')') {
            tokens.push({ kind: 'paren', value: char, pos: currentPos() });
            advanceChar(char);
            index += 1;
            continue;
        }
        if (char === "'") {
            tokens.push({ kind: 'quote', pos: currentPos() });
            advanceChar(char);
            index += 1;
            continue;
        }
        if (char === '"') {
            const pos = currentPos();
            const parsed = parseStringToken(input, index, pos);
            tokens.push({ kind: 'string', value: parsed.value, pos });
            for (let scan = index; scan < parsed.nextIndex; scan += 1) {
                advanceChar(input[scan]);
            }
            index = parsed.nextIndex;
            continue;
        }
        const pos = currentPos();
        let end = index;
        while (end < input.length && !isDelimiter(input[end])) {
            end += 1;
        }
        tokens.push({ kind: 'atom', value: input.slice(index, end), pos });
        col += end - index;
        index = end;
    }
    return tokens;
}
function parseStringToken(input, start, pos) {
    let index = start + 1;
    let value = '';
    while (index < input.length) {
        const char = input[index];
        if (char === '"') {
            return { value, nextIndex: index + 1 };
        }
        if (char === '\\') {
            index += 1;
            if (index >= input.length) {
                throw new EvalError('unterminated string literal', pos);
            }
            const escaped = input[index];
            switch (escaped) {
                case '"':
                    value += '"';
                    break;
                case '\\':
                    value += '\\';
                    break;
                case 'n':
                    value += '\n';
                    break;
                case 'r':
                    value += '\r';
                    break;
                case 't':
                    value += '\t';
                    break;
                default:
                    value += escaped;
                    break;
            }
            index += 1;
            continue;
        }
        value += char;
        index += 1;
    }
    throw new EvalError('unterminated string literal', pos);
}
function parseExpr(tokens, index) {
    const token = tokens[index];
    if (token === undefined) {
        throw new EvalError('unexpected end of input');
    }
    if (token.kind === 'quote') {
        const parsed = parseExpr(tokens, index + 1);
        return {
            expr: {
                kind: 'list',
                pos: token.pos,
                elements: [
                    { kind: 'symbol', name: 'quote', pos: token.pos },
                    parsed.expr,
                ],
            },
            nextIndex: parsed.nextIndex,
        };
    }
    if (token.kind === 'paren') {
        if (token.value === ')') {
            throw new EvalError('unexpected )', token.pos);
        }
        const elements = [];
        let nextIndex = index + 1;
        while (nextIndex < tokens.length) {
            const nextToken = tokens[nextIndex];
            if (nextToken.kind === 'paren' && nextToken.value === ')') {
                return {
                    expr: { kind: 'list', pos: token.pos, elements },
                    nextIndex: nextIndex + 1,
                };
            }
            const parsed = parseExpr(tokens, nextIndex);
            elements.push(parsed.expr);
            nextIndex = parsed.nextIndex;
        }
        throw new EvalError('unterminated list', token.pos);
    }
    if (token.kind === 'string') {
        return {
            expr: { kind: 'string', value: token.value, pos: token.pos },
            nextIndex: index + 1,
        };
    }
    return {
        expr: parseAtom(token),
        nextIndex: index + 1,
    };
}
function parseAtom(token) {
    if (token.value === '#t') {
        return { kind: 'boolean', value: true, pos: token.pos };
    }
    if (token.value === '#f') {
        return { kind: 'boolean', value: false, pos: token.pos };
    }
    if (token.value.startsWith('#\\')) {
        return { kind: 'char', value: parseCharLiteral(token), pos: token.pos };
    }
    if (/^[+-]?\d+$/.test(token.value)) {
        return { kind: 'number', value: Number.parseInt(token.value, 10), pos: token.pos };
    }
    return { kind: 'symbol', name: token.value, pos: token.pos };
}
function parseCharLiteral(token) {
    const literal = token.value.slice(2);
    switch (literal) {
        case 'space':
            return ' ';
        case 'newline':
            return '\n';
    }
    const chars = stringChars(literal);
    if (chars.length === 1) {
        return chars[0];
    }
    throw new EvalError('invalid character literal', token.pos);
}
function evaluateExpr(expr, env, context) {
    try {
        switch (expr.kind) {
            case 'number':
            case 'boolean':
                return expr;
            case 'string':
                return makeString(expr.value);
            case 'char':
                return makeChar(expr.value);
            case 'symbol':
                return env.lookup(expr.name);
            case 'list':
                return evaluateList(expr.elements, env, context);
        }
    }
    catch (error) {
        throw attachPosition(error, expr.pos);
    }
}
function evaluateList(elements, env, context) {
    if (elements.length === 0) {
        throw new EvalError('cannot evaluate empty list');
    }
    const [head, ...argExprs] = elements;
    if (head.kind === 'symbol') {
        switch (head.name) {
            case 'define-syntax':
                return evaluateDefineSyntax(argExprs, env);
            case 'define':
                return evaluateDefine(argExprs, env, context);
            case 'set!':
                return evaluateSet(argExprs, env, context);
            case 'if':
                return evaluateIf(argExprs, env, context);
            case 'quote':
                return evaluateQuote(argExprs);
            case 'lambda':
                return evaluateLambda(argExprs, env);
            case 'and':
                return evaluateAnd(argExprs, env, context);
            case 'or':
                return evaluateOr(argExprs, env, context);
            case 'begin':
                return evaluateBegin(argExprs, env, context);
            case 'let':
                return evaluateLet(argExprs, env, context);
            case 'cond':
                return evaluateCond(argExprs, env, context);
        }
        const macroRules = env.lookupMacro(head.name);
        if (macroRules !== undefined) {
            const expanded = expandMacroInvocation(elements, macroRules, env);
            return evaluateExpr(expanded.expr, expanded.env, context);
        }
    }
    const procedure = evaluateExpr(head, env, context);
    const args = argExprs.map((expr) => evaluateExpr(expr, env, context));
    return applyProcedure(procedure, args, context);
}
function evaluateDefine(argExprs, env, context) {
    if (argExprs.length < 2) {
        throw new EvalError('define expects a target and a value');
    }
    const [target, ...body] = argExprs;
    if (target.kind === 'symbol') {
        if (body.length !== 1) {
            throw new EvalError('define variable form expects exactly 1 value expression');
        }
        const value = evaluateExpr(body[0], env, context);
        env.define(target.name, value);
        return VOID_VALUE;
    }
    if (target.kind === 'list' && target.elements.length > 0) {
        const [nameExpr, ...paramExprs] = target.elements;
        if (nameExpr.kind !== 'symbol') {
            throw new EvalError('define function form expects a function name');
        }
        const params = readParameterList(paramExprs);
        const procedure = {
            kind: 'closure',
            params: params.fixedParams,
            restParam: params.restParam,
            body,
            env,
        };
        env.define(nameExpr.name, procedure);
        return VOID_VALUE;
    }
    throw new EvalError('invalid define form');
}
function evaluateSet(argExprs, env, context) {
    if (argExprs.length !== 2) {
        throw new EvalError('set! expects exactly 2 arguments');
    }
    const [target, valueExpr] = argExprs;
    if (target.kind !== 'symbol') {
        throw new EvalError('set! expects a symbol target');
    }
    const value = evaluateExpr(valueExpr, env, context);
    env.assign(target.name, value);
    return VOID_VALUE;
}
function evaluateIf(argExprs, env, context) {
    if (argExprs.length !== 3) {
        throw new EvalError('if expects exactly 3 arguments');
    }
    const condition = evaluateExpr(argExprs[0], env, context);
    return isTruthy(condition)
        ? evaluateExpr(argExprs[1], env, context)
        : evaluateExpr(argExprs[2], env, context);
}
function evaluateQuote(argExprs) {
    if (argExprs.length !== 1) {
        throw new EvalError('quote expects exactly 1 argument');
    }
    return quoteExpr(argExprs[0]);
}
function quoteExpr(expr) {
    switch (expr.kind) {
        case 'number':
        case 'boolean':
            return expr;
        case 'string':
            return makeString(expr.value);
        case 'char':
            return makeChar(expr.value);
        case 'symbol':
            return { kind: 'symbol', name: expr.name };
        case 'list':
            return buildList(expr.elements.map((element) => quoteExpr(element)));
    }
}
function evaluateLambda(argExprs, env) {
    if (argExprs.length < 2) {
        throw new EvalError('lambda expects parameters and a body');
    }
    const [paramsExpr, ...body] = argExprs;
    const params = paramsExpr.kind === 'symbol'
        ? { fixedParams: [], restParam: paramsExpr.name }
        : paramsExpr.kind === 'list'
            ? readParameterList(paramsExpr.elements)
            : undefined;
    if (params === undefined) {
        throw new EvalError('lambda parameters must be a list or symbol');
    }
    return {
        kind: 'closure',
        params: params.fixedParams,
        restParam: params.restParam,
        body,
        env,
    };
}
function readParameterList(exprs) {
    const fixedParams = [];
    for (let index = 0; index < exprs.length; index += 1) {
        const expr = exprs[index];
        if (expr.kind !== 'symbol') {
            throw new EvalError('parameter list must contain only symbols');
        }
        if (expr.name === '.') {
            const restExpr = exprs[index + 1];
            if (restExpr === undefined || restExpr.kind !== 'symbol' || index + 2 !== exprs.length) {
                throw new EvalError('invalid dotted parameter list');
            }
            return {
                fixedParams,
                restParam: restExpr.name,
            };
        }
        fixedParams.push(expr.name);
    }
    return { fixedParams };
}
function evaluateDefineSyntax(argExprs, env) {
    if (argExprs.length !== 2) {
        throw new EvalError('define-syntax expects exactly 2 arguments');
    }
    const [nameExpr, transformerExpr] = argExprs;
    if (nameExpr.kind !== 'symbol') {
        throw new EvalError('define-syntax expects a symbol name');
    }
    env.defineMacro(nameExpr.name, readSyntaxRules(nameExpr.name, transformerExpr, env));
    return VOID_VALUE;
}
function readSyntaxRules(name, transformerExpr, env) {
    if (transformerExpr.kind !== 'list') {
        throw new EvalError('define-syntax expects a syntax-rules transformer');
    }
    const items = transformerExpr.elements;
    if (items.length < 3) {
        throw new EvalError('define-syntax expects a syntax-rules transformer');
    }
    if (items[0].kind !== 'symbol' || items[0].name !== 'syntax-rules') {
        throw new EvalError('define-syntax expects a syntax-rules transformer');
    }
    const literalExprs = items[1];
    if (literalExprs.kind !== 'list') {
        throw new EvalError('syntax-rules literals must be a list');
    }
    const literals = new Set();
    for (const literalExpr of literalExprs.elements) {
        if (literalExpr.kind !== 'symbol') {
            throw new EvalError('syntax-rules literals must be identifiers');
        }
        literals.add(literalExpr.name);
    }
    const rules = [];
    for (const ruleExpr of items.slice(2)) {
        if (ruleExpr.kind !== 'list' || ruleExpr.elements.length !== 2) {
            throw new EvalError('syntax-rules rules must be (pattern template) pairs');
        }
        rules.push({
            pattern: ruleExpr.elements[0],
            template: ruleExpr.elements[1],
        });
    }
    return {
        name,
        literals,
        rules,
        env,
    };
}
function expandMacroInvocation(elements, macroRules, callEnv) {
    const invocation = {
        kind: 'list',
        pos: elements[0]?.pos ?? DEFAULT_SOURCE_POS,
        elements,
    };
    for (const rule of macroRules.rules) {
        const bindings = matchSyntaxRule(rule.pattern, invocation, macroRules.literals);
        if (bindings === undefined) {
            continue;
        }
        const expansionEnv = new Environment(callEnv);
        const aliases = new Map();
        const expr = instantiateTemplate(rule.template, bindings, macroRules, expansionEnv, aliases, new Map(), undefined);
        return { expr, env: expansionEnv };
    }
    throw new EvalError(`no matching syntax-rules clause for ${macroRules.name}`);
}
function matchSyntaxRule(pattern, invocation, literals) {
    if (pattern.kind !== 'list' || pattern.elements.length === 0) {
        throw new EvalError('syntax-rules patterns must be non-empty lists');
    }
    if (invocation.kind !== 'list') {
        return undefined;
    }
    return matchPatternList(pattern.elements, invocation.elements, literals, new Map(), true);
}
function matchPatternList(patternElements, exprElements, literals, bindings, isTopLevel) {
    const parts = splitEllipsisParts(patternElements);
    return matchPatternParts(parts, exprElements, literals, bindings, isTopLevel, 0, 0);
}
function matchPatternParts(parts, exprElements, literals, bindings, isTopLevel, partIndex, exprIndex) {
    if (partIndex === parts.length) {
        return exprIndex === exprElements.length ? bindings : undefined;
    }
    const part = parts[partIndex];
    if (!part.repeated) {
        const expr = exprElements[exprIndex];
        if (expr === undefined) {
            return undefined;
        }
        const nextBindings = clonePatternBindings(bindings);
        if (!matchPatternExpr(part.expr, expr, literals, nextBindings, isTopLevel && partIndex === 0)) {
            return undefined;
        }
        return matchPatternParts(parts, exprElements, literals, nextBindings, false, partIndex + 1, exprIndex + 1);
    }
    const minRemaining = countRequiredPatternParts(parts, partIndex + 1);
    const maxRepeats = exprElements.length - exprIndex - minRemaining;
    if (maxRepeats < 0) {
        return undefined;
    }
    for (let repeatCount = 0; repeatCount <= maxRepeats; repeatCount += 1) {
        const nextBindings = clonePatternBindings(bindings);
        let matched = true;
        for (let offset = 0; offset < repeatCount; offset += 1) {
            const localBindings = new Map();
            if (!matchPatternExpr(part.expr, exprElements[exprIndex + offset], literals, localBindings, false)) {
                matched = false;
                break;
            }
            if (!mergeRepeatedPatternBindings(nextBindings, localBindings)) {
                matched = false;
                break;
            }
        }
        if (!matched) {
            continue;
        }
        if (!ensureRepeatedPatternBindings(part.expr, literals, nextBindings)) {
            continue;
        }
        const result = matchPatternParts(parts, exprElements, literals, nextBindings, false, partIndex + 1, exprIndex + repeatCount);
        if (result !== undefined) {
            return result;
        }
    }
    return undefined;
}
function matchPatternExpr(pattern, expr, literals, bindings, ignoreKeyword) {
    switch (pattern.kind) {
        case 'number':
            return expr.kind === 'number' && expr.value === pattern.value;
        case 'boolean':
            return expr.kind === 'boolean' && expr.value === pattern.value;
        case 'string':
            return expr.kind === 'string' && expr.value === pattern.value;
        case 'char':
            return expr.kind === 'char' && expr.value === pattern.value;
        case 'symbol':
            if (pattern.name === '...') {
                throw new EvalError('invalid use of ellipsis');
            }
            if (ignoreKeyword) {
                return expr.kind === 'symbol';
            }
            if (literals.has(pattern.name)) {
                return expr.kind === 'symbol' && expr.name === pattern.name;
            }
            return bindPatternVariable(pattern.name, expr, bindings);
        case 'list': {
            if (expr.kind !== 'list') {
                return false;
            }
            const result = matchPatternList(pattern.elements, expr.elements, literals, clonePatternBindings(bindings), false);
            if (result === undefined) {
                return false;
            }
            replacePatternBindings(bindings, result);
            return true;
        }
    }
}
function bindPatternVariable(name, expr, bindings) {
    const binding = bindings.get(name);
    if (binding === undefined) {
        bindings.set(name, { kind: 'single', expr });
        return true;
    }
    if (binding.kind === 'single') {
        return equalExprSyntax(binding.expr, expr);
    }
    return false;
}
function mergeRepeatedPatternBindings(target, source) {
    for (const [name, binding] of source) {
        if (binding.kind !== 'single') {
            throw new EvalError('nested ellipsis patterns are not supported');
        }
        const existing = target.get(name);
        if (existing?.kind === 'repeated') {
            existing.exprs.push(binding.expr);
            continue;
        }
        if (existing?.kind === 'single') {
            return false;
        }
        target.set(name, { kind: 'repeated', exprs: [binding.expr] });
    }
    return true;
}
function ensureRepeatedPatternBindings(pattern, literals, bindings) {
    const names = new Set();
    collectPatternVariables(pattern, literals, names, false);
    for (const name of names) {
        const binding = bindings.get(name);
        if (binding?.kind === 'single') {
            return false;
        }
        if (binding === undefined) {
            bindings.set(name, { kind: 'repeated', exprs: [] });
        }
    }
    return true;
}
function collectPatternVariables(pattern, literals, names, ignoreKeyword) {
    switch (pattern.kind) {
        case 'number':
        case 'boolean':
        case 'string':
        case 'char':
            return;
        case 'symbol':
            if (pattern.name !== '...' && !ignoreKeyword && !literals.has(pattern.name)) {
                names.add(pattern.name);
            }
            return;
        case 'list':
            for (const part of splitEllipsisParts(pattern.elements)) {
                collectPatternVariables(part.expr, literals, names, false);
            }
    }
}
function countRequiredPatternParts(parts, startIndex) {
    return parts.slice(startIndex).filter((part) => !part.repeated).length;
}
function splitEllipsisParts(elements) {
    const parts = [];
    let index = 0;
    while (index < elements.length) {
        const expr = elements[index];
        if (isEllipsisExpr(expr)) {
            throw new EvalError('invalid use of ellipsis');
        }
        const repeated = isEllipsisExpr(elements[index + 1]);
        parts.push({ expr, repeated });
        index += repeated ? 2 : 1;
    }
    return parts;
}
function isEllipsisExpr(expr) {
    return expr?.kind === 'symbol' && expr.name === '...';
}
function instantiateTemplate(template, bindings, macroRules, expansionEnv, aliases, localScope, repeatIndex) {
    switch (template.kind) {
        case 'number':
        case 'boolean':
        case 'string':
        case 'char':
            return template;
        case 'symbol':
            return instantiateTemplateSymbol(template, bindings, macroRules, expansionEnv, aliases, localScope, repeatIndex);
        case 'list':
            return instantiateTemplateList(template, bindings, macroRules, expansionEnv, aliases, localScope, repeatIndex);
    }
}
function instantiateTemplateSymbol(template, bindings, macroRules, expansionEnv, aliases, localScope, repeatIndex) {
    if (template.name === '...') {
        return template;
    }
    const scopedName = localScope.get(template.name);
    if (scopedName !== undefined) {
        return { kind: 'symbol', name: scopedName, pos: template.pos };
    }
    const binding = bindings.get(template.name);
    if (binding !== undefined) {
        if (binding.kind === 'single') {
            return binding.expr;
        }
        if (repeatIndex === undefined) {
            throw new EvalError('ellipsis-bound pattern variable used outside ellipsis');
        }
        const expr = binding.exprs[repeatIndex];
        if (expr === undefined) {
            throw new EvalError('ellipsis repetition mismatch');
        }
        return expr;
    }
    if (isSpecialFormName(template.name)) {
        return template;
    }
    return {
        kind: 'symbol',
        name: resolveMacroIdentifier(template.name, macroRules, expansionEnv, aliases),
        pos: template.pos,
    };
}
function instantiateTemplateList(template, bindings, macroRules, expansionEnv, aliases, localScope, repeatIndex) {
    if (isQuoteForm(template.elements)) {
        return template;
    }
    const head = template.elements[0];
    if (head?.kind === 'symbol') {
        switch (head.name) {
            case 'lambda':
                if (template.elements.length >= 2) {
                    return instantiateLambdaTemplate(template, bindings, macroRules, expansionEnv, aliases, localScope, repeatIndex);
                }
                break;
            case 'let':
                if (template.elements.length >= 3) {
                    return instantiateLetTemplate(template, bindings, macroRules, expansionEnv, aliases, localScope, repeatIndex);
                }
                break;
        }
    }
    return instantiateGenericTemplateList(template, bindings, macroRules, expansionEnv, aliases, localScope, repeatIndex);
}
function instantiateGenericTemplateList(template, bindings, macroRules, expansionEnv, aliases, localScope, repeatIndex) {
    const elements = [];
    for (const part of splitEllipsisParts(template.elements)) {
        if (!part.repeated) {
            elements.push(instantiateTemplate(part.expr, bindings, macroRules, expansionEnv, aliases, localScope, repeatIndex));
            continue;
        }
        const repeats = getTemplateRepeatCount(part.expr, bindings);
        for (let index = 0; index < repeats; index += 1) {
            elements.push(instantiateTemplate(part.expr, bindings, macroRules, expansionEnv, aliases, localScope, index));
        }
    }
    return {
        kind: 'list',
        pos: template.pos,
        elements,
    };
}
function instantiateLambdaTemplate(template, bindings, macroRules, expansionEnv, aliases, localScope, repeatIndex) {
    const bodyScope = new Map(localScope);
    const elements = [template.elements[0]];
    elements.push(instantiateBindingSpec(template.elements[1], bindings, macroRules, expansionEnv, aliases, localScope, bodyScope, repeatIndex));
    for (const expr of template.elements.slice(2)) {
        elements.push(instantiateTemplate(expr, bindings, macroRules, expansionEnv, aliases, bodyScope, repeatIndex));
    }
    return {
        kind: 'list',
        pos: template.pos,
        elements,
    };
}
function instantiateLetTemplate(template, bindings, macroRules, expansionEnv, aliases, localScope, repeatIndex) {
    const elements = [template.elements[0]];
    const bodyScope = new Map(localScope);
    let bindingsIndex = 1;
    let bodyStartIndex = 2;
    if (template.elements[1]?.kind === 'symbol' && template.elements[2] !== undefined) {
        elements.push(instantiateBindingName(template.elements[1], bindings, macroRules, expansionEnv, aliases, localScope, bodyScope, repeatIndex));
        bindingsIndex = 2;
        bodyStartIndex = 3;
    }
    const bindingExpr = template.elements[bindingsIndex];
    if (bindingExpr?.kind !== 'list') {
        return instantiateGenericTemplateList(template, bindings, macroRules, expansionEnv, aliases, localScope, repeatIndex);
    }
    const instantiatedBindings = [];
    for (const entry of bindingExpr.elements) {
        if (entry.kind === 'list' && entry.elements.length === 2) {
            instantiatedBindings.push({
                kind: 'list',
                pos: entry.pos,
                elements: [
                    instantiateBindingName(entry.elements[0], bindings, macroRules, expansionEnv, aliases, localScope, bodyScope, repeatIndex),
                    instantiateTemplate(entry.elements[1], bindings, macroRules, expansionEnv, aliases, localScope, repeatIndex),
                ],
            });
            continue;
        }
        instantiatedBindings.push(instantiateTemplate(entry, bindings, macroRules, expansionEnv, aliases, localScope, repeatIndex));
    }
    elements.push({
        kind: 'list',
        pos: bindingExpr.pos,
        elements: instantiatedBindings,
    });
    for (const expr of template.elements.slice(bodyStartIndex)) {
        elements.push(instantiateTemplate(expr, bindings, macroRules, expansionEnv, aliases, bodyScope, repeatIndex));
    }
    return {
        kind: 'list',
        pos: template.pos,
        elements,
    };
}
function instantiateBindingSpec(spec, bindings, macroRules, expansionEnv, aliases, localScope, bodyScope, repeatIndex) {
    if (spec.kind === 'symbol') {
        return instantiateBindingName(spec, bindings, macroRules, expansionEnv, aliases, localScope, bodyScope, repeatIndex);
    }
    if (spec.kind === 'list') {
        return {
            kind: 'list',
            pos: spec.pos,
            elements: spec.elements.map((item) => item.kind === 'symbol' && item.name === '.'
                ? item
                : instantiateBindingName(item, bindings, macroRules, expansionEnv, aliases, localScope, bodyScope, repeatIndex)),
        };
    }
    return instantiateTemplate(spec, bindings, macroRules, expansionEnv, aliases, localScope, repeatIndex);
}
function instantiateBindingName(expr, bindings, macroRules, expansionEnv, aliases, localScope, bodyScope, repeatIndex) {
    if (expr.kind !== 'symbol') {
        return instantiateTemplate(expr, bindings, macroRules, expansionEnv, aliases, localScope, repeatIndex);
    }
    if (expr.name === '.') {
        return expr;
    }
    if (bindings.has(expr.name)) {
        return instantiateTemplate(expr, bindings, macroRules, expansionEnv, aliases, localScope, repeatIndex);
    }
    const alias = freshMacroIdentifier(expr.name);
    bodyScope.set(expr.name, alias);
    return {
        kind: 'symbol',
        name: alias,
        pos: expr.pos,
    };
}
function getTemplateRepeatCount(template, bindings) {
    const names = new Set();
    collectRepeatedPatternVariables(template, bindings, names);
    if (names.size === 0) {
        throw new EvalError('template ellipsis has no repeated pattern variables');
    }
    let repeatCount;
    for (const name of names) {
        const binding = bindings.get(name);
        if (binding?.kind !== 'repeated') {
            continue;
        }
        if (repeatCount === undefined) {
            repeatCount = binding.exprs.length;
            continue;
        }
        if (repeatCount !== binding.exprs.length) {
            throw new EvalError('ellipsis-bound pattern variables must repeat the same number of times');
        }
    }
    return repeatCount ?? 0;
}
function collectRepeatedPatternVariables(expr, bindings, names) {
    switch (expr.kind) {
        case 'number':
        case 'boolean':
        case 'string':
        case 'char':
            return;
        case 'symbol':
            if (bindings.get(expr.name)?.kind === 'repeated') {
                names.add(expr.name);
            }
            return;
        case 'list':
            if (isQuoteForm(expr.elements)) {
                return;
            }
            for (const part of splitEllipsisParts(expr.elements)) {
                collectRepeatedPatternVariables(part.expr, bindings, names);
            }
    }
}
function resolveMacroIdentifier(name, macroRules, expansionEnv, aliases) {
    const existing = aliases.get(name);
    if (existing !== undefined) {
        return existing;
    }
    const alias = freshMacroIdentifier(name);
    aliases.set(name, alias);
    const capturedMacro = macroRules.env.lookupMacro(name);
    if (capturedMacro !== undefined) {
        expansionEnv.defineMacro(alias, capturedMacro);
    }
    else {
        const value = macroRules.env.lookupOptional(name);
        if (value !== undefined) {
            expansionEnv.define(alias, value);
        }
    }
    return alias;
}
function replacePatternBindings(target, source) {
    target.clear();
    for (const [name, binding] of source) {
        target.set(name, clonePatternBinding(binding));
    }
}
function clonePatternBindings(bindings) {
    const cloned = new Map();
    for (const [name, binding] of bindings) {
        cloned.set(name, clonePatternBinding(binding));
    }
    return cloned;
}
function clonePatternBinding(binding) {
    if (binding.kind === 'single') {
        return binding;
    }
    return {
        kind: 'repeated',
        exprs: [...binding.exprs],
    };
}
function equalExprSyntax(left, right) {
    if (left.kind !== right.kind) {
        return false;
    }
    switch (left.kind) {
        case 'number':
            return left.value === right.value;
        case 'boolean':
            return left.value === right.value;
        case 'string':
            return left.value === right.value;
        case 'char':
            return left.value === right.value;
        case 'symbol':
            return left.name === right.name;
        case 'list': {
            const rightList = right;
            return (left.elements.length === rightList.elements.length &&
                left.elements.every((expr, index) => equalExprSyntax(expr, rightList.elements[index])));
        }
    }
}
function isQuoteForm(elements) {
    return elements.length === 2 && elements[0].kind === 'symbol' && elements[0].name === 'quote';
}
function isSpecialFormName(name) {
    switch (name) {
        case 'define':
        case 'define-syntax':
        case 'set!':
        case 'if':
        case 'quote':
        case 'lambda':
        case 'and':
        case 'or':
        case 'let':
        case 'begin':
        case 'cond':
        case 'syntax-rules':
        case 'else':
        case '.':
            return true;
        default:
            return false;
    }
}
function freshMacroIdentifier(name) {
    const counter = macroIdentifierCounter;
    macroIdentifierCounter += 1;
    const sanitized = Array.from(name)
        .map((char) => (/^[A-Za-z0-9_]$/.test(char) ? char : '_'))
        .join('');
    return `__macro_${counter}_${sanitized || 'id'}`;
}
function evaluateAnd(argExprs, env, context) {
    let lastValue = makeBoolean(true);
    for (const expr of argExprs) {
        lastValue = evaluateExpr(expr, env, context);
        if (!isTruthy(lastValue)) {
            return lastValue;
        }
    }
    return lastValue;
}
function evaluateOr(argExprs, env, context) {
    for (const expr of argExprs) {
        const value = evaluateExpr(expr, env, context);
        if (isTruthy(value)) {
            return value;
        }
    }
    return makeBoolean(false);
}
function evaluateBegin(argExprs, env, context) {
    return evaluateSequence(argExprs, env, context);
}
function evaluateLet(argExprs, env, context) {
    if (argExprs.length < 2) {
        throw new EvalError('let expects bindings and a body');
    }
    let name;
    let bindingsExpr;
    let body;
    if (argExprs[0].kind === 'symbol') {
        if (argExprs.length < 3) {
            throw new EvalError('named let expects a name, bindings, and a body');
        }
        name = argExprs[0].name;
        bindingsExpr = argExprs[1];
        body = argExprs.slice(2);
    }
    else {
        bindingsExpr = argExprs[0];
        body = argExprs.slice(1);
    }
    const bindings = readLetBindings(bindingsExpr);
    const values = bindings.initExprs.map((expr) => evaluateExpr(expr, env, context));
    if (name === undefined) {
        const letEnv = new Environment(env);
        for (let index = 0; index < bindings.names.length; index += 1) {
            letEnv.define(bindings.names[index], values[index]);
        }
        return evaluateSequence(body, letEnv, context);
    }
    const letEnv = new Environment(env);
    const procedure = {
        kind: 'closure',
        params: bindings.names,
        body,
        env: letEnv,
    };
    letEnv.define(name, procedure);
    return applyClosure(procedure, values, context);
}
function readLetBindings(bindingsExpr) {
    if (bindingsExpr.kind !== 'list') {
        throw new EvalError('let bindings must be a list');
    }
    const names = [];
    const initExprs = [];
    for (const bindingExpr of bindingsExpr.elements) {
        if (bindingExpr.kind !== 'list' || bindingExpr.elements.length !== 2) {
            throw new EvalError('let bindings must contain (name value) pairs');
        }
        const [nameExpr, initExpr] = bindingExpr.elements;
        if (nameExpr.kind !== 'symbol') {
            throw new EvalError('let binding name must be a symbol');
        }
        names.push(nameExpr.name);
        initExprs.push(initExpr);
    }
    return { names, initExprs };
}
function evaluateCond(argExprs, env, context) {
    for (let index = 0; index < argExprs.length; index += 1) {
        const clauseExpr = argExprs[index];
        if (clauseExpr.kind !== 'list' || clauseExpr.elements.length === 0) {
            throw new EvalError('cond clauses must be non-empty lists');
        }
        const [testExpr, ...body] = clauseExpr.elements;
        const isElseClause = testExpr.kind === 'symbol' && testExpr.name === 'else';
        if (isElseClause) {
            if (index !== argExprs.length - 1) {
                throw new EvalError('cond else clause must be last');
            }
            if (body.length === 0) {
                throw new EvalError('cond else clause expects at least 1 expression');
            }
            return evaluateSequence(body, env, context);
        }
        const testValue = evaluateExpr(testExpr, env, context);
        if (!isTruthy(testValue)) {
            continue;
        }
        if (body.length === 0) {
            return testValue;
        }
        return evaluateSequence(body, env, context);
    }
    return VOID_VALUE;
}
function applyProcedure(procedure, args, context) {
    switch (procedure.kind) {
        case 'builtin':
            return applyBuiltin(procedure.name, args, context);
        case 'closure':
            return applyClosure(procedure, args, context);
        default:
            throw new EvalError('attempted to call a non-procedure');
    }
}
function applyClosure(procedure, args, context) {
    if (procedure.restParam === undefined && args.length !== procedure.params.length) {
        throw new EvalError(`expected ${procedure.params.length} arguments, got ${args.length}`);
    }
    if (procedure.restParam !== undefined && args.length < procedure.params.length) {
        throw new EvalError(`expected at least ${procedure.params.length} arguments, got ${args.length}`);
    }
    const callEnv = new Environment(procedure.env);
    for (let index = 0; index < procedure.params.length; index += 1) {
        callEnv.define(procedure.params[index], args[index]);
    }
    if (procedure.restParam !== undefined) {
        callEnv.define(procedure.restParam, buildList(args.slice(procedure.params.length)));
    }
    return evaluateSequence(procedure.body, callEnv, context);
}
function evaluateSequence(exprs, env, context) {
    let result = VOID_VALUE;
    for (const expr of exprs) {
        result = evaluateExpr(expr, env, context);
    }
    return result;
}
function applyBuiltin(name, args, context) {
    switch (name) {
        case '+':
            return makeNumber(args.map((arg) => expectNumber(arg, '+')).reduce((sum, value) => sum + value, 0));
        case '-':
            return applySubtraction(args);
        case '*':
            return makeNumber(args.map((arg) => expectNumber(arg, '*')).reduce((product, value) => product * value, 1));
        case '/':
            return applyDivision(args);
        case 'abs':
            return applyAbs(args);
        case 'modulo':
            return applyIntegerDivision(args, 'modulo', 'modulo');
        case 'remainder':
            return applyIntegerDivision(args, 'remainder', 'remainder');
        case 'quotient':
            return applyIntegerDivision(args, 'quotient', 'quotient');
        case 'min':
            return applyMinMax(args, 'min', Math.min);
        case 'max':
            return applyMinMax(args, 'max', Math.max);
        case 'expt':
            return applyExpt(args);
        case '<':
            return applyComparison(args, '<', (left, right) => left < right);
        case '>':
            return applyComparison(args, '>', (left, right) => left > right);
        case '=':
            return applyComparison(args, '=', (left, right) => left === right);
        case '<=':
            return applyComparison(args, '<=', (left, right) => left <= right);
        case 'zero?':
            return applyNumericPredicate(args, 'zero?', (value) => value === 0);
        case 'positive?':
            return applyNumericPredicate(args, 'positive?', (value) => value > 0);
        case 'negative?':
            return applyNumericPredicate(args, 'negative?', (value) => value < 0);
        case 'odd?':
            return applyIntegerPredicate(args, 'odd?', (value) => Math.abs(value % 2) === 1);
        case 'even?':
            return applyIntegerPredicate(args, 'even?', (value) => value % 2 === 0);
        case 'not':
            if (args.length !== 1) {
                throw new EvalError('not expects exactly 1 argument');
            }
            return makeBoolean(!isTruthy(args[0]));
        case 'cons':
            if (args.length !== 2) {
                throw new EvalError('cons expects exactly 2 arguments');
            }
            return { kind: 'pair', car: args[0], cdr: args[1] };
        case 'car':
            if (args.length !== 1) {
                throw new EvalError('car expects exactly 1 argument');
            }
            return expectPair(args[0], 'car').car;
        case 'cdr':
            if (args.length !== 1) {
                throw new EvalError('cdr expects exactly 1 argument');
            }
            return expectPair(args[0], 'cdr').cdr;
        case 'null?':
            if (args.length !== 1) {
                throw new EvalError('null? expects exactly 1 argument');
            }
            return makeBoolean(args[0].kind === 'nil');
        case 'list':
            return buildList(args);
        case 'append':
            return applyAppend(args);
        case 'length':
            if (args.length !== 1) {
                throw new EvalError('length expects exactly 1 argument');
            }
            return makeNumber(listLength(args[0]));
        case 'list-ref':
            return applyListRef(args);
        case 'list-tail':
            return applyListTail(args);
        case 'list?':
            return applyTypePredicate(args, 'list?', (value) => isProperList(value));
        case 'assoc':
            return applyAssoc(args);
        case 'map':
            return applyMap(args, context);
        case 'string?':
            return applyTypePredicate(args, 'string?', (value) => value.kind === 'string');
        case 'number?':
            return applyTypePredicate(args, 'number?', (value) => value.kind === 'number');
        case 'boolean?':
            return applyTypePredicate(args, 'boolean?', (value) => value.kind === 'boolean');
        case 'pair?':
            return applyTypePredicate(args, 'pair?', (value) => value.kind === 'pair');
        case 'symbol?':
            return applyTypePredicate(args, 'symbol?', (value) => value.kind === 'symbol');
        case 'eq?':
            if (args.length !== 2) {
                throw new EvalError('eq? expects exactly 2 arguments');
            }
            return makeBoolean(eqValues(args[0], args[1]));
        case 'equal?':
            if (args.length !== 2) {
                throw new EvalError('equal? expects exactly 2 arguments');
            }
            return makeBoolean(equalValues(args[0], args[1]));
        case 'display':
            if (args.length !== 1) {
                throw new EvalError('display expects exactly 1 argument');
            }
            context.output.push(formatDisplayValue(args[0]));
            return VOID_VALUE;
        case 'write':
            if (args.length !== 1) {
                throw new EvalError('write expects exactly 1 argument');
            }
            context.output.push(formatValue(args[0]));
            return VOID_VALUE;
        case 'newline':
            if (args.length !== 0) {
                throw new EvalError('newline expects exactly 0 arguments');
            }
            context.output.push('\n');
            return VOID_VALUE;
        case 'string-append':
            return applyStringAppend(args);
        case 'string-length':
            if (args.length !== 1) {
                throw new EvalError('string-length expects exactly 1 argument');
            }
            return makeNumber(stringChars(expectStringValue(args[0], 'string-length').value).length);
        case 'substring':
            return applySubstring(args);
        case 'string->number':
            if (args.length !== 1) {
                throw new EvalError('string->number expects exactly 1 argument');
            }
            return parseNumberString(expectStringValue(args[0], 'string->number').value);
        case 'number->string':
            if (args.length !== 1) {
                throw new EvalError('number->string expects exactly 1 argument');
            }
            return makeString(formatNumber(expectNumber(args[0], 'number->string')));
        case 'symbol->string':
            if (args.length !== 1) {
                throw new EvalError('symbol->string expects exactly 1 argument');
            }
            return makeString(expectSymbol(args[0], 'symbol->string').name);
        case 'string->symbol':
            if (args.length !== 1) {
                throw new EvalError('string->symbol expects exactly 1 argument');
            }
            return { kind: 'symbol', name: expectStringValue(args[0], 'string->symbol').value };
        case 'string-ref':
            return applyStringRef(args);
        case 'string-copy':
            if (args.length !== 1) {
                throw new EvalError('string-copy expects exactly 1 argument');
            }
            return makeString(expectStringValue(args[0], 'string-copy').value);
        case 'string-set!':
            return applyStringSet(args);
        case 'string=?':
            return applyStringComparison(args, 'string=?', (value) => value, (left, right) => left === right);
        case 'string<?':
            return applyStringComparison(args, 'string<?', (value) => value, (left, right) => left < right);
        case 'string-ci=?':
            return applyStringComparison(args, 'string-ci=?', (value) => value.toLocaleLowerCase(), (left, right) => left === right);
        case 'string-upcase':
            return applyStringCase(args, 'string-upcase', (value) => value.toLocaleUpperCase());
        case 'string-downcase':
            return applyStringCase(args, 'string-downcase', (value) => value.toLocaleLowerCase());
        case 'char?':
            return applyTypePredicate(args, 'char?', (value) => value.kind === 'char');
        case 'char-alphabetic?':
            return applyCharPredicate(args, 'char-alphabetic?', (value) => ALPHABETIC_CHAR_RE.test(value));
        case 'char-numeric?':
            return applyCharPredicate(args, 'char-numeric?', (value) => NUMERIC_CHAR_RE.test(value));
        case 'char-upcase':
            return applyCharCase(args, 'char-upcase', (value) => value.toLocaleUpperCase());
        case 'char-downcase':
            return applyCharCase(args, 'char-downcase', (value) => value.toLocaleLowerCase());
        case 'char=?':
            return applyCharComparison(args, 'char=?', (left, right) => left === right);
        case 'char<?':
            return applyCharComparison(args, 'char<?', (left, right) => left < right);
        case 'apply':
            return applyApply(args, context);
    }
}
function applyApply(args, context) {
    if (args.length < 2) {
        throw new EvalError('apply expects at least 2 arguments');
    }
    const procedure = args[0];
    const prefixArgs = args.slice(1, -1);
    const tailArgs = listToArray(args[args.length - 1], 'apply');
    return applyProcedure(procedure, [...prefixArgs, ...tailArgs], context);
}
function applyAbs(args) {
    if (args.length !== 1) {
        throw new EvalError('abs expects exactly 1 argument');
    }
    return makeNumber(Math.abs(expectNumber(args[0], 'abs')));
}
function applyIntegerDivision(args, name, operation) {
    if (args.length !== 2) {
        throw new EvalError(`${name} expects exactly 2 arguments`);
    }
    const dividend = expectInteger(args[0], name);
    const divisor = expectInteger(args[1], name);
    if (divisor === 0) {
        throw new EvalError('division by zero');
    }
    switch (operation) {
        case 'quotient':
            return makeNumber(Math.trunc(dividend / divisor));
        case 'remainder':
            return makeNumber(dividend % divisor);
        case 'modulo': {
            let result = dividend % divisor;
            if (result !== 0 && Math.sign(result) !== Math.sign(divisor)) {
                result += divisor;
            }
            return makeNumber(result);
        }
    }
}
function applyMinMax(args, name, operator) {
    if (args.length === 0) {
        throw new EvalError(`${name} expects at least 1 argument`);
    }
    return makeNumber(operator(...args.map((arg) => expectNumber(arg, name))));
}
function applyExpt(args) {
    if (args.length !== 2) {
        throw new EvalError('expt expects exactly 2 arguments');
    }
    const base = expectNumber(args[0], 'expt');
    const exponent = expectInteger(args[1], 'expt');
    return makeNumber(base ** exponent);
}
function applyNumericPredicate(args, name, predicate) {
    if (args.length !== 1) {
        throw new EvalError(`${name} expects exactly 1 argument`);
    }
    return makeBoolean(predicate(expectNumber(args[0], name)));
}
function applyIntegerPredicate(args, name, predicate) {
    if (args.length !== 1) {
        throw new EvalError(`${name} expects exactly 1 argument`);
    }
    return makeBoolean(predicate(expectInteger(args[0], name)));
}
function applySubtraction(args) {
    if (args.length === 0) {
        throw new EvalError('- expects at least 1 argument');
    }
    const numbers = args.map((arg) => expectNumber(arg, '-'));
    if (numbers.length === 1) {
        return makeNumber(-numbers[0]);
    }
    const [first, ...rest] = numbers;
    return makeNumber(rest.reduce((result, value) => result - value, first));
}
function applyDivision(args) {
    if (args.length === 0) {
        throw new EvalError('/ expects at least 1 argument');
    }
    const numbers = args.map((arg) => expectNumber(arg, '/'));
    if (numbers.length === 1) {
        if (numbers[0] === 0) {
            throw new EvalError('division by zero');
        }
        return makeNumber(1 / numbers[0]);
    }
    const [first, ...rest] = numbers;
    let result = first;
    for (const value of rest) {
        if (value === 0) {
            throw new EvalError('division by zero');
        }
        result /= value;
    }
    return makeNumber(result);
}
function applyComparison(args, name, predicate) {
    if (args.length < 2) {
        throw new EvalError(`${name} expects at least 2 arguments`);
    }
    const numbers = args.map((arg) => expectNumber(arg, name));
    for (let index = 0; index < numbers.length - 1; index += 1) {
        if (!predicate(numbers[index], numbers[index + 1])) {
            return makeBoolean(false);
        }
    }
    return makeBoolean(true);
}
function applyStringAppend(args) {
    return makeString(args.map((arg) => expectStringValue(arg, 'string-append').value).join(''));
}
function applyListRef(args) {
    if (args.length !== 2) {
        throw new EvalError('list-ref expects exactly 2 arguments');
    }
    const tail = getListTail(args[0], expectIndex(args[1], 'list-ref'), 'list-ref');
    if (tail.kind !== 'pair') {
        throw new EvalError('list-ref index out of range');
    }
    return tail.car;
}
function applyListTail(args) {
    if (args.length !== 2) {
        throw new EvalError('list-tail expects exactly 2 arguments');
    }
    return getListTail(args[0], expectIndex(args[1], 'list-tail'), 'list-tail');
}
function applyAssoc(args) {
    if (args.length !== 2) {
        throw new EvalError('assoc expects exactly 2 arguments');
    }
    const [key, alist] = args;
    let current = alist;
    while (current.kind === 'pair') {
        const entry = current.car;
        if (entry.kind !== 'pair') {
            throw new EvalError('assoc expects an association list');
        }
        if (equalValues(key, entry.car)) {
            return entry;
        }
        current = current.cdr;
    }
    if (current.kind !== 'nil') {
        throw new EvalError('assoc expects a proper list');
    }
    return makeBoolean(false);
}
function applyMap(args, context) {
    if (args.length < 2) {
        throw new EvalError('map expects at least 2 arguments');
    }
    const [procedure, ...listArgs] = args;
    const lists = listArgs.map((listArg) => listToArray(listArg, 'map'));
    const resultLength = lists[0].length;
    for (const list of lists) {
        if (list.length !== resultLength) {
            throw new EvalError('map expects lists of equal length');
        }
    }
    const results = [];
    for (let index = 0; index < resultLength; index += 1) {
        results.push(applyProcedure(procedure, lists.map((list) => list[index]), context));
    }
    return buildList(results);
}
function applySubstring(args) {
    if (args.length !== 3) {
        throw new EvalError('substring expects exactly 3 arguments');
    }
    const chars = stringChars(expectStringValue(args[0], 'substring').value);
    const start = expectIndex(args[1], 'substring');
    const end = expectIndex(args[2], 'substring');
    if (start > end || end > chars.length) {
        throw new EvalError('substring indices out of range');
    }
    return makeString(chars.slice(start, end).join(''));
}
function applyStringRef(args) {
    if (args.length !== 2) {
        throw new EvalError('string-ref expects exactly 2 arguments');
    }
    const chars = stringChars(expectStringValue(args[0], 'string-ref').value);
    const index = expectIndex(args[1], 'string-ref');
    if (index >= chars.length) {
        throw new EvalError('string-ref index out of range');
    }
    return makeChar(chars[index]);
}
function applyStringSet(args) {
    if (args.length !== 3) {
        throw new EvalError('string-set! expects exactly 3 arguments');
    }
    const target = expectStringValue(args[0], 'string-set!');
    const index = expectIndex(args[1], 'string-set!');
    const char = expectChar(args[2], 'string-set!');
    const chars = stringChars(target.value);
    if (!target.mutable) {
        throw new EvalError('string-set! expects a mutable string');
    }
    if (index >= chars.length) {
        throw new EvalError('string-set! index out of range');
    }
    chars[index] = char.value;
    target.value = chars.join('');
    return VOID_VALUE;
}
function applyStringComparison(args, name, normalize, predicate) {
    if (args.length < 2) {
        throw new EvalError(`${name} expects at least 2 arguments`);
    }
    const strings = args.map((arg) => normalize(expectStringValue(arg, name).value));
    for (let index = 0; index < strings.length - 1; index += 1) {
        if (!predicate(strings[index], strings[index + 1])) {
            return makeBoolean(false);
        }
    }
    return makeBoolean(true);
}
function applyStringCase(args, name, transform) {
    if (args.length !== 1) {
        throw new EvalError(`${name} expects exactly 1 argument`);
    }
    return makeString(transform(expectStringValue(args[0], name).value));
}
function applyCharPredicate(args, name, predicate) {
    if (args.length !== 1) {
        throw new EvalError(`${name} expects exactly 1 argument`);
    }
    return makeBoolean(predicate(expectChar(args[0], name).value));
}
function applyCharCase(args, name, transform) {
    if (args.length !== 1) {
        throw new EvalError(`${name} expects exactly 1 argument`);
    }
    return makeChar(transform(expectChar(args[0], name).value));
}
function applyCharComparison(args, name, predicate) {
    if (args.length < 2) {
        throw new EvalError(`${name} expects at least 2 arguments`);
    }
    const codePoints = args.map((arg) => charCodePoint(expectChar(arg, name).value));
    for (let index = 0; index < codePoints.length - 1; index += 1) {
        if (!predicate(codePoints[index], codePoints[index + 1])) {
            return makeBoolean(false);
        }
    }
    return makeBoolean(true);
}
function expectNumber(value, procedure) {
    if (value.kind !== 'number') {
        throw new EvalError(`${procedure} expects numeric arguments`);
    }
    return value.value;
}
function expectInteger(value, procedure) {
    const numericValue = expectNumber(value, procedure);
    if (!Number.isInteger(numericValue)) {
        throw new EvalError(`${procedure} expects integer arguments`);
    }
    return numericValue;
}
function expectStringValue(value, procedure) {
    if (value.kind !== 'string') {
        throw new EvalError(`${procedure} expects string arguments`);
    }
    return value;
}
function expectChar(value, procedure) {
    if (value.kind !== 'char') {
        throw new EvalError(`${procedure} expects a character`);
    }
    return value;
}
function expectSymbol(value, procedure) {
    if (value.kind !== 'symbol') {
        throw new EvalError(`${procedure} expects a symbol`);
    }
    return value;
}
function expectIndex(value, procedure) {
    const numericValue = expectNumber(value, procedure);
    if (!Number.isInteger(numericValue) || numericValue < 0) {
        throw new EvalError(`${procedure} expects a non-negative integer index`);
    }
    return numericValue;
}
function expectPair(value, procedure) {
    if (value.kind !== 'pair') {
        throw new EvalError(`${procedure} expects a pair`);
    }
    return value;
}
function buildList(elements) {
    let list = NIL_VALUE;
    for (let index = elements.length - 1; index >= 0; index -= 1) {
        list = {
            kind: 'pair',
            car: elements[index],
            cdr: list,
        };
    }
    return list;
}
function applyAppend(args) {
    if (args.length === 0) {
        return NIL_VALUE;
    }
    let result = args[args.length - 1];
    for (let index = args.length - 2; index >= 0; index -= 1) {
        const elements = listToArray(args[index], 'append');
        for (let elementIndex = elements.length - 1; elementIndex >= 0; elementIndex -= 1) {
            result = {
                kind: 'pair',
                car: elements[elementIndex],
                cdr: result,
            };
        }
    }
    return result;
}
function listLength(value) {
    return listToArray(value, 'length').length;
}
function getListTail(value, index, procedure) {
    let current = value;
    for (let remaining = index; remaining > 0; remaining -= 1) {
        if (current.kind === 'pair') {
            current = current.cdr;
            continue;
        }
        if (current.kind === 'nil') {
            throw new EvalError(`${procedure} index out of range`);
        }
        throw new EvalError(`${procedure} expects a proper list`);
    }
    if (current.kind !== 'pair' && current.kind !== 'nil') {
        throw new EvalError(`${procedure} expects a proper list`);
    }
    return current;
}
function listToArray(value, procedure) {
    const elements = [];
    let current = value;
    while (current.kind === 'pair') {
        elements.push(current.car);
        current = current.cdr;
    }
    if (current.kind !== 'nil') {
        throw new EvalError(`${procedure} expects a proper list`);
    }
    return elements;
}
function isProperList(value) {
    const seen = new Set();
    let current = value;
    while (current.kind === 'pair') {
        if (seen.has(current)) {
            return false;
        }
        seen.add(current);
        current = current.cdr;
    }
    return current.kind === 'nil';
}
function eqValues(left, right) {
    if (left.kind === 'number' && right.kind === 'number') {
        return left.value === right.value;
    }
    if (left.kind === 'boolean' && right.kind === 'boolean') {
        return left.value === right.value;
    }
    if (left.kind === 'string' && right.kind === 'string') {
        return left.value === right.value;
    }
    if (left.kind === 'char' && right.kind === 'char') {
        return left.value === right.value;
    }
    if (left.kind === 'symbol' && right.kind === 'symbol') {
        return left.name === right.name;
    }
    if (left.kind === 'nil' && right.kind === 'nil') {
        return true;
    }
    if (left.kind === 'builtin' && right.kind === 'builtin') {
        return left.name === right.name;
    }
    if (left.kind === 'void' && right.kind === 'void') {
        return true;
    }
    return left === right;
}
function equalValues(left, right, seen = new WeakMap()) {
    if (left.kind === 'pair' && right.kind === 'pair') {
        let seenRights = seen.get(left);
        if (seenRights?.has(right)) {
            return true;
        }
        if (seenRights === undefined) {
            seenRights = new WeakSet();
            seen.set(left, seenRights);
        }
        seenRights.add(right);
        return equalValues(left.car, right.car, seen) && equalValues(left.cdr, right.cdr, seen);
    }
    if (left.kind === 'string' && right.kind === 'string') {
        return left.value === right.value;
    }
    return eqValues(left, right);
}
function applyTypePredicate(args, name, predicate) {
    if (args.length !== 1) {
        throw new EvalError(`${name} expects exactly 1 argument`);
    }
    return makeBoolean(predicate(args[0]));
}
function isTruthy(value) {
    return value.kind !== 'boolean' || value.value;
}
function makeNumber(value) {
    return {
        kind: 'number',
        pos: DEFAULT_SOURCE_POS,
        value: Object.is(value, -0) ? 0 : value,
    };
}
function makeBoolean(value) {
    return {
        kind: 'boolean',
        pos: DEFAULT_SOURCE_POS,
        value,
    };
}
function makeString(value, mutable = true) {
    return {
        kind: 'string',
        value,
        mutable,
    };
}
function makeChar(value) {
    if (stringChars(value).length !== 1) {
        throw new EvalError('character values must contain exactly 1 character');
    }
    return {
        kind: 'char',
        value,
    };
}
function formatValue(value) {
    return formatValueWithMode(value, 'write');
}
function formatDisplayValue(value) {
    return formatValueWithMode(value, 'display');
}
function formatValueWithMode(value, mode) {
    switch (value.kind) {
        case 'number':
            return formatNumber(value.value);
        case 'boolean':
            return value.value ? '#t' : '#f';
        case 'string':
            return mode === 'display' ? value.value : `"${escapeString(value.value)}"`;
        case 'char':
            return mode === 'display' ? value.value : formatChar(value.value);
        case 'symbol':
            return value.name;
        case 'nil':
            return '()';
        case 'pair':
            return formatPair(value, mode);
        case 'builtin':
        case 'closure':
            return '#<procedure>';
        case 'void':
            return '';
    }
}
function formatNumber(value) {
    if (Object.is(value, -0)) {
        return '0';
    }
    if (Number.isInteger(value)) {
        return value.toString(10);
    }
    return String(value);
}
function escapeString(value) {
    return value
        .replaceAll('\\', '\\\\')
        .replaceAll('"', '\\"')
        .replaceAll('\n', '\\n')
        .replaceAll('\r', '\\r')
        .replaceAll('\t', '\\t');
}
function isWhitespace(char) {
    return /\s/.test(char);
}
function isDelimiter(char) {
    return isWhitespace(char) || char === '(' || char === ')' || char === "'" || char === ';';
}
function formatPair(value, mode) {
    const parts = [];
    let current = value;
    while (current.kind === 'pair') {
        parts.push(formatValueWithMode(current.car, mode));
        current = current.cdr;
    }
    if (current.kind === 'nil') {
        return `(${parts.join(' ')})`;
    }
    return `(${parts.join(' ')} . ${formatValueWithMode(current, mode)})`;
}
function formatChar(value) {
    switch (value) {
        case ' ':
            return '#\\space';
        case '\n':
            return '#\\newline';
        default:
            return `#\\${value}`;
    }
}
function stringChars(value) {
    return Array.from(value);
}
function charCodePoint(value) {
    const codePoint = value.codePointAt(0);
    if (codePoint === undefined) {
        throw new EvalError('character values must contain exactly 1 character');
    }
    return codePoint;
}
function parseNumberString(value) {
    const trimmed = value.trim();
    if (/^[+-]?(?:\d+|\d+\.\d+|\.\d+)$/.test(trimmed)) {
        return makeNumber(Number(trimmed));
    }
    return makeBoolean(false);
}
function attachPosition(error, pos) {
    if (error instanceof EvalError) {
        return error.withPosition(pos);
    }
    if (error instanceof Error) {
        return new EvalError(error.message, pos);
    }
    return new EvalError(String(error), pos);
}
