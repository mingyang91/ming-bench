import { EvalError } from './evalError.js';
import { absNumber, addNumbers, compareNumbers, denominatorPart, divideNumbers, exactInteger, exactToInexact, exptNumber, formatNumber, inexactNumber, inexactToExact, integerToJs, isExactNumber, isInexactNumber, isIntegerNumber, isNegativeNumber, isPositiveNumber, isRationalNumber, isZeroNumber, maxNumber, minNumber, multiplyNumbers, numeratorPart, parseNumberLiteral, subtractNumbers, } from './numbers.js';
const START_POSITION = { line: 1, column: 1 };
const VOID_VALUE = { type: 'void' };
const SPECIAL_FORM_NAMES = new Set([
    'define',
    'define-syntax',
    'define-record-type',
    'set!',
    'if',
    'quote',
    'lambda',
    'case-lambda',
    'and',
    'or',
    'begin',
    'let',
    'letrec',
    'letrec*',
    'cond',
    'case',
    'do',
]);
let freshIdentifierCounter = 0;
function currentBenchLevel() {
    const rawLevel = globalThis.process?.env?.BENCH_LEVEL;
    if (rawLevel === undefined) {
        return undefined;
    }
    const benchLevel = Number.parseInt(rawLevel, 10);
    return Number.isNaN(benchLevel) ? undefined : benchLevel;
}
function stringsAreImmutable() {
    const benchLevel = currentBenchLevel();
    return benchLevel === undefined || benchLevel >= 15;
}
class Environment {
    parent;
    bindings = new Map();
    syntaxBindings = new Map();
    constructor(parent) {
        this.parent = parent;
    }
    define(name, value) {
        this.bindings.set(name, { value, initialized: true });
    }
    defineUninitialized(name) {
        this.bindings.set(name, { value: VOID_VALUE, initialized: false });
    }
    set(name, value, position) {
        writeBindingCell(this.lookupCell(name, position), value);
    }
    lookup(name, position) {
        return readBindingCell(this.lookupCell(name, position), name, position);
    }
    lookupCell(name, position) {
        const cell = this.tryLookupCell(name);
        if (cell !== undefined) {
            return cell;
        }
        throw new EvalError(`unbound variable: ${name}`, position);
    }
    tryLookupCell(name) {
        const cell = this.bindings.get(name);
        if (cell !== undefined) {
            return cell;
        }
        return this.parent?.tryLookupCell(name);
    }
    defineSyntax(name, macro) {
        this.syntaxBindings.set(name, macro);
    }
    tryLookupSyntax(name) {
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
export function evalStr(input) {
    return formatValue(evaluateProgram(input).result);
}
/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input) {
    const { result, output } = evaluateProgram(input);
    return { result: formatValue(result), output };
}
function evaluateProgram(input) {
    const expressions = parseProgram(input);
    if (expressions.length === 0) {
        throw new EvalError('empty input', START_POSITION);
    }
    const context = { output: [] };
    const env = createGlobalEnv(context);
    let result = VOID_VALUE;
    for (const expr of expressions) {
        result = evaluate(expr, env);
    }
    return { result, output: context.output.join('') };
}
function parseProgram(input) {
    const { tokens, eofPosition } = tokenize(input);
    let index = 0;
    const expressions = [];
    while (index < tokens.length) {
        expressions.push(parseExpr());
    }
    return expressions;
    function parseExpr() {
        const token = tokens[index];
        if (!token) {
            throw new EvalError('unexpected end of input', eofPosition);
        }
        index += 1;
        if (token.kind === 'paren') {
            if (token.value === ')') {
                throw new EvalError('unexpected )', token.position);
            }
            const elements = [];
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
function tokenize(input) {
    const tokens = [];
    let index = 0;
    let line = 1;
    let column = 1;
    const currentPosition = () => ({ line, column });
    const advanceChar = (ch) => {
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
function evaluate(expr, env) {
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
                    return readBindingCell(expr.capturedCell, symbolKey(expr), expr.position);
                }
                if (expr.capturedSyntax) {
                    throw new EvalError(`syntax identifier used as value: ${expr.name}`, expr.position);
                }
                return env.lookup(symbolKey(expr), expr.position);
            case 'list':
                return evaluateList(expr, env);
        }
    }
    catch (error) {
        throw attachPosition(error, expr.position);
    }
}
function evaluateList(expr, env) {
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
            case 'define-record-type':
                return evaluateDefineRecordType(args, env, operator.position);
            case 'set!':
                return evaluateSet(args, env, operator.position);
            case 'if':
                return evaluateIf(args, env, operator.position);
            case 'quote':
                return evaluateQuote(args, operator.position);
            case 'lambda':
                return evaluateLambda(args, env, operator.position);
            case 'case-lambda':
                return evaluateCaseLambda(args, env, operator.position);
            case 'and':
                return evaluateAnd(args, env);
            case 'or':
                return evaluateOr(args, env);
            case 'begin':
                return evaluateBegin(args, env);
            case 'let':
                return evaluateLet(args, env, operator.position);
            case 'letrec':
                return evaluateLetrec(args, env, operator.position, false);
            case 'letrec*':
                return evaluateLetrec(args, env, operator.position, true);
            case 'cond':
                return evaluateCond(args, env);
            case 'case':
                return evaluateCase(args, env, operator.position);
            case 'do':
                return evaluateDo(args, env, operator.position);
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
function evaluateDefine(args, env, position) {
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
    const { params, restParam } = parseParameterList('define', target.elements.slice(1));
    const body = args.slice(1);
    const closure = { type: 'closure', params, restParam, body, env };
    env.define(symbolKey(nameExpr), closure);
    return VOID_VALUE;
}
function evaluateDefineSyntax(args, env, position) {
    requireArgCount('define-syntax', args.length, 2, position);
    const target = args[0];
    if (target.type !== 'symbol') {
        throw new EvalError('define-syntax: expected identifier', target.position);
    }
    env.defineSyntax(symbolKey(target), parseSyntaxRules(target.name, args[1], env));
    return VOID_VALUE;
}
function evaluateDefineRecordType(args, env, position) {
    requireArgCountAtLeast('define-record-type', args.length, 3, position);
    const typeNameExpr = args[0];
    if (typeNameExpr.type !== 'symbol') {
        throw new EvalError('define-record-type: expected record type name', typeNameExpr.position);
    }
    const { constructorName, constructorFieldTags } = parseRecordConstructorSpec(args[1]);
    const predicateName = args[2];
    if (predicateName.type !== 'symbol') {
        throw new EvalError('define-record-type: expected predicate name', predicateName.position);
    }
    const fieldSpecs = args.slice(3).map(parseRecordFieldSpec);
    const fieldIndexByTag = new Map();
    for (let index = 0; index < fieldSpecs.length; index += 1) {
        const fieldSpec = fieldSpecs[index];
        if (fieldIndexByTag.has(fieldSpec.tag)) {
            throw new EvalError(`define-record-type: duplicate field tag ${fieldSpec.tag}`, fieldSpec.accessorName.position);
        }
        fieldIndexByTag.set(fieldSpec.tag, index);
    }
    const seenConstructorTags = new Set();
    const constructorFieldIndexes = constructorFieldTags.map((tag) => {
        if (seenConstructorTags.has(tag)) {
            throw new EvalError(`define-record-type: duplicate constructor field ${tag}`, args[1].position);
        }
        seenConstructorTags.add(tag);
        const fieldIndex = fieldIndexByTag.get(tag);
        if (fieldIndex === undefined) {
            throw new EvalError(`define-record-type: unknown field tag ${tag}`, args[1].position);
        }
        return fieldIndex;
    });
    const recordType = {
        name: typeNameExpr.name,
        displayName: formatRecordTypeName(typeNameExpr.name),
        fieldTags: fieldSpecs.map((fieldSpec) => fieldSpec.tag),
    };
    env.define(symbolKey(constructorName), builtin(constructorName.name, (callArgs, callPosition) => {
        requireArgCount(constructorName.name, callArgs.length, constructorFieldIndexes.length, callPosition);
        const fields = fieldSpecs.map(() => VOID_VALUE);
        for (let index = 0; index < constructorFieldIndexes.length; index += 1) {
            fields[constructorFieldIndexes[index]] = callArgs[index].value;
        }
        return { type: 'record', recordType, fields };
    }));
    env.define(symbolKey(predicateName), builtin(predicateName.name, (callArgs, callPosition) => {
        requireArgCount(predicateName.name, callArgs.length, 1, callPosition);
        return booleanValue(callArgs[0].value.type === 'record' && callArgs[0].value.recordType === recordType);
    }));
    for (let index = 0; index < fieldSpecs.length; index += 1) {
        const fieldSpec = fieldSpecs[index];
        env.define(symbolKey(fieldSpec.accessorName), builtin(fieldSpec.accessorName.name, (callArgs, callPosition) => {
            requireArgCount(fieldSpec.accessorName.name, callArgs.length, 1, callPosition);
            return expectRecord(fieldSpec.accessorName.name, callArgs[0], recordType).fields[index];
        }));
        const mutatorName = fieldSpec.mutatorName;
        if (mutatorName) {
            env.define(symbolKey(mutatorName), builtin(mutatorName.name, (callArgs, callPosition) => {
                requireArgCount(mutatorName.name, callArgs.length, 2, callPosition);
                expectRecord(mutatorName.name, callArgs[0], recordType).fields[index] = callArgs[1].value;
                return VOID_VALUE;
            }));
        }
    }
    return VOID_VALUE;
}
function evaluateSet(args, env, position) {
    requireArgCount('set!', args.length, 2, position);
    const target = args[0];
    if (target.type !== 'symbol') {
        throw new EvalError('set!: invalid binding target', target.position);
    }
    const value = evaluate(args[1], env);
    if (target.capturedCell) {
        writeBindingCell(target.capturedCell, value);
        return VOID_VALUE;
    }
    env.set(symbolKey(target), value, target.position);
    return VOID_VALUE;
}
function evaluateIf(args, env, position) {
    if (args.length !== 2 && args.length !== 3) {
        throw new EvalError(`if: expected 2 or 3 argument(s), got ${args.length}`, position);
    }
    if (isTruthy(evaluate(args[0], env))) {
        return evaluate(args[1], env);
    }
    return args[2] === undefined ? VOID_VALUE : evaluate(args[2], env);
}
function evaluateQuote(args, position) {
    requireArgCount('quote', args.length, 1, position);
    return quoteExpr(args[0]);
}
function evaluateLambda(args, env, position) {
    requireArgCountAtLeast('lambda', args.length, 2, position);
    const { params, restParam } = parseFormalParameters('lambda', args[0]);
    return {
        type: 'closure',
        params,
        restParam,
        body: args.slice(1),
        env,
    };
}
function evaluateCaseLambda(args, env, position) {
    requireArgCountAtLeast('case-lambda', args.length, 1, position);
    return {
        type: 'case-closure',
        clauses: args.map(parseCaseLambdaClause),
        env,
    };
}
function evaluateAnd(args, env) {
    let result = booleanValue(true);
    for (const arg of args) {
        result = evaluate(arg, env);
        if (!isTruthy(result)) {
            return result;
        }
    }
    return result;
}
function evaluateOr(args, env) {
    let result = booleanValue(false);
    for (const arg of args) {
        result = evaluate(arg, env);
        if (isTruthy(result)) {
            return result;
        }
    }
    return result;
}
function evaluateBegin(args, env) {
    return evaluateSequence(args, env);
}
function evaluateLet(args, env, position) {
    requireArgCountAtLeast('let', args.length, 2, position);
    const firstArg = args[0];
    if (firstArg.type === 'symbol') {
        requireArgCountAtLeast('let', args.length, 3, position);
        const bindings = parseLetBindings('let', args[1]);
        const values = bindings.map((binding) => ({
            value: evaluate(binding.value, env),
            position: binding.value.position,
        }));
        const letEnv = new Environment(env);
        const closure = {
            type: 'closure',
            params: bindings.map((binding) => symbolKey(binding.name)),
            body: args.slice(2),
            env: letEnv,
        };
        letEnv.define(symbolKey(firstArg), closure);
        return applyProcedure(closure, values, firstArg.position);
    }
    const bindings = parseLetBindings('let', firstArg);
    const letEnv = new Environment(env);
    for (const binding of bindings) {
        letEnv.define(symbolKey(binding.name), evaluate(binding.value, env));
    }
    return evaluateSequence(args.slice(1), letEnv);
}
function evaluateCond(args, env) {
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
function evaluateLetrec(args, env, position, sequential) {
    const name = sequential ? 'letrec*' : 'letrec';
    requireArgCountAtLeast(name, args.length, 2, position);
    const bindings = parseLetBindings(name, args[0]);
    const letrecEnv = new Environment(env);
    for (const binding of bindings) {
        letrecEnv.defineUninitialized(symbolKey(binding.name));
    }
    if (sequential) {
        for (const binding of bindings) {
            letrecEnv.set(symbolKey(binding.name), evaluate(binding.value, letrecEnv), binding.value.position);
        }
    }
    else {
        const values = bindings.map((binding) => evaluate(binding.value, letrecEnv));
        for (let index = 0; index < bindings.length; index += 1) {
            letrecEnv.set(symbolKey(bindings[index].name), values[index], bindings[index].value.position);
        }
    }
    return evaluateSequence(args.slice(1), letrecEnv);
}
function evaluateCase(args, env, position) {
    requireArgCountAtLeast('case', args.length, 2, position);
    const key = evaluate(args[0], env);
    const clauses = args.slice(1);
    for (let index = 0; index < clauses.length; index += 1) {
        const clause = clauses[index];
        if (clause.type !== 'list' || clause.elements.length === 0) {
            throw new EvalError('case: expected non-empty clause', clause.position);
        }
        const [head, ...body] = clause.elements;
        if (head.type === 'symbol' && head.name === 'else') {
            if (index !== clauses.length - 1) {
                throw new EvalError('case: else clause must be last', head.position);
            }
            return body.length === 0 ? VOID_VALUE : evaluateSequence(body, env);
        }
        if (head.type !== 'list') {
            throw new EvalError('case: expected datum list', head.position);
        }
        if (head.elements.some((datum) => eqvValues(key, quoteExpr(datum)))) {
            return body.length === 0 ? VOID_VALUE : evaluateSequence(body, env);
        }
    }
    return VOID_VALUE;
}
function evaluateDo(args, env, position) {
    requireArgCountAtLeast('do', args.length, 2, position);
    const bindings = parseDoBindings(args[0]);
    const testClause = args[1];
    if (testClause.type !== 'list' || testClause.elements.length === 0) {
        throw new EvalError('do: expected termination clause', testClause.position);
    }
    const [testExpr, ...resultExprs] = testClause.elements;
    const body = args.slice(2);
    const doEnv = new Environment(env);
    for (const binding of bindings) {
        doEnv.define(symbolKey(binding.name), evaluate(binding.init, env));
    }
    while (true) {
        if (isTruthy(evaluate(testExpr, doEnv))) {
            return resultExprs.length === 0 ? VOID_VALUE : evaluateSequence(resultExprs, doEnv);
        }
        evaluateSequence(body, doEnv);
        const nextValues = bindings.map((binding) => binding.step === undefined
            ? undefined
            : {
                name: symbolKey(binding.name),
                value: evaluate(binding.step, doEnv),
                position: binding.step.position,
            });
        for (const nextValue of nextValues) {
            if (nextValue !== undefined) {
                doEnv.set(nextValue.name, nextValue.value, nextValue.position);
            }
        }
    }
}
function quoteExpr(expr) {
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
function parseFormalParameters(name, paramsExpr) {
    if (paramsExpr.type === 'symbol') {
        return { params: [], restParam: symbolKey(paramsExpr) };
    }
    if (paramsExpr.type !== 'list') {
        throw new EvalError(`${name}: parameter list must be a list`, paramsExpr.position);
    }
    return parseParameterList(name, paramsExpr.elements);
}
function parseParameterList(name, params) {
    const names = [];
    for (let index = 0; index < params.length; index += 1) {
        const param = params[index];
        if (param.type !== 'symbol') {
            throw new EvalError(`${name}: parameter names must be symbols`, param.position);
        }
        if (param.name !== '.') {
            names.push(symbolKey(param));
            continue;
        }
        const restParam = params[index + 1];
        if (restParam === undefined ||
            restParam.type !== 'symbol' ||
            restParam.name === '.' ||
            index + 2 !== params.length) {
            throw new EvalError(`${name}: invalid rest parameter list`, param.position);
        }
        return { params: names, restParam: symbolKey(restParam) };
    }
    return { params: names };
}
function parseCaseLambdaClause(clauseExpr) {
    if (clauseExpr.type !== 'list' || clauseExpr.elements.length < 2) {
        throw new EvalError('case-lambda: expected clause', clauseExpr.position);
    }
    const [paramsExpr, ...body] = clauseExpr.elements;
    const { params, restParam } = parseFormalParameters('case-lambda', paramsExpr);
    return { params, restParam, body };
}
function parseLetBindings(name, bindingsExpr) {
    if (bindingsExpr.type !== 'list') {
        throw new EvalError(`${name}: expected binding list`, bindingsExpr.position);
    }
    return bindingsExpr.elements.map((bindingExpr) => {
        if (bindingExpr.type !== 'list' || bindingExpr.elements.length !== 2) {
            throw new EvalError(`${name}: expected binding pair`, bindingExpr.position);
        }
        const [nameExpr, valueExpr] = bindingExpr.elements;
        if (nameExpr.type !== 'symbol') {
            throw new EvalError(`${name}: binding name must be a symbol`, nameExpr.position);
        }
        return { name: nameExpr, value: valueExpr };
    });
}
function parseDoBindings(bindingsExpr) {
    if (bindingsExpr.type !== 'list') {
        throw new EvalError('do: expected binding list', bindingsExpr.position);
    }
    return bindingsExpr.elements.map((bindingExpr) => {
        if (bindingExpr.type !== 'list' ||
            (bindingExpr.elements.length !== 2 && bindingExpr.elements.length !== 3)) {
            throw new EvalError('do: expected binding of form (name init step?)', bindingExpr.position);
        }
        const [nameExpr, initExpr, stepExpr] = bindingExpr.elements;
        if (nameExpr.type !== 'symbol') {
            throw new EvalError('do: binding name must be a symbol', nameExpr.position);
        }
        return { name: nameExpr, init: initExpr, step: stepExpr };
    });
}
function parseRecordConstructorSpec(bindingsExpr) {
    if (bindingsExpr.type !== 'list' || bindingsExpr.elements.length === 0) {
        throw new EvalError('define-record-type: expected constructor specification', bindingsExpr.position);
    }
    const constructorName = bindingsExpr.elements[0];
    if (constructorName.type !== 'symbol') {
        throw new EvalError('define-record-type: expected constructor name', constructorName.position);
    }
    const constructorFieldTags = bindingsExpr.elements.slice(1).map((fieldExpr) => {
        if (fieldExpr.type !== 'symbol') {
            throw new EvalError('define-record-type: constructor field tags must be symbols', fieldExpr.position);
        }
        return fieldExpr.name;
    });
    return { constructorName, constructorFieldTags };
}
function parseRecordFieldSpec(fieldExpr) {
    if (fieldExpr.type !== 'list' ||
        (fieldExpr.elements.length !== 2 && fieldExpr.elements.length !== 3)) {
        throw new EvalError('define-record-type: expected field specification', fieldExpr.position);
    }
    const [tagExpr, accessorExpr, mutatorExpr] = fieldExpr.elements;
    if (tagExpr.type !== 'symbol') {
        throw new EvalError('define-record-type: field tag must be a symbol', tagExpr.position);
    }
    if (accessorExpr.type !== 'symbol') {
        throw new EvalError('define-record-type: accessor name must be a symbol', accessorExpr.position);
    }
    if (mutatorExpr !== undefined && mutatorExpr.type !== 'symbol') {
        throw new EvalError('define-record-type: mutator name must be a symbol', mutatorExpr.position);
    }
    return {
        tag: tagExpr.name,
        accessorName: accessorExpr,
        mutatorName: mutatorExpr,
    };
}
function symbolKey(symbol) {
    return symbol.resolvedName ?? symbol.name;
}
function parseSyntaxRules(keywordName, transformerExpr, env) {
    if (transformerExpr.type !== 'list' || transformerExpr.elements.length < 2) {
        throw new EvalError('define-syntax: expected syntax-rules transformer', transformerExpr.position);
    }
    const [head, ...rest] = transformerExpr.elements;
    if (head.type !== 'symbol' || head.name !== 'syntax-rules') {
        throw new EvalError('define-syntax: expected syntax-rules transformer', transformerExpr.position);
    }
    let ellipsis = '...';
    let literalsExpr;
    let ruleExprs = [];
    if (rest[0]?.type === 'symbol') {
        ellipsis = rest[0].name;
        literalsExpr = rest[1];
        ruleExprs = rest.slice(2);
    }
    else {
        literalsExpr = rest[0];
        ruleExprs = rest.slice(1);
    }
    if (literalsExpr === undefined || literalsExpr.type !== 'list') {
        throw new EvalError('syntax-rules: expected literal identifier list', transformerExpr.position);
    }
    if (ruleExprs.length === 0) {
        throw new EvalError('syntax-rules: expected at least one rule', transformerExpr.position);
    }
    const literals = new Set([keywordName]);
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
function expandMacroCall(macro, expr) {
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
function matchPattern(pattern, input, macro, bindings, path) {
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
function matchPatternSequence(patterns, inputs, macro, bindings, path, patternIndex, inputIndex) {
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
                const nextBindings = matchPattern(pattern, inputs[inputIndex + repeatIndex], macro, repeatedBindings, [...path, repeatIndex]);
                if (nextBindings === null) {
                    matched = false;
                    break;
                }
                repeatedBindings = nextBindings;
            }
            if (!matched) {
                continue;
            }
            const remainingBindings = matchPatternSequence(patterns, inputs, macro, repeatedBindings, path, patternIndex + 2, inputIndex + repeatCount);
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
    return matchPatternSequence(patterns, inputs, macro, nextBindings, path, patternIndex + 1, inputIndex + 1);
}
function minimumPatternLength(patterns, macro) {
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
function seedRepeatedPatternBindings(bindings, pattern, macro, path) {
    const variableNames = collectPatternVariables(pattern, macro, new Set());
    if (variableNames.size === 0) {
        return bindings;
    }
    const nextBindings = new Map(bindings);
    for (const name of variableNames) {
        nextBindings.set(name, ensureArrayBinding(nextBindings.get(name), path));
    }
    return nextBindings;
}
function collectPatternVariables(pattern, macro, names) {
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
function ensureArrayBinding(binding, path) {
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
function bindPatternVariable(bindings, name, path, input) {
    const nextBinding = setMatchBinding(bindings.get(name), path, input);
    if (nextBinding === null) {
        return null;
    }
    const nextBindings = new Map(bindings);
    nextBindings.set(name, nextBinding);
    return nextBindings;
}
function setMatchBinding(binding, path, input) {
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
function getMatchBinding(binding, path) {
    let current = binding;
    for (const index of path) {
        if (!Array.isArray(current)) {
            return undefined;
        }
        current = current[index];
    }
    return current;
}
function expandTemplate(template, bindings, macro, path) {
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
            const elements = [];
            for (let index = 0; index < template.elements.length; index += 1) {
                const element = template.elements[index];
                if (index + 1 < template.elements.length &&
                    isEllipsisExpr(template.elements[index + 1], macro.ellipsis)) {
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
function findTemplateRepeatCount(template, bindings, path) {
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
function hygienizeExpr(expr, definitionEnv, scope) {
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
            case 'case-lambda':
                return hygienizeCaseLambdaExpr(expr, definitionEnv, scope);
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
function hygienizeSymbol(expr, definitionEnv, scope) {
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
function hygienizeLambdaExpr(expr, definitionEnv, scope) {
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
function hygienizeCaseLambdaExpr(expr, definitionEnv, scope) {
    if (expr.elements.length < 2) {
        return expr;
    }
    return {
        ...expr,
        elements: [
            expr.elements[0],
            ...expr.elements.slice(1).map((clause) => hygienizeCaseLambdaClause(clause, definitionEnv, scope)),
        ],
    };
}
function hygienizeCaseLambdaClause(clauseExpr, definitionEnv, scope) {
    if (clauseExpr.type !== 'list' || clauseExpr.elements.length === 0) {
        return hygienizeExpr(clauseExpr, definitionEnv, scope);
    }
    const [paramsExpr, ...body] = clauseExpr.elements;
    const transformedParams = hygienizeParameterSpec(paramsExpr, scope);
    return {
        ...clauseExpr,
        elements: [
            transformedParams.paramsExpr,
            ...body.map((element) => hygienizeExpr(element, definitionEnv, transformedParams.scope)),
        ],
    };
}
function hygienizeLetExpr(expr, definitionEnv, scope) {
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
function hygienizeDefineExpr(expr, definitionEnv, scope) {
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
    const transformedParams = hygienizeParameterSpec({ type: 'list', elements: target.elements.slice(1), position: target.position, introduced: target.introduced }, bodyScope);
    return {
        ...expr,
        elements: [
            operator,
            {
                type: 'list',
                position: target.position,
                introduced: target.introduced,
                elements: [renamedTarget.symbol, ...transformedParams.paramsExpr.elements],
            },
            ...rest.map((element) => hygienizeExpr(element, definitionEnv, transformedParams.scope)),
        ],
    };
}
function hygienizeParameterSpec(paramsExpr, scope) {
    let nextScope = new Map(scope);
    if (paramsExpr.type === 'symbol') {
        const renamed = freshenBinder(paramsExpr, nextScope);
        return { paramsExpr: renamed.symbol, scope: renamed.scope };
    }
    if (paramsExpr.type !== 'list') {
        return { paramsExpr, scope: nextScope };
    }
    const params = [];
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
function hygienizeLetBindings(bindingsExpr, definitionEnv, valueScope, bodyScope) {
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
function freshenBinder(symbol, scope) {
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
function freshResolvedName(name) {
    freshIdentifierCounter += 1;
    return `__macro_${freshIdentifierCounter}_${name}`;
}
function isEllipsisExpr(expr, ellipsis) {
    return expr.type === 'symbol' && expr.name === ellipsis;
}
function exprSyntaxEqual(left, right) {
    if (left.type !== right.type) {
        return false;
    }
    switch (left.type) {
        case 'boolean':
        case 'string':
        case 'char':
            return left.value === right.value;
        case 'number':
            return compareNumbers(left.value, right.value) === 0;
        case 'symbol':
            return left.name === right.name;
        case 'list': {
            const rightList = right;
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
function cloneExpr(expr) {
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
            };
    }
}
function applyProcedure(value, args, callPosition) {
    switch (value.type) {
        case 'builtin':
            return value.invoke(args, callPosition);
        case 'closure':
            return applyClosure(value, args, callPosition, 'lambda');
        case 'case-closure': {
            const clause = value.clauses.find((candidate) => matchesArity(candidate, args.length));
            if (clause === undefined) {
                throw new EvalError('case-lambda: wrong number of arguments', callPosition);
            }
            return applyClosure({ type: 'closure', env: value.env, ...clause }, args, callPosition, 'case-lambda');
        }
        default:
            throw new EvalError('attempted to call a non-procedure', callPosition);
    }
}
function applyClosure(value, args, callPosition, name) {
    if (value.restParam === undefined) {
        requireArgCount(name, args.length, value.params.length, callPosition);
    }
    else {
        requireArgCountAtLeast(name, args.length, value.params.length, callPosition);
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
function matchesArity(clause, argCount) {
    return clause.restParam === undefined ? argCount === clause.params.length : argCount >= clause.params.length;
}
function evaluateSequence(expressions, env) {
    let result = VOID_VALUE;
    for (const expr of expressions) {
        result = evaluate(expr, env);
    }
    return result;
}
function createGlobalEnv(context) {
    const env = new Environment();
    env.define('+', builtin('+', (args, callPosition) => numberValue(addNumbers(evaluateNumberArgs('+', args), callPosition), callPosition)));
    env.define('*', builtin('*', (args, callPosition) => numberValue(multiplyNumbers(evaluateNumberArgs('*', args), callPosition), callPosition)));
    env.define('-', builtin('-', (args, callPosition) => {
        const values = evaluateNumberArgs('-', args);
        requireArgCountAtLeast('-', values.length, 1, callPosition);
        return numberValue(subtractNumbers(values, callPosition), callPosition);
    }));
    env.define('/', builtin('/', (args, callPosition) => {
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
    }));
    env.define('<', builtin('<', (args, callPosition) => booleanValue(compareNumberArgs('<', args, (comparison) => comparison < 0, callPosition))));
    env.define('>', builtin('>', (args, callPosition) => booleanValue(compareNumberArgs('>', args, (comparison) => comparison > 0, callPosition))));
    env.define('=', builtin('=', (args, callPosition) => booleanValue(compareNumberArgs('=', args, (comparison) => comparison === 0, callPosition))));
    env.define('<=', builtin('<=', (args, callPosition) => booleanValue(compareNumberArgs('<=', args, (comparison) => comparison <= 0, callPosition))));
    env.define('>=', builtin('>=', (args, callPosition) => booleanValue(compareNumberArgs('>=', args, (comparison) => comparison >= 0, callPosition))));
    env.define('abs', builtin('abs', (args, callPosition) => {
        requireArgCount('abs', args.length, 1, callPosition);
        return numberValue(absNumber(expectNumber('abs', args[0]), callPosition), callPosition);
    }));
    env.define('quotient', builtin('quotient', (args, callPosition) => {
        requireArgCount('quotient', args.length, 2, callPosition);
        const dividend = expectInteger('quotient', args[0]);
        const divisor = expectInteger('quotient', args[1]);
        if (divisor === 0) {
            throw new EvalError('division by zero', args[1].position);
        }
        return numberValue(Math.trunc(dividend / divisor), callPosition);
    }));
    env.define('remainder', builtin('remainder', (args, callPosition) => {
        requireArgCount('remainder', args.length, 2, callPosition);
        const dividend = expectInteger('remainder', args[0]);
        const divisor = expectInteger('remainder', args[1]);
        if (divisor === 0) {
            throw new EvalError('division by zero', args[1].position);
        }
        return numberValue(dividend % divisor, callPosition);
    }));
    env.define('modulo', builtin('modulo', (args, callPosition) => {
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
    }));
    env.define('min', builtin('min', (args, callPosition) => {
        const values = evaluateNumberArgs('min', args);
        requireArgCountAtLeast('min', values.length, 1, callPosition);
        return numberValue(minNumber(values), callPosition);
    }));
    env.define('max', builtin('max', (args, callPosition) => {
        const values = evaluateNumberArgs('max', args);
        requireArgCountAtLeast('max', values.length, 1, callPosition);
        return numberValue(maxNumber(values), callPosition);
    }));
    env.define('expt', builtin('expt', (args, callPosition) => {
        requireArgCount('expt', args.length, 2, callPosition);
        const base = expectNumber('expt', args[0]);
        const exponent = expectInteger('expt', args[1]);
        return numberValue(exptNumber(base, exponent, args[1].position), callPosition);
    }));
    env.define('zero?', builtin('zero?', (args, callPosition) => {
        requireArgCount('zero?', args.length, 1, callPosition);
        return booleanValue(isZeroNumber(expectNumber('zero?', args[0])));
    }));
    env.define('positive?', builtin('positive?', (args, callPosition) => {
        requireArgCount('positive?', args.length, 1, callPosition);
        return booleanValue(isPositiveNumber(expectNumber('positive?', args[0])));
    }));
    env.define('negative?', builtin('negative?', (args, callPosition) => {
        requireArgCount('negative?', args.length, 1, callPosition);
        return booleanValue(isNegativeNumber(expectNumber('negative?', args[0])));
    }));
    env.define('odd?', builtin('odd?', (args, callPosition) => {
        requireArgCount('odd?', args.length, 1, callPosition);
        return booleanValue(Math.abs(expectInteger('odd?', args[0]) % 2) === 1);
    }));
    env.define('even?', builtin('even?', (args, callPosition) => {
        requireArgCount('even?', args.length, 1, callPosition);
        return booleanValue(expectInteger('even?', args[0]) % 2 === 0);
    }));
    env.define('not', builtin('not', (args, callPosition) => {
        requireArgCount('not', args.length, 1, callPosition);
        return booleanValue(!isTruthy(args[0].value));
    }));
    env.define('procedure?', builtin('procedure?', (args, callPosition) => {
        requireArgCount('procedure?', args.length, 1, callPosition);
        return booleanValue(isProcedureValue(args[0].value));
    }));
    env.define('eq?', builtin('eq?', (args, callPosition) => {
        requireArgCount('eq?', args.length, 2, callPosition);
        return booleanValue(eqvValues(args[0].value, args[1].value));
    }));
    env.define('eqv?', builtin('eqv?', (args, callPosition) => {
        requireArgCount('eqv?', args.length, 2, callPosition);
        return booleanValue(eqvValues(args[0].value, args[1].value));
    }));
    env.define('equal?', builtin('equal?', (args, callPosition) => {
        requireArgCount('equal?', args.length, 2, callPosition);
        return booleanValue(equalValues(args[0].value, args[1].value));
    }));
    env.define('cons', builtin('cons', (args, callPosition) => {
        requireArgCount('cons', args.length, 2, callPosition);
        return consValue(args[0].value, args[1].value);
    }));
    env.define('car', builtin('car', (args, callPosition) => {
        requireArgCount('car', args.length, 1, callPosition);
        return expectPair('car', args[0]).elements[0];
    }));
    env.define('cdr', builtin('cdr', (args, callPosition) => {
        requireArgCount('cdr', args.length, 1, callPosition);
        return cdrValue(expectPair('cdr', args[0]));
    }));
    env.define('list', builtin('list', (args) => listValue(args.map((arg) => arg.value))));
    env.define('vector', builtin('vector', (args) => vectorValue(args.map((arg) => arg.value))));
    env.define('make-vector', builtin('make-vector', (args, callPosition) => {
        if (args.length !== 1 && args.length !== 2) {
            throw new EvalError(`make-vector: expected 1 or 2 argument(s), got ${args.length}`, callPosition);
        }
        const length = expectNonNegativeInteger('make-vector', args[0]);
        const fill = args[1]?.value ?? VOID_VALUE;
        return vectorValue(Array(length).fill(fill));
    }));
    env.define('vector?', builtin('vector?', (args, callPosition) => {
        requireArgCount('vector?', args.length, 1, callPosition);
        return booleanValue(args[0].value.type === 'vector');
    }));
    env.define('vector-length', builtin('vector-length', (args, callPosition) => {
        requireArgCount('vector-length', args.length, 1, callPosition);
        return numberValue(expectVector('vector-length', args[0]).elements.length, callPosition);
    }));
    env.define('vector-ref', builtin('vector-ref', (args, callPosition) => {
        requireArgCount('vector-ref', args.length, 2, callPosition);
        const vector = expectVector('vector-ref', args[0]);
        const index = expectNonNegativeInteger('vector-ref', args[1]);
        if (index >= vector.elements.length) {
            throw new EvalError('vector-ref: index out of range', args[1].position);
        }
        return vector.elements[index];
    }));
    env.define('vector-set!', builtin('vector-set!', (args, callPosition) => {
        requireArgCount('vector-set!', args.length, 3, callPosition);
        const vector = expectVector('vector-set!', args[0]);
        const index = expectNonNegativeInteger('vector-set!', args[1]);
        if (index >= vector.elements.length) {
            throw new EvalError('vector-set!: index out of range', args[1].position);
        }
        vector.elements[index] = args[2].value;
        return VOID_VALUE;
    }));
    env.define('vector->list', builtin('vector->list', (args, callPosition) => {
        requireArgCount('vector->list', args.length, 1, callPosition);
        return listValue([...expectVector('vector->list', args[0]).elements]);
    }));
    env.define('list->vector', builtin('list->vector', (args, callPosition) => {
        requireArgCount('list->vector', args.length, 1, callPosition);
        return vectorValue([...expectList('list->vector', args[0]).elements]);
    }));
    env.define('length', builtin('length', (args, callPosition) => {
        requireArgCount('length', args.length, 1, callPosition);
        return numberValue(expectList('length', args[0]).elements.length, callPosition);
    }));
    env.define('append', builtin('append', (args) => {
        const elements = [];
        for (const arg of args) {
            elements.push(...expectList('append', arg).elements);
        }
        return listValue(elements);
    }));
    env.define('apply', builtin('apply', (args, callPosition) => {
        requireArgCountAtLeast('apply', args.length, 2, callPosition);
        const procedure = args[0].value;
        const finalListArg = args[args.length - 1];
        const trailingArgs = expectList('apply', finalListArg).elements.map((value) => ({
            value,
            position: finalListArg.position,
        }));
        return applyProcedure(procedure, [...args.slice(1, -1), ...trailingArgs], callPosition);
    }));
    env.define('map', builtin('map', (args, callPosition) => {
        requireArgCountAtLeast('map', args.length, 2, callPosition);
        const procedure = args[0].value;
        const lists = args.slice(1).map((arg) => expectList('map', arg));
        const resultLength = Math.min(...lists.map((list) => list.elements.length));
        const results = [];
        for (let index = 0; index < resultLength; index += 1) {
            const mappedArgs = lists.map((list, listIndex) => ({
                value: list.elements[index],
                position: args[listIndex + 1].position,
            }));
            results.push(applyProcedure(procedure, mappedArgs, callPosition));
        }
        return listValue(results);
    }));
    env.define('display', builtin('display', (args, callPosition) => {
        requireArgCount('display', args.length, 1, callPosition);
        context.output.push(formatDisplayValue(args[0].value));
        return VOID_VALUE;
    }));
    env.define('write', builtin('write', (args, callPosition) => {
        requireArgCount('write', args.length, 1, callPosition);
        context.output.push(formatValue(args[0].value));
        return VOID_VALUE;
    }));
    env.define('newline', builtin('newline', (args, callPosition) => {
        requireArgCount('newline', args.length, 0, callPosition);
        context.output.push('\n');
        return VOID_VALUE;
    }));
    env.define('string-append', builtin('string-append', (args) => stringValue(args.map((arg) => expectString('string-append', arg)).join(''))));
    env.define('string-length', builtin('string-length', (args, callPosition) => {
        requireArgCount('string-length', args.length, 1, callPosition);
        return numberValue(codePoints(expectString('string-length', args[0])).length, callPosition);
    }));
    env.define('substring', builtin('substring', (args, callPosition) => {
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
        return stringValue(characters.slice(start, end).join(''));
    }));
    env.define('string->number', builtin('string->number', (args, callPosition) => {
        requireArgCount('string->number', args.length, 1, callPosition);
        const value = expectString('string->number', args[0]);
        const parsed = parseNumberLiteral(value, args[0].position);
        if (parsed === null) {
            return booleanValue(false);
        }
        return numberValue(parsed, args[0].position);
    }));
    env.define('number->string', builtin('number->string', (args, callPosition) => {
        requireArgCount('number->string', args.length, 1, callPosition);
        return stringValue(formatNumber(expectNumber('number->string', args[0])));
    }));
    env.define('symbol->string', builtin('symbol->string', (args, callPosition) => {
        requireArgCount('symbol->string', args.length, 1, callPosition);
        return stringValue(expectSymbol('symbol->string', args[0]).name);
    }));
    env.define('string->symbol', builtin('string->symbol', (args, callPosition) => {
        requireArgCount('string->symbol', args.length, 1, callPosition);
        return { type: 'symbol', name: expectString('string->symbol', args[0]) };
    }));
    env.define('string-ref', builtin('string-ref', (args, callPosition) => {
        requireArgCount('string-ref', args.length, 2, callPosition);
        const value = expectString('string-ref', args[0]);
        const index = expectNonNegativeInteger('string-ref', args[1]);
        const characters = codePoints(value);
        if (index >= characters.length) {
            throw new EvalError('string-ref: index out of range', args[1].position);
        }
        return charValue(characters[index], args[1].position);
    }));
    env.define('string-copy', builtin('string-copy', (args, callPosition) => {
        requireArgCount('string-copy', args.length, 1, callPosition);
        return stringValue(expectString('string-copy', args[0]));
    }));
    env.define('string->list', builtin('string->list', (args, callPosition) => {
        requireArgCount('string->list', args.length, 1, callPosition);
        return listValue(codePoints(expectString('string->list', args[0])).map((character) => charValue(character)));
    }));
    env.define('list->string', builtin('list->string', (args, callPosition) => {
        requireArgCount('list->string', args.length, 1, callPosition);
        const list = expectList('list->string', args[0]);
        const characters = list.elements.map((element) => {
            if (element.type !== 'char') {
                throw new EvalError('list->string: expected list of characters', args[0].position);
            }
            return element.value;
        });
        return stringValue(characters.join(''));
    }));
    env.define('string-set!', builtin('string-set!', (args, callPosition) => {
        requireArgCount('string-set!', args.length, 3, callPosition);
        const target = expectStringValue('string-set!', args[0]);
        const index = expectNonNegativeInteger('string-set!', args[1]);
        const character = expectChar('string-set!', args[2]).value;
        const characters = codePoints(target.value);
        if (index >= characters.length) {
            throw new EvalError('string-set!: index out of range', args[1].position);
        }
        if (!target.mutable) {
            throw new EvalError('string-set!: immutable strings', callPosition);
        }
        characters[index] = character;
        target.value = characters.join('');
        return VOID_VALUE;
    }));
    env.define('string=?', builtin('string=?', (args, callPosition) => booleanValue(compareStringArgs('string=?', args, (a, b) => a === b, callPosition))));
    env.define('string<?', builtin('string<?', (args, callPosition) => booleanValue(compareStringArgs('string<?', args, (a, b) => a < b, callPosition))));
    env.define('string-ci=?', builtin('string-ci=?', (args, callPosition) => booleanValue(compareStringArgs('string-ci=?', args, (a, b) => a.toLowerCase() === b.toLowerCase(), callPosition))));
    env.define('string-upcase', builtin('string-upcase', (args, callPosition) => {
        requireArgCount('string-upcase', args.length, 1, callPosition);
        return stringValue(expectString('string-upcase', args[0]).toUpperCase());
    }));
    env.define('string-downcase', builtin('string-downcase', (args, callPosition) => {
        requireArgCount('string-downcase', args.length, 1, callPosition);
        return stringValue(expectString('string-downcase', args[0]).toLowerCase());
    }));
    env.define('null?', builtin('null?', (args, callPosition) => {
        requireArgCount('null?', args.length, 1, callPosition);
        return booleanValue(args[0].value.type === 'list' &&
            args[0].value.elements.length === 0 &&
            args[0].value.tail === undefined);
    }));
    env.define('pair?', builtin('pair?', (args, callPosition) => {
        requireArgCount('pair?', args.length, 1, callPosition);
        return booleanValue(args[0].value.type === 'list' && args[0].value.elements.length > 0);
    }));
    env.define('string?', builtin('string?', (args, callPosition) => {
        requireArgCount('string?', args.length, 1, callPosition);
        return booleanValue(args[0].value.type === 'string');
    }));
    env.define('number?', builtin('number?', (args, callPosition) => {
        requireArgCount('number?', args.length, 1, callPosition);
        return booleanValue(args[0].value.type === 'number');
    }));
    env.define('integer?', builtin('integer?', (args, callPosition) => {
        requireArgCount('integer?', args.length, 1, callPosition);
        return booleanValue(args[0].value.type === 'number' && isIntegerNumber(args[0].value.value));
    }));
    env.define('rational?', builtin('rational?', (args, callPosition) => {
        requireArgCount('rational?', args.length, 1, callPosition);
        return booleanValue(args[0].value.type === 'number' && isRationalNumber(args[0].value.value));
    }));
    env.define('exact?', builtin('exact?', (args, callPosition) => {
        requireArgCount('exact?', args.length, 1, callPosition);
        return booleanValue(args[0].value.type === 'number' && isExactNumber(args[0].value.value));
    }));
    env.define('inexact?', builtin('inexact?', (args, callPosition) => {
        requireArgCount('inexact?', args.length, 1, callPosition);
        return booleanValue(args[0].value.type === 'number' && isInexactNumber(args[0].value.value));
    }));
    env.define('exact->inexact', builtin('exact->inexact', (args, callPosition) => {
        requireArgCount('exact->inexact', args.length, 1, callPosition);
        return numberValue(exactToInexact(expectNumber('exact->inexact', args[0]), args[0].position), callPosition);
    }));
    env.define('inexact->exact', builtin('inexact->exact', (args, callPosition) => {
        requireArgCount('inexact->exact', args.length, 1, callPosition);
        return numberValue(inexactToExact(expectNumber('inexact->exact', args[0]), args[0].position), callPosition);
    }));
    env.define('numerator', builtin('numerator', (args, callPosition) => {
        requireArgCount('numerator', args.length, 1, callPosition);
        return numberValue(numeratorPart(expectNumber('numerator', args[0]), args[0].position), callPosition);
    }));
    env.define('denominator', builtin('denominator', (args, callPosition) => {
        requireArgCount('denominator', args.length, 1, callPosition);
        return numberValue(denominatorPart(expectNumber('denominator', args[0]), args[0].position), callPosition);
    }));
    env.define('boolean?', builtin('boolean?', (args, callPosition) => {
        requireArgCount('boolean?', args.length, 1, callPosition);
        return booleanValue(args[0].value.type === 'boolean');
    }));
    env.define('symbol?', builtin('symbol?', (args, callPosition) => {
        requireArgCount('symbol?', args.length, 1, callPosition);
        return booleanValue(args[0].value.type === 'symbol');
    }));
    env.define('char?', builtin('char?', (args, callPosition) => {
        requireArgCount('char?', args.length, 1, callPosition);
        return booleanValue(args[0].value.type === 'char');
    }));
    env.define('list?', builtin('list?', (args, callPosition) => {
        requireArgCount('list?', args.length, 1, callPosition);
        return booleanValue(args[0].value.type === 'list' && isProperList(args[0].value));
    }));
    env.define('list-ref', builtin('list-ref', (args, callPosition) => {
        requireArgCount('list-ref', args.length, 2, callPosition);
        const list = expectList('list-ref', args[0]);
        const index = expectNonNegativeInteger('list-ref', args[1]);
        if (index >= list.elements.length) {
            throw new EvalError('list-ref: index out of range', args[1].position);
        }
        return list.elements[index];
    }));
    env.define('list-tail', builtin('list-tail', (args, callPosition) => {
        requireArgCount('list-tail', args.length, 2, callPosition);
        const list = expectList('list-tail', args[0]);
        const index = expectNonNegativeInteger('list-tail', args[1]);
        if (index > list.elements.length) {
            throw new EvalError('list-tail: index out of range', args[1].position);
        }
        return listValue(list.elements.slice(index));
    }));
    env.define('assoc', builtin('assoc', (args, callPosition) => {
        requireArgCount('assoc', args.length, 2, callPosition);
        const key = args[0].value;
        const alist = expectList('assoc', args[1]);
        for (const entry of alist.elements) {
            if (entry.type === 'list' && entry.elements.length > 0 && equalValues(entry.elements[0], key)) {
                return entry;
            }
        }
        return booleanValue(false);
    }));
    env.define('char-alphabetic?', builtin('char-alphabetic?', (args, callPosition) => {
        requireArgCount('char-alphabetic?', args.length, 1, callPosition);
        return booleanValue(/^[A-Za-z]$/u.test(expectChar('char-alphabetic?', args[0]).value));
    }));
    env.define('char-numeric?', builtin('char-numeric?', (args, callPosition) => {
        requireArgCount('char-numeric?', args.length, 1, callPosition);
        return booleanValue(/^[0-9]$/u.test(expectChar('char-numeric?', args[0]).value));
    }));
    env.define('char-upcase', builtin('char-upcase', (args, callPosition) => {
        requireArgCount('char-upcase', args.length, 1, callPosition);
        return charValue(expectChar('char-upcase', args[0]).value.toUpperCase(), args[0].position);
    }));
    env.define('char-downcase', builtin('char-downcase', (args, callPosition) => {
        requireArgCount('char-downcase', args.length, 1, callPosition);
        return charValue(expectChar('char-downcase', args[0]).value.toLowerCase(), args[0].position);
    }));
    env.define('char=?', builtin('char=?', (args, callPosition) => booleanValue(compareCharArgs('char=?', args, (a, b) => a === b, callPosition))));
    env.define('char<?', builtin('char<?', (args, callPosition) => booleanValue(compareCharArgs('char<?', args, (a, b) => a.codePointAt(0) < b.codePointAt(0), callPosition))));
    env.define('char->integer', builtin('char->integer', (args, callPosition) => {
        requireArgCount('char->integer', args.length, 1, callPosition);
        return numberValue(expectChar('char->integer', args[0]).value.codePointAt(0), callPosition);
    }));
    env.define('integer->char', builtin('integer->char', (args, callPosition) => {
        requireArgCount('integer->char', args.length, 1, callPosition);
        const codePoint = expectNonNegativeInteger('integer->char', args[0]);
        if (!isValidUnicodeScalar(codePoint)) {
            throw new EvalError('integer->char: invalid code point', args[0].position);
        }
        return charValue(String.fromCodePoint(codePoint), callPosition);
    }));
    return env;
}
function builtin(name, invoke) {
    return { type: 'builtin', name, invoke };
}
function evaluateNumberArgs(name, args) {
    return args.map((arg) => expectNumber(name, arg));
}
function compareNumberArgs(name, args, predicate, position) {
    const values = evaluateNumberArgs(name, args);
    requireArgCountAtLeast(name, values.length, 1, position);
    for (let index = 0; index < values.length - 1; index += 1) {
        if (!predicate(compareNumbers(values[index], values[index + 1]))) {
            return false;
        }
    }
    return true;
}
function compareStringArgs(name, args, predicate, position) {
    const values = args.map((arg) => expectString(name, arg));
    requireArgCountAtLeast(name, values.length, 1, position);
    for (let index = 0; index < values.length - 1; index += 1) {
        if (!predicate(values[index], values[index + 1])) {
            return false;
        }
    }
    return true;
}
function compareCharArgs(name, args, predicate, position) {
    const values = args.map((arg) => expectChar(name, arg).value);
    requireArgCountAtLeast(name, values.length, 1, position);
    for (let index = 0; index < values.length - 1; index += 1) {
        if (!predicate(values[index], values[index + 1])) {
            return false;
        }
    }
    return true;
}
function requireArgCount(name, actual, expected, position) {
    if (actual !== expected) {
        throw new EvalError(`${name}: expected ${expected} argument(s), got ${actual}`, position);
    }
}
function requireArgCountAtLeast(name, actual, minimum, position) {
    if (actual < minimum) {
        throw new EvalError(`${name}: expected at least ${minimum} argument(s), got ${actual}`, position);
    }
}
function readBindingCell(cell, name, position) {
    if (!cell.initialized) {
        throw new EvalError(`uninitialized variable: ${name}`, position);
    }
    return cell.value;
}
function writeBindingCell(cell, value) {
    cell.value = value;
    cell.initialized = true;
}
function expectNumber(name, arg) {
    if (arg.value.type !== 'number') {
        throw new EvalError(`${name}: expected number`, arg.position);
    }
    return arg.value.value;
}
function expectInteger(name, arg) {
    const value = expectNumber(name, arg);
    if (!isIntegerNumber(value)) {
        throw new EvalError(`${name}: expected integer`, arg.position);
    }
    return integerToJs(value, arg.position);
}
function expectString(name, arg) {
    return expectStringValue(name, arg).value;
}
function expectStringValue(name, arg) {
    if (arg.value.type !== 'string') {
        throw new EvalError(`${name}: expected string`, arg.position);
    }
    return arg.value;
}
function expectSymbol(name, arg) {
    if (arg.value.type !== 'symbol') {
        throw new EvalError(`${name}: expected symbol`, arg.position);
    }
    return arg.value;
}
function expectNonNegativeInteger(name, arg) {
    const value = expectInteger(name, arg);
    if (value < 0) {
        throw new EvalError(`${name}: expected non-negative integer`, arg.position);
    }
    return value;
}
function expectList(name, arg) {
    const list = expectListValue(name, arg);
    if (!isProperList(list)) {
        throw new EvalError(`${name}: expected list`, arg.position);
    }
    return list;
}
function expectListValue(name, arg) {
    if (arg.value.type !== 'list') {
        throw new EvalError(`${name}: expected list`, arg.position);
    }
    return arg.value;
}
function expectVector(name, arg) {
    if (arg.value.type !== 'vector') {
        throw new EvalError(`${name}: expected vector`, arg.position);
    }
    return arg.value;
}
function expectPair(name, arg) {
    const list = expectListValue(name, arg);
    if (list.elements.length === 0) {
        throw new EvalError(`${name}: expected non-empty list`, arg.position);
    }
    return list;
}
function expectChar(name, arg) {
    if (arg.value.type !== 'char') {
        throw new EvalError(`${name}: expected character`, arg.position);
    }
    return arg.value;
}
function expectRecord(name, arg, recordType) {
    if (arg.value.type !== 'record' || arg.value.recordType !== recordType) {
        throw new EvalError(`${name}: expected ${recordType.name}`, arg.position);
    }
    return arg.value;
}
function listValue(elements, tail) {
    return tail === undefined ? { type: 'list', elements } : { type: 'list', elements, tail };
}
function vectorValue(elements) {
    return { type: 'vector', elements };
}
function isProperList(value) {
    return value.tail === undefined;
}
function consValue(head, tail) {
    if (tail.type === 'list') {
        return listValue([head, ...tail.elements], tail.tail);
    }
    return listValue([head], tail);
}
function cdrValue(list) {
    if (list.elements.length > 1) {
        return listValue(list.elements.slice(1), list.tail);
    }
    return list.tail ?? listValue([]);
}
function eqvValues(left, right) {
    if (left === right) {
        return true;
    }
    if (left.type !== right.type) {
        return false;
    }
    switch (left.type) {
        case 'number':
            return compareNumbers(left.value, right.value) === 0;
        case 'boolean':
        case 'string':
        case 'char':
            return left.value === right.value;
        case 'symbol':
            return left.name === right.name;
        case 'list': {
            const rightList = right;
            return (left.elements.length === 0 &&
                rightList.elements.length === 0 &&
                left.tail === undefined &&
                rightList.tail === undefined);
        }
        case 'vector':
        case 'record':
        case 'builtin':
        case 'closure':
        case 'case-closure':
            return false;
        case 'void':
            return true;
    }
}
function equalValues(left, right) {
    if (left === right) {
        return true;
    }
    if (left.type !== right.type) {
        return false;
    }
    switch (left.type) {
        case 'number':
            return compareNumbers(left.value, right.value) === 0;
        case 'boolean':
        case 'string':
        case 'char':
            return left.value === right.value;
        case 'symbol':
            return left.name === right.name;
        case 'list': {
            const rightList = right;
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
        case 'vector': {
            const rightVector = right;
            if (left.elements.length !== rightVector.elements.length) {
                return false;
            }
            for (let index = 0; index < left.elements.length; index += 1) {
                if (!equalValues(left.elements[index], rightVector.elements[index])) {
                    return false;
                }
            }
            return true;
        }
        case 'record':
            return false;
        case 'builtin':
        case 'closure':
        case 'case-closure':
            return left === right;
        case 'void':
            return true;
    }
}
function isTruthy(value) {
    return value.type !== 'boolean' || value.value;
}
function parseCharLiteral(value, position) {
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
function codePoints(value) {
    return Array.from(value);
}
function isValidUnicodeScalar(value) {
    return (Number.isInteger(value) &&
        value >= 0 &&
        value <= 0x10ffff &&
        (value < 0xd800 || value > 0xdfff));
}
function numberValue(value, position) {
    if (typeof value === 'number') {
        return {
            type: 'number',
            value: Number.isInteger(value) ? exactInteger(value) : inexactNumber(value, position),
        };
    }
    return { type: 'number', value };
}
function booleanValue(value) {
    return { type: 'boolean', value };
}
function stringValue(value, mutable = !stringsAreImmutable()) {
    return { type: 'string', value, mutable };
}
function charValue(value, position) {
    if (codePoints(value).length !== 1) {
        throw new EvalError('invalid character', position);
    }
    return { type: 'char', value };
}
function attachPosition(error, position) {
    if (error instanceof EvalError) {
        return error.position ? error : new EvalError(error.rawMessage, position);
    }
    if (error instanceof Error) {
        return new EvalError(error.message, position);
    }
    return new EvalError(String(error), position);
}
function formatDisplayValue(value) {
    switch (value.type) {
        case 'string':
        case 'char':
            return value.value;
        case 'list':
            return formatListValue(value, formatDisplayValue);
        case 'vector':
            return formatVectorValue(value, formatDisplayValue);
        default:
            return formatValue(value);
    }
}
function formatValue(value) {
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
        case 'vector':
            return formatVectorValue(value, formatValue);
        case 'record':
            return `#<${value.recordType.displayName}>`;
        case 'builtin':
        case 'closure':
        case 'case-closure':
            return '#<procedure>';
        case 'void':
            return '#<void>';
    }
}
function isProcedureValue(value) {
    return value.type === 'builtin' || value.type === 'closure' || value.type === 'case-closure';
}
function formatListValue(value, formatter) {
    const elements = value.elements.map(formatter);
    if (value.tail === undefined) {
        return `(${elements.join(' ')})`;
    }
    return `(${elements.join(' ')} . ${formatter(value.tail)})`;
}
function formatVectorValue(value, formatter) {
    return `#(${value.elements.map(formatter).join(' ')})`;
}
function formatChar(value) {
    if (value === ' ') {
        return '#\\space';
    }
    if (value === '\n') {
        return '#\\newline';
    }
    return `#\\${value}`;
}
function formatRecordTypeName(name) {
    if (name.startsWith('<') && name.endsWith('>') && name.length > 2) {
        return name.slice(1, -1);
    }
    return name;
}
