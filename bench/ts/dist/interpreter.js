import { EvalError, attachPosition } from './evalError.js';
import * as num from './numbers.js';
const EMPTY_LIST = { kind: 'empty-list' };
const VOID = { kind: 'void' };
const UNINITIALIZED = Symbol('uninitialized');
const STRING_IMMUTABILITY_LEVEL = 15;
const CORE_SYNTAX = new Set([
    'and',
    'begin',
    'case',
    'case-lambda',
    'cond',
    'define',
    'define-record-type',
    'define-syntax',
    'do',
    'if',
    'lambda',
    'let',
    'let*',
    'letrec',
    'letrec*',
    'or',
    'quote',
    'set!',
]);
let macroIdentifierCounter = 0;
class OutputBuffer {
    parts = [];
    write(text) {
        this.parts.push(text);
    }
    toString() {
        return this.parts.join('');
    }
}
class Env {
    parent;
    bindings = new Map();
    constructor(parent) {
        this.parent = parent;
    }
    define(name, value) {
        this.bindings.set(name, { value });
    }
    defineUninitialized(name) {
        this.bindings.set(name, { value: UNINITIALIZED });
    }
    defineAlias(name, cell) {
        this.bindings.set(name, cell);
    }
    lookupCell(name) {
        const local = this.bindings.get(name);
        if (local !== undefined) {
            return local;
        }
        return this.parent?.lookupCell(name);
    }
    lookup(name) {
        const cell = this.lookupCell(name);
        if (cell !== undefined) {
            if (cell.value === UNINITIALIZED) {
                throw new EvalError(`uninitialized binding: ${name}`);
            }
            return cell.value;
        }
        throw new EvalError(`unbound symbol: ${name}`);
    }
    set(name, value) {
        const cell = this.lookupCell(name);
        if (cell !== undefined) {
            cell.value = value;
            return;
        }
        throw new EvalError(`unbound symbol: ${name}`);
    }
}
class MacroEnv {
    parent;
    bindings = new Map();
    constructor(parent) {
        this.parent = parent;
    }
    define(name, transformer) {
        this.bindings.set(name, transformer);
    }
    lookup(name) {
        const local = this.bindings.get(name);
        if (local !== undefined) {
            return local;
        }
        return this.parent?.lookup(name);
    }
}
class Parser {
    input;
    index = 0;
    line = 1;
    column = 1;
    constructor(input) {
        this.input = input;
    }
    parseProgram() {
        const exprs = [];
        this.skipIgnored();
        while (!this.isAtEnd()) {
            exprs.push(this.parseExpr());
            this.skipIgnored();
        }
        return exprs;
    }
    parseExpr() {
        this.skipIgnored();
        if (this.isAtEnd()) {
            this.error('unexpected end of input');
        }
        const position = this.currentPosition();
        const ch = this.peek();
        if (ch === '\'') {
            this.advance();
            return {
                kind: 'list',
                position,
                items: [{ kind: 'symbol', name: 'quote', position }, this.parseExpr()],
            };
        }
        if (ch === '(') {
            return this.parseList();
        }
        if (ch === ')') {
            this.error('unexpected )', position);
        }
        if (ch === '"') {
            return this.parseString();
        }
        return this.parseAtom();
    }
    parseList() {
        const position = this.currentPosition();
        this.advance();
        const items = [];
        this.skipIgnored();
        while (!this.isAtEnd() && this.peek() !== ')') {
            items.push(this.parseExpr());
            this.skipIgnored();
        }
        if (this.isAtEnd()) {
            this.error('unterminated list', position);
        }
        this.advance();
        return { kind: 'list', items, position };
    }
    parseString() {
        const position = this.currentPosition();
        this.advance();
        let value = '';
        while (!this.isAtEnd()) {
            const ch = this.advance();
            if (ch === '"') {
                return { kind: 'string', value, position };
            }
            if (ch === '\\') {
                if (this.isAtEnd()) {
                    this.error('unterminated string', position);
                }
                const escaped = this.advance();
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
                    case 't':
                        value += '\t';
                        break;
                    default:
                        value += escaped;
                        break;
                }
            }
            else {
                value += ch;
            }
        }
        this.error('unterminated string', position);
    }
    parseAtom() {
        const position = this.currentPosition();
        const start = this.index;
        while (!this.isAtEnd() && !isDelimiter(this.peek())) {
            this.advance();
        }
        const token = this.input.slice(start, this.index);
        if (token.length === 0) {
            this.error('expected expression', position);
        }
        if (token === '#t') {
            return { kind: 'boolean', value: true, position };
        }
        if (token === '#f') {
            return { kind: 'boolean', value: false, position };
        }
        if (token.startsWith('#\\')) {
            return parseCharToken(token, position);
        }
        const numericValue = num.parseNumberToken(token);
        if (numericValue !== undefined) {
            return { kind: 'number', value: numericValue, position };
        }
        return { kind: 'symbol', name: token, position };
    }
    skipIgnored() {
        while (!this.isAtEnd()) {
            const ch = this.peek();
            if (isWhitespace(ch)) {
                this.advance();
                continue;
            }
            if (ch === ';') {
                while (!this.isAtEnd() && this.peek() !== '\n') {
                    this.advance();
                }
                continue;
            }
            return;
        }
    }
    isAtEnd() {
        return this.index >= this.input.length;
    }
    peek() {
        return this.input[this.index];
    }
    advance() {
        const ch = this.input[this.index];
        this.index += 1;
        if (ch === '\n') {
            this.line += 1;
            this.column = 1;
        }
        else {
            this.column += 1;
        }
        return ch;
    }
    currentPosition() {
        return { line: this.line, column: this.column };
    }
    error(message, position = this.currentPosition()) {
        throw new EvalError(message, position);
    }
}
function freshMacroIdentifier(name) {
    macroIdentifierCounter += 1;
    return `__macro_${macroIdentifierCounter}_${name}`;
}
function cloneExpr(expr) {
    switch (expr.kind) {
        case 'number':
            return { kind: 'number', value: expr.value, position: expr.position };
        case 'boolean':
            return { kind: 'boolean', value: expr.value, position: expr.position };
        case 'char':
            return { kind: 'char', value: expr.value, position: expr.position };
        case 'string':
            return { kind: 'string', value: expr.value, position: expr.position };
        case 'symbol':
            return { kind: 'symbol', name: expr.name, position: expr.position };
        case 'list':
            return { kind: 'list', items: expr.items.map((item) => cloneExpr(item)), position: expr.position };
    }
}
function isEllipsisSymbol(expr) {
    return expr?.kind === 'symbol' && expr.name === '...';
}
function parseSyntaxRules(name, expr, env) {
    if (expr.kind !== 'list' || expr.items.length < 2) {
        throw new EvalError('define-syntax expects a syntax-rules transformer');
    }
    const [headExpr, literalsExpr, ...ruleExprs] = expr.items;
    if (headExpr.kind !== 'symbol' || headExpr.name !== 'syntax-rules') {
        throw new EvalError('define-syntax expects a syntax-rules transformer');
    }
    if (literalsExpr.kind !== 'list') {
        throw new EvalError('syntax-rules expects a literal identifier list');
    }
    const literals = new Set();
    for (const literalExpr of literalsExpr.items) {
        if (literalExpr.kind !== 'symbol') {
            throw new EvalError('syntax-rules literals must be identifiers');
        }
        literals.add(literalExpr.name);
    }
    if (ruleExprs.length === 0) {
        throw new EvalError('syntax-rules expects at least one rule');
    }
    const rules = ruleExprs.map((ruleExpr) => {
        if (ruleExpr.kind !== 'list' || ruleExpr.items.length !== 2) {
            throw new EvalError('syntax-rules expects (pattern template) rules');
        }
        return {
            pattern: ruleExpr.items[0],
            template: ruleExpr.items[1],
        };
    });
    return {
        name,
        literals,
        rules,
        definitionEnv: env,
    };
}
function expandMacroInvocation(transformer, invocation) {
    const literals = new Set(transformer.literals);
    literals.add(transformer.name);
    for (const rule of transformer.rules) {
        const captures = new Map();
        if (!matchPattern(rule.pattern, invocation, literals, captures)) {
            continue;
        }
        return expandTemplate(rule.template, {
            macro: transformer,
            captures,
            renameMap: new Map(),
            capturedFreeNames: new Map(),
        });
    }
    throw new EvalError(`no matching syntax-rules pattern for ${transformer.name}`);
}
function matchPattern(pattern, expr, literals, captures, repeatedContext = false) {
    switch (pattern.kind) {
        case 'number':
        case 'boolean':
        case 'char':
        case 'string':
            return pattern.kind === expr.kind && pattern.value === expr.value;
        case 'symbol':
            if (pattern.name === '_') {
                return true;
            }
            if (pattern.name === '...') {
                return false;
            }
            if (literals.has(pattern.name)) {
                return expr.kind === 'symbol' && expr.name === pattern.name;
            }
            if (repeatedContext) {
                return addRepeatedCapture(pattern.name, expr, captures);
            }
            return addSingleCapture(pattern.name, expr, captures);
        case 'list':
            return expr.kind === 'list'
                ? matchListPattern(pattern.items, expr.items, literals, captures, repeatedContext)
                : false;
    }
}
function matchListPattern(patternItems, exprItems, literals, captures, repeatedContext) {
    let patternIndex = 0;
    let exprIndex = 0;
    while (patternIndex < patternItems.length) {
        const currentPattern = patternItems[patternIndex];
        if (isEllipsisSymbol(currentPattern)) {
            return false;
        }
        if (isEllipsisSymbol(patternItems[patternIndex + 1])) {
            if (patternIndex + 2 !== patternItems.length) {
                throw new EvalError('syntax-rules only supports trailing ellipsis patterns');
            }
            ensureRepeatedCaptureSlots(currentPattern, literals, captures);
            while (exprIndex < exprItems.length) {
                if (!matchPattern(currentPattern, exprItems[exprIndex], literals, captures, true)) {
                    return false;
                }
                exprIndex += 1;
            }
            return true;
        }
        if (exprIndex >= exprItems.length) {
            return false;
        }
        if (!matchPattern(currentPattern, exprItems[exprIndex], literals, captures, repeatedContext)) {
            return false;
        }
        patternIndex += 1;
        exprIndex += 1;
    }
    return exprIndex === exprItems.length;
}
function addSingleCapture(name, expr, captures) {
    const existing = captures.get(name);
    if (existing === undefined) {
        captures.set(name, { kind: 'single', expr });
        return true;
    }
    return existing.kind === 'single' && sameExpr(existing.expr, expr);
}
function addRepeatedCapture(name, expr, captures) {
    const existing = captures.get(name);
    if (existing === undefined) {
        captures.set(name, { kind: 'repeat', exprs: [expr] });
        return true;
    }
    if (existing.kind !== 'repeat') {
        return false;
    }
    existing.exprs.push(expr);
    return true;
}
function ensureRepeatedCaptureSlots(pattern, literals, captures) {
    switch (pattern.kind) {
        case 'symbol':
            if (pattern.name === '_' || pattern.name === '...' || literals.has(pattern.name)) {
                return;
            }
            if (!captures.has(pattern.name)) {
                captures.set(pattern.name, { kind: 'repeat', exprs: [] });
                return;
            }
            if (captures.get(pattern.name)?.kind !== 'repeat') {
                throw new EvalError('syntax-rules pattern variable used inconsistently with ellipsis');
            }
            return;
        case 'list':
            pattern.items.forEach((item) => {
                if (!isEllipsisSymbol(item)) {
                    ensureRepeatedCaptureSlots(item, literals, captures);
                }
            });
            return;
        default:
            return;
    }
}
function sameExpr(left, right) {
    if (left.kind !== right.kind) {
        return false;
    }
    switch (left.kind) {
        case 'number':
            return right.kind === 'number' && num.numericEqual(left.value, right.value);
        case 'boolean':
            return right.kind === 'boolean' && left.value === right.value;
        case 'char':
            return right.kind === 'char' && left.value === right.value;
        case 'string':
            return right.kind === 'string' && left.value === right.value;
        case 'symbol':
            return right.kind === 'symbol' && left.name === right.name;
        case 'list':
            return (right.kind === 'list' &&
                left.items.length === right.items.length &&
                left.items.every((item, index) => sameExpr(item, right.items[index])));
    }
}
function expandTemplate(expr, ctx, repetitionIndex) {
    switch (expr.kind) {
        case 'number':
        case 'boolean':
        case 'char':
        case 'string':
            return cloneExpr(expr);
        case 'symbol':
            return expandTemplateSymbol(expr, ctx, repetitionIndex);
        case 'list':
            return expandTemplateList(expr, ctx, repetitionIndex);
    }
}
function expandTemplateSymbol(expr, ctx, repetitionIndex) {
    const capture = ctx.captures.get(expr.name);
    if (capture !== undefined) {
        return cloneExpr(resolveCapture(capture, repetitionIndex));
    }
    const renamed = ctx.renameMap.get(expr.name);
    if (renamed !== undefined) {
        return { kind: 'symbol', name: renamed, position: expr.position };
    }
    if (expr.name === '...' || expr.name === ctx.macro.name || ctx.macro.literals.has(expr.name) || CORE_SYNTAX.has(expr.name)) {
        return cloneExpr(expr);
    }
    const capturedName = captureDefinitionIdentifier(expr.name, ctx);
    if (capturedName !== undefined) {
        return { kind: 'symbol', name: capturedName, position: expr.position };
    }
    return cloneExpr(expr);
}
function expandTemplateList(expr, ctx, repetitionIndex) {
    const [headExpr] = expr.items;
    if (headExpr?.kind === 'symbol' && headExpr.name === 'quote') {
        return cloneExpr(expr);
    }
    if (headExpr?.kind === 'symbol' && headExpr.name === 'let') {
        return expandLetTemplate(expr, ctx, repetitionIndex);
    }
    return {
        kind: 'list',
        items: expandTemplateSequence(expr.items, ctx, repetitionIndex),
        position: expr.position,
    };
}
function expandTemplateSequence(items, ctx, repetitionIndex) {
    const expanded = [];
    for (let index = 0; index < items.length; index += 1) {
        const item = items[index];
        if (isEllipsisSymbol(item)) {
            throw new EvalError('unexpected ellipsis in syntax-rules template');
        }
        if (isEllipsisSymbol(items[index + 1])) {
            const repeatCount = templateRepetitionCount(item, ctx.captures);
            for (let repeatIndex = 0; repeatIndex < repeatCount; repeatIndex += 1) {
                expanded.push(expandTemplate(item, ctx, repeatIndex));
            }
            index += 1;
            continue;
        }
        expanded.push(expandTemplate(item, ctx, repetitionIndex));
    }
    return expanded;
}
function expandLetTemplate(expr, ctx, repetitionIndex) {
    if (expr.items.length < 3) {
        return {
            kind: 'list',
            items: expandTemplateSequence(expr.items, ctx, repetitionIndex),
            position: expr.position,
        };
    }
    const bindingsExpr = expr.items[1];
    if (bindingsExpr.kind !== 'list') {
        return {
            kind: 'list',
            items: expandTemplateSequence(expr.items, ctx, repetitionIndex),
            position: expr.position,
        };
    }
    const scopedRenameMap = new Map(ctx.renameMap);
    const expandedBindings = bindingsExpr.items.map((bindingExpr) => {
        if (bindingExpr.kind !== 'list' || bindingExpr.items.length !== 2) {
            throw new EvalError('syntax-rules let template expects binding pairs');
        }
        const [nameExpr, initExpr] = bindingExpr.items;
        if (nameExpr.kind !== 'symbol') {
            throw new EvalError('syntax-rules let template expects symbol bindings');
        }
        let expandedName;
        const capture = ctx.captures.get(nameExpr.name);
        if (capture !== undefined) {
            expandedName = cloneExpr(resolveCapture(capture, repetitionIndex));
        }
        else {
            const renamedName = freshMacroIdentifier(nameExpr.name);
            scopedRenameMap.set(nameExpr.name, renamedName);
            expandedName = { kind: 'symbol', name: renamedName, position: nameExpr.position };
        }
        return {
            kind: 'list',
            items: [expandedName, expandTemplate(initExpr, ctx, repetitionIndex)],
            position: bindingExpr.position,
        };
    });
    const bodyContext = {
        ...ctx,
        renameMap: scopedRenameMap,
    };
    return {
        kind: 'list',
        items: [
            cloneExpr(expr.items[0]),
            { kind: 'list', items: expandedBindings, position: bindingsExpr.position },
            ...expandTemplateSequence(expr.items.slice(2), bodyContext, repetitionIndex),
        ],
        position: expr.position,
    };
}
function resolveCapture(capture, repetitionIndex) {
    if (capture.kind === 'single') {
        return capture.expr;
    }
    if (repetitionIndex === undefined) {
        throw new EvalError('syntax-rules template expected ellipsis for repeated pattern variable');
    }
    const value = capture.exprs[repetitionIndex];
    if (value === undefined) {
        throw new EvalError('syntax-rules ellipsis repetition mismatch');
    }
    return value;
}
function templateRepetitionCount(template, captures) {
    const repeatedNames = [...collectRepeatedCaptureNames(template, captures)];
    if (repeatedNames.length === 0) {
        throw new EvalError('syntax-rules ellipsis requires a repeated pattern variable');
    }
    const count = repeatedCaptureLength(captures.get(repeatedNames[0]));
    for (const name of repeatedNames.slice(1)) {
        if (repeatedCaptureLength(captures.get(name)) !== count) {
            throw new EvalError('syntax-rules ellipsis groups must repeat in lockstep');
        }
    }
    return count;
}
function collectRepeatedCaptureNames(template, captures, names = new Set()) {
    switch (template.kind) {
        case 'symbol': {
            const capture = captures.get(template.name);
            if (capture?.kind === 'repeat') {
                names.add(template.name);
            }
            return names;
        }
        case 'list':
            if (template.items[0]?.kind === 'symbol' && template.items[0].name === 'quote') {
                return names;
            }
            template.items.forEach((item) => {
                collectRepeatedCaptureNames(item, captures, names);
            });
            return names;
        default:
            return names;
    }
}
function repeatedCaptureLength(capture) {
    return capture.kind === 'repeat' ? capture.exprs.length : 0;
}
function captureDefinitionIdentifier(name, ctx) {
    const cached = ctx.capturedFreeNames.get(name);
    if (cached !== undefined) {
        return cached;
    }
    const cell = ctx.macro.definitionEnv.lookupCell(name);
    if (cell === undefined) {
        return undefined;
    }
    const alias = freshMacroIdentifier(name);
    ctx.macro.definitionEnv.defineAlias(alias, cell);
    ctx.capturedFreeNames.set(name, alias);
    return alias;
}
function createBuiltins(output, macroEnv) {
    return new Map([
        builtin('+', (args) => sumNumbers('+', args)),
        builtin('*', (args) => productNumbers('*', args)),
        builtin('-', (args) => subtractNumbers(args)),
        builtin('/', (args) => divideNumbers(args)),
        builtin('<', (args) => compareNumbers('<', args, (left, right) => num.numericCompare(left, right) < 0)),
        builtin('>', (args) => compareNumbers('>', args, (left, right) => num.numericCompare(left, right) > 0)),
        builtin('=', (args) => compareNumbers('=', args, (left, right) => num.numericEqual(left, right))),
        builtin('<=', (args) => compareNumbers('<=', args, (left, right) => num.numericCompare(left, right) <= 0)),
        builtin('>=', (args) => compareNumbers('>=', args, (left, right) => num.numericCompare(left, right) >= 0)),
        builtin('abs', (args) => absoluteValue(args)),
        builtin('apply', (args, position, k) => applyBuiltin(args, position, macroEnv, k)),
        builtin('append', (args) => appendValues(args)),
        builtin('assoc', (args) => assocBuiltin(args)),
        builtin('assv', (args) => assvBuiltin(args)),
        builtin('boolean?', (args) => unaryPredicate('boolean?', args, (value) => typeof value === 'boolean')),
        builtin('car', (args) => {
            assertExactArity('car', args, 1);
            return expectPair('car', args[0]).car;
        }),
        builtin('cdr', (args) => {
            assertExactArity('cdr', args, 1);
            return expectPair('cdr', args[0]).cdr;
        }),
        builtin('cddr', (args) => {
            assertExactArity('cddr', args, 1);
            return expectPair('cddr', expectPair('cddr', args[0]).cdr).cdr;
        }),
        builtin('call-with-current-continuation', (args, position, k) => callCcBuiltin(args, position, k)),
        builtin('call/cc', (args, position, k) => callCcBuiltin(args, position, k)),
        builtin('char-alphabetic?', (args) => {
            assertExactArity('char-alphabetic?', args, 1);
            return isAlphabeticChar(expectCharValue('char-alphabetic?', args[0]).value);
        }),
        builtin('char-downcase', (args) => {
            assertExactArity('char-downcase', args, 1);
            return { kind: 'char', value: expectCharValue('char-downcase', args[0]).value.toLowerCase() };
        }),
        builtin('char->integer', (args) => {
            assertExactArity('char->integer', args, 1);
            return num.exactIntegerFromNumber(expectCharValue('char->integer', args[0]).value.codePointAt(0));
        }),
        builtin('char-numeric?', (args) => {
            assertExactArity('char-numeric?', args, 1);
            return isNumericChar(expectCharValue('char-numeric?', args[0]).value);
        }),
        builtin('char<?', (args) => compareChars('char<?', args, (left, right) => left < right)),
        builtin('char=?', (args) => compareChars('char=?', args, (left, right) => left === right)),
        builtin('char-upcase', (args) => {
            assertExactArity('char-upcase', args, 1);
            return { kind: 'char', value: expectCharValue('char-upcase', args[0]).value.toUpperCase() };
        }),
        builtin('char?', (args) => unaryPredicate('char?', args, isCharValue)),
        builtin('cons', (args) => {
            assertExactArity('cons', args, 2);
            return { kind: 'pair', car: args[0], cdr: args[1] };
        }),
        builtin('display', (args) => {
            assertExactArity('display', args, 1);
            output.write(formatDisplayValue(args[0]));
            return VOID;
        }),
        builtin('eq?', (args) => {
            assertExactArity('eq?', args, 2);
            return eqValues(args[0], args[1]);
        }),
        builtin('eqv?', (args) => {
            assertExactArity('eqv?', args, 2);
            return eqvValues(args[0], args[1]);
        }),
        builtin('equal?', (args) => {
            assertExactArity('equal?', args, 2);
            return equalValues(args[0], args[1]);
        }),
        builtin('denominator', (args) => {
            assertExactArity('denominator', args, 1);
            return num.denominatorOf(expectNumberValue('denominator', args[0]));
        }),
        builtin('even?', (args) => integerPredicate('even?', args, (value) => num.numericIsEven(value))),
        builtin('exact->inexact', (args) => {
            assertExactArity('exact->inexact', args, 1);
            return num.exactToInexact(expectNumberValue('exact->inexact', args[0]));
        }),
        builtin('exact?', (args) => unaryPredicate('exact?', args, (value) => num.isNumericValue(value) && num.isExactNumeric(value))),
        builtin('expt', (args) => exptNumbers(args)),
        builtin('for-each', (args, position, k) => forEachBuiltin(args, position, macroEnv, k)),
        builtin('gcd', (args) => gcdBuiltin(args)),
        builtin('inexact->exact', (args) => {
            assertExactArity('inexact->exact', args, 1);
            return num.inexactToExact(expectNumberValue('inexact->exact', args[0]));
        }),
        builtin('inexact?', (args) => unaryPredicate('inexact?', args, (value) => num.isNumericValue(value) && num.isInexactNumeric(value))),
        builtin('integer->char', (args) => {
            assertExactArity('integer->char', args, 1);
            return { kind: 'char', value: String.fromCodePoint(expectCodePoint('integer->char', args[0])) };
        }),
        builtin('integer?', (args) => unaryPredicate('integer?', args, (value) => num.isNumericValue(value) && num.numericIsInteger(value))),
        builtin('length', (args) => {
            assertExactArity('length', args, 1);
            return num.exactIntegerFromNumber(expectProperList('length', args[0]).length);
        }),
        builtin('list', (args) => makeList(args)),
        builtin('list-ref', (args) => listRefBuiltin(args)),
        builtin('list-tail', (args) => listTailBuiltin(args)),
        builtin('list->string', (args) => {
            assertExactArity('list->string', args, 1);
            return makeRuntimeString(expectProperList('list->string', args[0])
                .map((item) => expectCharValue('list->string', item).value)
                .join(''));
        }),
        builtin('list->vector', (args) => {
            assertExactArity('list->vector', args, 1);
            return { kind: 'vector', items: expectProperList('list->vector', args[0]) };
        }),
        builtin('list?', (args) => {
            assertExactArity('list?', args, 1);
            return isProperListValue(args[0]);
        }),
        builtin('lcm', (args) => lcmBuiltin(args)),
        builtin('make-string', (args) => makeStringBuiltin(args)),
        builtin('make-vector', (args) => makeVectorBuiltin(args)),
        builtin('map', (args, position, k) => mapBuiltin(args, position, macroEnv, k)),
        builtin('max', (args) => extremum('max', args, (left, right) => (num.numericCompare(left, right) >= 0 ? left : right))),
        builtin('member', (args) => memberBuiltin(args)),
        builtin('min', (args) => extremum('min', args, (left, right) => (num.numericCompare(left, right) <= 0 ? left : right))),
        builtin('modulo', (args) => moduloNumbers(args)),
        builtin('newline', (args) => {
            assertExactArity('newline', args, 0);
            output.write('\n');
            return VOID;
        }),
        builtin('negative?', (args) => numberPredicate('negative?', args, (value) => num.numericIsNegative(value))),
        builtin('not', (args) => {
            assertExactArity('not', args, 1);
            return !isTruthy(args[0]);
        }),
        builtin('null?', (args) => unaryPredicate('null?', args, isEmptyList)),
        builtin('number->string', (args) => {
            assertExactArity('number->string', args, 1);
            return makeRuntimeString(num.formatNumber(expectNumberValue('number->string', args[0])));
        }),
        builtin('number?', (args) => unaryPredicate('number?', args, (value) => num.isNumericValue(value))),
        builtin('numerator', (args) => {
            assertExactArity('numerator', args, 1);
            return num.numeratorOf(expectNumberValue('numerator', args[0]));
        }),
        builtin('odd?', (args) => integerPredicate('odd?', args, (value) => num.numericIsOdd(value))),
        builtin('pair?', (args) => unaryPredicate('pair?', args, isPair)),
        builtin('positive?', (args) => numberPredicate('positive?', args, (value) => num.numericIsPositive(value))),
        builtin('procedure?', (args) => unaryPredicate('procedure?', args, isProcedure)),
        builtin('quotient', (args) => quotientNumbers(args)),
        builtin('rational?', (args) => unaryPredicate('rational?', args, (value) => num.isNumericValue(value))),
        builtin('remainder', (args) => remainderNumbers(args)),
        builtin('round', (args) => roundBuiltin(args)),
        builtin('reverse', (args) => {
            assertExactArity('reverse', args, 1);
            return makeList(expectProperList('reverse', args[0]).slice().reverse());
        }),
        builtin('string', (args) => stringBuiltin(args)),
        builtin('string->number', (args) => {
            assertExactArity('string->number', args, 1);
            return num.parseStringNumber(expectStringValue('string->number', args[0]));
        }),
        builtin('string->list', (args) => {
            assertExactArity('string->list', args, 1);
            return makeList(stringChars(expectStringValue('string->list', args[0])).map((char) => ({ kind: 'char', value: char })));
        }),
        builtin('string->symbol', (args) => {
            assertExactArity('string->symbol', args, 1);
            return { kind: 'symbol-value', name: expectStringValue('string->symbol', args[0]) };
        }),
        builtin('string-append', (args) => makeRuntimeString(args.map((arg) => expectStringValue('string-append', arg)).join(''))),
        builtin('string-copy', (args) => {
            assertExactArity('string-copy', args, 1);
            return makeRuntimeString(expectStringValue('string-copy', args[0]));
        }),
        builtin('string-ci=?', (args) => compareStrings('string-ci=?', args, (value) => value.toLowerCase(), (left, right) => left === right)),
        builtin('string-downcase', (args) => {
            assertExactArity('string-downcase', args, 1);
            return makeRuntimeString(expectStringValue('string-downcase', args[0]).toLowerCase());
        }),
        builtin('string>?', (args) => compareStrings('string>?', args, (value) => value, (left, right) => left > right)),
        builtin('string<=?', (args) => compareStrings('string<=?', args, (value) => value, (left, right) => left <= right)),
        builtin('string>=?', (args) => compareStrings('string>=?', args, (value) => value, (left, right) => left >= right)),
        builtin('string<?', (args) => compareStrings('string<?', args, (value) => value, (left, right) => left < right)),
        builtin('string=?', (args) => compareStrings('string=?', args, (value) => value, (left, right) => left === right)),
        builtin('string-length', (args) => {
            assertExactArity('string-length', args, 1);
            return num.exactIntegerFromNumber(stringChars(expectStringValue('string-length', args[0])).length);
        }),
        builtin('string-ref', (args) => {
            assertExactArity('string-ref', args, 2);
            const chars = stringChars(expectStringValue('string-ref', args[0]));
            const index = expectIndex('string-ref', args[1]);
            if (index >= chars.length) {
                throw new EvalError('string-ref index out of bounds');
            }
            return { kind: 'char', value: chars[index] };
        }),
        builtin('string-set!', (args) => {
            assertExactArity('string-set!', args, 3);
            if (stringsAreImmutable()) {
                throw new EvalError('string-set! cannot mutate immutable strings');
            }
            const stringValue = expectMutableStringValue('string-set!', args[0]);
            const index = expectIndex('string-set!', args[1]);
            const charValue = expectCharValue('string-set!', args[2]);
            if (index >= stringValue.chars.length) {
                throw new EvalError('string-set! index out of bounds');
            }
            stringValue.chars[index] = charValue.value;
            return VOID;
        }),
        builtin('string?', (args) => unaryPredicate('string?', args, isStringValue)),
        builtin('string-upcase', (args) => {
            assertExactArity('string-upcase', args, 1);
            return makeRuntimeString(expectStringValue('string-upcase', args[0]).toUpperCase());
        }),
        builtin('substring', (args) => {
            assertExactArity('substring', args, 3);
            const chars = stringChars(expectStringValue('substring', args[0]));
            const start = expectIndex('substring', args[1]);
            const end = expectIndex('substring', args[2]);
            if (start > end || end > chars.length) {
                throw new EvalError('substring expects valid start/end indices');
            }
            return makeRuntimeString(chars.slice(start, end).join(''));
        }),
        builtin('symbol->string', (args) => {
            assertExactArity('symbol->string', args, 1);
            return makeRuntimeString(expectSymbolValue('symbol->string', args[0]).name);
        }),
        builtin('symbol?', (args) => unaryPredicate('symbol?', args, isSymbolValue)),
        builtin('truncate', (args) => truncateBuiltin(args)),
        builtin('set-car!', (args) => {
            assertExactArity('set-car!', args, 2);
            expectPair('set-car!', args[0]).car = args[1];
            return VOID;
        }),
        builtin('set-cdr!', (args) => {
            assertExactArity('set-cdr!', args, 2);
            expectPair('set-cdr!', args[0]).cdr = args[1];
            return VOID;
        }),
        builtin('vector', (args) => ({ kind: 'vector', items: [...args] })),
        builtin('vector->list', (args) => {
            assertExactArity('vector->list', args, 1);
            return makeList(expectVectorValue('vector->list', args[0]).items);
        }),
        builtin('vector-length', (args) => {
            assertExactArity('vector-length', args, 1);
            return num.exactIntegerFromNumber(expectVectorValue('vector-length', args[0]).items.length);
        }),
        builtin('vector-ref', (args) => {
            assertExactArity('vector-ref', args, 2);
            const vector = expectVectorValue('vector-ref', args[0]);
            const index = expectIndex('vector-ref', args[1]);
            if (index >= vector.items.length) {
                throw new EvalError('vector-ref index out of bounds');
            }
            return vector.items[index];
        }),
        builtin('vector-set!', (args) => {
            assertExactArity('vector-set!', args, 3);
            const vector = expectVectorValue('vector-set!', args[0]);
            const index = expectIndex('vector-set!', args[1]);
            if (index >= vector.items.length) {
                throw new EvalError('vector-set! index out of bounds');
            }
            vector.items[index] = args[2];
            return VOID;
        }),
        builtin('vector?', (args) => unaryPredicate('vector?', args, isVectorValue)),
        builtin('write', (args) => {
            assertExactArity('write', args, 1);
            output.write(formatValue(args[0]));
            return VOID;
        }),
        builtin('zero?', (args) => numberPredicate('zero?', args, (value) => num.numericIsZero(value))),
    ]);
}
/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input) {
    return evalStrWithOutput(input).result;
}
/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input) {
    const program = new Parser(input).parseProgram();
    if (program.length === 0) {
        throw new EvalError('empty input', { line: 1, column: 1 });
    }
    const output = new OutputBuffer();
    const macroEnv = new MacroEnv();
    const env = createGlobalEnv(output, macroEnv);
    const lastValue = evalSequence(program, env, macroEnv);
    return {
        result: formatValue(lastValue),
        output: output.toString(),
    };
}
function createGlobalEnv(output, macroEnv) {
    const env = new Env();
    for (const [name, value] of createBuiltins(output, macroEnv)) {
        env.define(name, value);
    }
    return env;
}
function doneStep(value) {
    return { kind: 'done-step', value };
}
function isMachineStep(value) {
    return (typeof value === 'object' &&
        value !== null &&
        (value.kind === 'eval-step' || value.kind === 'apply-step' || value.kind === 'done-step'));
}
function runMachine(step, macroEnv) {
    let current = step;
    while (true) {
        switch (current.kind) {
            case 'done-step':
                return current.value;
            case 'eval-step': {
                const evalStep = current;
                try {
                    current = evalExprStep(evalStep.expr, evalStep.env, macroEnv, evalStep.k);
                }
                catch (error) {
                    throw attachPosition(error, evalStep.expr.position);
                }
                break;
            }
            case 'apply-step': {
                const applyStep = current;
                try {
                    current = applyProcedure(applyStep.proc, applyStep.args, applyStep.position, macroEnv, applyStep.k);
                }
                catch (error) {
                    throw attachPosition(error, applyStep.position);
                }
                break;
            }
        }
    }
}
function evalSequence(exprs, env, macroEnv) {
    return runMachine(evalSequenceStep(exprs, env, macroEnv, doneStep), macroEnv);
}
function evalExpr(expr, env, macroEnv) {
    return runMachine({ kind: 'eval-step', expr, env, k: doneStep }, macroEnv);
}
function evalSequenceStep(exprs, env, macroEnv, k) {
    if (exprs.length === 0) {
        return k(VOID);
    }
    const [firstExpr, ...restExprs] = exprs;
    if (restExprs.length === 0) {
        return { kind: 'eval-step', expr: firstExpr, env, k };
    }
    return {
        kind: 'eval-step',
        expr: firstExpr,
        env,
        k: () => evalSequenceStep(restExprs, env, macroEnv, k),
    };
}
function evalExprsStep(exprs, env, macroEnv, k, values = []) {
    if (exprs.length === 0) {
        return k([...values]);
    }
    const [firstExpr, ...restExprs] = exprs;
    return {
        kind: 'eval-step',
        expr: firstExpr,
        env,
        k: (value) => evalExprsStep(restExprs, env, macroEnv, k, [...values, value]),
    };
}
function evalCallArgsStep(exprs, env, macroEnv, k, values = []) {
    if (exprs.length === 0) {
        return k([...values]);
    }
    const lastIndex = exprs.length - 1;
    const currentExpr = exprs[lastIndex];
    return {
        kind: 'eval-step',
        expr: currentExpr,
        env,
        k: (value) => evalCallArgsStep(exprs.slice(0, lastIndex), env, macroEnv, k, [value, ...values]),
    };
}
function evalExprStep(expr, env, macroEnv, k) {
    switch (expr.kind) {
        case 'number':
        case 'boolean':
            return k(expr.value);
        case 'string':
            return k(makeRuntimeString(expr.value));
        case 'char':
            return k({ kind: 'char', value: expr.value });
        case 'symbol':
            return k(env.lookup(expr.name));
        case 'list':
            return evalListStep(expr, env, macroEnv, k);
    }
}
function evalListStep(expr, env, macroEnv, k) {
    const { items } = expr;
    if (items.length === 0) {
        throw new EvalError('cannot evaluate empty list');
    }
    const first = items[0];
    if (first.kind === 'symbol') {
        switch (first.name) {
            case 'and':
                return evalAndStep(items.slice(1), env, macroEnv, k);
            case 'begin':
                return evalSequenceStep(items.slice(1), env, macroEnv, k);
            case 'case':
                return evalCaseStep(items.slice(1), env, macroEnv, k);
            case 'case-lambda':
                return evalCaseLambdaStep(items.slice(1), env, k);
            case 'cond':
                return evalCondStep(items.slice(1), env, macroEnv, k);
            case 'define':
                return evalDefineStep(items.slice(1), env, macroEnv, k);
            case 'define-record-type':
                return k(evalDefineRecordType(items.slice(1), env));
            case 'define-syntax':
                return evalDefineSyntaxStep(items.slice(1), env, macroEnv, k);
            case 'do':
                return evalDoStep(items.slice(1), env, macroEnv, k);
            case 'if':
                return evalIfStep(items.slice(1), env, macroEnv, k);
            case 'lambda':
                return evalLambdaStep(items.slice(1), env, k);
            case 'let':
                return evalLetStep(items.slice(1), env, macroEnv, k);
            case 'let*':
                return evalLetStarStep(items.slice(1), env, macroEnv, k);
            case 'letrec':
                return evalLetRecStep(items.slice(1), env, macroEnv, false, k);
            case 'letrec*':
                return evalLetRecStep(items.slice(1), env, macroEnv, true, k);
            case 'or':
                return evalOrStep(items.slice(1), env, macroEnv, k);
            case 'quote':
                return evalQuoteStep(items.slice(1), k);
            case 'set!':
                return evalSetStep(items.slice(1), env, macroEnv, k);
        }
        const macro = macroEnv.lookup(first.name);
        if (macro !== undefined) {
            return {
                kind: 'eval-step',
                expr: expandMacroInvocation(macro, expr),
                env,
                k,
            };
        }
    }
    return {
        kind: 'eval-step',
        expr: first,
        env,
        k: (proc) => {
            if (!isProcedure(proc)) {
                throw new EvalError('attempted to call a non-procedure');
            }
            return evalCallArgsStep(items.slice(1), env, macroEnv, (args) => ({
                kind: 'apply-step',
                proc,
                args,
                position: first.position,
                k,
            }));
        },
    };
}
function evalAndStep(args, env, macroEnv, k) {
    if (args.length === 0) {
        return k(true);
    }
    const [firstExpr, ...restExprs] = args;
    if (restExprs.length === 0) {
        return { kind: 'eval-step', expr: firstExpr, env, k };
    }
    return {
        kind: 'eval-step',
        expr: firstExpr,
        env,
        k: (value) => (!isTruthy(value) ? k(value) : evalAndStep(restExprs, env, macroEnv, k)),
    };
}
function evalCaseStep(args, env, macroEnv, k) {
    assertAtLeastArity('case', args, 1);
    return {
        kind: 'eval-step',
        expr: args[0],
        env,
        k: (key) => evalCaseClausesStep(key, args.slice(1), env, macroEnv, k),
    };
}
function evalCaseClausesStep(key, clauses, env, macroEnv, k) {
    for (let index = 0; index < clauses.length; index += 1) {
        const clauseExpr = clauses[index];
        if (clauseExpr.kind !== 'list' || clauseExpr.items.length === 0) {
            throw new EvalError('case expects non-empty clauses');
        }
        const [headExpr, ...body] = clauseExpr.items;
        if (headExpr.kind === 'symbol' && headExpr.name === 'else') {
            if (index !== clauses.length - 1) {
                throw new EvalError('case else clause must be last');
            }
            return evalSequenceStep(body, env, macroEnv, k);
        }
        if (headExpr.kind !== 'list') {
            throw new EvalError('case expects each clause datum list to be a list');
        }
        if (headExpr.items.some((datumExpr) => eqvValues(key, quoteExpr(datumExpr)))) {
            return evalSequenceStep(body, env, macroEnv, k);
        }
    }
    return k(VOID);
}
function evalCondStep(clauses, env, macroEnv, k) {
    if (clauses.length === 0) {
        return k(VOID);
    }
    const [clauseExpr, ...restClauses] = clauses;
    if (clauseExpr.kind !== 'list' || clauseExpr.items.length === 0) {
        throw new EvalError('cond expects non-empty clauses');
    }
    const [testExpr, ...body] = clauseExpr.items;
    if (testExpr.kind === 'symbol' && testExpr.name === 'else') {
        if (restClauses.length !== 0) {
            throw new EvalError('cond else clause must be last');
        }
        return evalSequenceStep(body, env, macroEnv, k);
    }
    return {
        kind: 'eval-step',
        expr: testExpr,
        env,
        k: (testValue) => isTruthy(testValue)
            ? body.length === 0
                ? k(testValue)
                : evalSequenceStep(body, env, macroEnv, k)
            : evalCondStep(restClauses, env, macroEnv, k),
    };
}
function evalOrStep(args, env, macroEnv, k) {
    if (args.length === 0) {
        return k(false);
    }
    const [firstExpr, ...restExprs] = args;
    if (restExprs.length === 0) {
        return { kind: 'eval-step', expr: firstExpr, env, k };
    }
    return {
        kind: 'eval-step',
        expr: firstExpr,
        env,
        k: (value) => (isTruthy(value) ? k(value) : evalOrStep(restExprs, env, macroEnv, k)),
    };
}
function evalDefineStep(args, env, macroEnv, k) {
    assertAtLeastArity('define', args, 2);
    const target = args[0];
    const body = args.slice(1);
    if (target.kind === 'symbol') {
        assertExactArity('define', body, 1);
        return {
            kind: 'eval-step',
            expr: body[0],
            env,
            k: (value) => {
                env.define(target.name, value);
                return k(VOID);
            },
        };
    }
    if (target.kind === 'list' && target.items.length > 0) {
        const nameExpr = target.items[0];
        if (nameExpr.kind !== 'symbol') {
            throw new EvalError('define expects a symbol name');
        }
        assertAtLeastArity('define', body, 1);
        env.define(nameExpr.name, {
            kind: 'lambda',
            name: nameExpr.name,
            params: parseParamItems(target.items.slice(1)),
            body,
            env,
        });
        return k(VOID);
    }
    throw new EvalError('define expects a symbol name');
}
function evalDefineSyntaxStep(args, env, macroEnv, k) {
    assertExactArity('define-syntax', args, 2);
    const nameExpr = args[0];
    if (nameExpr.kind !== 'symbol') {
        throw new EvalError('define-syntax expects a symbol name');
    }
    macroEnv.define(nameExpr.name, parseSyntaxRules(nameExpr.name, args[1], env));
    return k(VOID);
}
function evalDefineRecordType(args, env) {
    assertAtLeastArity('define-record-type', args, 3);
    const recordTypeName = expectSymbolExprName('define-record-type', args[0], 'type name');
    const constructorSpec = parseRecordConstructorSpec(args[1]);
    const predicateName = expectSymbolExprName('define-record-type', args[2], 'predicate name');
    const fieldSpecs = args.slice(3).map((fieldExpr) => parseRecordFieldSpec(fieldExpr));
    const fieldIndices = new Map();
    fieldSpecs.forEach((fieldSpec, index) => {
        if (fieldIndices.has(fieldSpec.name)) {
            throw new EvalError('define-record-type field names must be unique');
        }
        fieldIndices.set(fieldSpec.name, index);
    });
    const constructorFieldIndices = constructorSpec.fields.map((fieldName) => {
        const fieldIndex = fieldIndices.get(fieldName);
        if (fieldIndex === undefined) {
            throw new EvalError(`define-record-type constructor field not found: ${fieldName}`);
        }
        return fieldIndex;
    });
    const recordType = {
        name: recordTypeName,
        fieldNames: fieldSpecs.map((fieldSpec) => fieldSpec.name),
    };
    env.define(constructorSpec.name, {
        kind: 'builtin',
        name: constructorSpec.name,
        apply: (constructorArgs) => {
            assertExactArity(constructorSpec.name, constructorArgs, constructorSpec.fields.length);
            const fields = fieldSpecs.map(() => VOID);
            constructorFieldIndices.forEach((fieldIndex, argIndex) => {
                fields[fieldIndex] = constructorArgs[argIndex];
            });
            return { kind: 'record', recordType, fields };
        },
    });
    env.define(predicateName, {
        kind: 'builtin',
        name: predicateName,
        apply: (predicateArgs) => {
            assertExactArity(predicateName, predicateArgs, 1);
            return isRecordValue(predicateArgs[0]) && predicateArgs[0].recordType === recordType;
        },
    });
    fieldSpecs.forEach((fieldSpec, index) => {
        env.define(fieldSpec.accessor, {
            kind: 'builtin',
            name: fieldSpec.accessor,
            apply: (accessorArgs) => {
                assertExactArity(fieldSpec.accessor, accessorArgs, 1);
                return expectRecordValue(fieldSpec.accessor, accessorArgs[0], recordType).fields[index];
            },
        });
        if (fieldSpec.mutator !== undefined) {
            env.define(fieldSpec.mutator, {
                kind: 'builtin',
                name: fieldSpec.mutator,
                apply: (mutatorArgs) => {
                    assertExactArity(fieldSpec.mutator, mutatorArgs, 2);
                    expectRecordValue(fieldSpec.mutator, mutatorArgs[0], recordType).fields[index] = mutatorArgs[1];
                    return VOID;
                },
            });
        }
    });
    return VOID;
}
function evalIfStep(args, env, macroEnv, k) {
    if (args.length !== 2 && args.length !== 3) {
        throw new EvalError('if expects 2 or 3 argument(s)');
    }
    const [conditionExpr, thenExpr, elseExpr] = args;
    return {
        kind: 'eval-step',
        expr: conditionExpr,
        env,
        k: (conditionValue) => isTruthy(conditionValue)
            ? { kind: 'eval-step', expr: thenExpr, env, k }
            : elseExpr === undefined
                ? k(VOID)
                : { kind: 'eval-step', expr: elseExpr, env, k },
    };
}
function evalCaseLambdaStep(args, env, k) {
    assertAtLeastArity('case-lambda', args, 1);
    return k({
        kind: 'case-lambda',
        clauses: args.map((clauseExpr) => parseCaseLambdaClause(clauseExpr)),
        env,
    });
}
function evalLambdaStep(args, env, k) {
    assertAtLeastArity('lambda', args, 2);
    const paramsExpr = args[0];
    return k({
        kind: 'lambda',
        params: parseFormals(paramsExpr),
        body: args.slice(1),
        env,
    });
}
function evalLetStep(args, env, macroEnv, k) {
    assertAtLeastArity('let', args, 2);
    const firstArg = args[0];
    if (firstArg.kind === 'symbol') {
        assertAtLeastArity('let', args, 3);
        return evalNamedLetStep(firstArg.name, args[1], args.slice(2), env, macroEnv, k);
    }
    const bindings = parseBindings(firstArg);
    const body = args.slice(1);
    return evalExprsStep(bindings.map((binding) => binding.init), env, macroEnv, (values) => {
        const letEnv = new Env(env);
        bindings.forEach((binding, index) => {
            letEnv.define(binding.name, values[index]);
        });
        return evalSequenceStep(body, letEnv, macroEnv, k);
    });
}
function evalLetStarStep(args, env, macroEnv, k) {
    assertAtLeastArity('let*', args, 2);
    const bindings = parseBindings(args[0], 'let*');
    const body = args.slice(1);
    const letStarEnv = new Env(env);
    return evalLetStarBindingsStep(bindings, 0, letStarEnv, body, macroEnv, k);
}
function evalLetStarBindingsStep(bindings, index, letStarEnv, body, macroEnv, k) {
    if (index >= bindings.length) {
        return evalSequenceStep(body, letStarEnv, macroEnv, k);
    }
    const binding = bindings[index];
    return {
        kind: 'eval-step',
        expr: binding.init,
        env: letStarEnv,
        k: (value) => {
            letStarEnv.define(binding.name, value);
            return evalLetStarBindingsStep(bindings, index + 1, letStarEnv, body, macroEnv, k);
        },
    };
}
function evalLetRecStep(args, env, macroEnv, sequential, k) {
    const name = sequential ? 'letrec*' : 'letrec';
    assertAtLeastArity(name, args, 2);
    const bindings = parseBindings(args[0], name);
    const body = args.slice(1);
    const letEnv = new Env(env);
    bindings.forEach((binding) => {
        letEnv.defineUninitialized(binding.name);
    });
    if (sequential) {
        return evalLetRecSequentialStep(bindings, 0, letEnv, body, macroEnv, k);
    }
    return evalExprsStep(bindings.map((binding) => binding.init), letEnv, macroEnv, (values) => {
        bindings.forEach((binding, index) => {
            letEnv.set(binding.name, values[index]);
        });
        return evalSequenceStep(body, letEnv, macroEnv, k);
    });
}
function evalLetRecSequentialStep(bindings, index, letEnv, body, macroEnv, k) {
    if (index >= bindings.length) {
        return evalSequenceStep(body, letEnv, macroEnv, k);
    }
    const binding = bindings[index];
    return {
        kind: 'eval-step',
        expr: binding.init,
        env: letEnv,
        k: (value) => {
            letEnv.set(binding.name, value);
            return evalLetRecSequentialStep(bindings, index + 1, letEnv, body, macroEnv, k);
        },
    };
}
function evalNamedLetStep(name, bindingsExpr, body, env, macroEnv, k) {
    const bindings = parseBindings(bindingsExpr);
    return evalExprsStep(bindings.map((binding) => binding.init), env, macroEnv, (values) => {
        const letEnv = new Env(env);
        const proc = {
            kind: 'lambda',
            name,
            params: { required: bindings.map((binding) => binding.name) },
            body,
            env: letEnv,
        };
        letEnv.define(name, proc);
        return {
            kind: 'apply-step',
            proc,
            args: values,
            position: bindingsExpr.position,
            k,
        };
    });
}
function evalDoStep(args, env, macroEnv, k) {
    assertAtLeastArity('do', args, 2);
    const bindings = parseDoBindings(args[0]);
    const testClause = parseDoTestClause(args[1]);
    const body = args.slice(2);
    return evalExprsStep(bindings.map((binding) => binding.init), env, macroEnv, (initialValues) => {
        const doEnv = new Env(env);
        bindings.forEach((binding, index) => {
            doEnv.define(binding.name, initialValues[index]);
        });
        return evalDoLoopStep(bindings, testClause, body, doEnv, macroEnv, k);
    });
}
function evalDoLoopStep(bindings, testClause, body, doEnv, macroEnv, k) {
    return {
        kind: 'eval-step',
        expr: testClause.test,
        env: doEnv,
        k: (testValue) => isTruthy(testValue)
            ? evalSequenceStep(testClause.results, doEnv, macroEnv, k)
            : evalSequenceStep(body, doEnv, macroEnv, () => evalDoNextValuesStep(bindings, 0, [], doEnv, testClause, body, macroEnv, k)),
    };
}
function evalDoNextValuesStep(bindings, index, values, doEnv, testClause, body, macroEnv, k) {
    if (index >= bindings.length) {
        bindings.forEach((binding, bindingIndex) => {
            doEnv.set(binding.name, values[bindingIndex]);
        });
        return evalDoLoopStep(bindings, testClause, body, doEnv, macroEnv, k);
    }
    const binding = bindings[index];
    if (binding.step === undefined) {
        return evalDoNextValuesStep(bindings, index + 1, [...values, doEnv.lookup(binding.name)], doEnv, testClause, body, macroEnv, k);
    }
    return {
        kind: 'eval-step',
        expr: binding.step,
        env: doEnv,
        k: (value) => evalDoNextValuesStep(bindings, index + 1, [...values, value], doEnv, testClause, body, macroEnv, k),
    };
}
function evalQuoteStep(args, k) {
    assertExactArity('quote', args, 1);
    return k(quoteExpr(args[0]));
}
function evalSetStep(args, env, macroEnv, k) {
    assertExactArity('set!', args, 2);
    const target = args[0];
    if (target.kind !== 'symbol') {
        throw new EvalError('set! expects a symbol name');
    }
    return {
        kind: 'eval-step',
        expr: args[1],
        env,
        k: (value) => {
            env.set(target.name, value);
            return k(VOID);
        },
    };
}
function parseFormals(expr) {
    if (expr.kind === 'symbol') {
        return { required: [], rest: expr.name };
    }
    if (expr.kind !== 'list') {
        throw new EvalError('lambda expects a parameter list');
    }
    return parseParamItems(expr.items);
}
function parseCaseLambdaClause(expr) {
    if (expr.kind !== 'list' || expr.items.length < 2) {
        throw new EvalError('case-lambda clauses must be of the form (formals body ...)');
    }
    const [paramsExpr, ...body] = expr.items;
    return {
        params: parseFormals(paramsExpr),
        body,
    };
}
function parseParamItems(items) {
    const required = [];
    for (let index = 0; index < items.length; index += 1) {
        const item = items[index];
        if (item.kind !== 'symbol') {
            throw new EvalError('lambda parameters must be symbols');
        }
        if (item.name === '.') {
            if (index !== items.length - 2) {
                throw new EvalError('lambda expects a valid dotted parameter list');
            }
            const restExpr = items[index + 1];
            if (restExpr.kind !== 'symbol' || restExpr.name === '.') {
                throw new EvalError('lambda parameters must be symbols');
            }
            return {
                required,
                rest: restExpr.name,
            };
        }
        required.push(item.name);
    }
    return { required };
}
function parseBindings(expr, formName = 'let') {
    if (expr.kind !== 'list') {
        throw new EvalError(`${formName} expects a binding list`);
    }
    return expr.items.map((bindingExpr) => {
        if (bindingExpr.kind !== 'list' || bindingExpr.items.length !== 2) {
            throw new EvalError(`${formName} bindings must be pairs`);
        }
        const nameExpr = bindingExpr.items[0];
        if (nameExpr.kind !== 'symbol') {
            throw new EvalError(`${formName} bindings must start with a symbol`);
        }
        return {
            name: nameExpr.name,
            init: bindingExpr.items[1],
        };
    });
}
function parseDoBindings(expr) {
    if (expr.kind !== 'list') {
        throw new EvalError('do expects a binding list');
    }
    return expr.items.map((bindingExpr) => {
        if (bindingExpr.kind !== 'list' || (bindingExpr.items.length !== 2 && bindingExpr.items.length !== 3)) {
            throw new EvalError('do bindings must be of the form (name init) or (name init step)');
        }
        const [nameExpr, initExpr, stepExpr] = bindingExpr.items;
        if (nameExpr.kind !== 'symbol') {
            throw new EvalError('do bindings must start with a symbol');
        }
        return {
            name: nameExpr.name,
            init: initExpr,
            step: stepExpr,
        };
    });
}
function parseDoTestClause(expr) {
    if (expr.kind !== 'list' || expr.items.length === 0) {
        throw new EvalError('do expects a termination clause');
    }
    const [test, ...results] = expr.items;
    return { test: test, results };
}
function parseRecordConstructorSpec(expr) {
    if (expr.kind !== 'list' || expr.items.length === 0) {
        throw new EvalError('define-record-type expects a constructor specification');
    }
    const name = expectSymbolExprName('define-record-type', expr.items[0], 'constructor name');
    const fields = expr.items.slice(1).map((fieldExpr) => expectSymbolExprName('define-record-type', fieldExpr, 'constructor field'));
    assertUniqueNames('define-record-type constructor fields', fields);
    return { name, fields };
}
function parseRecordFieldSpec(expr) {
    if (expr.kind !== 'list' || (expr.items.length !== 2 && expr.items.length !== 3)) {
        throw new EvalError('define-record-type expects field clauses of the form (field accessor) or (field accessor mutator)');
    }
    const [fieldExpr, accessorExpr, mutatorExpr] = expr.items;
    return {
        name: expectSymbolExprName('define-record-type', fieldExpr, 'field name'),
        accessor: expectSymbolExprName('define-record-type', accessorExpr, 'field accessor'),
        mutator: mutatorExpr === undefined
            ? undefined
            : expectSymbolExprName('define-record-type', mutatorExpr, 'field mutator'),
    };
}
function expectSymbolExprName(formName, expr, role) {
    if (expr.kind !== 'symbol') {
        throw new EvalError(`${formName} expects ${role} to be a symbol`);
    }
    return expr.name;
}
function assertUniqueNames(context, names) {
    const seen = new Set();
    for (const name of names) {
        if (seen.has(name)) {
            throw new EvalError(`${context} must be unique`);
        }
        seen.add(name);
    }
}
function quoteExpr(expr) {
    switch (expr.kind) {
        case 'number':
        case 'boolean':
            return expr.value;
        case 'string':
            return makeRuntimeString(expr.value);
        case 'char':
            return { kind: 'char', value: expr.value };
        case 'symbol':
            return { kind: 'symbol-value', name: expr.name };
        case 'list':
            return makeList(expr.items.map((item) => quoteExpr(item)));
    }
}
function applyProcedure(proc, args, position, macroEnv, k) {
    if (proc.kind === 'builtin') {
        const result = proc.apply(args, position, k);
        return isMachineStep(result) ? result : k(result);
    }
    if (proc.kind === 'continuation') {
        assertExactArity('continuation', args, 1);
        return proc.resume(args[0]);
    }
    if (proc.kind === 'lambda') {
        return applyProcedureClause(proc.name ?? 'lambda', proc.env, { params: proc.params, body: proc.body }, args, macroEnv, k);
    }
    const clause = proc.clauses.find((candidate) => procedureArityMatches(args, candidate.params));
    if (clause === undefined) {
        throw new EvalError(`${proc.name ?? 'case-lambda'} has no matching clause for ${args.length} argument(s)`);
    }
    return applyProcedureClause(proc.name ?? 'case-lambda', proc.env, clause, args, macroEnv, k);
}
function applyProcedureClause(name, env, clause, args, macroEnv, k) {
    assertProcedureArity(name, args, clause.params);
    const callEnv = new Env(env);
    clause.params.required.forEach((param, index) => {
        callEnv.define(param, args[index]);
    });
    if (clause.params.rest !== undefined) {
        callEnv.define(clause.params.rest, makeList(args.slice(clause.params.required.length)));
    }
    return evalSequenceStep(clause.body, callEnv, macroEnv, k);
}
function builtin(name, apply) {
    return [name, { kind: 'builtin', name, apply }];
}
function callCcBuiltin(args, position, k) {
    assertExactArity('call/cc', args, 1);
    const proc = args[0];
    if (!isProcedure(proc)) {
        throw new EvalError('call/cc expects a procedure');
    }
    return {
        kind: 'apply-step',
        proc,
        args: [{ kind: 'continuation', resume: k }],
        position,
        k,
    };
}
function applyBuiltin(args, position, macroEnv, k) {
    assertAtLeastArity('apply', args, 2);
    const proc = args[0];
    if (!isProcedure(proc)) {
        throw new EvalError('apply expects a procedure');
    }
    const prefixArgs = args.slice(1, -1);
    const listArgs = expectProperList('apply', args[args.length - 1]);
    return applyProcedure(proc, [...prefixArgs, ...listArgs], position, macroEnv, k);
}
function absoluteValue(args) {
    assertExactArity('abs', args, 1);
    return num.absNumeric(expectNumberValue('abs', args[0]));
}
function unaryPredicate(name, args, predicate) {
    assertExactArity(name, args, 1);
    return predicate(args[0]);
}
function numberPredicate(name, args, predicate) {
    assertExactArity(name, args, 1);
    return predicate(expectNumberValue(name, args[0]));
}
function integerPredicate(name, args, predicate) {
    assertExactArity(name, args, 1);
    return predicate(expectIntegerValue(name, args[0]));
}
function compareChars(name, args, compare) {
    assertAtLeastArity(name, args, 2);
    const codePoints = args.map((arg) => {
        const value = expectCharValue(name, arg).value.codePointAt(0);
        if (value === undefined) {
            throw new EvalError(`${name} expects valid characters`);
        }
        return value;
    });
    for (let index = 1; index < codePoints.length; index += 1) {
        if (!compare(codePoints[index - 1], codePoints[index])) {
            return false;
        }
    }
    return true;
}
function compareStrings(name, args, normalize, compare) {
    assertAtLeastArity(name, args, 2);
    const values = args.map((arg) => normalize(expectStringValue(name, arg)));
    for (let index = 1; index < values.length; index += 1) {
        if (!compare(values[index - 1], values[index])) {
            return false;
        }
    }
    return true;
}
function sumNumbers(name, args) {
    return num.sumNumeric(expectNumbers(name, args));
}
function productNumbers(name, args) {
    return num.productNumeric(expectNumbers(name, args));
}
function subtractNumbers(args) {
    const numbers = expectNumbers('-', args);
    assertAtLeastArity('-', numbers, 1);
    return num.subtractNumeric(numbers);
}
function divideNumbers(args) {
    const numbers = expectNumbers('/', args);
    assertAtLeastArity('/', numbers, 1);
    return num.divideNumeric(numbers);
}
function compareNumbers(name, args, compare) {
    const numbers = expectNumbers(name, args);
    assertAtLeastArity(name, numbers, 2);
    for (let index = 1; index < numbers.length; index += 1) {
        if (!compare(numbers[index - 1], numbers[index])) {
            return false;
        }
    }
    return true;
}
function quotientNumbers(args) {
    assertExactArity('quotient', args, 2);
    return num.quotientNumeric(expectIntegerValue('quotient', args[0]), expectIntegerValue('quotient', args[1]));
}
function remainderNumbers(args) {
    assertExactArity('remainder', args, 2);
    return num.remainderNumeric(expectIntegerValue('remainder', args[0]), expectIntegerValue('remainder', args[1]));
}
function moduloNumbers(args) {
    assertExactArity('modulo', args, 2);
    return num.moduloNumeric(expectIntegerValue('modulo', args[0]), expectIntegerValue('modulo', args[1]));
}
function extremum(name, args, select) {
    const numbers = expectNumbers(name, args);
    assertAtLeastArity(name, numbers, 1);
    let result = numbers[0];
    for (const value of numbers.slice(1)) {
        result = select(result, value);
    }
    return result;
}
function exptNumbers(args) {
    assertExactArity('expt', args, 2);
    return num.exptNumeric(expectNumberValue('expt', args[0]), expectIntegerValue('expt', args[1]));
}
function makeVectorBuiltin(args) {
    if (args.length !== 1 && args.length !== 2) {
        throw new EvalError('make-vector expects 1 or 2 argument(s)');
    }
    const length = expectIndex('make-vector', args[0]);
    const fill = args.length === 2 ? args[1] : VOID;
    return { kind: 'vector', items: Array.from({ length }, () => fill) };
}
function appendValues(args) {
    if (args.length === 0) {
        return EMPTY_LIST;
    }
    let result = args[args.length - 1];
    for (let index = args.length - 2; index >= 0; index -= 1) {
        result = appendList(args[index], result);
    }
    return result;
}
function appendList(list, tail) {
    if (isEmptyList(list)) {
        return tail;
    }
    if (!isPair(list)) {
        throw new EvalError('append expects list arguments');
    }
    return {
        kind: 'pair',
        car: list.car,
        cdr: appendList(list.cdr, tail),
    };
}
function makeList(items) {
    let result = EMPTY_LIST;
    for (let index = items.length - 1; index >= 0; index -= 1) {
        result = {
            kind: 'pair',
            car: items[index],
            cdr: result,
        };
    }
    return result;
}
function expectPair(name, value) {
    if (!isPair(value)) {
        throw new EvalError(`${name} expects a pair`);
    }
    return value;
}
function expectVectorValue(name, value) {
    if (!isVectorValue(value)) {
        throw new EvalError(`${name} expects a vector`);
    }
    return value;
}
function expectRecordValue(name, value, recordType) {
    if (!isRecordValue(value)) {
        throw new EvalError(`${name} expects a record`);
    }
    if (recordType !== undefined && value.recordType !== recordType) {
        throw new EvalError(`${name} expects a ${recordType.name} record`);
    }
    return value;
}
function expectProperList(name, value) {
    const items = [];
    let current = value;
    const seen = new Set();
    while (isPair(current)) {
        if (seen.has(current)) {
            throw new EvalError(`${name} expects a proper list`);
        }
        seen.add(current);
        items.push(current.car);
        current = current.cdr;
    }
    if (!isEmptyList(current)) {
        throw new EvalError(`${name} expects a proper list`);
    }
    return items;
}
function expectNumbers(name, args) {
    return args.map((arg) => {
        if (!num.isNumericValue(arg)) {
            throw new EvalError(`${name} expects number arguments`);
        }
        return arg;
    });
}
function expectNumberValue(name, value) {
    if (!num.isNumericValue(value)) {
        throw new EvalError(`${name} expects a number`);
    }
    return value;
}
function expectIntegerValue(name, value) {
    const numericValue = expectNumberValue(name, value);
    if (!num.numericIsInteger(numericValue)) {
        throw new EvalError(`${name} expects an integer`);
    }
    return numericValue;
}
function expectStringValue(name, value) {
    if (typeof value === 'string') {
        return value;
    }
    if (isMutableStringValue(value)) {
        return value.chars.join('');
    }
    throw new EvalError(`${name} expects a string`);
}
function expectMutableStringValue(name, value) {
    if (!isMutableStringValue(value)) {
        throw new EvalError(`${name} expects a mutable string`);
    }
    return value;
}
function expectCharValue(name, value) {
    if (!isCharValue(value)) {
        throw new EvalError(`${name} expects a character`);
    }
    return value;
}
function expectSymbolValue(name, value) {
    if (!isSymbolValue(value)) {
        throw new EvalError(`${name} expects a symbol`);
    }
    return value;
}
function expectIndex(name, value) {
    const index = expectIntegerValue(name, value);
    const indexValue = num.numericToNumber(index);
    if (indexValue < 0) {
        throw new EvalError(`${name} expects a non-negative integer index`);
    }
    return indexValue;
}
function expectCodePoint(name, value) {
    const numericValue = expectIntegerValue(name, value);
    const codePoint = num.numericToNumber(numericValue);
    if (!Number.isSafeInteger(codePoint) || codePoint < 0 || codePoint > 0x10ffff) {
        throw new EvalError(`${name} expects a valid character code`);
    }
    return codePoint;
}
function assertExactArity(name, args, expected) {
    if (args.length !== expected) {
        throw new EvalError(`${name} expects exactly ${expected} argument(s)`);
    }
}
function assertAtLeastArity(name, args, minimum) {
    if (args.length < minimum) {
        throw new EvalError(`${name} expects at least ${minimum} argument(s)`);
    }
}
function assertProcedureArity(name, args, params) {
    if (params.rest === undefined) {
        assertExactArity(name, args, params.required.length);
        return;
    }
    assertAtLeastArity(name, args, params.required.length);
}
function procedureArityMatches(args, params) {
    if (params.rest === undefined) {
        return args.length === params.required.length;
    }
    return args.length >= params.required.length;
}
function listRefBuiltin(args) {
    assertExactArity('list-ref', args, 2);
    const tail = listTailValue('list-ref', args[0], expectIndex('list-ref', args[1]));
    if (!isPair(tail)) {
        throw new EvalError('list-ref index out of bounds');
    }
    return tail.car;
}
function listTailBuiltin(args) {
    assertExactArity('list-tail', args, 2);
    return listTailValue('list-tail', args[0], expectIndex('list-tail', args[1]));
}
function listTailValue(name, list, index) {
    let current = list;
    for (let offset = 0; offset < index; offset += 1) {
        if (isEmptyList(current)) {
            throw new EvalError(`${name} index out of bounds`);
        }
        if (!isPair(current)) {
            throw new EvalError(`${name} expects a proper list`);
        }
        current = current.cdr;
    }
    ensureProperList(name, current);
    return current;
}
function ensureProperList(name, value) {
    let current = value;
    const seen = new Set();
    while (isPair(current)) {
        if (seen.has(current)) {
            throw new EvalError(`${name} expects a proper list`);
        }
        seen.add(current);
        current = current.cdr;
    }
    if (!isEmptyList(current)) {
        throw new EvalError(`${name} expects a proper list`);
    }
}
function isProperListValue(value) {
    let current = value;
    const seen = new Set();
    while (isPair(current)) {
        if (seen.has(current)) {
            return false;
        }
        seen.add(current);
        current = current.cdr;
    }
    return isEmptyList(current);
}
function mapBuiltin(args, position, macroEnv, k) {
    assertAtLeastArity('map', args, 2);
    const proc = args[0];
    if (!isProcedure(proc)) {
        throw new EvalError('map expects a procedure');
    }
    return mapLoopStep(proc, args.slice(1), args.slice(1).map(() => new Set()), [], position, macroEnv, k);
}
function mapLoopStep(proc, currentLists, seenLists, results, position, macroEnv, k) {
    let sawEmpty = false;
    let sawPair = false;
    for (const current of currentLists) {
        if (isEmptyList(current)) {
            sawEmpty = true;
            continue;
        }
        if (!isPair(current)) {
            throw new EvalError('map expects proper list arguments');
        }
        sawPair = true;
    }
    if (sawEmpty) {
        if (sawPair) {
            throw new EvalError('map expects lists of equal length');
        }
        return k(makeList([...results]));
    }
    const elementArgs = [];
    const nextLists = [...currentLists];
    const nextSeenLists = seenLists.map((seen) => new Set(seen));
    for (let index = 0; index < currentLists.length; index += 1) {
        const current = currentLists[index];
        if (!isPair(current)) {
            throw new EvalError('map expects proper list arguments');
        }
        if (nextSeenLists[index].has(current)) {
            throw new EvalError('map expects proper list arguments');
        }
        nextSeenLists[index].add(current);
        elementArgs.push(current.car);
        nextLists[index] = current.cdr;
    }
    return applyProcedure(proc, elementArgs, position, macroEnv, (value) => mapLoopStep(proc, nextLists, nextSeenLists, [...results, value], position, macroEnv, k));
}
function forEachBuiltin(args, position, macroEnv, k) {
    assertAtLeastArity('for-each', args, 2);
    const proc = args[0];
    if (!isProcedure(proc)) {
        throw new EvalError('for-each expects a procedure');
    }
    return forEachLoopStep(proc, args.slice(1), args.slice(1).map(() => new Set()), position, macroEnv, k);
}
function forEachLoopStep(proc, currentLists, seenLists, position, macroEnv, k) {
    let sawEmpty = false;
    let sawPair = false;
    for (const current of currentLists) {
        if (isEmptyList(current)) {
            sawEmpty = true;
            continue;
        }
        if (!isPair(current)) {
            throw new EvalError('for-each expects proper list arguments');
        }
        sawPair = true;
    }
    if (sawEmpty) {
        if (sawPair) {
            throw new EvalError('for-each expects lists of equal length');
        }
        return k(VOID);
    }
    const elementArgs = [];
    const nextLists = [...currentLists];
    const nextSeenLists = seenLists.map((seen) => new Set(seen));
    for (let index = 0; index < currentLists.length; index += 1) {
        const current = currentLists[index];
        if (!isPair(current)) {
            throw new EvalError('for-each expects proper list arguments');
        }
        if (nextSeenLists[index].has(current)) {
            throw new EvalError('for-each expects proper list arguments');
        }
        nextSeenLists[index].add(current);
        elementArgs.push(current.car);
        nextLists[index] = current.cdr;
    }
    return applyProcedure(proc, elementArgs, position, macroEnv, () => forEachLoopStep(proc, nextLists, nextSeenLists, position, macroEnv, k));
}
function memberBuiltin(args) {
    assertExactArity('member', args, 2);
    const key = args[0];
    let current = args[1];
    const seen = new Set();
    while (isPair(current)) {
        if (seen.has(current)) {
            throw new EvalError('member expects a proper list');
        }
        seen.add(current);
        if (equalValues(key, current.car)) {
            return current;
        }
        current = current.cdr;
    }
    if (!isEmptyList(current)) {
        throw new EvalError('member expects a proper list');
    }
    return false;
}
function assvBuiltin(args) {
    assertExactArity('assv', args, 2);
    const key = args[0];
    let current = args[1];
    const seen = new Set();
    while (isPair(current)) {
        if (seen.has(current)) {
            throw new EvalError('assv expects an association list');
        }
        seen.add(current);
        const entry = current.car;
        if (!isPair(entry)) {
            throw new EvalError('assv expects an association list');
        }
        if (eqvValues(key, entry.car)) {
            return entry;
        }
        current = current.cdr;
    }
    if (!isEmptyList(current)) {
        throw new EvalError('assv expects an association list');
    }
    return false;
}
function assocBuiltin(args) {
    assertExactArity('assoc', args, 2);
    const key = args[0];
    let current = args[1];
    while (isPair(current)) {
        const entry = current.car;
        if (!isPair(entry)) {
            throw new EvalError('assoc expects an association list');
        }
        if (equalValues(key, entry.car)) {
            return entry;
        }
        current = current.cdr;
    }
    if (!isEmptyList(current)) {
        throw new EvalError('assoc expects an association list');
    }
    return false;
}
function gcdBuiltin(args) {
    const integers = args.map((arg) => expectIntegerValue('gcd', arg));
    if (integers.length === 0) {
        return makeExactIntegerValue(0n);
    }
    if (integers.every((value) => num.isExactNumeric(value))) {
        let result = 0n;
        for (const value of integers) {
            result = gcdBigInt(result, absBigInt(integerNumericToBigInt(value)));
        }
        return makeExactIntegerValue(result);
    }
    let result = 0;
    for (const value of integers) {
        result = gcdNumber(result, Math.abs(Math.trunc(num.numericToNumber(value))));
    }
    return makeInexactNumberValue(result);
}
function lcmBuiltin(args) {
    const integers = args.map((arg) => expectIntegerValue('lcm', arg));
    if (integers.length === 0) {
        return makeExactIntegerValue(1n);
    }
    if (integers.every((value) => num.isExactNumeric(value))) {
        let result = 1n;
        for (const value of integers) {
            result = lcmBigInt(result, integerNumericToBigInt(value));
        }
        return makeExactIntegerValue(result);
    }
    let result = 1;
    for (const value of integers) {
        result = lcmNumber(result, Math.trunc(num.numericToNumber(value)));
    }
    return makeInexactNumberValue(result);
}
function truncateBuiltin(args) {
    assertExactArity('truncate', args, 1);
    return integerizingNumericResult(expectNumberValue('truncate', args[0]), Math.trunc);
}
function roundBuiltin(args) {
    assertExactArity('round', args, 1);
    return integerizingNumericResult(expectNumberValue('round', args[0]), Math.round);
}
function makeStringBuiltin(args) {
    if (args.length !== 1 && args.length !== 2) {
        throw new EvalError('make-string expects 1 or 2 argument(s)');
    }
    const length = expectIndex('make-string', args[0]);
    const fill = args[1] === undefined ? '\0' : expectCharValue('make-string', args[1]).value;
    return makeRuntimeString(fill.repeat(length));
}
function stringBuiltin(args) {
    return makeRuntimeString(args.map((arg) => expectCharValue('string', arg).value).join(''));
}
function integerizingNumericResult(value, transform) {
    if (value.kind === 'exact-integer') {
        return value;
    }
    const transformed = transform(num.numericToNumber(value));
    return value.kind === 'inexact-number'
        ? makeInexactNumberValue(transformed)
        : makeExactIntegerValue(BigInt(transformed));
}
function integerNumericToBigInt(value) {
    return value.kind === 'exact-integer' ? value.value : BigInt(Math.trunc(num.numericToNumber(value)));
}
function makeExactIntegerValue(value) {
    return { kind: 'exact-integer', value };
}
function makeInexactNumberValue(value) {
    return { kind: 'inexact-number', value: Object.is(value, -0) ? 0 : value };
}
function absBigInt(value) {
    return value < 0n ? -value : value;
}
function gcdBigInt(left, right) {
    let a = absBigInt(left);
    let b = absBigInt(right);
    while (b !== 0n) {
        const remainder = a % b;
        a = b;
        b = remainder;
    }
    return a;
}
function lcmBigInt(left, right) {
    if (left === 0n || right === 0n) {
        return 0n;
    }
    return absBigInt((left / gcdBigInt(left, right)) * right);
}
function gcdNumber(left, right) {
    let a = Math.abs(left);
    let b = Math.abs(right);
    while (b !== 0) {
        const remainder = a % b;
        a = b;
        b = remainder;
    }
    return a;
}
function lcmNumber(left, right) {
    if (left === 0 || right === 0) {
        return 0;
    }
    return Math.abs((left / gcdNumber(left, right)) * right);
}
function formatValue(value) {
    return formatValueWithMode(value, 'write', { activePairs: new Set(), activeVectors: new Set() });
}
function formatDisplayValue(value) {
    return formatValueWithMode(value, 'display', { activePairs: new Set(), activeVectors: new Set() });
}
function formatValueWithMode(value, mode, state) {
    if (num.isNumericValue(value)) {
        return num.formatNumber(value);
    }
    if (typeof value === 'boolean') {
        return value ? '#t' : '#f';
    }
    if (typeof value === 'string') {
        return mode === 'display' ? value : JSON.stringify(value);
    }
    if (isMutableStringValue(value)) {
        const contents = value.chars.join('');
        return mode === 'display' ? contents : JSON.stringify(contents);
    }
    switch (value.kind) {
        case 'char':
            return mode === 'display' ? value.value : formatCharLiteral(value.value);
        case 'symbol-value':
            return value.name;
        case 'empty-list':
            return '()';
        case 'pair':
            return `(${formatPairContents(value, mode, state)})`;
        case 'vector':
            return formatVectorValue(value, mode, state);
        case 'record':
            return `#<record:${value.recordType.name}>`;
        case 'void':
            return '';
        case 'builtin':
            return `#<procedure:${value.name}>`;
        case 'lambda':
            return value.name === undefined ? '#<procedure>' : `#<procedure:${value.name}>`;
        case 'case-lambda':
            return value.name === undefined ? '#<procedure>' : `#<procedure:${value.name}>`;
        case 'continuation':
            return '#<procedure>';
    }
}
function formatPairContents(pair, mode, state) {
    if (state.activePairs.has(pair)) {
        return '#<circular>';
    }
    state.activePairs.add(pair);
    try {
        const head = formatValueWithMode(pair.car, mode, state);
        if (isEmptyList(pair.cdr)) {
            return head;
        }
        if (isPair(pair.cdr)) {
            return `${head} ${formatPairContents(pair.cdr, mode, state)}`;
        }
        return `${head} . ${formatValueWithMode(pair.cdr, mode, state)}`;
    }
    finally {
        state.activePairs.delete(pair);
    }
}
function formatVectorValue(vector, mode, state) {
    if (state.activeVectors.has(vector)) {
        return '#<circular>';
    }
    state.activeVectors.add(vector);
    try {
        return `#(${vector.items.map((item) => formatValueWithMode(item, mode, state)).join(' ')})`;
    }
    finally {
        state.activeVectors.delete(vector);
    }
}
function formatCharLiteral(value) {
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
function currentBenchLevel() {
    const rawLevel = globalThis.process?.env?.BENCH_LEVEL;
    if (rawLevel === undefined) {
        return undefined;
    }
    const level = Number.parseInt(rawLevel, 10);
    return Number.isNaN(level) ? undefined : level;
}
function stringsAreImmutable() {
    const level = currentBenchLevel();
    return level === undefined || level >= STRING_IMMUTABILITY_LEVEL;
}
function makeMutableString(value) {
    return { kind: 'mutable-string', chars: stringChars(value) };
}
function makeRuntimeString(value) {
    return stringsAreImmutable() ? value : makeMutableString(value);
}
function stringValueText(value) {
    return typeof value === 'string' ? value : value.chars.join('');
}
function parseCharToken(token, position) {
    const rawValue = token.slice(2);
    if (rawValue.length === 0) {
        throw new EvalError('invalid character literal', position);
    }
    switch (rawValue.toLowerCase()) {
        case 'space':
            return { kind: 'char', value: ' ', position };
        case 'newline':
            return { kind: 'char', value: '\n', position };
    }
    const chars = Array.from(rawValue);
    if (chars.length !== 1) {
        throw new EvalError('invalid character literal', position);
    }
    return { kind: 'char', value: chars[0], position };
}
function eqValues(left, right) {
    if (num.isNumericValue(left) && num.isNumericValue(right)) {
        return num.numericEqual(left, right);
    }
    if (typeof left === 'boolean') {
        return typeof right === 'boolean' && left === right;
    }
    if (isStringValue(left) && isStringValue(right)) {
        return stringValueText(left) === stringValueText(right);
    }
    if (isCharValue(left) && isCharValue(right)) {
        return left.value === right.value;
    }
    if (isSymbolValue(left) && isSymbolValue(right)) {
        return left.name === right.name;
    }
    if (isEmptyList(left) || isEmptyList(right)) {
        return isEmptyList(left) && isEmptyList(right);
    }
    return left === right;
}
function eqvValues(left, right) {
    return eqValues(left, right);
}
function equalValues(left, right) {
    return equalValuesWithState(left, right, { seenPairs: new Map(), seenVectors: new Map() });
}
function equalValuesWithState(left, right, state) {
    if (eqvValues(left, right)) {
        return true;
    }
    if (isStringValue(left) && isStringValue(right)) {
        return stringValueText(left) === stringValueText(right);
    }
    if (isPair(left) && isPair(right)) {
        if (rememberComparison(state.seenPairs, left, right)) {
            return true;
        }
        return equalValuesWithState(left.car, right.car, state) && equalValuesWithState(left.cdr, right.cdr, state);
    }
    if (isVectorValue(left) && isVectorValue(right)) {
        if (left.items.length !== right.items.length) {
            return false;
        }
        if (rememberComparison(state.seenVectors, left, right)) {
            return true;
        }
        for (let index = 0; index < left.items.length; index += 1) {
            if (!equalValuesWithState(left.items[index], right.items[index], state)) {
                return false;
            }
        }
        return true;
    }
    return false;
}
function rememberComparison(seen, left, right) {
    let rights = seen.get(left);
    if (rights === undefined) {
        rights = new Set();
        seen.set(left, rights);
    }
    else if (rights.has(right)) {
        return true;
    }
    rights.add(right);
    return false;
}
function isAlphabeticChar(value) {
    return value.toLowerCase() !== value.toUpperCase();
}
function isNumericChar(value) {
    return /^[0-9]$/u.test(value);
}
function isTruthy(value) {
    return value !== false;
}
function isProcedure(value) {
    return (typeof value === 'object' &&
        value !== null &&
        (value.kind === 'builtin' || value.kind === 'lambda' || value.kind === 'case-lambda' || value.kind === 'continuation'));
}
function isPair(value) {
    return typeof value === 'object' && value !== null && value.kind === 'pair';
}
function isRecordValue(value) {
    return typeof value === 'object' && value !== null && value.kind === 'record';
}
function isVectorValue(value) {
    return typeof value === 'object' && value !== null && value.kind === 'vector';
}
function isCharValue(value) {
    return typeof value === 'object' && value !== null && value.kind === 'char';
}
function isMutableStringValue(value) {
    return typeof value === 'object' && value !== null && value.kind === 'mutable-string';
}
function isStringValue(value) {
    return typeof value === 'string' || isMutableStringValue(value);
}
function isEmptyList(value) {
    return typeof value === 'object' && value !== null && value.kind === 'empty-list';
}
function isSymbolValue(value) {
    return typeof value === 'object' && value !== null && value.kind === 'symbol-value';
}
function isWhitespace(ch) {
    return /\s/.test(ch);
}
function isDelimiter(ch) {
    return isWhitespace(ch) || ch === '(' || ch === ')' || ch === ';' || ch === '\'';
}
