import { EvalError, attachPosition } from './evalError.js';
import * as num from './numbers.js';
const EMPTY_LIST = { kind: 'empty-list' };
const VOID = { kind: 'void' };
const CORE_SYNTAX = new Set([
    'and',
    'begin',
    'cond',
    'define',
    'define-syntax',
    'if',
    'lambda',
    'let',
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
        builtin('abs', (args) => absoluteValue(args)),
        builtin('apply', (args, position) => applyBuiltin(args, position, macroEnv)),
        builtin('append', (args) => appendValues(args)),
        builtin('assoc', (args) => assocBuiltin(args)),
        builtin('boolean?', (args) => unaryPredicate('boolean?', args, (value) => typeof value === 'boolean')),
        builtin('car', (args) => {
            assertExactArity('car', args, 1);
            return expectPair('car', args[0]).car;
        }),
        builtin('cdr', (args) => {
            assertExactArity('cdr', args, 1);
            return expectPair('cdr', args[0]).cdr;
        }),
        builtin('char-alphabetic?', (args) => {
            assertExactArity('char-alphabetic?', args, 1);
            return isAlphabeticChar(expectCharValue('char-alphabetic?', args[0]).value);
        }),
        builtin('char-downcase', (args) => {
            assertExactArity('char-downcase', args, 1);
            return { kind: 'char', value: expectCharValue('char-downcase', args[0]).value.toLowerCase() };
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
        builtin('inexact->exact', (args) => {
            assertExactArity('inexact->exact', args, 1);
            return num.inexactToExact(expectNumberValue('inexact->exact', args[0]));
        }),
        builtin('inexact?', (args) => unaryPredicate('inexact?', args, (value) => num.isNumericValue(value) && num.isInexactNumeric(value))),
        builtin('integer?', (args) => unaryPredicate('integer?', args, (value) => num.isNumericValue(value) && num.numericIsInteger(value))),
        builtin('length', (args) => {
            assertExactArity('length', args, 1);
            return num.exactIntegerFromNumber(expectProperList('length', args[0]).length);
        }),
        builtin('list', (args) => makeList(args)),
        builtin('list-ref', (args) => listRefBuiltin(args)),
        builtin('list-tail', (args) => listTailBuiltin(args)),
        builtin('list?', (args) => {
            assertExactArity('list?', args, 1);
            return isProperListValue(args[0]);
        }),
        builtin('map', (args, position) => mapBuiltin(args, position, macroEnv)),
        builtin('max', (args) => extremum('max', args, (left, right) => (num.numericCompare(left, right) >= 0 ? left : right))),
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
            return makeMutableString(num.formatNumber(expectNumberValue('number->string', args[0])));
        }),
        builtin('number?', (args) => unaryPredicate('number?', args, (value) => num.isNumericValue(value))),
        builtin('numerator', (args) => {
            assertExactArity('numerator', args, 1);
            return num.numeratorOf(expectNumberValue('numerator', args[0]));
        }),
        builtin('odd?', (args) => integerPredicate('odd?', args, (value) => num.numericIsOdd(value))),
        builtin('pair?', (args) => unaryPredicate('pair?', args, isPair)),
        builtin('positive?', (args) => numberPredicate('positive?', args, (value) => num.numericIsPositive(value))),
        builtin('quotient', (args) => quotientNumbers(args)),
        builtin('rational?', (args) => unaryPredicate('rational?', args, (value) => num.isNumericValue(value))),
        builtin('remainder', (args) => remainderNumbers(args)),
        builtin('string->number', (args) => {
            assertExactArity('string->number', args, 1);
            return num.parseStringNumber(expectStringValue('string->number', args[0]));
        }),
        builtin('string->symbol', (args) => {
            assertExactArity('string->symbol', args, 1);
            return { kind: 'symbol-value', name: expectStringValue('string->symbol', args[0]) };
        }),
        builtin('string-append', (args) => makeMutableString(args.map((arg) => expectStringValue('string-append', arg)).join(''))),
        builtin('string-copy', (args) => {
            assertExactArity('string-copy', args, 1);
            return makeMutableString(expectStringValue('string-copy', args[0]));
        }),
        builtin('string-ci=?', (args) => compareStrings('string-ci=?', args, (value) => value.toLowerCase(), (left, right) => left === right)),
        builtin('string-downcase', (args) => {
            assertExactArity('string-downcase', args, 1);
            return makeMutableString(expectStringValue('string-downcase', args[0]).toLowerCase());
        }),
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
            return makeMutableString(expectStringValue('string-upcase', args[0]).toUpperCase());
        }),
        builtin('substring', (args) => {
            assertExactArity('substring', args, 3);
            const chars = stringChars(expectStringValue('substring', args[0]));
            const start = expectIndex('substring', args[1]);
            const end = expectIndex('substring', args[2]);
            if (start > end || end > chars.length) {
                throw new EvalError('substring expects valid start/end indices');
            }
            return makeMutableString(chars.slice(start, end).join(''));
        }),
        builtin('symbol->string', (args) => {
            assertExactArity('symbol->string', args, 1);
            return makeMutableString(expectSymbolValue('symbol->string', args[0]).name);
        }),
        builtin('symbol?', (args) => unaryPredicate('symbol?', args, isSymbolValue)),
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
function evalSequence(exprs, env, macroEnv) {
    let result = VOID;
    for (const expr of exprs) {
        result = evalExpr(expr, env, macroEnv);
    }
    return result;
}
function evalExpr(expr, env, macroEnv) {
    try {
        switch (expr.kind) {
            case 'number':
            case 'boolean':
            case 'string':
                return expr.value;
            case 'char':
                return { kind: 'char', value: expr.value };
            case 'symbol':
                return env.lookup(expr.name);
            case 'list':
                return evalList(expr, env, macroEnv);
        }
    }
    catch (error) {
        throw attachPosition(error, expr.position);
    }
}
function evalList(expr, env, macroEnv) {
    const { items } = expr;
    if (items.length === 0) {
        throw new EvalError('cannot evaluate empty list');
    }
    const first = items[0];
    if (first.kind === 'symbol') {
        switch (first.name) {
            case 'and':
                return evalAnd(items.slice(1), env, macroEnv);
            case 'begin':
                return evalSequence(items.slice(1), env, macroEnv);
            case 'cond':
                return evalCond(items.slice(1), env, macroEnv);
            case 'define':
                return evalDefine(items.slice(1), env, macroEnv);
            case 'define-syntax':
                return evalDefineSyntax(items.slice(1), env, macroEnv);
            case 'if':
                return evalIf(items.slice(1), env, macroEnv);
            case 'lambda':
                return evalLambda(items.slice(1), env);
            case 'let':
                return evalLet(items.slice(1), env, macroEnv);
            case 'or':
                return evalOr(items.slice(1), env, macroEnv);
            case 'quote':
                return evalQuote(items.slice(1));
            case 'set!':
                return evalSet(items.slice(1), env, macroEnv);
        }
        const macro = macroEnv.lookup(first.name);
        if (macro !== undefined) {
            return evalExpr(expandMacroInvocation(macro, expr), env, macroEnv);
        }
    }
    const proc = evalExpr(first, env, macroEnv);
    if (!isProcedure(proc)) {
        throw new EvalError('attempted to call a non-procedure');
    }
    const args = items.slice(1).map((item) => evalExpr(item, env, macroEnv));
    return applyProcedure(proc, args, first.position, macroEnv);
}
function evalAnd(args, env, macroEnv) {
    let result = true;
    for (const arg of args) {
        result = evalExpr(arg, env, macroEnv);
        if (!isTruthy(result)) {
            return result;
        }
    }
    return result;
}
function evalCond(clauses, env, macroEnv) {
    for (let index = 0; index < clauses.length; index += 1) {
        const clauseExpr = clauses[index];
        if (clauseExpr.kind !== 'list' || clauseExpr.items.length === 0) {
            throw new EvalError('cond expects non-empty clauses');
        }
        const [testExpr, ...body] = clauseExpr.items;
        if (testExpr.kind === 'symbol' && testExpr.name === 'else') {
            if (index !== clauses.length - 1) {
                throw new EvalError('cond else clause must be last');
            }
            return evalSequence(body, env, macroEnv);
        }
        const testValue = evalExpr(testExpr, env, macroEnv);
        if (isTruthy(testValue)) {
            return body.length === 0 ? testValue : evalSequence(body, env, macroEnv);
        }
    }
    return VOID;
}
function evalOr(args, env, macroEnv) {
    for (const arg of args) {
        const value = evalExpr(arg, env, macroEnv);
        if (isTruthy(value)) {
            return value;
        }
    }
    return false;
}
function evalDefine(args, env, macroEnv) {
    assertAtLeastArity('define', args, 2);
    const target = args[0];
    const body = args.slice(1);
    if (target.kind === 'symbol') {
        assertExactArity('define', body, 1);
        env.define(target.name, evalExpr(body[0], env, macroEnv));
        return VOID;
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
        return VOID;
    }
    throw new EvalError('define expects a symbol name');
}
function evalDefineSyntax(args, env, macroEnv) {
    assertExactArity('define-syntax', args, 2);
    const nameExpr = args[0];
    if (nameExpr.kind !== 'symbol') {
        throw new EvalError('define-syntax expects a symbol name');
    }
    macroEnv.define(nameExpr.name, parseSyntaxRules(nameExpr.name, args[1], env));
    return VOID;
}
function evalIf(args, env, macroEnv) {
    assertExactArity('if', args, 3);
    const [conditionExpr, thenExpr, elseExpr] = args;
    return isTruthy(evalExpr(conditionExpr, env, macroEnv))
        ? evalExpr(thenExpr, env, macroEnv)
        : evalExpr(elseExpr, env, macroEnv);
}
function evalLambda(args, env) {
    assertAtLeastArity('lambda', args, 2);
    const paramsExpr = args[0];
    return {
        kind: 'lambda',
        params: parseFormals(paramsExpr),
        body: args.slice(1),
        env,
    };
}
function evalLet(args, env, macroEnv) {
    assertAtLeastArity('let', args, 2);
    const firstArg = args[0];
    if (firstArg.kind === 'symbol') {
        assertAtLeastArity('let', args, 3);
        return evalNamedLet(firstArg.name, args[1], args.slice(2), env, macroEnv);
    }
    const bindings = parseBindings(firstArg);
    const body = args.slice(1);
    const values = bindings.map((binding) => evalExpr(binding.init, env, macroEnv));
    const letEnv = new Env(env);
    bindings.forEach((binding, index) => {
        letEnv.define(binding.name, values[index]);
    });
    return evalSequence(body, letEnv, macroEnv);
}
function evalNamedLet(name, bindingsExpr, body, env, macroEnv) {
    const bindings = parseBindings(bindingsExpr);
    const values = bindings.map((binding) => evalExpr(binding.init, env, macroEnv));
    const letEnv = new Env(env);
    const proc = {
        kind: 'lambda',
        name,
        params: { required: bindings.map((binding) => binding.name) },
        body,
        env: letEnv,
    };
    letEnv.define(name, proc);
    return applyProcedure(proc, values, bindingsExpr.position, macroEnv);
}
function evalQuote(args) {
    assertExactArity('quote', args, 1);
    return quoteExpr(args[0]);
}
function evalSet(args, env, macroEnv) {
    assertExactArity('set!', args, 2);
    const target = args[0];
    if (target.kind !== 'symbol') {
        throw new EvalError('set! expects a symbol name');
    }
    env.set(target.name, evalExpr(args[1], env, macroEnv));
    return VOID;
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
function parseBindings(expr) {
    if (expr.kind !== 'list') {
        throw new EvalError('let expects a binding list');
    }
    return expr.items.map((bindingExpr) => {
        if (bindingExpr.kind !== 'list' || bindingExpr.items.length !== 2) {
            throw new EvalError('let bindings must be pairs');
        }
        const nameExpr = bindingExpr.items[0];
        if (nameExpr.kind !== 'symbol') {
            throw new EvalError('let bindings must start with a symbol');
        }
        return {
            name: nameExpr.name,
            init: bindingExpr.items[1],
        };
    });
}
function quoteExpr(expr) {
    switch (expr.kind) {
        case 'number':
        case 'boolean':
        case 'string':
            return expr.value;
        case 'char':
            return { kind: 'char', value: expr.value };
        case 'symbol':
            return { kind: 'symbol-value', name: expr.name };
        case 'list':
            return makeList(expr.items.map((item) => quoteExpr(item)));
    }
}
function applyProcedure(proc, args, position, macroEnv) {
    try {
        if (proc.kind === 'builtin') {
            return proc.apply(args, position);
        }
        assertProcedureArity(proc.name ?? 'lambda', args, proc.params);
        const callEnv = new Env(proc.env);
        proc.params.required.forEach((param, index) => {
            callEnv.define(param, args[index]);
        });
        if (proc.params.rest !== undefined) {
            callEnv.define(proc.params.rest, makeList(args.slice(proc.params.required.length)));
        }
        return evalSequence(proc.body, callEnv, macroEnv);
    }
    catch (error) {
        throw attachPosition(error, position);
    }
}
function builtin(name, apply) {
    return [name, { kind: 'builtin', name, apply }];
}
function applyBuiltin(args, position, macroEnv) {
    assertAtLeastArity('apply', args, 2);
    const proc = args[0];
    if (!isProcedure(proc)) {
        throw new EvalError('apply expects a procedure');
    }
    const prefixArgs = args.slice(1, -1);
    const listArgs = expectProperList('apply', args[args.length - 1]);
    return applyProcedure(proc, [...prefixArgs, ...listArgs], position, macroEnv);
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
function expectProperList(name, value) {
    const items = [];
    let current = value;
    while (isPair(current)) {
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
function mapBuiltin(args, position, macroEnv) {
    assertAtLeastArity('map', args, 2);
    const proc = args[0];
    if (!isProcedure(proc)) {
        throw new EvalError('map expects a procedure');
    }
    const currentLists = args.slice(1);
    const results = [];
    while (true) {
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
            return makeList(results);
        }
        const elementArgs = [];
        for (let index = 0; index < currentLists.length; index += 1) {
            const current = currentLists[index];
            if (!isPair(current)) {
                throw new EvalError('map expects proper list arguments');
            }
            elementArgs.push(current.car);
            currentLists[index] = current.cdr;
        }
        results.push(applyProcedure(proc, elementArgs, position, macroEnv));
    }
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
function formatValue(value) {
    return formatValueWithMode(value, 'write');
}
function formatDisplayValue(value) {
    return formatValueWithMode(value, 'display');
}
function formatValueWithMode(value, mode) {
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
            return `(${formatPairContents(value, mode)})`;
        case 'void':
            return '';
        case 'builtin':
            return `#<procedure:${value.name}>`;
        case 'lambda':
            return value.name === undefined ? '#<procedure>' : `#<procedure:${value.name}>`;
    }
}
function formatPairContents(pair, mode) {
    const parts = [];
    let current = pair;
    while (isPair(current)) {
        parts.push(formatValueWithMode(current.car, mode));
        current = current.cdr;
    }
    if (isEmptyList(current)) {
        return parts.join(' ');
    }
    return `${parts.join(' ')} . ${formatValueWithMode(current, mode)}`;
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
function makeMutableString(value) {
    return { kind: 'mutable-string', chars: stringChars(value) };
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
    if (typeof left === 'boolean' || typeof left === 'string') {
        return typeof right === typeof left && left === right;
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
function equalValues(left, right) {
    if (eqValues(left, right)) {
        return true;
    }
    if (isStringValue(left) && isStringValue(right)) {
        return stringValueText(left) === stringValueText(right);
    }
    if (isPair(left) && isPair(right)) {
        return equalValues(left.car, right.car) && equalValues(left.cdr, right.cdr);
    }
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
    return typeof value === 'object' && value !== null && (value.kind === 'builtin' || value.kind === 'lambda');
}
function isPair(value) {
    return typeof value === 'object' && value !== null && value.kind === 'pair';
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
