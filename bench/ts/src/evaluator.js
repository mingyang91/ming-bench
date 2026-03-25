// src/evalError.ts
var EvalError = class extends Error {
  constructor(message) {
    super(message);
    this.name = "EvalError";
  }
};

// src/evaluator.ts
function posStr(pos) {
  return pos ? `${pos.line}:${pos.col}` : "?:?";
}
function errAt(msg, pos) {
  return new EvalError(`${posStr(pos)}: ${msg}`);
}
var NIL = { tag: "nil" };
var VOID = { tag: "void" };
var TRUE_VAL = { tag: "boolean", value: true };
var FALSE_VAL = { tag: "boolean", value: false };
var eqIdCounter = 0;
function gcd(a, b) {
  a = Math.abs(a);
  b = Math.abs(b);
  while (b) {
    [a, b] = [b, a % b];
  }
  return a;
}
function makeRat(num, den) {
  if (den === 0) throw new EvalError("division by zero");
  if (den < 0) {
    num = -num;
    den = -den;
  }
  const g = gcd(Math.abs(num), den);
  return { tag: "rational", num: num / g, den: den / g };
}
function toFloat(v) {
  if (v.tag === "number") return v.value;
  if (v.tag === "rational") return v.num / v.den;
  throw new EvalError("expected number");
}
function isNumeric(v) {
  return v.tag === "number" || v.tag === "rational";
}
function assertNumeric(v, op) {
  if (!isNumeric(v)) throw new EvalError(`${op}: expected number`);
}
function ratAdd(a, b) {
  if (a.tag === "rational" && b.tag === "rational") {
    return makeRat(a.num * b.den + b.num * a.den, a.den * b.den);
  }
  return { tag: "number", value: toFloat(a) + toFloat(b) };
}
function ratSub(a, b) {
  if (a.tag === "rational" && b.tag === "rational") {
    return makeRat(a.num * b.den - b.num * a.den, a.den * b.den);
  }
  return { tag: "number", value: toFloat(a) - toFloat(b) };
}
function ratMul(a, b) {
  if (a.tag === "rational" && b.tag === "rational") {
    return makeRat(a.num * b.num, a.den * b.den);
  }
  return { tag: "number", value: toFloat(a) * toFloat(b) };
}
function ratDiv(a, b) {
  if (a.tag === "rational" && b.tag === "rational") {
    if (b.num === 0) throw new EvalError("division by zero");
    return makeRat(a.num * b.den, a.den * b.num);
  }
  const bv = toFloat(b);
  if (bv === 0) throw new EvalError("division by zero");
  return { tag: "number", value: toFloat(a) / bv };
}
var outputBuffer = "";
function displayValUnquoted(val) {
  if (val.tag === "string") return val.value;
  if (val.tag === "char") return val.value;
  return displayVal(val);
}
function writeVal(val) {
  return displayVal(val);
}
function makeList(items) {
  let result = NIL;
  for (let i = items.length - 1; i >= 0; i--) {
    result = { tag: "pair", car: items[i], cdr: result };
  }
  return result;
}
function pairToArray(val) {
  const result = [];
  let cur = val;
  const seen = /* @__PURE__ */ new Set();
  while (cur.tag === "pair") {
    if (seen.has(cur)) throw new EvalError("not a proper list");
    seen.add(cur);
    result.push(cur.car);
    cur = cur.cdr;
  }
  if (cur.tag !== "nil") throw new EvalError("not a proper list");
  return result;
}
var Env = class {
  bindings = null;
  parent;
  constructor(parent = null) {
    this.parent = parent;
  }
  get(name, pos) {
    if (this.bindings) {
      const val = this.bindings.get(name);
      if (val !== void 0) return val;
    }
    if (this.parent) return this.parent.get(name, pos);
    throw errAt(`unbound variable: ${name}`, pos);
  }
  set(name, val, pos) {
    if (this.bindings?.has(name)) {
      this.bindings.set(name, val);
      return;
    }
    if (this.parent) {
      this.parent.set(name, val, pos);
      return;
    }
    throw errAt(`set!: unbound variable: ${name}`, pos);
  }
  define(name, val) {
    if (!this.bindings) this.bindings = /* @__PURE__ */ new Map();
    this.bindings.set(name, val);
  }
};
function expectNum(v, op) {
  if (v.tag === "number") return v.value;
  if (v.tag === "rational") return v.num / v.den;
  throw new EvalError(`${op}: expected number`);
}
function schemeEqv(a, b) {
  if (isNumeric(a) && isNumeric(b)) return toFloat(a) === toFloat(b);
  if (a.tag !== b.tag) return false;
  if (a.tag === "boolean" && b.tag === "boolean") return a.value === b.value;
  if (a.tag === "symbol" && b.tag === "symbol") return a.value === b.value;
  if (a.tag === "char" && b.tag === "char") return a.value === b.value;
  if (a.tag === "nil" && b.tag === "nil") return true;
  if (a.tag === "void" && b.tag === "void") return true;
  return a === b;
}
function schemeEqual(a, b, seen) {
  if (isNumeric(a) && isNumeric(b)) return toFloat(a) === toFloat(b);
  if (a.tag !== b.tag) return false;
  if (a.tag === "boolean" && b.tag === "boolean") return a.value === b.value;
  if (a.tag === "string" && b.tag === "string") return a.value === b.value;
  if (a.tag === "symbol" && b.tag === "symbol") return a.value === b.value;
  if (a.tag === "char" && b.tag === "char") return a.value === b.value;
  if (a.tag === "nil" && b.tag === "nil") return true;
  if (a.tag === "pair" && b.tag === "pair") {
    if (a === b) return true;
    if (!seen) seen = /* @__PURE__ */ new Set();
    const idA = a.__eqid ?? (a.__eqid = ++eqIdCounter);
    const idB = b.__eqid ?? (b.__eqid = ++eqIdCounter);
    const key = `${idA}:${idB}`;
    if (seen.has(key)) return true;
    seen.add(key);
    return schemeEqual(a.car, b.car, seen) && schemeEqual(a.cdr, b.cdr, seen);
  }
  if (a.tag === "vector" && b.tag === "vector") {
    if (a.elements.length !== b.elements.length) return false;
    for (let i = 0; i < a.elements.length; i++) {
      if (!schemeEqual(a.elements[i], b.elements[i], seen)) return false;
    }
    return true;
  }
  return false;
}
function isBounce(b) {
  return b !== null && typeof b === "object" && b.__isBounce === true;
}
function mkBounce(fn) {
  return { __isBounce: true, fn };
}
function runTrampoline(b) {
  while (isBounce(b)) b = b.fn();
  return b;
}
var stepLimitActive = false;
var stepCount = 0;
var stepMax = 0;
function checkStepLimit() {
  if (stepLimitActive && ++stepCount > stepMax) {
    throw new EvalError("step limit exceeded");
  }
}
var contReentry = false;
var windStack = [];
var exceptionHandlers = [];
function applySync(func, args, pos) {
  return runTrampoline(applyCPS(func, args, pos, (v) => v));
}
function makeGlobalEnv() {
  const env = new Env();
  function defBuiltin(name, fn) {
    env.define(name, { tag: "builtin", name, fn });
  }
  defBuiltin("+", (args) => {
    for (const a of args) assertNumeric(a, "+");
    if (args.length === 0) return makeRat(0, 1);
    let result = args[0];
    for (let i = 1; i < args.length; i++) result = ratAdd(result, args[i]);
    return result;
  });
  defBuiltin("-", (args) => {
    if (args.length === 0) throw new EvalError("-: expected at least 1 argument");
    for (const a of args) assertNumeric(a, "-");
    if (args.length === 1) {
      if (args[0].tag === "rational") return makeRat(-args[0].num, args[0].den);
      return { tag: "number", value: -args[0].value };
    }
    let result = args[0];
    for (let i = 1; i < args.length; i++) result = ratSub(result, args[i]);
    return result;
  });
  defBuiltin("*", (args) => {
    for (const a of args) assertNumeric(a, "*");
    if (args.length === 0) return makeRat(1, 1);
    let result = args[0];
    for (let i = 1; i < args.length; i++) result = ratMul(result, args[i]);
    return result;
  });
  defBuiltin("/", (args) => {
    if (args.length < 1) throw new EvalError("/: expected at least 1 argument");
    for (const a of args) assertNumeric(a, "/");
    if (args.length === 1) {
      return ratDiv(makeRat(1, 1), args[0]);
    }
    let result = args[0];
    for (let i = 1; i < args.length; i++) result = ratDiv(result, args[i]);
    return result;
  });
  for (const op of ["<", ">", "=", ">=", "<="]) {
    defBuiltin(op, (args) => {
      if (args.length < 2) throw new EvalError(`${op}: expected at least 2 arguments`);
      for (let i = 0; i < args.length; i++) assertNumeric(args[i], op);
      for (let i = 0; i < args.length - 1; i++) {
        const a = toFloat(args[i]), b = toFloat(args[i + 1]);
        let r;
        switch (op) {
          case "<":
            r = a < b;
            break;
          case ">":
            r = a > b;
            break;
          case "=":
            r = a === b;
            break;
          case ">=":
            r = a >= b;
            break;
          case "<=":
            r = a <= b;
            break;
        }
        if (!r) return FALSE_VAL;
      }
      return TRUE_VAL;
    });
  }
  defBuiltin("cons", (args) => {
    if (args.length !== 2) throw new EvalError("cons: expected 2 arguments");
    return { tag: "pair", car: args[0], cdr: args[1] };
  });
  defBuiltin("car", (args) => {
    if (args.length !== 1) throw new EvalError("car: expected 1 argument");
    if (args[0].tag !== "pair") throw new EvalError("car: expected pair");
    return args[0].car;
  });
  defBuiltin("cdr", (args) => {
    if (args.length !== 1) throw new EvalError("cdr: expected 1 argument");
    if (args[0].tag !== "pair") throw new EvalError("cdr: expected pair");
    return args[0].cdr;
  });
  defBuiltin("null?", (args) => {
    if (args.length !== 1) throw new EvalError("null?: expected 1 argument");
    return { tag: "boolean", value: args[0].tag === "nil" };
  });
  defBuiltin("list", (args) => {
    return makeList(args);
  });
  defBuiltin("length", (args) => {
    if (args.length !== 1) throw new EvalError("length: expected 1 argument");
    let count = 0;
    let slow = args[0], fast = args[0];
    while (fast.tag === "pair") {
      count++;
      slow = slow.cdr;
      fast = fast.cdr;
      if (fast.tag !== "pair") break;
      count++;
      fast = fast.cdr;
      if (slow === fast) throw new EvalError("length: circular list");
    }
    if (fast.tag !== "nil") throw new EvalError("length: expected proper list");
    return makeRat(count, 1);
  });
  defBuiltin("append", (args) => {
    if (args.length === 0) return NIL;
    if (args.length === 1) return args[0];
    let result = args[args.length - 1];
    for (let i = args.length - 2; i >= 0; i--) {
      const items = pairToArray(args[i]);
      for (let j = items.length - 1; j >= 0; j--) {
        result = { tag: "pair", car: items[j], cdr: result };
      }
    }
    return result;
  });
  defBuiltin("number?", (args) => {
    if (args.length !== 1) throw new EvalError("number?: expected 1 argument");
    return { tag: "boolean", value: isNumeric(args[0]) };
  });
  defBuiltin("exact?", (args) => {
    if (args.length !== 1) throw new EvalError("exact?: expected 1 argument");
    return { tag: "boolean", value: args[0].tag === "rational" };
  });
  defBuiltin("inexact?", (args) => {
    if (args.length !== 1) throw new EvalError("inexact?: expected 1 argument");
    return { tag: "boolean", value: args[0].tag === "number" };
  });
  defBuiltin("integer?", (args) => {
    if (args.length !== 1) throw new EvalError("integer?: expected 1 argument");
    if (args[0].tag === "rational") return { tag: "boolean", value: args[0].den === 1 };
    if (args[0].tag === "number") return { tag: "boolean", value: Number.isInteger(args[0].value) };
    return { tag: "boolean", value: false };
  });
  defBuiltin("rational?", (args) => {
    if (args.length !== 1) throw new EvalError("rational?: expected 1 argument");
    return { tag: "boolean", value: isNumeric(args[0]) };
  });
  defBuiltin("exact->inexact", (args) => {
    if (args.length !== 1) throw new EvalError("exact->inexact: expected 1 argument");
    assertNumeric(args[0], "exact->inexact");
    return { tag: "number", value: toFloat(args[0]) };
  });
  defBuiltin("inexact->exact", (args) => {
    if (args.length !== 1) throw new EvalError("inexact->exact: expected 1 argument");
    assertNumeric(args[0], "inexact->exact");
    if (args[0].tag === "rational") return args[0];
    const v = args[0].value;
    if (Number.isInteger(v)) return makeRat(v, 1);
    let num = v, den = 1;
    while (num !== Math.floor(num) && den < 1e15) {
      num *= 2;
      den *= 2;
    }
    return makeRat(Math.round(num), den);
  });
  defBuiltin("numerator", (args) => {
    if (args.length !== 1) throw new EvalError("numerator: expected 1 argument");
    assertNumeric(args[0], "numerator");
    if (args[0].tag === "rational") return makeRat(args[0].num, 1);
    return { tag: "number", value: args[0].value };
  });
  defBuiltin("denominator", (args) => {
    if (args.length !== 1) throw new EvalError("denominator: expected 1 argument");
    assertNumeric(args[0], "denominator");
    if (args[0].tag === "rational") return makeRat(args[0].den, 1);
    return { tag: "number", value: 1 };
  });
  defBuiltin("boolean?", (args) => {
    if (args.length !== 1) throw new EvalError("boolean?: expected 1 argument");
    return { tag: "boolean", value: args[0].tag === "boolean" };
  });
  defBuiltin("string?", (args) => {
    if (args.length !== 1) throw new EvalError("string?: expected 1 argument");
    return { tag: "boolean", value: args[0].tag === "string" };
  });
  defBuiltin("symbol?", (args) => {
    if (args.length !== 1) throw new EvalError("symbol?: expected 1 argument");
    return { tag: "boolean", value: args[0].tag === "symbol" };
  });
  defBuiltin("pair?", (args) => {
    if (args.length !== 1) throw new EvalError("pair?: expected 1 argument");
    return { tag: "boolean", value: args[0].tag === "pair" };
  });
  defBuiltin("char?", (args) => {
    if (args.length !== 1) throw new EvalError("char?: expected 1 argument");
    return { tag: "boolean", value: args[0].tag === "char" };
  });
  defBuiltin("display", (args) => {
    if (args.length !== 1) throw new EvalError("display: expected 1 argument");
    outputBuffer += displayValUnquoted(args[0]);
    return VOID;
  });
  defBuiltin("write", (args) => {
    if (args.length !== 1) throw new EvalError("write: expected 1 argument");
    outputBuffer += writeVal(args[0]);
    return VOID;
  });
  defBuiltin("newline", (args) => {
    if (args.length !== 0) throw new EvalError("newline: expected 0 arguments");
    outputBuffer += "\n";
    return VOID;
  });
  defBuiltin("string-append", (args) => {
    let result = "";
    for (const a of args) {
      if (a.tag !== "string") throw new EvalError("string-append: expected string");
      result += a.value;
    }
    return { tag: "string", value: result };
  });
  defBuiltin("string-length", (args) => {
    if (args.length !== 1 || args[0].tag !== "string")
      throw new EvalError("string-length: expected 1 string argument");
    return makeRat(args[0].value.length, 1);
  });
  defBuiltin("substring", (args) => {
    if (args.length !== 3) throw new EvalError("substring: expected 3 arguments");
    if (args[0].tag !== "string") throw new EvalError("substring: expected string");
    const start = expectNum(args[1], "substring");
    const end = expectNum(args[2], "substring");
    return { tag: "string", value: args[0].value.substring(start, end) };
  });
  defBuiltin("string->number", (args) => {
    if (args.length !== 1 || args[0].tag !== "string")
      throw new EvalError("string->number: expected 1 string argument");
    const s = args[0].value;
    const ratMatch = s.match(/^(-?\d+)\/(\d+)$/);
    if (ratMatch) return makeRat(parseInt(ratMatch[1], 10), parseInt(ratMatch[2], 10));
    const n = Number(s);
    if (isNaN(n)) return { tag: "boolean", value: false };
    if (Number.isInteger(n) && !s.includes(".")) return makeRat(n, 1);
    return { tag: "number", value: n };
  });
  defBuiltin("number->string", (args) => {
    if (args.length !== 1 || !isNumeric(args[0]))
      throw new EvalError("number->string: expected 1 number argument");
    return { tag: "string", value: displayVal(args[0]) };
  });
  defBuiltin("string-ref", (args) => {
    if (args.length !== 2) throw new EvalError("string-ref: expected 2 arguments");
    if (args[0].tag !== "string") throw new EvalError("string-ref: expected string");
    const idx = expectNum(args[1], "string-ref");
    return { tag: "char", value: args[0].value[idx] };
  });
  defBuiltin("string-copy", (args) => {
    if (args.length !== 1 || args[0].tag !== "string")
      throw new EvalError("string-copy: expected 1 string argument");
    return { tag: "string", value: args[0].value, mutable: true };
  });
  defBuiltin("string-set!", (args) => {
    if (args.length !== 3 || args[0].tag !== "string" || args[2].tag !== "char")
      throw new EvalError("string-set!: expected string, index, char");
    const s = args[0];
    if (!s.mutable) throw new EvalError("string-set!: strings are immutable");
    const idx = expectNum(args[1], "string-set!");
    if (idx < 0 || idx >= s.value.length) throw new EvalError("string-set!: index out of range");
    s.value = s.value.substring(0, idx) + args[2].value + s.value.substring(idx + 1);
    return VOID;
  });
  defBuiltin("string->list", (args) => {
    if (args.length !== 1 || args[0].tag !== "string")
      throw new EvalError("string->list: expected 1 string argument");
    let result = { tag: "nil" };
    const s = args[0].value;
    for (let i = s.length - 1; i >= 0; i--) {
      result = { tag: "pair", car: { tag: "char", value: s[i] }, cdr: result };
    }
    return result;
  });
  defBuiltin("list->string", (args) => {
    if (args.length !== 1) throw new EvalError("list->string: expected 1 argument");
    let result = "";
    let cur = args[0];
    while (cur.tag === "pair") {
      if (cur.car.tag !== "char") throw new EvalError("list->string: expected list of characters");
      result += cur.car.value;
      cur = cur.cdr;
    }
    if (cur.tag !== "nil") throw new EvalError("list->string: expected proper list");
    return { tag: "string", value: result };
  });
  defBuiltin("char->integer", (args) => {
    if (args.length !== 1 || args[0].tag !== "char")
      throw new EvalError("char->integer: expected 1 char argument");
    return { tag: "number", value: args[0].value.charCodeAt(0) };
  });
  defBuiltin("integer->char", (args) => {
    if (args.length !== 1 || args[0].tag !== "number")
      throw new EvalError("integer->char: expected 1 integer argument");
    return { tag: "char", value: String.fromCharCode(args[0].value) };
  });
  defBuiltin("symbol->string", (args) => {
    if (args.length !== 1 || args[0].tag !== "symbol")
      throw new EvalError("symbol->string: expected 1 symbol argument");
    return { tag: "string", value: args[0].value };
  });
  defBuiltin("string->symbol", (args) => {
    if (args.length !== 1 || args[0].tag !== "string")
      throw new EvalError("string->symbol: expected 1 string argument");
    return { tag: "symbol", value: args[0].value };
  });
  defBuiltin("eq?", (args) => {
    if (args.length !== 2) throw new EvalError("eq?: expected 2 arguments");
    const a = args[0], b = args[1];
    if (a.tag !== b.tag) return { tag: "boolean", value: false };
    if (a.tag === "symbol" && b.tag === "symbol") return { tag: "boolean", value: a.value === b.value };
    if (a.tag === "number" && b.tag === "number") return { tag: "boolean", value: a.value === b.value };
    if (a.tag === "rational" && b.tag === "rational") return { tag: "boolean", value: a.num === b.num && a.den === b.den };
    if (a.tag === "boolean" && b.tag === "boolean") return { tag: "boolean", value: a.value === b.value };
    if (a.tag === "char" && b.tag === "char") return { tag: "boolean", value: a.value === b.value };
    if (a.tag === "string" && b.tag === "string") return { tag: "boolean", value: a === b };
    if (a.tag === "nil" && b.tag === "nil") return { tag: "boolean", value: true };
    if (a.tag === "void" && b.tag === "void") return { tag: "boolean", value: true };
    return { tag: "boolean", value: a === b };
  });
  defBuiltin("eqv?", (args) => {
    if (args.length !== 2) throw new EvalError("eqv?: expected 2 arguments");
    return { tag: "boolean", value: schemeEqv(args[0], args[1]) };
  });
  defBuiltin("equal?", (args) => {
    if (args.length !== 2) throw new EvalError("equal?: expected 2 arguments");
    return { tag: "boolean", value: schemeEqual(args[0], args[1]) };
  });
  defBuiltin("abs", (args) => {
    if (args.length !== 1) throw new EvalError("abs: expected 1 argument");
    assertNumeric(args[0], "abs");
    if (args[0].tag === "rational") return makeRat(Math.abs(args[0].num), args[0].den);
    return { tag: "number", value: Math.abs(args[0].value) };
  });
  defBuiltin("modulo", (args) => {
    if (args.length !== 2) throw new EvalError("modulo: expected 2 arguments");
    const a = expectNum(args[0], "modulo"), b = expectNum(args[1], "modulo");
    if (b === 0) throw new EvalError("modulo: division by zero");
    const r = (a % b + b) % b;
    if (args[0].tag === "rational" && args[1].tag === "rational") return makeRat(r, 1);
    return { tag: "number", value: r };
  });
  defBuiltin("remainder", (args) => {
    if (args.length !== 2) throw new EvalError("remainder: expected 2 arguments");
    const a = expectNum(args[0], "remainder"), b = expectNum(args[1], "remainder");
    if (b === 0) throw new EvalError("remainder: division by zero");
    const r = a % b;
    if (args[0].tag === "rational" && args[1].tag === "rational") return makeRat(r, 1);
    return { tag: "number", value: r };
  });
  defBuiltin("quotient", (args) => {
    if (args.length !== 2) throw new EvalError("quotient: expected 2 arguments");
    const a = expectNum(args[0], "quotient"), b = expectNum(args[1], "quotient");
    if (b === 0) throw new EvalError("quotient: division by zero");
    const r = Math.trunc(a / b);
    if (args[0].tag === "rational" && args[1].tag === "rational") return makeRat(r, 1);
    return { tag: "number", value: r };
  });
  defBuiltin("min", (args) => {
    if (args.length < 1) throw new EvalError("min: expected at least 1 argument");
    for (const a of args) assertNumeric(a, "min");
    let best = args[0];
    for (let i = 1; i < args.length; i++) {
      if (toFloat(args[i]) < toFloat(best)) best = args[i];
    }
    return best;
  });
  defBuiltin("max", (args) => {
    if (args.length < 1) throw new EvalError("max: expected at least 1 argument");
    for (const a of args) assertNumeric(a, "max");
    let best = args[0];
    for (let i = 1; i < args.length; i++) {
      if (toFloat(args[i]) > toFloat(best)) best = args[i];
    }
    return best;
  });
  defBuiltin("expt", (args) => {
    if (args.length !== 2) throw new EvalError("expt: expected 2 arguments");
    assertNumeric(args[0], "expt");
    assertNumeric(args[1], "expt");
    if (args[0].tag === "rational" && args[1].tag === "rational" && args[1].den === 1) {
      const exp = args[1].num;
      if (exp >= 0) {
        return makeRat(Math.pow(args[0].num, exp), Math.pow(args[0].den, exp));
      }
      return makeRat(Math.pow(args[0].den, -exp), Math.pow(args[0].num, -exp));
    }
    return { tag: "number", value: Math.pow(toFloat(args[0]), toFloat(args[1])) };
  });
  defBuiltin("zero?", (args) => {
    if (args.length !== 1) throw new EvalError("zero?: expected 1 argument");
    assertNumeric(args[0], "zero?");
    return { tag: "boolean", value: toFloat(args[0]) === 0 };
  });
  defBuiltin("positive?", (args) => {
    if (args.length !== 1) throw new EvalError("positive?: expected 1 argument");
    assertNumeric(args[0], "positive?");
    return { tag: "boolean", value: toFloat(args[0]) > 0 };
  });
  defBuiltin("negative?", (args) => {
    if (args.length !== 1) throw new EvalError("negative?: expected 1 argument");
    assertNumeric(args[0], "negative?");
    return { tag: "boolean", value: toFloat(args[0]) < 0 };
  });
  defBuiltin("odd?", (args) => {
    if (args.length !== 1) throw new EvalError("odd?: expected 1 argument");
    const n = expectNum(args[0], "odd?");
    return { tag: "boolean", value: Math.abs(n) % 2 === 1 };
  });
  defBuiltin("even?", (args) => {
    if (args.length !== 1) throw new EvalError("even?: expected 1 argument");
    const n = expectNum(args[0], "even?");
    return { tag: "boolean", value: n % 2 === 0 };
  });
  defBuiltin("list?", (args) => {
    if (args.length !== 1) throw new EvalError("list?: expected 1 argument");
    let slow = args[0];
    let fast = args[0];
    while (fast.tag === "pair") {
      slow = slow.cdr;
      fast = fast.cdr;
      if (fast.tag !== "pair") break;
      fast = fast.cdr;
      if (slow === fast) return { tag: "boolean", value: false };
    }
    return { tag: "boolean", value: fast.tag === "nil" };
  });
  defBuiltin("list-ref", (args) => {
    if (args.length !== 2) throw new EvalError("list-ref: expected 2 arguments");
    let cur = args[0];
    let idx = expectNum(args[1], "list-ref");
    while (idx > 0 && cur.tag === "pair") {
      cur = cur.cdr;
      idx--;
    }
    if (cur.tag !== "pair") throw new EvalError("list-ref: index out of range");
    return cur.car;
  });
  defBuiltin("list-tail", (args) => {
    if (args.length !== 2) throw new EvalError("list-tail: expected 2 arguments");
    let cur = args[0];
    let idx = expectNum(args[1], "list-tail");
    while (idx > 0) {
      if (cur.tag !== "pair") throw new EvalError("list-tail: index out of range");
      cur = cur.cdr;
      idx--;
    }
    return cur;
  });
  defBuiltin("assoc", (args) => {
    if (args.length !== 2) throw new EvalError("assoc: expected 2 arguments");
    const key = args[0];
    let alist = args[1];
    while (alist.tag === "pair") {
      const entry = alist.car;
      if (entry.tag === "pair" && schemeEqual(entry.car, key)) return entry;
      alist = alist.cdr;
    }
    return { tag: "boolean", value: false };
  });
  defBuiltin("procedure?", (args) => {
    if (args.length !== 1) throw new EvalError("procedure?: expected 1 argument");
    const v = args[0];
    return { tag: "boolean", value: v.tag === "lambda" || v.tag === "builtin" || v.tag === "case-lambda" || v.tag === "continuation" };
  });
  defBuiltin("syntax->datum", (args) => {
    if (args.length !== 1) throw new EvalError("syntax->datum: expected 1 argument");
    return args[0];
  });
  defBuiltin("datum->syntax", (args) => {
    if (args.length !== 2) throw new EvalError("datum->syntax: expected 2 arguments");
    return args[1];
  });
  defBuiltin("for-each", (args) => {
    if (args.length < 2) throw new EvalError("for-each: expected at least 2 arguments");
    const func = args[0];
    const lists = args.slice(1).map((a) => pairToArray(a));
    const len = lists[0].length;
    for (let i = 0; i < len; i++) {
      const callArgs = lists.map((l) => l[i]);
      applySync(func, callArgs);
    }
    return VOID;
  });
  function cxrNav(val, ops, name) {
    let cur = val;
    for (let i = ops.length - 1; i >= 0; i--) {
      if (cur.tag !== "pair") throw new EvalError(`${name}: expected pair`);
      cur = ops[i] === "a" ? cur.car : cur.cdr;
    }
    return cur;
  }
  for (const ops of [
    "aa",
    "ad",
    "da",
    "dd",
    "aaa",
    "aad",
    "ada",
    "add",
    "daa",
    "dad",
    "dda",
    "ddd",
    "aaaa",
    "aaad",
    "aada",
    "aadd",
    "adaa",
    "adad",
    "adda",
    "addd",
    "daaa",
    "daad",
    "dada",
    "dadd",
    "ddaa",
    "ddad",
    "ddda",
    "dddd"
  ]) {
    const name = `c${ops}r`;
    defBuiltin(name, (args) => {
      if (args.length !== 1) throw new EvalError(`${name}: expected 1 argument`);
      return cxrNav(args[0], ops, name);
    });
  }
  defBuiltin("memq", (args) => {
    if (args.length !== 2) throw new EvalError("memq: expected 2 arguments");
    let cur = args[1];
    while (cur.tag === "pair") {
      const a = args[0], b = cur.car;
      if (a.tag === b.tag) {
        if (a.tag === "symbol" && b.tag === "symbol" && a.value === b.value) return cur;
        if (a.tag === "number" && b.tag === "number" && a.value === b.value) return cur;
        if (a.tag === "rational" && b.tag === "rational" && a.num === b.num && a.den === b.den) return cur;
        if (a.tag === "boolean" && b.tag === "boolean" && a.value === b.value) return cur;
        if (a.tag === "char" && b.tag === "char" && a.value === b.value) return cur;
        if (a.tag === "nil" && b.tag === "nil") return cur;
        if (a === b) return cur;
      }
      cur = cur.cdr;
    }
    return { tag: "boolean", value: false };
  });
  defBuiltin("memv", (args) => {
    if (args.length !== 2) throw new EvalError("memv: expected 2 arguments");
    let cur = args[1];
    while (cur.tag === "pair") {
      if (schemeEqv(args[0], cur.car)) return cur;
      cur = cur.cdr;
    }
    return { tag: "boolean", value: false };
  });
  defBuiltin("assv", (args) => {
    if (args.length !== 2) throw new EvalError("assv: expected 2 arguments");
    const key = args[0];
    let alist = args[1];
    while (alist.tag === "pair") {
      const entry = alist.car;
      if (entry.tag === "pair" && schemeEqv(entry.car, key)) return entry;
      alist = alist.cdr;
    }
    return { tag: "boolean", value: false };
  });
  defBuiltin("member", (args) => {
    if (args.length !== 2) throw new EvalError("member: expected 2 arguments");
    let cur = args[1];
    while (cur.tag === "pair") {
      if (schemeEqual(args[0], cur.car)) return cur;
      cur = cur.cdr;
    }
    return { tag: "boolean", value: false };
  });
  defBuiltin("reverse", (args) => {
    if (args.length !== 1) throw new EvalError("reverse: expected 1 argument");
    let cur = args[0];
    let result = NIL;
    while (cur.tag === "pair") {
      result = { tag: "pair", car: cur.car, cdr: result };
      cur = cur.cdr;
    }
    if (cur.tag !== "nil") throw new EvalError("reverse: expected proper list");
    return result;
  });
  defBuiltin("gcd", (args) => {
    if (args.length === 0) return makeRat(0, 1);
    let result = Math.abs(expectNum(args[0], "gcd"));
    for (let i = 1; i < args.length; i++) {
      let b = Math.abs(expectNum(args[i], "gcd"));
      while (b) {
        [result, b] = [b, result % b];
      }
    }
    return makeRat(result, 1);
  });
  defBuiltin("lcm", (args) => {
    if (args.length === 0) return makeRat(1, 1);
    let result = Math.abs(expectNum(args[0], "lcm"));
    for (let i = 1; i < args.length; i++) {
      let b = Math.abs(expectNum(args[i], "lcm"));
      if (result === 0 && b === 0) {
        result = 0;
        continue;
      }
      let a2 = result, b2 = b;
      while (b2) {
        [a2, b2] = [b2, a2 % b2];
      }
      result = result / a2 * b;
    }
    return makeRat(result, 1);
  });
  defBuiltin("truncate", (args) => {
    if (args.length !== 1) throw new EvalError("truncate: expected 1 argument");
    assertNumeric(args[0], "truncate");
    return { tag: "number", value: Math.trunc(toFloat(args[0])) };
  });
  defBuiltin("round", (args) => {
    if (args.length !== 1) throw new EvalError("round: expected 1 argument");
    assertNumeric(args[0], "round");
    return { tag: "number", value: Math.round(toFloat(args[0])) };
  });
  defBuiltin("floor", (args) => {
    if (args.length !== 1) throw new EvalError("floor: expected 1 argument");
    assertNumeric(args[0], "floor");
    return { tag: "number", value: Math.floor(toFloat(args[0])) };
  });
  defBuiltin("ceiling", (args) => {
    if (args.length !== 1) throw new EvalError("ceiling: expected 1 argument");
    assertNumeric(args[0], "ceiling");
    return { tag: "number", value: Math.ceil(toFloat(args[0])) };
  });
  defBuiltin("make-string", (args) => {
    if (args.length < 1 || args.length > 2) throw new EvalError("make-string: expected 1 or 2 arguments");
    const len = expectNum(args[0], "make-string");
    const ch = args.length === 2 && args[1].tag === "char" ? args[1].value : "\0";
    return { tag: "string", value: ch.repeat(len), mutable: true };
  });
  defBuiltin("string", (args) => {
    let result = "";
    for (const a of args) {
      if (a.tag !== "char") throw new EvalError("string: expected char arguments");
      result += a.value;
    }
    return { tag: "string", value: result };
  });
  defBuiltin("string>?", (args) => {
    if (args.length !== 2 || args[0].tag !== "string" || args[1].tag !== "string")
      throw new EvalError("string>?: expected 2 strings");
    return { tag: "boolean", value: args[0].value > args[1].value };
  });
  defBuiltin("string<=?", (args) => {
    if (args.length !== 2 || args[0].tag !== "string" || args[1].tag !== "string")
      throw new EvalError("string<=?: expected 2 strings");
    return { tag: "boolean", value: args[0].value <= args[1].value };
  });
  defBuiltin("string>=?", (args) => {
    if (args.length !== 2 || args[0].tag !== "string" || args[1].tag !== "string")
      throw new EvalError("string>=?: expected 2 strings");
    return { tag: "boolean", value: args[0].value >= args[1].value };
  });
  defBuiltin("error", (args) => {
    if (args.length < 1) throw new EvalError("error: expected at least 1 argument");
    const parts = args.map((a) => a.tag === "string" ? a.value : displayVal(a));
    throw new EvalError(parts.join(" "));
  });
  defBuiltin("set-car!", (args) => {
    if (args.length !== 2) throw new EvalError("set-car!: expected 2 arguments");
    if (args[0].tag !== "pair") throw new EvalError("set-car!: expected pair");
    args[0].car = args[1];
    return VOID;
  });
  defBuiltin("set-cdr!", (args) => {
    if (args.length !== 2) throw new EvalError("set-cdr!: expected 2 arguments");
    if (args[0].tag !== "pair") throw new EvalError("set-cdr!: expected pair");
    args[0].cdr = args[1];
    return VOID;
  });
  defBuiltin("map", (args) => {
    if (args.length < 2) throw new EvalError("map: expected at least 2 arguments");
    const func = args[0];
    const lists = args.slice(1).map((a) => pairToArray(a));
    const len = lists[0].length;
    const result = [];
    for (let i = 0; i < len; i++) {
      const callArgs = lists.map((l) => l[i]);
      result.push(applySync(func, callArgs));
    }
    return makeList(result);
  });
  defBuiltin("char=?", (args) => {
    if (args.length !== 2) throw new EvalError("char=?: expected 2 arguments");
    if (args[0].tag !== "char" || args[1].tag !== "char") throw new EvalError("char=?: expected chars");
    return { tag: "boolean", value: args[0].value === args[1].value };
  });
  defBuiltin("char<?", (args) => {
    if (args.length !== 2) throw new EvalError("char<?: expected 2 arguments");
    if (args[0].tag !== "char" || args[1].tag !== "char") throw new EvalError("char<?: expected chars");
    return { tag: "boolean", value: args[0].value < args[1].value };
  });
  defBuiltin("char-alphabetic?", (args) => {
    if (args.length !== 1 || args[0].tag !== "char") throw new EvalError("char-alphabetic?: expected 1 char");
    return { tag: "boolean", value: /^[a-zA-Z]$/.test(args[0].value) };
  });
  defBuiltin("char-numeric?", (args) => {
    if (args.length !== 1 || args[0].tag !== "char") throw new EvalError("char-numeric?: expected 1 char");
    return { tag: "boolean", value: /^[0-9]$/.test(args[0].value) };
  });
  defBuiltin("char-upcase", (args) => {
    if (args.length !== 1 || args[0].tag !== "char") throw new EvalError("char-upcase: expected 1 char");
    return { tag: "char", value: args[0].value.toUpperCase() };
  });
  defBuiltin("char-downcase", (args) => {
    if (args.length !== 1 || args[0].tag !== "char") throw new EvalError("char-downcase: expected 1 char");
    return { tag: "char", value: args[0].value.toLowerCase() };
  });
  defBuiltin("string=?", (args) => {
    if (args.length !== 2) throw new EvalError("string=?: expected 2 arguments");
    if (args[0].tag !== "string" || args[1].tag !== "string") throw new EvalError("string=?: expected strings");
    return { tag: "boolean", value: args[0].value === args[1].value };
  });
  defBuiltin("string<?", (args) => {
    if (args.length !== 2) throw new EvalError("string<?: expected 2 arguments");
    if (args[0].tag !== "string" || args[1].tag !== "string") throw new EvalError("string<?: expected strings");
    return { tag: "boolean", value: args[0].value < args[1].value };
  });
  defBuiltin("string-ci=?", (args) => {
    if (args.length !== 2) throw new EvalError("string-ci=?: expected 2 arguments");
    if (args[0].tag !== "string" || args[1].tag !== "string") throw new EvalError("string-ci=?: expected strings");
    return { tag: "boolean", value: args[0].value.toLowerCase() === args[1].value.toLowerCase() };
  });
  defBuiltin("string-upcase", (args) => {
    if (args.length !== 1 || args[0].tag !== "string") throw new EvalError("string-upcase: expected 1 string");
    return { tag: "string", value: args[0].value.toUpperCase() };
  });
  defBuiltin("string-downcase", (args) => {
    if (args.length !== 1 || args[0].tag !== "string") throw new EvalError("string-downcase: expected 1 string");
    return { tag: "string", value: args[0].value.toLowerCase() };
  });
  defBuiltin("vector", (args) => {
    return { tag: "vector", elements: [...args] };
  });
  defBuiltin("make-vector", (args) => {
    if (args.length < 1 || args.length > 2) throw new EvalError("make-vector: expected 1 or 2 arguments");
    const len = expectNum(args[0], "make-vector");
    const fill = args.length === 2 ? args[1] : makeRat(0, 1);
    const elements = [];
    for (let i = 0; i < len; i++) elements.push(fill);
    return { tag: "vector", elements };
  });
  defBuiltin("vector-ref", (args) => {
    if (args.length !== 2) throw new EvalError("vector-ref: expected 2 arguments");
    if (args[0].tag !== "vector") throw new EvalError("vector-ref: expected vector");
    const idx = expectNum(args[1], "vector-ref");
    if (idx < 0 || idx >= args[0].elements.length) throw new EvalError("vector-ref: index out of range");
    return args[0].elements[idx];
  });
  defBuiltin("vector-set!", (args) => {
    if (args.length !== 3) throw new EvalError("vector-set!: expected 3 arguments");
    if (args[0].tag !== "vector") throw new EvalError("vector-set!: expected vector");
    const idx = expectNum(args[1], "vector-set!");
    if (idx < 0 || idx >= args[0].elements.length) throw new EvalError("vector-set!: index out of range");
    args[0].elements[idx] = args[2];
    return VOID;
  });
  defBuiltin("vector-length", (args) => {
    if (args.length !== 1) throw new EvalError("vector-length: expected 1 argument");
    if (args[0].tag !== "vector") throw new EvalError("vector-length: expected vector");
    return makeRat(args[0].elements.length, 1);
  });
  defBuiltin("vector?", (args) => {
    if (args.length !== 1) throw new EvalError("vector?: expected 1 argument");
    return { tag: "boolean", value: args[0].tag === "vector" };
  });
  defBuiltin("vector->list", (args) => {
    if (args.length !== 1) throw new EvalError("vector->list: expected 1 argument");
    if (args[0].tag !== "vector") throw new EvalError("vector->list: expected vector");
    return makeList(args[0].elements);
  });
  defBuiltin("list->vector", (args) => {
    if (args.length !== 1) throw new EvalError("list->vector: expected 1 argument");
    return { tag: "vector", elements: pairToArray(args[0]) };
  });
  defBuiltin("apply", (_args) => {
    throw new EvalError("apply: internal error - should be handled by CPS evaluator");
  });
  defBuiltin("call/cc", (_args) => {
    throw new EvalError("call/cc: internal error - should be handled by CPS evaluator");
  });
  defBuiltin("call-with-current-continuation", (_args) => {
    throw new EvalError("call-with-current-continuation: internal error");
  });
  defBuiltin("dynamic-wind", (_args) => {
    throw new EvalError("dynamic-wind: internal error - should be handled by CPS evaluator");
  });
  defBuiltin("raise", (_args) => {
    throw new EvalError("raise: internal error - should be handled by CPS evaluator");
  });
  defBuiltin("values", (_args) => {
    throw new EvalError("values: internal error - should be handled by CPS evaluator");
  });
  defBuiltin("call-with-values", (_args) => {
    throw new EvalError("call-with-values: internal error - should be handled by CPS evaluator");
  });
  defBuiltin("with-exception-handler", (_args) => {
    throw new EvalError("with-exception-handler: internal error - should be handled by CPS evaluator");
  });
  return env;
}
function tokenize(input) {
  const tokens = [];
  let i = 0;
  let line = 1, col = 1;
  function advance() {
    const ch = input[i++];
    if (ch === "\n") {
      line++;
      col = 1;
    } else {
      col++;
    }
    return ch;
  }
  while (i < input.length) {
    const ch = input[i];
    if (ch === ";") {
      while (i < input.length && input[i] !== "\n") advance();
      continue;
    }
    if (/\s/.test(ch)) {
      advance();
      continue;
    }
    const startPos = { line, col };
    if (ch === "(" || ch === ")") {
      advance();
      tokens.push({ text: ch, pos: startPos });
      continue;
    }
    if (ch === "'") {
      advance();
      tokens.push({ text: "'", pos: startPos });
      continue;
    }
    if (ch === "`") {
      advance();
      tokens.push({ text: "`", pos: startPos });
      continue;
    }
    if (ch === ",") {
      advance();
      if (i < input.length && input[i] === "@") {
        advance();
        tokens.push({ text: ",@", pos: startPos });
      } else {
        tokens.push({ text: ",", pos: startPos });
      }
      continue;
    }
    if (ch === '"') {
      let s = '"';
      advance();
      while (i < input.length && input[i] !== '"') {
        if (input[i] === "\\") {
          s += input[i];
          advance();
        }
        s += input[i];
        advance();
      }
      s += '"';
      advance();
      tokens.push({ text: s, pos: startPos });
      continue;
    }
    if (ch === "#") {
      if (input[i + 1] === "'") {
        advance();
        advance();
        tokens.push({ text: "#'", pos: startPos });
        continue;
      }
      if (input[i + 1] === "t" && (i + 2 >= input.length || /[\s()]/.test(input[i + 2]))) {
        advance();
        advance();
        tokens.push({ text: "#t", pos: startPos });
        continue;
      }
      if (input[i + 1] === "f" && (i + 2 >= input.length || /[\s()]/.test(input[i + 2]))) {
        advance();
        advance();
        tokens.push({ text: "#f", pos: startPos });
        continue;
      }
    }
    let tok = "";
    while (i < input.length && !/[\s()]/.test(input[i])) {
      tok += input[i];
      advance();
    }
    tokens.push({ text: tok, pos: startPos });
  }
  return tokens;
}
function parse(tokens) {
  let idx = 0;
  function parseExpr() {
    if (idx >= tokens.length) throw new EvalError("unexpected end of input");
    const tok = tokens[idx++];
    if (tok.text === "(") {
      const elems = [];
      while (idx < tokens.length && tokens[idx].text !== ")") {
        elems.push(parseExpr());
      }
      if (idx >= tokens.length) throw new EvalError("missing closing paren");
      idx++;
      return { tag: "list", elements: elems, pos: tok.pos };
    }
    if (tok.text === ")") throw new EvalError("unexpected )");
    if (tok.text === "'") {
      const inner = parseExpr();
      return { tag: "list", elements: [{ tag: "symbol", value: "quote", pos: tok.pos }, inner], pos: tok.pos };
    }
    if (tok.text === "#'") {
      const inner = parseExpr();
      return { tag: "list", elements: [{ tag: "symbol", value: "syntax", pos: tok.pos }, inner], pos: tok.pos };
    }
    if (tok.text === "`") {
      const inner = parseExpr();
      return { tag: "list", elements: [{ tag: "symbol", value: "quasiquote", pos: tok.pos }, inner], pos: tok.pos };
    }
    if (tok.text === ",") {
      const inner = parseExpr();
      return { tag: "list", elements: [{ tag: "symbol", value: "unquote", pos: tok.pos }, inner], pos: tok.pos };
    }
    if (tok.text === ",@") {
      const inner = parseExpr();
      return { tag: "list", elements: [{ tag: "symbol", value: "unquote-splicing", pos: tok.pos }, inner], pos: tok.pos };
    }
    return parseAtom(tok);
  }
  function parseAtom(tok) {
    if (tok.text === "#t") return { tag: "boolean", value: true, pos: tok.pos };
    if (tok.text === "#f") return { tag: "boolean", value: false, pos: tok.pos };
    if (tok.text.startsWith('"')) return { tag: "string", value: tok.text.slice(1, -1), pos: tok.pos };
    if (tok.text.startsWith("#\\")) {
      const charPart = tok.text.slice(2);
      if (charPart === "space") return { tag: "char", value: " ", pos: tok.pos };
      if (charPart === "newline") return { tag: "char", value: "\n", pos: tok.pos };
      if (charPart === "tab") return { tag: "char", value: "	", pos: tok.pos };
      if (charPart.length === 1) return { tag: "char", value: charPart, pos: tok.pos };
      throw new EvalError(`unknown character literal: ${tok.text}`);
    }
    if (/^-?\d+\/\d+$/.test(tok.text)) {
      const parts = tok.text.split("/");
      const r = makeRat(parseInt(parts[0], 10), parseInt(parts[1], 10));
      r.pos = tok.pos;
      return r;
    }
    if (/^-?\d+\.\d+$/.test(tok.text)) return { tag: "number", value: parseFloat(tok.text), pos: tok.pos };
    if (/^-?\d+$/.test(tok.text)) {
      const r = makeRat(parseInt(tok.text, 10), 1);
      r.pos = tok.pos;
      return r;
    }
    return { tag: "symbol", value: tok.text, pos: tok.pos };
  }
  const exprs = [];
  while (idx < tokens.length) {
    exprs.push(parseExpr());
  }
  return exprs;
}
function quoteSyntaxToValue(expr) {
  if (expr.tag === "list") {
    const elems = expr.elements;
    const dotIdx = elems.findIndex((e) => e.tag === "symbol" && e.value === ".");
    if (dotIdx >= 0 && dotIdx === elems.length - 2) {
      const tail = quoteSyntaxToValue(elems[elems.length - 1]);
      let result = tail;
      for (let i = dotIdx - 1; i >= 0; i--) {
        result = { tag: "pair", car: quoteSyntaxToValue(elems[i]), cdr: result };
      }
      return result;
    }
    const items = elems.map(quoteSyntaxToValue);
    return makeList(items);
  }
  return expr;
}
function parseParams(paramList, pos) {
  const params = [];
  for (let i = 0; i < paramList.length; i++) {
    const p = paramList[i];
    if (p.tag === "symbol" && p.value === ".") {
      if (i !== paramList.length - 2) throw errAt("bad dot syntax in parameter list", pos);
      const restParam = paramList[i + 1];
      if (restParam.tag !== "symbol") throw errAt("rest parameter must be a symbol", pos);
      return { params, rest: restParam.value };
    }
    if (p.tag !== "symbol") throw errAt("parameter must be a symbol", pos);
    params.push(p.value);
  }
  return { params };
}
var gensymCounter = 0;
function gensym(prefix) {
  return `__${prefix}_${++gensymCounter}`;
}
var SPECIAL_FORMS = /* @__PURE__ */ new Set([
  "quote",
  "quasiquote",
  "if",
  "define",
  "lambda",
  "case-lambda",
  "set!",
  "begin",
  "let",
  "let*",
  "letrec",
  "letrec*",
  "cond",
  "and",
  "or",
  "not",
  "define-syntax",
  "syntax-rules",
  "syntax-case",
  "syntax",
  "with-syntax",
  "case",
  "do",
  "call/cc",
  "call-with-current-continuation",
  "guard",
  "define-record-type"
]);
var syntaxBindingsStack = [];
var pendingSyntaxRenames = [];
function matchPattern(pattern, input, literals, bindings) {
  let pi = 0, ii = 0;
  while (pi < pattern.length) {
    if (pi + 1 < pattern.length && pattern[pi + 1].tag === "symbol" && pattern[pi + 1].value === "...") {
      const subPat = pattern[pi];
      const remaining = pattern.length - pi - 2;
      const matchCount = input.length - ii - remaining;
      if (matchCount < 0) return false;
      if (subPat.tag === "symbol" && !literals.has(subPat.value)) {
        const values = [];
        for (let k = 0; k < matchCount; k++) values.push(input[ii + k]);
        bindings.set(subPat.value, { ellipsis: true, values });
      }
      ii += matchCount;
      pi += 2;
      continue;
    }
    if (ii >= input.length) return false;
    const pat = pattern[pi], inp = input[ii];
    if (pat.tag === "symbol") {
      if (literals.has(pat.value)) {
        if (inp.tag !== "symbol" || inp.value !== pat.value) return false;
      } else if (pat.value !== "_") {
        bindings.set(pat.value, { ellipsis: false, value: inp });
      }
    } else if (pat.tag === "list") {
      if (inp.tag !== "list") return false;
      if (!matchPattern(pat.elements, inp.elements, literals, bindings)) return false;
    }
    pi++;
    ii++;
  }
  return ii === input.length;
}
function collectIntroduced(template, patVars, result) {
  if (template.tag === "symbol") {
    if (!patVars.has(template.value) && !SPECIAL_FORMS.has(template.value) && template.value !== "...") {
      result.add(template.value);
    }
  } else if (template.tag === "list") {
    if (template.elements.length >= 1 && template.elements[0].tag === "symbol" && template.elements[0].value === "quote") return;
    for (const elem of template.elements) collectIntroduced(elem, patVars, result);
  }
}
function collectEllipsisVars(template, bindings) {
  const vars = [];
  if (template.tag === "symbol") {
    const b = bindings.get(template.value);
    if (b && b.ellipsis) vars.push(template.value);
  } else if (template.tag === "list") {
    for (const elem of template.elements) vars.push(...collectEllipsisVars(elem, bindings));
  }
  return vars;
}
function expandTemplate(template, bindings, renames) {
  if (template.tag === "symbol") {
    const b = bindings.get(template.value);
    if (b) {
      if (!b.ellipsis) return b.value;
      throw new EvalError("syntax: ellipsis variable outside ellipsis context");
    }
    const r = renames.get(template.value);
    if (r) return { tag: "symbol", value: r };
    return template;
  }
  if (template.tag === "list") {
    if (template.elements.length >= 1 && template.elements[0].tag === "symbol" && template.elements[0].value === "quote") {
      return template;
    }
    const result = [];
    for (let i = 0; i < template.elements.length; i++) {
      if (i + 1 < template.elements.length && template.elements[i + 1].tag === "symbol" && template.elements[i + 1].value === "...") {
        const sub = template.elements[i];
        const evars = collectEllipsisVars(sub, bindings);
        if (evars.length > 0) {
          const count = bindings.get(evars[0]).values.length;
          for (let k = 0; k < count; k++) {
            const iter = new Map(bindings);
            for (const v of evars) {
              const vb = bindings.get(v);
              iter.set(v, { ellipsis: false, value: vb.values[k] });
            }
            result.push(expandTemplate(sub, iter, renames));
          }
        }
        i++;
        continue;
      }
      result.push(expandTemplate(template.elements[i], bindings, renames));
    }
    return { tag: "list", elements: result };
  }
  return template;
}
function expandMacro(macro, form) {
  const literalSet = new Set(macro.literals);
  for (const clause of macro.clauses) {
    const bindings = /* @__PURE__ */ new Map();
    if (matchPattern(clause.pattern.slice(1), form.slice(1), literalSet, bindings)) {
      const patVars = new Set(bindings.keys());
      const introduced = /* @__PURE__ */ new Set();
      collectIntroduced(clause.template, patVars, introduced);
      const renames = /* @__PURE__ */ new Map();
      for (const sym of introduced) renames.set(sym, gensym(sym));
      const expanded = expandTemplate(clause.template, bindings, renames);
      return { expanded, renames: Array.from(renames.entries()) };
    }
  }
  throw new EvalError("syntax-rules: no matching pattern");
}
function isTruthy(val) {
  return !(val.tag === "boolean" && val.value === false);
}
var identityCont = (v) => v;
var CC_SYMBOLS = /* @__PURE__ */ new Set(["call/cc", "call-with-current-continuation", "dynamic-wind", "raise", "with-exception-handler", "guard", "values", "call-with-values"]);
function exprMayCallCC(expr) {
  const cached = expr._mcc;
  if (cached !== void 0) return cached;
  let result;
  if (expr.tag === "symbol") result = CC_SYMBOLS.has(expr.value);
  else if (expr.tag === "list") result = expr.elements.some(exprMayCallCC);
  else result = false;
  expr._mcc = result;
  return result;
}
function evalFast(expr, env) {
  if (expr.tag === "symbol") {
    try {
      return env.get(expr.value, expr.pos);
    } catch {
      return null;
    }
  }
  if (expr.tag === "number" || expr.tag === "rational" || expr.tag === "boolean" || expr.tag === "string" || expr.tag === "char") return expr;
  return null;
}
function evalCallDirect(expr, env) {
  if (expr.tag !== "list") return evalFast(expr, env);
  const elems = expr.elements;
  if (elems.length === 0) return null;
  const func = evalFast(elems[0], env);
  if (func === null || func.tag !== "builtin") return null;
  if (CC_SYMBOLS.has(func.name) || func.name === "apply") return null;
  const args = [];
  for (let i = 1; i < elems.length; i++) {
    const arg = evalFast(elems[i], env);
    if (arg === null) return null;
    args.push(arg);
  }
  try {
    return func.fn(args);
  } catch (e) {
    if (e instanceof EvalError) throw errAt(e.message, expr.pos);
    throw e;
  }
}
var callccActive = false;
var fastPathCont = null;
function evalSeqCPS(exprs, idx, env, k) {
  while (idx < exprs.length - 1 && !callccActive && !exprMayCallCC(exprs[idx])) {
    const nextIdx = idx + 1;
    const cpsK = (_) => evalSeqCPS(exprs, nextIdx, env, k);
    fastPathCont = cpsK;
    const smartK = (val) => callccActive ? cpsK(val) : val;
    runTrampoline(evalCPS(exprs[idx], env, smartK));
    fastPathCont = null;
    if (callccActive) {
      return evalSeqCPS(exprs, nextIdx, env, k);
    }
    idx++;
  }
  if (idx >= exprs.length) return k(VOID);
  if (idx === exprs.length - 1) return evalCPS(exprs[idx], env, k);
  return evalCPS(exprs[idx], env, (_) => evalSeqCPS(exprs, idx + 1, env, k));
}
function evalArgsCPS(exprs, idx, env, acc, k) {
  if (idx >= exprs.length) return k(acc);
  return evalCPS(exprs[idx], env, (val) => {
    acc.push(val);
    return evalArgsCPS(exprs, idx + 1, env, acc, k);
  });
}
function applyCPS(func, args, pos, k) {
  if (func.tag === "continuation") {
    let unwind2 = function(idx) {
      if (idx <= commonLen) return rewind2(commonLen);
      const entry = currentWinds[idx - 1];
      windStack.pop();
      return applyCPS(entry.outThunk, [], pos, (_) => unwind2(idx - 1));
    }, rewind2 = function(idx) {
      if (idx >= targetWinds.length) return mkBounce(() => func.k(theVal));
      const entry = targetWinds[idx];
      return applyCPS(entry.inThunk, [], pos, (_) => {
        windStack.push(entry);
        return rewind2(idx + 1);
      });
    };
    var unwind = unwind2, rewind = rewind2;
    contReentry = true;
    let val;
    if (args.length === 1) {
      val = args[0];
    } else {
      val = { tag: "values", vals: args };
    }
    const theVal = val;
    const targetWinds = func.winds;
    const currentWinds = [...windStack];
    let commonLen = 0;
    while (commonLen < currentWinds.length && commonLen < targetWinds.length && currentWinds[commonLen] === targetWinds[commonLen]) {
      commonLen++;
    }
    return unwind2(currentWinds.length);
  }
  if (func.tag === "lambda") {
    if (!func.rest && contReentry && args.length > func.params.length) {
      contReentry = false;
      const fixed = args.slice(0, func.params.length - 1);
      fixed.push(args[args.length - 1]);
      args = fixed;
    }
    contReentry = false;
    if (func.rest) {
      if (args.length < func.params.length) {
        throw errAt(`lambda: expected at least ${func.params.length} arguments, got ${args.length}`, pos);
      }
    } else {
      if (args.length !== func.params.length) {
        throw errAt(`lambda: expected ${func.params.length} arguments, got ${args.length}`, pos);
      }
    }
    const callEnv = new Env(func.env);
    for (let i = 0; i < func.params.length; i++) {
      callEnv.define(func.params[i], args[i]);
    }
    if (func.rest) {
      callEnv.define(func.rest, makeList(args.slice(func.params.length)));
    }
    return mkBounce(() => evalSeqCPS(func.body, 0, callEnv, k));
  }
  if (func.tag === "builtin") {
    if (func.name === "call/cc" || func.name === "call-with-current-continuation") {
      if (args.length !== 1) throw errAt("call/cc: expected 1 argument", pos);
      const contVal = { tag: "continuation", k, winds: [...windStack] };
      return applyCPS(args[0], [contVal], pos, k);
    }
    if (func.name === "dynamic-wind") {
      if (args.length !== 3) throw errAt("dynamic-wind: expected 3 arguments", pos);
      const [inThunk, bodyThunk, outThunk] = args;
      const entry = { inThunk, outThunk };
      return applyCPS(inThunk, [], pos, (_) => {
        windStack.push(entry);
        return applyCPS(bodyThunk, [], pos, (bodyVal) => {
          windStack.pop();
          return applyCPS(outThunk, [], pos, (_2) => {
            return k(bodyVal);
          });
        });
      });
    }
    if (func.name === "raise") {
      let unwindForRaise2 = function(idx) {
        if (idx <= commonLen) return entry.handler(val);
        const w = currentWinds[idx - 1];
        windStack.pop();
        return applyCPS(w.outThunk, [], pos, (_) => unwindForRaise2(idx - 1));
      };
      var unwindForRaise = unwindForRaise2;
      if (args.length !== 1) throw errAt("raise: expected 1 argument", pos);
      const val = args[0];
      if (exceptionHandlers.length === 0) {
        throw new EvalError(`unhandled exception: ${displayVal(val)}`);
      }
      const entry = exceptionHandlers.pop();
      const currentWinds = [...windStack];
      const targetWinds = entry.winds;
      let commonLen = 0;
      while (commonLen < currentWinds.length && commonLen < targetWinds.length && currentWinds[commonLen] === targetWinds[commonLen]) {
        commonLen++;
      }
      return unwindForRaise2(currentWinds.length);
    }
    if (func.name === "with-exception-handler") {
      if (args.length !== 2) throw errAt("with-exception-handler: expected 2 arguments", pos);
      const [handlerProc, thunk] = args;
      const savedWinds = [...windStack];
      exceptionHandlers.push({
        handler: (val) => {
          return applyCPS(handlerProc, [val], pos, (_result) => {
            throw new EvalError("raise: handler returned");
          });
        },
        winds: savedWinds
      });
      return applyCPS(thunk, [], pos, (result) => {
        exceptionHandlers.pop();
        return k(result);
      });
    }
    if (func.name === "values") {
      if (args.length === 1) return k(args[0]);
      return k({ tag: "values", vals: args });
    }
    if (func.name === "call-with-values") {
      if (args.length !== 2) throw errAt("call-with-values: expected 2 arguments", pos);
      const [producer, consumer] = args;
      return applyCPS(producer, [], pos, (result) => {
        const vals = result.tag === "values" ? result.vals : [result];
        return applyCPS(consumer, vals, pos, k);
      });
    }
    if (func.name === "apply") {
      if (args.length < 2) throw new EvalError("apply: expected at least 2 arguments");
      const applyFunc = args[0];
      const lastArg = args[args.length - 1];
      const prefixArgs = args.slice(1, args.length - 1);
      const tailArgs = pairToArray(lastArg);
      const allArgs = [...prefixArgs, ...tailArgs];
      return applyCPS(applyFunc, allArgs, pos, k);
    }
    try {
      return k(func.fn(args));
    } catch (e) {
      if (e instanceof EvalError && !/^\d+:/.test(e.message)) {
        throw errAt(e.message, pos);
      }
      throw e;
    }
  }
  if (func.tag === "case-lambda") {
    for (const clause of func.clauses) {
      if (clause.rest ? args.length >= clause.params.length : args.length === clause.params.length) {
        const callEnv = new Env(func.env);
        for (let i = 0; i < clause.params.length; i++) {
          callEnv.define(clause.params[i], args[i]);
        }
        if (clause.rest) {
          callEnv.define(clause.rest, makeList(args.slice(clause.params.length)));
        }
        return mkBounce(() => evalSeqCPS(clause.body, 0, callEnv, k));
      }
    }
    throw new EvalError(`case-lambda: no matching clause for ${args.length} arguments`);
  }
  throw errAt(`not a procedure: ${displayVal(func)}`, pos);
}
function evalQuasiquote(tmpl, env, k) {
  if (tmpl.tag === "list") {
    const elems = tmpl.elements;
    if (elems.length === 2 && elems[0].tag === "symbol" && elems[0].value === "unquote") {
      return evalCPS(elems[1], env, k);
    }
    const dotIdx = elems.findIndex((e) => e.tag === "symbol" && e.value === ".");
    if (dotIdx >= 0 && dotIdx === elems.length - 2) {
      return evalQQList(elems.slice(0, dotIdx), env, (headItems) => {
        return evalQuasiquote(elems[elems.length - 1], env, (tail) => {
          let result = tail;
          for (let i = headItems.length - 1; i >= 0; i--) {
            result = { tag: "pair", car: headItems[i], cdr: result };
          }
          return k(result);
        });
      });
    }
    return evalQQList(elems, env, (items) => {
      return k(makeList(items));
    });
  }
  return k(quoteSyntaxToValue(tmpl));
}
function evalQQList(elems, env, k) {
  const result = [];
  function loop(i) {
    if (i >= elems.length) return k(result);
    const el = elems[i];
    if (el.tag === "list" && el.elements.length === 2 && el.elements[0].tag === "symbol" && el.elements[0].value === "unquote-splicing") {
      return evalCPS(el.elements[1], env, (spliced) => {
        let cur = spliced;
        while (cur.tag === "pair") {
          result.push(cur.car);
          cur = cur.cdr;
        }
        if (cur.tag === "nil") {
        } else if (cur.tag === "list") {
          for (const e of cur.elements) result.push(e);
        }
        return loop(i + 1);
      });
    }
    return evalQuasiquote(el, env, (val) => {
      result.push(val);
      return loop(i + 1);
    });
  }
  return loop(0);
}
function evalCPS(expr, env, k) {
  tailLoop: while (true) {
    checkStepLimit();
    if (expr.tag === "number" || expr.tag === "rational" || expr.tag === "boolean" || expr.tag === "string" || expr.tag === "char") {
      return k(expr);
    }
    if (expr.tag === "symbol") {
      return k(env.get(expr.value, expr.pos));
    }
    if (expr.tag !== "list") throw errAt("cannot evaluate", expr.pos);
    const elems = expr.elements;
    const epos = expr.pos;
    if (elems.length === 0) throw errAt("empty application", epos);
    if (elems[0].tag === "symbol") {
      const op = elems[0].value;
      if (!SPECIAL_FORMS.has(op)) {
        try {
          const macroVal = env.get(op);
          if (macroVal.tag === "macro") {
            let cache = expr._mc;
            if (!cache) {
              cache = expandMacro(macroVal, elems);
              expr._mc = cache;
            }
            const { expanded, renames } = cache;
            for (const [original, renamed] of renames) {
              try {
                env.define(renamed, macroVal.defEnv.get(original));
              } catch {
              }
            }
            expr = expanded;
            continue tailLoop;
          }
          if (macroVal.tag === "syntax-case-macro") {
            const formVal = { tag: "list", elements: elems };
            pendingSyntaxRenames = [];
            return applyCPS(macroVal.transformer, [formVal], epos, (expanded) => {
              for (const { original, renamed, defEnv: dEnv } of pendingSyntaxRenames) {
                try {
                  env.define(renamed, dEnv.get(original));
                } catch {
                }
              }
              pendingSyntaxRenames = [];
              return mkBounce(() => evalCPS(expanded, env, k));
            });
          }
        } catch {
        }
      } else {
        if (op === "quote") {
          if (elems.length !== 2) throw errAt("quote: expected 1 argument", epos);
          return k(quoteSyntaxToValue(elems[1]));
        }
        if (op === "quasiquote") {
          if (elems.length !== 2) throw errAt("quasiquote: expected 1 argument", epos);
          return evalQuasiquote(elems[1], env, k);
        }
        if (op === "if") {
          if (elems.length < 3 || elems.length > 4) throw errAt("if: expected 2 or 3 arguments", epos);
          if (!callccActive && !exprMayCallCC(elems[1])) {
            let cond = evalCallDirect(elems[1], env);
            if (cond === null) cond = runTrampoline(evalCPS(elems[1], env, identityCont));
            if (!callccActive) {
              if (isTruthy(cond)) {
                expr = elems[2];
                continue tailLoop;
              } else if (elems.length === 4) {
                expr = elems[3];
                continue tailLoop;
              } else return k(VOID);
            }
          }
          return evalCPS(elems[1], env, (cond) => {
            if (isTruthy(cond)) {
              return evalCPS(elems[2], env, k);
            } else {
              if (elems.length === 4) return evalCPS(elems[3], env, k);
              return k(VOID);
            }
          });
        }
        if (op === "define") {
          if (elems.length < 3) throw errAt("define: bad syntax", epos);
          const target = elems[1];
          if (target.tag === "symbol") {
            return evalCPS(elems[2], env, (val) => {
              env.define(target.value, val);
              return k(VOID);
            });
          }
          if (target.tag === "list" && target.elements.length > 0 && target.elements[0].tag === "symbol") {
            const name = target.elements[0].value;
            const { params, rest } = parseParams(target.elements.slice(1), epos);
            const body = elems.slice(2);
            const lambda = { tag: "lambda", params, rest, body, env };
            env.define(name, lambda);
            return k(VOID);
          }
          throw errAt("define: bad syntax", epos);
        }
        if (op === "lambda") {
          if (elems.length < 3) throw errAt("lambda: bad syntax", epos);
          const paramList = elems[1];
          if (paramList.tag === "symbol") {
            const body2 = elems.slice(2);
            return k({ tag: "lambda", params: [], rest: paramList.value, body: body2, env });
          }
          if (paramList.tag !== "list") throw errAt("lambda: parameters must be a list", epos);
          const { params, rest } = parseParams(paramList.elements, epos);
          const body = elems.slice(2);
          return k({ tag: "lambda", params, rest, body, env });
        }
        if (op === "case-lambda") {
          if (elems.length < 2) throw errAt("case-lambda: bad syntax", epos);
          const clauses = [];
          for (let i = 1; i < elems.length; i++) {
            const clause = elems[i];
            if (clause.tag !== "list" || clause.elements.length < 2) throw errAt("case-lambda: bad clause", epos);
            const pl = clause.elements[0];
            if (pl.tag !== "list") throw errAt("case-lambda: parameters must be a list", epos);
            const { params, rest } = parseParams(pl.elements, epos);
            const body = clause.elements.slice(1);
            clauses.push({ params, rest, body });
          }
          return k({ tag: "case-lambda", clauses, env });
        }
        if (op === "set!") {
          if (elems.length !== 3) throw errAt("set!: bad syntax", epos);
          if (elems[1].tag !== "symbol") throw errAt("set!: expected symbol", epos);
          if (!callccActive && !exprMayCallCC(elems[2])) {
            let val = evalCallDirect(elems[2], env);
            if (val === null) val = runTrampoline(evalCPS(elems[2], env, identityCont));
            if (!callccActive) {
              env.set(elems[1].value, val, epos);
              return k(VOID);
            }
          }
          return evalCPS(elems[2], env, (val) => {
            env.set(elems[1].value, val, epos);
            return k(VOID);
          });
        }
        if (op === "begin") {
          if (elems.length === 1) return k(VOID);
          if (!callccActive) {
            let idx = 1;
            let safe = true;
            while (idx < elems.length - 1) {
              if (exprMayCallCC(elems[idx])) {
                safe = false;
                break;
              }
              const sub = elems[idx];
              let handled = false;
              if (sub.tag === "list" && sub.elements.length === 3 && sub.elements[0].tag === "symbol" && sub.elements[0].value === "set!" && sub.elements[1].tag === "symbol") {
                const val = evalCallDirect(sub.elements[2], env);
                if (val !== null) {
                  env.set(sub.elements[1].value, val, sub.pos);
                  handled = true;
                }
              }
              if (!handled) {
                runTrampoline(evalCPS(elems[idx], env, identityCont));
              }
              if (callccActive) {
                safe = false;
                break;
              }
              idx++;
            }
            if (safe) {
              expr = elems[elems.length - 1];
              continue tailLoop;
            }
          }
          return evalSeqCPS(elems, 1, env, k);
        }
        if (op === "let") {
          let evalLetBindings2 = function(idx, vals) {
            if (idx >= bindings.elements.length) {
              const letEnv = new Env(env);
              for (let i = 0; i < bindings.elements.length; i++) {
                letEnv.define(bindings.elements[i].elements[0].value, vals[i]);
              }
              return evalSeqCPS(elems, 2, letEnv, k);
            }
            const b = bindings.elements[idx];
            if (b.tag !== "list" || b.elements.length !== 2 || b.elements[0].tag !== "symbol")
              throw new EvalError("let: bad binding");
            return evalCPS(b.elements[1], env, (val) => {
              return mkBounce(() => evalLetBindings2(idx + 1, [...vals, val]));
            });
          };
          var evalLetBindings = evalLetBindings2;
          if (elems.length >= 3 && elems[1].tag === "symbol") {
            let evalNamedLetInits2 = function(idx, vals) {
              if (idx >= bindingsList.elements.length) {
                const lambda = { tag: "lambda", params: paramNames, body, env };
                const letEnv = new Env(env);
                letEnv.define(name, lambda);
                lambda.env = letEnv;
                const callEnv = new Env(letEnv);
                for (let i = 0; i < paramNames.length; i++) {
                  callEnv.define(paramNames[i], vals[i]);
                }
                return mkBounce(() => evalSeqCPS(body, 0, callEnv, k));
              }
              return evalCPS(bindingsList.elements[idx].elements[1], env, (val) => {
                return evalNamedLetInits2(idx + 1, [...vals, val]);
              });
            };
            var evalNamedLetInits = evalNamedLetInits2;
            const name = elems[1].value;
            const bindingsList = elems[2];
            if (bindingsList.tag !== "list") throw errAt("let: bad syntax", epos);
            const paramNames = [];
            for (const b of bindingsList.elements) {
              if (b.tag !== "list" || b.elements.length !== 2 || b.elements[0].tag !== "symbol")
                throw errAt("let: bad binding", epos);
              paramNames.push(b.elements[0].value);
            }
            const body = elems.slice(3);
            return evalNamedLetInits2(0, []);
          }
          if (elems.length < 3) throw errAt("let: bad syntax", epos);
          const bindings = elems[1];
          if (bindings.tag !== "list") throw errAt("let: bad syntax", epos);
          return evalLetBindings2(0, []);
        }
        if (op === "let*") {
          let evalLetStarBindings2 = function(idx) {
            if (idx >= bindings.elements.length) {
              return evalSeqCPS(elems, 2, letStarEnv, k);
            }
            const b = bindings.elements[idx];
            if (b.tag !== "list" || b.elements.length !== 2 || b.elements[0].tag !== "symbol")
              throw errAt("let*: bad binding", epos);
            return evalCPS(b.elements[1], letStarEnv, (val) => {
              letStarEnv.define(b.elements[0].value, val);
              return evalLetStarBindings2(idx + 1);
            });
          };
          var evalLetStarBindings = evalLetStarBindings2;
          if (elems.length < 3) throw errAt("let*: bad syntax", epos);
          const bindings = elems[1];
          if (bindings.tag !== "list") throw errAt("let*: bad syntax", epos);
          const letStarEnv = new Env(env);
          return evalLetStarBindings2(0);
        }
        if (op === "letrec") {
          let evalLetrecBindings2 = function(idx) {
            if (idx >= bindings.elements.length) {
              return evalSeqCPS(elems, 2, letrecEnv, k);
            }
            return evalCPS(bindings.elements[idx].elements[1], letrecEnv, (val) => {
              letrecEnv.define(names[idx], val);
              return evalLetrecBindings2(idx + 1);
            });
          };
          var evalLetrecBindings = evalLetrecBindings2;
          if (elems.length < 3) throw errAt("letrec: bad syntax", epos);
          const bindings = elems[1];
          if (bindings.tag !== "list") throw errAt("letrec: bad syntax", epos);
          const letrecEnv = new Env(env);
          const names = [];
          for (const b of bindings.elements) {
            if (b.tag !== "list" || b.elements.length !== 2 || b.elements[0].tag !== "symbol")
              throw errAt("letrec: bad binding", epos);
            names.push(b.elements[0].value);
            letrecEnv.define(b.elements[0].value, VOID);
          }
          return evalLetrecBindings2(0);
        }
        if (op === "letrec*") {
          let evalLetrecStarBindings2 = function(idx) {
            if (idx >= bindings.elements.length) {
              return evalSeqCPS(elems, 2, letrecEnv, k);
            }
            const b = bindings.elements[idx];
            if (b.tag !== "list" || b.elements.length !== 2 || b.elements[0].tag !== "symbol")
              throw errAt("letrec*: bad binding", epos);
            return evalCPS(b.elements[1], letrecEnv, (val) => {
              letrecEnv.define(b.elements[0].value, val);
              return evalLetrecStarBindings2(idx + 1);
            });
          };
          var evalLetrecStarBindings = evalLetrecStarBindings2;
          if (elems.length < 3) throw errAt("letrec*: bad syntax", epos);
          const bindings = elems[1];
          if (bindings.tag !== "list") throw errAt("letrec*: bad syntax", epos);
          const letrecEnv = new Env(env);
          return evalLetrecStarBindings2(0);
        }
        if (op === "case") {
          if (elems.length < 2) throw errAt("case: bad syntax", epos);
          if (!callccActive && !exprMayCallCC(elems[1])) {
            let key = evalCallDirect(elems[1], env);
            if (key === null) key = runTrampoline(evalCPS(elems[1], env, identityCont));
            if (!callccActive) {
              for (let ci = 2; ci < elems.length; ci++) {
                const clause = elems[ci];
                if (clause.tag !== "list" || clause.elements.length < 2) throw errAt("case: bad clause", epos);
                let matched = false;
                if (clause.elements[0].tag === "symbol" && clause.elements[0].value === "else") {
                  matched = true;
                } else {
                  if (clause.elements[0].tag !== "list") throw errAt("case: expected datum list", epos);
                  for (const datum of clause.elements[0].elements) {
                    if (schemeEqv(key, quoteSyntaxToValue(datum))) {
                      matched = true;
                      break;
                    }
                  }
                }
                if (matched) {
                  for (let bi = 1; bi < clause.elements.length - 1; bi++) {
                    runTrampoline(evalCPS(clause.elements[bi], env, identityCont));
                  }
                  expr = clause.elements[clause.elements.length - 1];
                  continue tailLoop;
                }
              }
              return k(VOID);
            }
          }
          return evalCPS(elems[1], env, (key) => {
            function evalCaseClauses(idx) {
              if (idx >= elems.length) return k(VOID);
              const clause = elems[idx];
              if (clause.tag !== "list" || clause.elements.length < 2) throw errAt("case: bad clause", epos);
              if (clause.elements[0].tag === "symbol" && clause.elements[0].value === "else") {
                return evalSeqCPS(clause.elements, 1, env, k);
              }
              if (clause.elements[0].tag !== "list") throw errAt("case: expected datum list", epos);
              const datums = clause.elements[0].elements;
              for (const datum of datums) {
                const dval = quoteSyntaxToValue(datum);
                if (schemeEqv(key, dval)) {
                  return evalSeqCPS(clause.elements, 1, env, k);
                }
              }
              return evalCaseClauses(idx + 1);
            }
            return evalCaseClauses(2);
          });
        }
        if (op === "do") {
          let evalDoInits2 = function(idx, vals) {
            if (idx >= specs.length) {
              const doEnv = new Env(env);
              for (let i = 0; i < specs.length; i++) {
                doEnv.define(specs[i].name, vals[i]);
              }
              return doLoop2(doEnv);
            }
            return evalCPS(varSpecs.elements[idx].elements[1], env, (val) => {
              vals.push(val);
              return evalDoInits2(idx + 1, vals);
            });
          }, doLoop2 = function(doEnv) {
            return evalCPS(testClause.elements[0], doEnv, (testResult) => {
              if (isTruthy(testResult)) {
                if (testClause.elements.length === 1) return k(VOID);
                return evalSeqCPS(testClause.elements, 1, doEnv, k);
              }
              function afterBody() {
                function evalSteps(i, newVals) {
                  if (i >= specs.length) {
                    for (let j = 0; j < specs.length; j++) {
                      if (newVals[j] !== void 0) doEnv.define(specs[j].name, newVals[j]);
                    }
                    return mkBounce(() => doLoop2(doEnv));
                  }
                  if (!specs[i].stepExpr) {
                    newVals.push(void 0);
                    return evalSteps(i + 1, newVals);
                  }
                  return evalCPS(specs[i].stepExpr, doEnv, (val) => {
                    newVals.push(val);
                    return evalSteps(i + 1, newVals);
                  });
                }
                return evalSteps(0, []);
              }
              if (bodyExprs.length === 0) return afterBody();
              return evalSeqCPS(bodyExprs, 0, doEnv, (_) => afterBody());
            });
          };
          var evalDoInits = evalDoInits2, doLoop = doLoop2;
          if (elems.length < 3) throw errAt("do: bad syntax", epos);
          const varSpecs = elems[1];
          if (varSpecs.tag !== "list") throw errAt("do: bad syntax", epos);
          const testClause = elems[2];
          if (testClause.tag !== "list" || testClause.elements.length < 1) throw errAt("do: bad test clause", epos);
          const bodyExprs = elems.slice(3);
          const specs = [];
          for (const spec of varSpecs.elements) {
            if (spec.tag !== "list" || spec.elements.length < 2 || spec.elements[0].tag !== "symbol")
              throw errAt("do: bad variable spec", epos);
            specs.push({ name: spec.elements[0].value, stepExpr: spec.elements.length >= 3 ? spec.elements[2] : void 0 });
          }
          return evalDoInits2(0, []);
        }
        if (op === "cond") {
          let evalCondClauses2 = function(idx) {
            if (idx >= elems.length) return k(VOID);
            const clause = elems[idx];
            if (clause.tag !== "list" || clause.elements.length < 1) throw errAt("cond: bad clause", epos);
            if (clause.elements[0].tag === "symbol" && clause.elements[0].value === "else") {
              return evalSeqCPS(clause.elements, 1, env, k);
            }
            return evalCPS(clause.elements[0], env, (test) => {
              if (isTruthy(test)) {
                if (clause.elements.length === 1) return k(test);
                if (clause.elements.length === 3 && clause.elements[1].tag === "symbol" && clause.elements[1].value === "=>") {
                  return evalCPS(clause.elements[2], env, (proc) => {
                    return applyCPS(proc, [test], epos, k);
                  });
                }
                return evalSeqCPS(clause.elements, 1, env, k);
              }
              return evalCondClauses2(idx + 1);
            });
          };
          var evalCondClauses = evalCondClauses2;
          if (!callccActive) {
            for (let ci = 1; ci < elems.length; ci++) {
              const clause = elems[ci];
              if (clause.tag !== "list" || clause.elements.length < 1) throw errAt("cond: bad clause", epos);
              if (clause.elements[0].tag === "symbol" && clause.elements[0].value === "else") {
                if (clause.elements.length === 2) {
                  expr = clause.elements[1];
                  continue tailLoop;
                }
                for (let bi = 1; bi < clause.elements.length - 1; bi++) {
                  runTrampoline(evalCPS(clause.elements[bi], env, identityCont));
                  if (callccActive) break;
                }
                if (!callccActive) {
                  expr = clause.elements[clause.elements.length - 1];
                  continue tailLoop;
                }
                break;
              }
              if (exprMayCallCC(clause.elements[0])) break;
              const test = evalCallDirect(clause.elements[0], env) ?? runTrampoline(evalCPS(clause.elements[0], env, identityCont));
              if (callccActive) break;
              if (isTruthy(test)) {
                if (clause.elements.length === 1) return k(test);
                if (clause.elements.length === 3 && clause.elements[1].tag === "symbol" && clause.elements[1].value === "=>") {
                  const proc = evalCallDirect(clause.elements[2], env) ?? runTrampoline(evalCPS(clause.elements[2], env, identityCont));
                  if (callccActive) break;
                  return applyCPS(proc, [test], epos, k);
                }
                if (clause.elements.length === 2) {
                  expr = clause.elements[1];
                  continue tailLoop;
                }
                for (let bi = 1; bi < clause.elements.length - 1; bi++) {
                  runTrampoline(evalCPS(clause.elements[bi], env, identityCont));
                  if (callccActive) break;
                }
                if (!callccActive) {
                  expr = clause.elements[clause.elements.length - 1];
                  continue tailLoop;
                }
                break;
              }
            }
          }
          return evalCondClauses2(1);
        }
        if (op === "and") {
          let evalAndExprs2 = function(idx) {
            if (idx === elems.length - 1) return evalCPS(elems[idx], env, k);
            return evalCPS(elems[idx], env, (result) => {
              if (!isTruthy(result)) return k(result);
              return evalAndExprs2(idx + 1);
            });
          };
          var evalAndExprs = evalAndExprs2;
          if (elems.length === 1) return k(TRUE_VAL);
          if (!callccActive) {
            let allTrue = true;
            for (let i = 1; i < elems.length - 1; i++) {
              if (exprMayCallCC(elems[i])) {
                allTrue = false;
                break;
              }
              const r = evalCallDirect(elems[i], env) ?? runTrampoline(evalCPS(elems[i], env, identityCont));
              if (callccActive) {
                allTrue = false;
                break;
              }
              if (!isTruthy(r)) return k(r);
            }
            if (allTrue) {
              expr = elems[elems.length - 1];
              continue tailLoop;
            }
          }
          return evalAndExprs2(1);
        }
        if (op === "or") {
          let evalOrExprs2 = function(idx) {
            if (idx === elems.length - 1) return evalCPS(elems[idx], env, k);
            return evalCPS(elems[idx], env, (result) => {
              if (isTruthy(result)) return k(result);
              return evalOrExprs2(idx + 1);
            });
          };
          var evalOrExprs = evalOrExprs2;
          if (elems.length === 1) return k(FALSE_VAL);
          if (!callccActive) {
            let allFalse = true;
            for (let i = 1; i < elems.length - 1; i++) {
              if (exprMayCallCC(elems[i])) {
                allFalse = false;
                break;
              }
              const r = evalCallDirect(elems[i], env) ?? runTrampoline(evalCPS(elems[i], env, identityCont));
              if (callccActive) {
                allFalse = false;
                break;
              }
              if (isTruthy(r)) return k(r);
            }
            if (allFalse) {
              expr = elems[elems.length - 1];
              continue tailLoop;
            }
          }
          return evalOrExprs2(1);
        }
        if (op === "not") {
          if (elems.length !== 2) throw errAt("not: expected 1 argument", epos);
          return evalCPS(elems[1], env, (val) => {
            return k({ tag: "boolean", value: !isTruthy(val) });
          });
        }
        if (op === "define-syntax") {
          if (elems.length !== 3) throw errAt("define-syntax: bad syntax", epos);
          if (elems[1].tag !== "symbol") throw errAt("define-syntax: expected symbol", epos);
          const name = elems[1].value;
          const transformer = elems[2];
          if (transformer.tag === "list" && transformer.elements.length >= 3 && transformer.elements[0].tag === "symbol" && transformer.elements[0].value === "lambda") {
            return evalCPS(transformer, env, (lambdaVal) => {
              env.define(name, { tag: "syntax-case-macro", transformer: lambdaVal, defEnv: env });
              return k(VOID);
            });
          }
          if (transformer.tag !== "list" || transformer.elements.length < 2 || transformer.elements[0].tag !== "symbol" || transformer.elements[0].value !== "syntax-rules") {
            throw errAt("define-syntax: expected syntax-rules or lambda", epos);
          }
          const srElems = transformer.elements;
          if (srElems[1].tag !== "list") throw errAt("syntax-rules: expected literals list", epos);
          const literals = srElems[1].elements.map((e) => {
            if (e.tag !== "symbol") throw errAt("syntax-rules: literal must be symbol", epos);
            return e.value;
          });
          const clauses = [];
          for (let ci = 2; ci < srElems.length; ci++) {
            const c = srElems[ci];
            if (c.tag !== "list" || c.elements.length !== 2)
              throw errAt("syntax-rules: bad clause", epos);
            if (c.elements[0].tag !== "list")
              throw errAt("syntax-rules: pattern must be list", epos);
            clauses.push({ pattern: c.elements[0].elements, template: c.elements[1] });
          }
          env.define(name, { tag: "macro", literals, clauses, defEnv: env });
          return k(VOID);
        }
        if (op === "define-record-type") {
          if (elems.length < 4) throw errAt("define-record-type: bad syntax", epos);
          if (elems[1].tag !== "symbol") throw errAt("define-record-type: expected type name", epos);
          const typeName = elems[1].value;
          const ctorSpec = elems[2];
          if (ctorSpec.tag !== "list" || ctorSpec.elements.length < 1 || ctorSpec.elements[0].tag !== "symbol")
            throw errAt("define-record-type: bad constructor spec", epos);
          const ctorName = ctorSpec.elements[0].value;
          const ctorFields = [];
          for (let i = 1; i < ctorSpec.elements.length; i++) {
            if (ctorSpec.elements[i].tag !== "symbol") throw errAt("define-record-type: field must be symbol", epos);
            ctorFields.push(ctorSpec.elements[i].value);
          }
          if (elems[3].tag !== "symbol") throw errAt("define-record-type: expected predicate name", epos);
          const predName = elems[3].value;
          const accessors = [];
          for (let i = 4; i < elems.length; i++) {
            const spec = elems[i];
            if (spec.tag !== "list" || spec.elements.length < 2 || spec.elements[0].tag !== "symbol" || spec.elements[1].tag !== "symbol")
              throw errAt("define-record-type: bad field spec", epos);
            accessors.push({ field: spec.elements[0].value, accessor: spec.elements[1].value });
          }
          env.define(ctorName, { tag: "builtin", name: ctorName, fn: (args) => {
            if (args.length !== ctorFields.length)
              throw new EvalError(`${ctorName}: expected ${ctorFields.length} arguments, got ${args.length}`);
            const fields = /* @__PURE__ */ new Map();
            for (let i = 0; i < ctorFields.length; i++) fields.set(ctorFields[i], args[i]);
            return { tag: "record", typeName, fields };
          } });
          env.define(predName, { tag: "builtin", name: predName, fn: (args) => {
            if (args.length !== 1) throw new EvalError(`${predName}: expected 1 argument`);
            return { tag: "boolean", value: args[0].tag === "record" && args[0].typeName === typeName };
          } });
          for (const { field, accessor } of accessors) {
            env.define(accessor, { tag: "builtin", name: accessor, fn: (args) => {
              if (args.length !== 1) throw new EvalError(`${accessor}: expected 1 argument`);
              if (args[0].tag !== "record" || args[0].typeName !== typeName)
                throw new EvalError(`${accessor}: expected ${typeName}`);
              return args[0].fields.get(field);
            } });
          }
          return k(VOID);
        }
        if (op === "guard") {
          if (elems.length < 3) throw errAt("guard: bad syntax", epos);
          const guardSpec = elems[1];
          if (guardSpec.tag !== "list" || guardSpec.elements.length < 1)
            throw errAt("guard: bad syntax", epos);
          if (guardSpec.elements[0].tag !== "symbol")
            throw errAt("guard: expected variable name", epos);
          const varName = guardSpec.elements[0].value;
          const clauses = guardSpec.elements.slice(1);
          const body = elems.slice(2);
          callccActive = true;
          const guardWinds = [...windStack];
          const guardK = k;
          exceptionHandlers.push({
            handler: (val) => {
              const guardEnv = new Env(env);
              guardEnv.define(varName, val);
              function testClauses(cidx) {
                if (cidx >= clauses.length) {
                  let unwindReRaise2 = function(idx) {
                    if (idx <= cLen) return next.handler(val);
                    const w = curWinds[idx - 1];
                    windStack.pop();
                    return applyCPS(w.outThunk, [], epos, (_) => unwindReRaise2(idx - 1));
                  };
                  var unwindReRaise = unwindReRaise2;
                  if (exceptionHandlers.length === 0) {
                    throw new EvalError(`unhandled exception: ${displayVal(val)}`);
                  }
                  const next = exceptionHandlers.pop();
                  const curWinds = [...windStack];
                  const tgtWinds = next.winds;
                  let cLen = 0;
                  while (cLen < curWinds.length && cLen < tgtWinds.length && curWinds[cLen] === tgtWinds[cLen]) cLen++;
                  return unwindReRaise2(curWinds.length);
                }
                const clause = clauses[cidx];
                if (clause.tag !== "list" || clause.elements.length < 1)
                  throw errAt("guard: bad clause", epos);
                if (clause.elements[0].tag === "symbol" && clause.elements[0].value === "else") {
                  if (clause.elements.length < 2) throw errAt("guard: bad else clause", epos);
                  return evalSeqCPS(clause.elements, 1, guardEnv, guardK);
                }
                return evalCPS(clause.elements[0], guardEnv, (testResult) => {
                  if (isTruthy(testResult)) {
                    if (clause.elements.length > 1) {
                      return evalSeqCPS(clause.elements, 1, guardEnv, guardK);
                    }
                    return guardK(testResult);
                  }
                  return testClauses(cidx + 1);
                });
              }
              return testClauses(0);
            },
            winds: guardWinds
          });
          return evalSeqCPS(body, 0, env, (result) => {
            exceptionHandlers.pop();
            return mkBounce(() => k(result));
          });
        }
        if (op === "call/cc" || op === "call-with-current-continuation") {
          if (elems.length !== 2) throw errAt("call/cc: expected 1 argument", epos);
          callccActive = true;
          return evalCPS(elems[1], env, (proc) => {
            const contVal = { tag: "continuation", k, winds: [...windStack] };
            return applyCPS(proc, [contVal], epos, k);
          });
        }
        if (op === "syntax-case") {
          if (elems.length < 4) throw errAt("syntax-case: bad syntax", epos);
          return evalCPS(elems[1], env, (stxVal) => {
            if (elems[2].tag !== "list") throw errAt("syntax-case: expected literals list", epos);
            const literals = elems[2].elements.map((e) => {
              if (e.tag !== "symbol") throw errAt("syntax-case: literal must be symbol", epos);
              return e.value;
            });
            const litSet = new Set(literals);
            const inputElems = stxVal.tag === "list" ? stxVal.elements : [stxVal];
            for (let ci = 3; ci < elems.length; ci++) {
              const clause = elems[ci];
              if (clause.tag !== "list" || clause.elements.length < 2)
                throw errAt("syntax-case: bad clause", epos);
              const pattern = clause.elements[0];
              const patElems = pattern.tag === "list" ? pattern.elements : [pattern];
              const bindings = /* @__PURE__ */ new Map();
              if (matchPattern(patElems, inputElems, litSet, bindings)) {
                const merged = /* @__PURE__ */ new Map();
                if (syntaxBindingsStack.length > 0) {
                  const outer = syntaxBindingsStack[syntaxBindingsStack.length - 1];
                  for (const [k2, v] of outer) merged.set(k2, v);
                }
                for (const [k2, v] of bindings) merged.set(k2, v);
                syntaxBindingsStack.push(merged);
                const body = clause.elements.length === 3 ? clause.elements[2] : clause.elements[1];
                return evalCPS(body, env, (result) => {
                  syntaxBindingsStack.pop();
                  return k(result);
                });
              }
            }
            throw errAt("syntax-case: no matching pattern", epos);
          });
        }
        if (op === "syntax") {
          if (elems.length !== 2) throw errAt("syntax: expected 1 argument", epos);
          const template = elems[1];
          if (syntaxBindingsStack.length === 0) throw errAt("syntax: not in syntax-case context", epos);
          const bindings = syntaxBindingsStack[syntaxBindingsStack.length - 1];
          const patVars = new Set(bindings.keys());
          if (template.tag === "symbol" && bindings.has(template.value)) {
            const b = bindings.get(template.value);
            if (!b.ellipsis) return k(b.value);
            throw errAt("syntax: ellipsis variable used outside ellipsis context", epos);
          }
          const introduced = /* @__PURE__ */ new Set();
          collectIntroduced(template, patVars, introduced);
          const renames = /* @__PURE__ */ new Map();
          for (const sym of introduced) renames.set(sym, gensym(sym));
          const expanded = expandTemplate(template, bindings, renames);
          for (const [original, renamed] of renames) {
            pendingSyntaxRenames.push({ original, renamed, defEnv: env });
          }
          return k(expanded);
        }
        if (op === "with-syntax") {
          let evalWithSyntaxBindings2 = function(idx) {
            if (idx >= bindSpecs.elements.length) {
              return evalSeqCPS(body, 0, env, k);
            }
            const spec = bindSpecs.elements[idx];
            if (spec.tag !== "list" || spec.elements.length !== 2 || spec.elements[0].tag !== "symbol")
              throw errAt("with-syntax: bad binding", epos);
            const varName = spec.elements[0].value;
            return evalCPS(spec.elements[1], env, (val) => {
              if (syntaxBindingsStack.length === 0) syntaxBindingsStack.push(/* @__PURE__ */ new Map());
              const current = syntaxBindingsStack[syntaxBindingsStack.length - 1];
              current.set(varName, { ellipsis: false, value: val });
              return evalWithSyntaxBindings2(idx + 1);
            });
          };
          var evalWithSyntaxBindings = evalWithSyntaxBindings2;
          if (elems.length < 3) throw errAt("with-syntax: bad syntax", epos);
          const bindSpecs = elems[1];
          if (bindSpecs.tag !== "list") throw errAt("with-syntax: expected binding list", epos);
          const body = elems.slice(2);
          return evalWithSyntaxBindings2(0);
        }
        try {
          const macroVal = env.get(op);
          if (macroVal.tag === "macro") {
            let cache = expr._mc;
            if (!cache) {
              cache = expandMacro(macroVal, elems);
              expr._mc = cache;
            }
            const { expanded, renames } = cache;
            for (const [original, renamed] of renames) {
              try {
                env.define(renamed, macroVal.defEnv.get(original));
              } catch {
              }
            }
            expr = expanded;
            continue tailLoop;
          }
          if (macroVal.tag === "syntax-case-macro") {
            const formVal = { tag: "list", elements: elems };
            pendingSyntaxRenames = [];
            return applyCPS(macroVal.transformer, [formVal], epos, (expanded) => {
              for (const { original, renamed, defEnv: dEnv } of pendingSyntaxRenames) {
                try {
                  env.define(renamed, dEnv.get(original));
                } catch {
                }
              }
              pendingSyntaxRenames = [];
              return mkBounce(() => evalCPS(expanded, env, k));
            });
          }
        } catch {
        }
      }
    }
    if (!callccActive && !exprMayCallCC(expr)) {
      const func = evalFast(elems[0], env) ?? runTrampoline(evalCPS(elems[0], env, identityCont));
      if (!callccActive) {
        const args = [];
        let argsOk = true;
        for (let i = 1; i < elems.length; i++) {
          const fast = evalFast(elems[i], env);
          if (fast !== null) {
            args.push(fast);
          } else {
            args.push(runTrampoline(evalCPS(elems[i], env, identityCont)));
            if (callccActive) {
              argsOk = false;
              break;
            }
          }
        }
        if (argsOk) {
          if (func.tag === "lambda") {
            contReentry = false;
            if (func.rest) {
              if (args.length < func.params.length)
                throw errAt(`lambda: expected at least ${func.params.length} arguments, got ${args.length}`, epos);
            } else {
              if (args.length !== func.params.length)
                throw errAt(`lambda: expected ${func.params.length} arguments, got ${args.length}`, epos);
            }
            const bodyEnv = func.params.length === 0 && !func.rest ? func.env : (() => {
              const e = new Env(func.env);
              for (let i = 0; i < func.params.length; i++) e.define(func.params[i], args[i]);
              if (func.rest) e.define(func.rest, makeList(args.slice(func.params.length)));
              return e;
            })();
            let bodyOk = true;
            for (let i = 0; i < func.body.length - 1; i++) {
              if (callccActive || exprMayCallCC(func.body[i])) {
                bodyOk = false;
                break;
              }
              runTrampoline(evalCPS(func.body[i], bodyEnv, identityCont));
              if (callccActive) {
                bodyOk = false;
                break;
              }
            }
            if (bodyOk && !callccActive) {
              expr = func.body[func.body.length - 1];
              env = bodyEnv;
              continue tailLoop;
            }
            return mkBounce(() => evalSeqCPS(func.body, 0, bodyEnv, k));
          }
          if (func.tag === "case-lambda") {
            for (const clause of func.clauses) {
              if (clause.rest ? args.length >= clause.params.length : args.length === clause.params.length) {
                const callEnv = new Env(func.env);
                for (let i = 0; i < clause.params.length; i++) callEnv.define(clause.params[i], args[i]);
                if (clause.rest) callEnv.define(clause.rest, makeList(args.slice(clause.params.length)));
                let bodyOk = true;
                for (let i = 0; i < clause.body.length - 1; i++) {
                  if (callccActive || exprMayCallCC(clause.body[i])) {
                    bodyOk = false;
                    break;
                  }
                  runTrampoline(evalCPS(clause.body[i], callEnv, identityCont));
                  if (callccActive) {
                    bodyOk = false;
                    break;
                  }
                }
                if (bodyOk && !callccActive) {
                  expr = clause.body[clause.body.length - 1];
                  env = callEnv;
                  continue tailLoop;
                }
                return mkBounce(() => evalSeqCPS(clause.body, 0, callEnv, k));
              }
            }
          }
          return applyCPS(func, args, epos, k);
        }
        return evalArgsCPS(elems, 1, env, [], (allArgs) => {
          return applyCPS(func, allArgs, epos, k);
        });
      }
    }
    return evalCPS(elems[0], env, (func) => {
      return evalArgsCPS(elems, 1, env, [], (args) => {
        return applyCPS(func, args, epos, k);
      });
    });
  }
}
function displayVal(val, seen) {
  switch (val.tag) {
    case "number": {
      const s = String(val.value);
      if (Number.isInteger(val.value) && !s.includes(".")) return s + ".0";
      return s;
    }
    case "rational":
      return val.den === 1 ? String(val.num) : `${val.num}/${val.den}`;
    case "boolean":
      return val.value ? "#t" : "#f";
    case "string":
      return `"${val.value}"`;
    case "char":
      return `#\\${val.value}`;
    case "symbol":
      return val.value;
    case "list":
      return `(${val.elements.map((e) => displayVal(e, seen)).join(" ")})`;
    case "nil":
      return "()";
    case "pair": {
      if (!seen) seen = /* @__PURE__ */ new Set();
      if (seen.has(val)) return "(...)";
      seen.add(val);
      let parts = [];
      let cur = val;
      while (cur.tag === "pair") {
        if (cur !== val && seen.has(cur)) {
          parts.push("...");
          cur = NIL;
          break;
        }
        seen.add(cur);
        parts.push(displayVal(cur.car, seen));
        cur = cur.cdr;
      }
      if (cur.tag === "nil") {
        return `(${parts.join(" ")})`;
      }
      return `(${parts.join(" ")} . ${displayVal(cur, seen)})`;
    }
    case "void":
      return "";
    case "lambda":
      return "#<procedure>";
    case "case-lambda":
      return "#<procedure>";
    case "builtin":
      return `#<builtin:${val.name}>`;
    case "macro":
      return "#<macro>";
    case "record":
      return `#<record:${val.typeName}>`;
    case "vector":
      return `#(${val.elements.map((e) => displayVal(e, seen)).join(" ")})`;
    case "continuation":
      return "#<continuation>";
    case "values":
      return val.vals.map((v) => displayVal(v, seen)).join("\n");
    case "syntax-case-macro":
      return "#<macro>";
  }
}
function evalStr(input) {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) throw new EvalError("no expressions");
  outputBuffer = "";
  contReentry = false;
  callccActive = false;
  fastPathCont = null;
  windStack = [];
  exceptionHandlers = [];
  syntaxBindingsStack = [];
  pendingSyntaxRenames = [];
  const env = makeGlobalEnv();
  const result = runTrampoline(evalSeqCPS(exprs, 0, env, (v) => v));
  return displayVal(result);
}
function evalStrWithOutput(input) {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) throw new EvalError("no expressions");
  outputBuffer = "";
  contReentry = false;
  callccActive = false;
  fastPathCont = null;
  windStack = [];
  exceptionHandlers = [];
  syntaxBindingsStack = [];
  pendingSyntaxRenames = [];
  const env = makeGlobalEnv();
  const result = runTrampoline(evalSeqCPS(exprs, 0, env, (v) => v));
  return { result: displayValUnquoted(result), output: outputBuffer };
}
function evalStrWithLimit(input, maxSteps) {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) throw new EvalError("no expressions");
  outputBuffer = "";
  contReentry = false;
  callccActive = false;
  fastPathCont = null;
  windStack = [];
  exceptionHandlers = [];
  syntaxBindingsStack = [];
  pendingSyntaxRenames = [];
  stepLimitActive = true;
  stepCount = 0;
  stepMax = maxSteps;
  try {
    const env = makeGlobalEnv();
    const result = runTrampoline(evalSeqCPS(exprs, 0, env, (v) => v));
    return displayVal(result);
  } finally {
    stepLimitActive = false;
  }
}
export {
  evalStr,
  evalStrWithLimit,
  evalStrWithOutput
};
