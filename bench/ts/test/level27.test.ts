import { describe, test, expect } from 'vitest';

const benchLevel = process.env.BENCH_LEVEL ? parseInt(process.env.BENCH_LEVEL, 10) : undefined;
const shouldSkip = benchLevel !== undefined && benchLevel < 27;

describe.skipIf(shouldSkip)('Level 27: Step-Limited Evaluation', () => {
  // Import evalStr and evalStrWithLimit
  const { evalStr, evalStrWithLimit } = require('../src/evaluator.js');

  test('l27_step_limit_normal', () => {
    expect(evalStrWithLimit('(+ 1 2)', 1000)).toBe('3');
  });

  test('l27_step_limit_loop_within_budget', () => {
    expect(evalStrWithLimit(
      "(let loop ((n 50)) (if (= n 0) 'done (loop (- n 1))))", 10000
    )).toBe('done');
  });

  test('l27_step_limit_infinite_loop', () => {
    expect(() => evalStrWithLimit('(let loop () (loop))', 1000)).toThrow();
  });

  test('l27_step_limit_exceeded', () => {
    expect(() => evalStrWithLimit(
      "(let loop ((n 1000)) (if (= n 0) 'done (loop (- n 1))))", 50
    )).toThrow();
  });

  test('l27_step_limit_factorial', () => {
    expect(evalStrWithLimit(
      '(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 10)', 10000
    )).toBe('3628800');
  });

  test('l27_normal_eval_unaffected', () => {
    expect(evalStr(
      "(let loop ((n 100000)) (if (= n 0) 'done (loop (- n 1))))"
    )).toBe('done');
  });
});
