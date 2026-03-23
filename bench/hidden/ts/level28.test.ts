import { describe, test, expect } from 'vitest';
import { Worker, isMainThread } from 'node:worker_threads';
import path from 'node:path';
import fs from 'node:fs';

const benchLevel = process.env.BENCH_LEVEL ? parseInt(process.env.BENCH_LEVEL, 10) : undefined;
const shouldSkip = benchLevel !== undefined && benchLevel < 28;

// Helper: run evalStr in a Worker thread and get the result
function evalInWorker(code: string): Promise<{ result?: string; output?: string; error?: string }> {
  return new Promise((resolve, reject) => {
    // Worker inline script that imports evalStr and runs the code
    const workerCode = `
      const { parentPort, workerData } = require('node:worker_threads');
      async function run() {
        try {
          const mod = require(workerData.evaluatorPath);
          const evalStr = mod.evalStr || mod.default?.evalStr;
          const evalStrWithOutput = mod.evalStrWithOutput || mod.default?.evalStrWithOutput;
          if (workerData.withOutput && evalStrWithOutput) {
            const { result, output } = evalStrWithOutput(workerData.code);
            parentPort.postMessage({ result, output });
          } else if (evalStr) {
            const result = evalStr(workerData.code);
            parentPort.postMessage({ result });
          } else {
            parentPort.postMessage({ error: 'evalStr not found' });
          }
        } catch (e) {
          parentPort.postMessage({ error: String(e) });
        }
      }
      run();
    `;

    // Find the compiled evaluator path
    const evaluatorPath = path.resolve(__dirname, '../src/evaluator.js');
    const distPath = path.resolve(__dirname, '../dist/evaluator.js');
    const actualPath = fs.existsSync(distPath) ? distPath : evaluatorPath;

    const worker = new Worker(workerCode, {
      eval: true,
      workerData: { code, evaluatorPath: actualPath, withOutput: code.includes('display') },
    });

    worker.on('message', resolve);
    worker.on('error', reject);
    worker.on('exit', (exitCode) => {
      if (exitCode !== 0) reject(new Error(`Worker exited with code ${exitCode}`));
    });
  });
}

describe.skipIf(shouldSkip)('Level 28: Concurrent Evaluation', () => {
  test('l28_concurrent_independent_eval', async () => {
    const promises = Array.from({ length: 8 }, (_, i) =>
      evalInWorker(
        `(let loop ((n 1000) (acc 0)) (if (= n 0) acc (loop (- n 1) (+ acc ${i}))))`
      )
    );

    const results = await Promise.all(promises);
    results.forEach((r, i) => {
      expect(r.error).toBeUndefined();
      expect(r.result).toBe(String(i * 1000));
    });
  });

  test('l28_concurrent_output_isolation', async () => {
    const promises = Array.from({ length: 4 }, (_, i) =>
      evalInWorker(
        `(begin (display "thread${i}") (display " ") (display "done${i}") "ok")`
      )
    );

    const results = await Promise.all(promises);
    results.forEach((r, i) => {
      expect(r.error).toBeUndefined();
      expect(r.result).toBe('ok');
      expect(r.output).toBe(`thread${i} done${i}`);
    });
  });

  test('l28_concurrent_closures_and_mutation', async () => {
    const code = `(let ((count 0))
      (define (inc!) (set! count (+ count 1)) count)
      (inc!) (inc!) (inc!)
      count)`;

    const promises = Array.from({ length: 4 }, () => evalInWorker(code));
    const results = await Promise.all(promises);
    results.forEach((r) => {
      expect(r.error).toBeUndefined();
      expect(r.result).toBe('3');
    });
  });

  test('l28_concurrent_stress', async () => {
    const promises = Array.from({ length: 16 }, (_, i) =>
      evalInWorker(
        `(let ((x ${i})) (define (f n) (if (= n 0) x (f (- n 1)))) (f 100))`
      )
    );

    const results = await Promise.all(promises);
    results.forEach((r, i) => {
      expect(r.error).toBeUndefined();
      expect(r.result).toBe(String(i));
    });
  });

  // ===== State isolation tests (sequential — no workers needed) =====

  test('l28_sequential_state_leak', () => {
    // Import evalStr directly for sequential tests (same thread)
    const { evalStr: localEvalStr } = require('../src/evaluator.js');
    const r1 = localEvalStr('(begin (define x 42) x)');
    expect(r1).toBe('42');

    // x must not leak to next evalStr call
    expect(() => localEvalStr('x')).toThrow();
  });

  test('l28_sequential_output_leak', () => {
    const { evalStrWithOutput: localEvalStrWithOutput } = require('../src/evaluator.js');
    const r1 = localEvalStrWithOutput('(display "aaa")');
    const r2 = localEvalStrWithOutput('(display "bbb")');
    expect(r1.output).toBe('aaa');
    expect(r2.output).toBe('bbb');
  });

  // ===== Concurrent continuation/macro collision tests =====

  test('l28_concurrent_callcc_collision', async () => {
    const promises = Array.from({ length: 8 }, () =>
      evalInWorker(
        `(let ((count 0))
           (set! count (+ count (call/cc (lambda (k) (k 10)))))
           count)`
      )
    );

    const results = await Promise.all(promises);
    results.forEach((r) => {
      expect(r.error).toBeUndefined();
      expect(r.result).toBe('10');
    });
  });

  test('l28_concurrent_macro_hygiene', async () => {
    const promises = Array.from({ length: 4 }, () =>
      evalInWorker(
        `(begin
           (define-syntax my-swap!
             (syntax-rules ()
               ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))
           (let ((x 1) (y 2))
             (my-swap! x y)
             (list x y)))`
      )
    );

    const results = await Promise.all(promises);
    results.forEach((r) => {
      expect(r.error).toBeUndefined();
      expect(r.result).toBe('(2 1)');
    });
  });
});
