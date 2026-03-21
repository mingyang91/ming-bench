import { describe, test, expect } from 'vitest';
import fs from 'node:fs';
import path from 'node:path';
import { evalStr, evalStrWithOutput } from '../src/evaluator.js';

interface TestCase {
  name: string;
  level: number;
  fixture: string;
  kind: 'eval_str_ok' | 'eval_str_err' | 'eval_str_err_with_position' | 'eval_str_with_output';
  expected?: string;
  expected_output?: string;
  deprecated_after?: number;
}

const manifestPath = path.resolve(__dirname, '../../tests.json');
const fixturesDir = path.resolve(__dirname, '../../fixtures');
const manifest: TestCase[] = JSON.parse(fs.readFileSync(manifestPath, 'utf-8'));

const benchLevel = process.env.BENCH_LEVEL ? parseInt(process.env.BENCH_LEVEL, 10) : undefined;

describe('scheme', () => {
  for (const tc of manifest) {
    // Skip tests above the requested level
    if (benchLevel !== undefined && tc.level > benchLevel) {
      continue;
    }

    // Skip deprecated tests when current level exceeds deprecated_after
    if (
      tc.deprecated_after !== undefined &&
      benchLevel !== undefined &&
      benchLevel > tc.deprecated_after
    ) {
      continue;
    }

    const fixturePath = path.join(fixturesDir, tc.fixture);

    switch (tc.kind) {
      case 'eval_str_ok':
        test(tc.name, () => {
          const input = fs.readFileSync(fixturePath, 'utf-8');
          const result = evalStr(input);
          expect(result).toBe(tc.expected);
        });
        break;

      case 'eval_str_err':
        test(tc.name, () => {
          const input = fs.readFileSync(fixturePath, 'utf-8');
          expect(() => evalStr(input)).toThrow();
        });
        break;

      case 'eval_str_err_with_position':
        test(tc.name, () => {
          const input = fs.readFileSync(fixturePath, 'utf-8');
          expect(() => evalStr(input)).toThrowError(/\d:/);
        });
        break;

      case 'eval_str_with_output':
        test(tc.name, () => {
          const input = fs.readFileSync(fixturePath, 'utf-8');
          const { output } = evalStrWithOutput(input);
          expect(output).toBe(tc.expected_output);
        });
        break;
    }
  }
});
