# Session JSONL Analysis & Token Billing Guide

## Task

Write a Python script that reads Claude Code session JSONL files, aggregates token usage, and calculates billing costs.

## Session file locations

Claude Code stores sessions at:

```
~/.claude/projects/<project-slug>/<session-uuid>.jsonl
```

For MING benchmark runs, sessions are also copied to:

```
results/<run-name>/L<NN>/session.jsonl
results/<run-name>/L<NN>/session-regression.jsonl
results/<run-name>/L<NN>/session-cleanup.jsonl
```

## JSONL format

Each line is a JSON object with a `type` field:

- `"queue-operation"` — internal, skip
- `"last-prompt"` — internal, skip
- `"user"` — user message (no token usage)
- `"assistant"` — model response with token usage

### Assistant message structure

```json
{
  "type": "assistant",
  "message": {
    "model": "claude-opus-4-6",
    "id": "msg_...",
    "role": "assistant",
    "content": [
      { "type": "thinking", "thinking": "..." },
      { "type": "text", "text": "..." },
      { "type": "tool_use", "id": "toolu_...", "name": "Bash", "input": { "command": "..." } }
    ],
    "stop_reason": "tool_use",
    "usage": {
      "input_tokens": 48,
      "output_tokens": 8190,
      "cache_creation_input_tokens": 108805,
      "cache_read_input_tokens": 1053851,
      "cache_creation": {
        "ephemeral_5m_input_tokens": 108805,
        "ephemeral_1h_input_tokens": 0
      },
      "service_tier": "standard",
      "inference_geo": "global"
    }
  },
  "timestamp": "2026-03-23T00:35:26.123Z",
  "sessionId": "uuid-here"
}
```

### Token fields

| Field | Meaning |
|---|---|
| `input_tokens` | Non-cached input tokens (prompt text not in any cache) |
| `output_tokens` | All output tokens including extended thinking |
| `cache_creation_input_tokens` | Tokens written to prompt cache this turn |
| `cache_read_input_tokens` | Tokens served from prompt cache (cheap) |

Cache is ephemeral — `ephemeral_5m_input_tokens` is the 5-minute sliding window cache that Claude Code uses. The `ephemeral_1h_input_tokens` field exists but is typically 0 for CLI sessions.

## Pricing (per million tokens, as of 2025-05)

### Claude Opus 4.6 (`claude-opus-4-6`)

| Token type | Price / 1M tokens |
|---|---|
| Input (non-cached) | $15.00 |
| Input (cache write) | $18.75 (1.25× input) |
| Input (cache read) | $1.50 (0.1× input) |
| Output | $75.00 |

### Claude Sonnet 4.6 (`claude-sonnet-4-6`)

| Token type | Price / 1M tokens |
|---|---|
| Input (non-cached) | $3.00 |
| Input (cache write) | $3.75 (1.25× input) |
| Input (cache read) | $0.30 (0.1× input) |
| Output | $15.00 |

### Claude Haiku 4.5 (`claude-haiku-4-5-20251001`)

| Token type | Price / 1M tokens |
|---|---|
| Input (non-cached) | $0.80 |
| Input (cache write) | $1.00 (1.25× input) |
| Input (cache read) | $0.08 (0.1× input) |
| Output | $4.00 |

## Billing formula

```python
cost = (
    input_tokens * input_price
    + cache_creation_input_tokens * cache_write_price
    + cache_read_input_tokens * cache_read_price
    + output_tokens * output_price
) / 1_000_000
```

## Requirements

1. Accept a path (file or directory) as argument
2. If directory, recursively find all `*.jsonl` files
3. For each file, parse assistant events and aggregate:
   - `input_tokens`, `output_tokens`, `cache_creation_input_tokens`, `cache_read_input_tokens`
   - Count of assistant turns
   - Model name (for price selection)
4. Print per-file summary and grand total
5. Calculate cost using the correct pricing for each model
6. Handle mixed-model sessions (some turns Opus, some Sonnet)

## Example output

```
=== results/quality-gate_cl-qg-r17/L12/session.jsonl ===
  Model: claude-opus-4-6
  Turns: 17
  Input:          48 tokens    $0.00
  Cache write: 108,805 tokens  $2.04
  Cache read: 1,053,851 tokens $1.58
  Output:       8,190 tokens   $0.61
  Subtotal: $4.23

=== GRAND TOTAL ===
  Files: 23
  Turns: 487
  Total cost: $113.56
```

## Notes

- The `content` array in assistant messages contains the actual response: `thinking` blocks (extended thinking / chain-of-thought), `text` blocks (visible output), and `tool_use` blocks (tool calls). These don't affect billing directly — billing is purely from the `usage` object.
- `stop_reason: "tool_use"` means the model stopped to call a tool. `stop_reason: "end_turn"` means the model finished its response. Both have valid `usage` data.
- Some JSONL files may have `user` events with `tool_result` content — these are tool responses fed back to the model. They contribute to `input_tokens` on the next assistant turn.
- Sessions can span multiple models if the user switches mid-conversation. Always check `message.model` per turn for correct pricing.
