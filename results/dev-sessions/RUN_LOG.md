# MING Benchmark — Run Log

Historical record of all benchmark rounds. Latest first.
Data sources: `cargo xtask results`, `cargo xtask tokens`, session narratives.

---

## R26 — 2026-03-24

**Framework:** Post-difficulty-wall, two-pass QG, 1500-line limit, 26+2 levels
**Runs:** 6 Claude agents (5 languages, default + QG)

| Run | Lang | Strategy | Levels | Cost | Duration | Notes |
|-----|------|----------|--------|------|----------|-------|
| cl-def-r26 | Rust | default | 25/26 | $317 | 6h10m | Died at L14 — sboyer stack overflow |
| cl-go-def-r26 | Go | default | 28/28 | $272 | 3h49m | Full completion |
| cl-java-def-r26 | Java | default | **28/28** | **$162** | 2h41m | **Cheapest full completer** |
| cl-ts-def-r26 | TS | default | 28/28 | $377 | 4h53m | Full but expensive |
| cl-qg-r26 | Rust | quality-gate | 13/14 | $139 | 1h26m | Died at L14 — sboyer |
| cl-scala-qg-r26 | Scala | quality-gate | 28/28 | $201 | 4h38m | Full completion |

**Highlight:** Java won at $162/28 levels — cheapest full completer across all rounds. Both Rust runs (default + QG) died at L14 sboyer stack overflow in debug mode.
**Led to:** PreToolUse hooks blocking direct `cargo test`, `[profile.test] opt-level = 2` fix.

---

## R25 — 2026-03-24

**Framework:** Post-difficulty-wall, two-pass QG, 1500-line limit
**Runs:** 6 Claude agents

| Run | Lang | Strategy | Levels | Cost | Duration | Notes |
|-----|------|----------|--------|------|----------|-------|
| cl-def-r25 | Rust | default | 28/28 | $266 | 5h04m | Full completion |
| cl-go-def-r25 | Go | default | 28/28 | $262 | 3h34m | Full completion |
| cl-java-def-r25 | Java | default | 28/28 | $261 | 4h29m | Full completion |
| cl-ts-def-r25 | TS | default | 0/0 | n/a | — | Session data missing |
| cl-qg-r25 | Rust | quality-gate | 28/28 | $242 | 4h54m | Full completion |
| cl-scala-qg-r25 | Scala | quality-gate | **28/28** | **$169** | 4h20m | **Benchmark cost record** |

**Highlight:** Scala-QG set the cost record at $169/28 levels (~$6/level). Accidental over-compliance: agent targeted 300-line files instead of 1500, creating 19 files — smaller files = less context re-read = cheaper.
**Key insight:** 5/6 runs completed all 28 levels. Most complete round to date.

---

## R24 — 2026-03-24

**Framework:** Two-pass QG strategy swap + 1500-line file limit (first round with both)
**Runs:** 6 Claude agents

| Run | Lang | Strategy | Levels | Cost | Duration | Notes |
|-----|------|----------|--------|------|----------|-------|
| cl-def-r24 | Rust | default | 13/14 | $141 | 1h19m | Died at L14 — sboyer |
| cl-go-def-r24 | Go | default | 28/28 | $231 | 3h29m | Full completion |
| cl-java-def-r24 | Java | default | 28/28 | $261 | 4h56m | Full completion |
| cl-ts-def-r24 | TS | default | 28/28 | $245 | 4h01m | Full completion |
| cl-qg-r24 | Rust | quality-gate | **28/28** | **$228** | 4h47m | **First QG 28/28 ever** |
| cl-scala-qg-r24 | Scala | quality-gate | 28/28 | $220 | 5h29m | Full completion |

**Highlight:** First QG 28/28 completion (Rust-QG). Strategy swap works — QG coding pass matched default speed (20.4 vs 21.7 turns/level). Rust default died at L14 again (sboyer).
**Key insight:** 5/6 completed all 28 levels. Rust default is the weak link due to sboyer.

---

## R23 — 2026-03-24

**Framework:** Two-pass QG strategy swap (first round with swap), pre-1500 limit
**Runs:** 6 Claude agents

| Run | Lang | Strategy | Levels | Cost | Duration | Notes |
|-----|------|----------|--------|------|----------|-------|
| cl-def-r23 | Rust | default | 25/26 | $220 | — | Stuck at L26 |
| cl-go-def-r23 | Go | default | 28/28 | $211 | 2h54m | Full completion |
| cl-java-def-r23 | Java | default | 23/24 | $241 | — | Stuck |
| cl-ts-def-r23 | TS | default | 27/28 | $200 | — | Almost |
| cl-qg-r23 | Rust | quality-gate | 18/19 | $164 | — | Died at wall |
| cl-scala-qg-r23 | Scala | quality-gate | 19/20 | $162 | — | Died at wall |

**Highlight:** QG coding pass matched default speed — 44% turn reduction vs R22. Go completed all 28.
**Key insight:** Strategy swap successful but file size limits still constraining QG cleanup pass.

---

## R22 — 2026-03-24

**Framework:** First round with L16-L18 difficulty wall (TCO/pair-mutation/call-cc reordered)
**Runs:** 6 Claude agents

| Run | Lang | Strategy | Levels | Cost | Duration | Notes |
|-----|------|----------|--------|------|----------|-------|
| cl-def-r22 | Rust | default | 23/24 | $171 | — | Stuck at wall |
| cl-go-def-r22 | Go | default | 23/24 | $147 | — | Stuck at wall |
| cl-java-def-r22 | Java | default | 23/24 | $129 | — | Stuck at wall |
| cl-ts-def-r22 | TS | default | **28/28** | $162 | 3h13m | **Only completer** |
| cl-qg-r22 | Rust | quality-gate | 21/22 | $232 | — | Stuck at wall |
| cl-scala-qg-r22 | Scala | quality-gate | 21/22 | $174 | — | Stuck at wall |

**Highlight:** Only 1/6 completed 26/26 (TS). Difficulty wall works as differentiator — dropped completion from 83% (R21) to 17%.
**Key insight:** The wall at L16-L18 is working exactly as designed: agents with clean code survive, tech debt kills.

---

## R21 — 2026-03-23

**Framework:** Pre-difficulty-wall, 26+2 levels (L27-L28 surprise levels added), 5 languages + Codex baseline
**Runs:** 10 agents (6 Claude + 4 Codex)

| Run | Lang | Strategy | Agent | Levels | Cost | Duration |
|-----|------|----------|-------|--------|------|----------|
| cl-def-r21 | Rust | default | Claude | 28/28 | $199 | 3h35m |
| cl-go-def-r21 | Go | default | Claude | 28/28 | $176 | 3h13m |
| cl-java-def-r21 | Java | default | Claude | 18/19 | $100 | 3h37m |
| cl-ts-def-r21 | TS | default | Claude | 28/28 | $169 | 3h13m |
| cl-qg-r21 | Rust | quality-gate | Claude | 28/28 | $311 | 5h14m |
| cl-scala-qg-r21 | Scala | quality-gate | Claude | 28/28 | $278 | 5h32m |
| cx-def-r21 | Rust | default | Codex | 22/23 | n/a | — |
| cx-go-def-r21 | Go | default | Codex | 26/27 | n/a | — |
| cx-java-def-r21 | Java | default | Codex | 5/6 | n/a | — |
| cx-ts-def-r21 | TS | default | Codex | 1/2 | n/a | — |
| cx-qg-r21 | Rust | quality-gate | Codex | 15/16 | n/a | — |
| cx-scala-qg-r21 | Scala | quality-gate | Codex | 3/4 | n/a | — |

**Highlight:** 5/6 Claude runs completed 28/28 (83%). Codex struggled — best was Go at 26/27. QG runs cost ~50% more ($278-311 vs $169-199) for same completion.
**Key insight:** Last round before difficulty wall reorder. QG premium is ~50% cost for identical completion rate.

---

## R20 — 2026-03-23

**Framework:** Pre-difficulty-wall, pre-surprise-levels, 5 languages
**Runs:** 6 Claude agents

| Run | Lang | Strategy | Levels | Cost | Duration |
|-----|------|----------|--------|------|----------|
| cl-def-r20 | Rust | default | 22/22 | $203 | 3h38m |
| cl-go-def-r20 | Go | default | 9/10 | $131 | 1h52m |
| cl-java-def-r20 | Java | default | 26/26 | $158 | 3h55m |
| cl-ts-def-r20 | TS | default | 26/26 | $175 | 3h56m |
| cl-qg-r20 | Rust | quality-gate | 26/26 | $266 | 4h42m |
| cl-scala-qg-r20 | Scala | quality-gate | 26/26 | $204 | 5h28m |

**Highlight:** First multi-language round with all 5 languages. Go failed early (9/10). Java and TS completed all 26.
**Key insight:** Go agent struggled — may need language-specific strategy tuning.

---

## R19 — 2026-03-23

**Framework:** First round with multi-language support (5 languages), pre-difficulty-wall
**Runs:** 6 Claude agents

| Run | Lang | Strategy | Levels | Cost | Duration |
|-----|------|----------|--------|------|----------|
| cl-def-r19 | Rust | default | 26/26 | $193 | 3h30m |
| cl-go-def-r19 | Go | default | 26/26 | $190 | 2h54m |
| cl-java-def-r19 | Java | default | 26/26 | $140 | 2h47m |
| cl-ts-def-r19 | TS | default | 9/10 | $31 | — |
| cl-qg-r19 | Rust | quality-gate | 20/21 | $289 | — |
| cl-scala-qg-r19 | Scala | quality-gate | 11/12 | $59 | — |

**Highlight:** First 5-language round. Rust/Go/Java defaults all completed 26/26. TS and Scala-QG failed early. Java cheapest at $140.

---

## R14 — 2026-03-22 *(reconstructed from session history)*

**Framework:** Pre-difficulty-wall, pre-multi-language (Rust + early Java/TS/Scala). 24 levels at the time.
**Session:** [c0952486](c0952486-486d-417f-aac5-67701af16ff3.jsonl), [4e0cc227](4e0cc227-561b-4e10-b2d1-81b95ac92395.jsonl)

**Key results:**
- Rust default: 24/24, 736 turns
- Rust QG: 24/24, 784 turns (+6.5% overhead)
- Java: Failed at L10 — gradle-wrapper.jar gitignored, preventing container build
- Both Rust runs cleared all 24 levels — QG cost ~6.5% more turns for same completion

**Key insight:** First head-to-head default vs QG comparison. QG overhead moderate. Java failure led to .gitignore fix.

---

## R10-R13 — 2026-03-20/21 *(reconstructed from session history)*

**Framework:** Post-xtask migration, compliance/compare skills being built. Rust-only.
**Session:** [197d481a](197d481a-20e7-42f3-bc4f-947a2416f6c3.jsonl), [d2f6719d](d2f6719d-a833-4e67-bf53-611e63b23b80.jsonl)

**Key findings:**
- R4-R8: Used to investigate QG over-thinking — agents spending all tokens on style compliance instead of passing tests
- Led to: Two-pass QG strategy design (code freely first, then cleanup)
- Led to: Migrating clippy enforcement from agent-side to orchestrator --gate flag
- Led to: Auto-retry for infra failures (Anthropic 529s, container timeouts)
- Led to: Tiered turn limits (L01-L09→40, L10+→60)

---

## Round 4 — 2026-03-19 *(reconstructed from session history)*

**Framework:** Shell-script orchestration (pre-xtask), 4 branches (main/strategy x base/test), Claude + Codex
**Session:** [9eb1b65d](9eb1b65d-9a4c-473f-90ca-5841961d93f3.jsonl)

**8 completed runs:**
- codex-main-full: 97/97 tests
- codex-strat-full: 97/97 tests
- codex-main-lvl: 97/97 tests
- codex-strat-lvl: 97/97 tests
- claude-main-full: 96/97 tests
- claude-strat-full: 96/97 tests
- claude-main-lvl: 97/97 tests
- claude-strat-lvl: 77/97 tests

**Highlight:** First cost analysis round. Led to building `cargo xtask tokens` command. Claude strategy-levels run underperformed (77/97).
**Key insight:** Codex achieved perfect scores on all 4 runs. Claude struggled with levels mode + strategy combo.

---

## Rounds 1-3 — 2026-03-18 *(reconstructed from session history)*

**Framework:** Shell-script orchestration (run-agent.sh, test-level.sh), first containerized runs
**Session:** [f0e44004](f0e44004-0b00-456a-84ec-b3f6cbbda88c.jsonl)

**4 parallel agents:** Claude + Codex on base and strategy branches.
- First verification bench to test the framework
- Dealt with Codex AGENTS.md setup issues and agent monitoring
- Identified L16 (TCO) as biggest time sink (43 minutes — 3x any other level)

**Led to:** Removing full-test-run default from test-level.sh, removing clippy from CLAUDE.md

---

## Pre-Framework Runs — 2026-03-17 *(reconstructed from session history)*

**Framework:** No orchestration — manual `cargo test`, no containers, no levels mode
**Sessions:** [6433449f](6433449f-301c-4a1b-b2b6-4a7095f9fc52.jsonl), [c66f28ae](c66f28ae-2c88-4d39-a267-a2cf1d41fe18.jsonl), [254341ae](254341ae-ee19-4361-b148-c02191a93136.jsonl), [378952f4](378952f4-4fee-4d81-ac61-3446b60e635a.jsonl), [e993769b](e993769b-9891-4582-9de2-5a3a121acaba.jsonl), [8f7a91db](8f7a91db-b41a-4420-8637-13bf5a6317ea.jsonl)

**Exploratory runs (no scoring):**
- **baseline branch (monolithic):** Passed all 60 tests in one shot — single mod.rs file
- **strategy-test-1 (multi-file):** Hit max_tokens on first attempt. Second attempt reached 83/92 with multiple "continue" prompts
- **strategy-test-1 (parser/eval split):** Split into types.rs/parser.rs/eval.rs — ran out of tokens before completing
- **main-test-1 (continuations):** Built continuation.rs module, focused on L14-level features

**Key insight:** Monolithic approach was faster for small codebases but creates tech debt at scale. Multi-file agents ran out of tokens before completing. This insight directly drove the quality-gate strategy design.

---

## Cost Trend Summary

| Round | Date | Best Cost (full) | Best Lang | Completion Rate | Framework Change |
|-------|------|-----------------|-----------|----------------|-----------------|
| Pre | Mar 17 | n/a | — | n/a | No framework |
| R1-3 | Mar 18 | n/a | — | — | Shell scripts |
| R4 | Mar 19 | n/a | Rust | 7/8 runs pass | First cost analysis |
| R14 | Mar 22 | — | Rust | 2/2 (24/24) | Pre-wall |
| R19 | Mar 23 | $140 | Java | 3/6 (26/26) | Multi-language |
| R20 | Mar 23 | $158 | Java | 4/6 (26/26) | 5 languages |
| R21 | Mar 23 | $169 | TS | 5/6 (28/28) | +surprise levels |
| R22 | Mar 24 | $162 | TS | **1/6 (28/28)** | **Difficulty wall** |
| R23 | Mar 24 | $162 | Scala-QG | 1/6 (28/28) | +QG strategy swap |
| R24 | Mar 24 | $220 | Scala-QG | 5/6 (28/28) | +1500-line limit |
| R25 | Mar 24 | **$169** | **Scala-QG** | 5/6 (28/28) | Stable |
| R26 | Mar 24 | $162 | Java | 4/6 (28/28) | Pre-hooks |
