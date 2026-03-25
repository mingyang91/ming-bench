# MING Dev Session Archive

Generated: 2026-03-25
Sessions: 40 | Source: `~/.claude/projects/-home-my--zeroclaw-workspace-cs61a-bench/`

---

## 2026-03-17

### [Project Genesis: Scheme Interpreter Benchmark Concept](94db3ae3-cf37-42f4-9c58-2f40fdabff2e.jsonl)
- **First prompt:** "help me build a git project template here. background: I want to build a test playground enviroment for coding agent bench. my initial thought is let coding agent write a scheme interrupter, so I think sicp-like test case is best"
- **Summary:** The genesis session for the entire MING project. Designed the benchmark framework concept — a Scheme interpreter as a coding agent evaluation task. Installed guile for ground-truth testing, removed old hw01 content, created the initial project skeleton with Cargo.toml, CLAUDE.md, README, test cases, and the level-by-level development strategy.
- **Topics:** project genesis, benchmark design, CS61A/SICP-style test cases, Scheme interpreter task design, guile installation
- **Files:** Cargo.toml, lib.rs, mod.rs, CLAUDE.md, README.md, .gitignore, test_cases.sh
- **Commands:** which chez/racket/guile, sudo apt install guile-3.0, rm -rf src/hw01, mkdir -p src/scheme
- **Changes:**
  - Conceived the Scheme interpreter benchmark concept for coding agent evaluation
  - Installed guile-3.0 as ground-truth reference implementation
  - Created initial project skeleton (Cargo.toml, src/scheme/, CLAUDE.md)
  - Designed level-by-level test progression (L01-L16 at that point)
  - Created 92 initial test cases across levels
- **Size:** 447K

---

### [Initial Interpreter Build: L01-L10](6433449f-301c-4a1b-b2b6-4a7095f9fc52.jsonl)
- **First prompt:** "pass the test_l01"
- **Summary:** Built the initial Scheme interpreter from scratch, passing L01-L10 tests (all 92 tests). Created the full module structure (parser, eval, builtins, types, error, value) and committed as "Implement complete Scheme interpreter (L1-L10)". Also explored using rust-analyzer LSP MCP.
- **Topics:** initial interpreter build, L01-L10 implementation, tokenizer, parser, evaluator, builtins
- **Files:** eval.rs, parser.rs, value.rs, error.rs, mod.rs, lib.rs, Cargo.toml, helpers.md
- **Commands:** cargo test test_l01..l10, git add, git commit
- **Changes:**
  - Created complete Scheme interpreter module structure from scratch
  - Implemented tokenizer, parser, and evaluator covering L01-L10
  - Passed all 92 tests
- **Size:** 489K

---

### [Agent Bench Run: strategy-test-1 (incomplete)](378952f4-4fee-4d81-ac61-3446b60e635a.jsonl)
- **First prompt:** "pass all tests"
- **Summary:** Agent benchmark run on the "strategy-test-1" branch. Hit max_tokens limit before completing. Only modified mod.rs before running out of budget.
- **Topics:** agent benchmark run, strategy-test-1, max_tokens failure
- **Size:** 425K

---

### [Agent Bench Run: strategy-test-1 (multi-file)](254341ae-ee19-4361-b148-c02191a93136.jsonl)
- **First prompt:** "pass all tests"
- **Summary:** Agent benchmark run on "strategy-test-1" with multiple "continue" prompts. Split the interpreter into separate files (types.rs, parser.rs, eval.rs, mod.rs). Multi-file architecture approach contrasting with monolithic style.
- **Topics:** agent benchmark run, multi-file architecture, parser/types/eval split
- **Size:** 400K

---

### [Agent Bench Run: baseline (all 60 tests)](c66f28ae-2c88-4d39-a267-a2cf1d41fe18.jsonl)
- **First prompt:** "pass all test"
- **Summary:** Agent benchmark run on the "baseline" branch. Implemented a Scheme interpreter passing all 60 tests across levels 1-9+ including atoms, arithmetic, comparisons, define/if/quote, lambda/closures, list operations, let/begin/cond, type predicates, and tail call optimization. Single monolithic mod.rs approach.
- **Topics:** agent benchmark run, baseline strategy, all-at-once test pass
- **Size:** 434K

---

### [Agent Bench Run: strategy-test-1 (L01-L16)](e993769b-9891-4582-9de2-5a3a121acaba.jsonl)
- **First prompt:** "go ahead"
- **Summary:** Agent attempted to build a complete Scheme interpreter from scratch, reaching 83/92 tests passing before being interrupted. Worked through parser, eval, macros, and call/cc issues across L01-L16.
- **Topics:** scheme interpreter implementation, parser fixes, macro ellipsis handling, call/cc
- **Size:** 1.6M

---

### [Agent Bench Run: main-test-1 (continuations)](8f7a91db-b41a-4420-8637-13bf5a6317ea.jsonl)
- **First prompt:** "go ahead"
- **Summary:** Full interpreter build attempt — created eval.rs, parser.rs, builtins.rs, types.rs, and a continuation.rs module. Multiple user interruptions. Focused on L14-level features including continuations.
- **Topics:** scheme interpreter implementation, continuations, build errors
- **Size:** 1.5M

---

### [Agent Exploration: main-test-1 (reconnaissance)](c5d8d6fb-cbf0-49b3-962c-4ad2d31f533c.jsonl)
- **First prompt:** "go ahead"
- **Summary:** Brief exploratory session. Agent ran cargo test (92 failures), examined project structure, determined interpreter needed building from scratch. Session ended with /exit before implementation.
- **Topics:** project exploration, test status check
- **Size:** 72K

---

### [Git Push: All Branches to GitHub](6065b353-0c59-41f3-82c8-e669478087ae.jsonl)
- **First prompt:** "commit all files, and push all branch to git@github.com:mingyang91/scheme-bench.git"
- **Summary:** Staged 7 Scheme interpreter source files (1335 lines), committed on main-test-1 branch, added remote origin, pushed all 4 branches (main, main-test-1, strategy, strategy-test-1).
- **Topics:** git commit, git push, remote setup
- **Changes:**
  - Committed 7 interpreter modules on main-test-1
  - Added remote origin git@github.com:mingyang91/scheme-bench.git
  - Pushed all 4 branches to remote
- **Size:** 27K

---

## 2026-03-18

### [FP Rules Translation & xtask Bootstrap](2d132d3e-5ca1-4946-822f-bba48e413e3f.jsonl)
- **First prompt:** "@MY_CLAUDE.md this file from 2 claude rules project can you merge it and translate to rust, discard N/A rule"
- **Summary:** The foundational session that bootstrapped the entire benchmark framework. Merged FP coding rules from two existing CLAUDE.md files into Rust-focused quality-gate strategy, built the initial orchestration infrastructure (worktrees, container isolation, level-by-level testing), created the xtask CLI with test/run-agent/tokens/analyze commands, added AST-based quality gates, designed the level-by-level progression system, and ran first benchmark rounds.
- **Topics:** FP rule translation to Rust, quality-gate strategy creation, xtask CLI bootstrapping, worktree-based agent isolation, container sandboxing, AST quality gates, Codex integration
- **Files:** CLAUDE.md, bench/SPEC.md, bench/strategies/default.md, bench/strategies/quality-gate.md, Dockerfile.bench, xtask/src/cmd/test_level.rs, xtask/src/cmd/run_agent.rs, xtask/src/main.rs, xtask/src/model.rs
- **Changes:**
  - Merged FP/strict-type rules from two CLAUDE.md projects into Rust quality-gate strategy
  - Created bench/strategies/default.md and quality-gate.md
  - Built xtask CLI framework (test, run-agent, tokens, analyze commands)
  - Implemented worktree-based agent isolation for parallel benchmark runs
  - Added Dockerfile.bench for containerized test execution
  - Built level-by-level test progression with BENCH_LEVEL env var
  - Added AST-based Result<_, String> check as quality gate
  - Integrated Codex agent support alongside Claude
  - Added regression testing after each level pass
  - Implemented tiered turn limits per level difficulty
  - Added string immutability breaking change level (L15)
- **Size:** 8.9M

---

### [Bench Framework Shell Scripts](f0e44004-0b00-456a-84ec-b3f6cbbda88c.jsonl)
- **First prompt:** "Implement the following plan: # Plan: Bench Framework with Session Capture..."
- **Summary:** Built the initial bench framework shell scripts (run-agent.sh, test-level.sh, list-results.sh, setup.sh) and BENCH_GUIDE.md. Launched 4 parallel agent runs (Claude + Codex on base and strategy branches). Dealt with Codex AGENTS.md setup issues.
- **Topics:** bench framework creation, shell scripts, agent orchestration, Claude vs Codex
- **Files:** scripts/run-agent.sh, scripts/test-level.sh, scripts/list-results.sh, scripts/setup.sh, BENCH_GUIDE.md
- **Changes:**
  - Created scripts/run-agent.sh, test-level.sh, list-results.sh, setup.sh
  - Created BENCH_GUIDE.md documentation
  - Added clippy gate enforcement to test-level.sh
  - Launched first verification bench with 4 parallel agents
  - Fixed AGENTS.md symlink issue for Codex compatibility
- **Size:** 369K

---

### [Agent Performance Diagnosis](f89469f2-eb68-4276-8701-62cc4039a89a.jsonl)
- **First prompt:** "this bench test are take long time to complete. the agent log here: ... can you help me identity what is the reason?"
- **Summary:** Diagnosed why agent runs were slow — identified L13, L14, L15, L16 (43min!) as time/token consumers. Discussed framework improvements including removing default full-test-run behavior and removing clippy from CLAUDE.md.
- **Topics:** agent performance diagnosis, L16 struggle analysis, test-level.sh refactoring
- **Changes:**
  - Identified L16 (TCO) as the biggest agent time sink (43 minutes)
  - Removed no-args full test run logic from test-level.sh
  - Removed clippy requirement from CLAUDE.md/AGENTS.md
- **Size:** 1.2M

---

### [Quality-Gate Coding Style Rules](a48cc1f5-5dec-47b6-8e57-f7eb2e816bf8.jsonl)
- **First prompt:** "how do you think those rules? ## Functional Style - Prefer immutable-first data flow..."
- **Summary:** Reviewed and refined quality-gate coding style rules for Rust. Discussed functional style, structural recursion, helper extraction thresholds, and Haskell-style monadic evaluation approaches.
- **Topics:** quality-gate rules, functional style guidelines, Rust idioms, iterator pipelines
- **Changes:**
  - Refined functional style rules (immutable-first, iterator pipelines, collect::<Result>)
  - Set helper extraction threshold to 5 ops
  - Added detailed helpers.md documentation
- **Size:** 521K

---

### [claude-hud Plugin Installation](d6a6ea2c-5a11-44d7-a9fc-113d89080435.jsonl)
- **First prompt:** "can you install this plugin https://github.com/jarrodwatts/claude-hud"
- **Summary:** Installed the claude-hud plugin from GitHub. Created plugin config.
- **Topics:** claude-hud plugin, Claude Code plugins
- **Size:** 154K

---

### [CLAUDE.md Review: Rule Completeness](3c7b9376-839a-4716-860a-e402fc35fdad.jsonl)
- **First prompt:** "please tell me is the rule file detail enough for your?"
- **Summary:** Claude reviewed CLAUDE.md and assessed completeness. Identified six areas to tighten: eval_str return semantics, display format, numeric scope, error message format, level boundaries, helpers registry.
- **Topics:** CLAUDE.md review, strategy rules, Scheme semantics gaps
- **Size:** 5.7K

---

### [CLAUDE.md Review: Rule Completeness (2)](662fe79e-0e1f-4714-b785-422e849b3b78.jsonl)
- **First prompt:** "please tell me is the rule file detail enough for your?"
- **Summary:** Second review of CLAUDE.md. Noted thorough coverage; suggested expanding one-liner level descriptions with concrete behavioral specs.
- **Topics:** CLAUDE.md review, level descriptions
- **Size:** 5.1K

---

### [/exit sessions](28fff666-2e35-4682-8b4f-839fcb0d049f.jsonl), [2edf7c41](2edf7c41-179f-4730-acf3-4b438b40af88.jsonl)
- Immediate /exit sessions on main-test-1 branch. No interaction.
- **Size:** 1.9K, 1.8K

---

## 2026-03-19

### [Token Billing & Cost Analysis Infrastructure](9eb1b65d-9a4c-473f-90ca-5841961d93f3.jsonl)
- **First prompt:** "these bench tests are latest round and done, please analysis the token bill each bench. All 8 runs are already complete."
- **Summary:** Built the token billing and cost analysis infrastructure. Created xtask tokens command, added OpenAI pricing model, fixed PATH issues for agent CLI. Refactored project structure to separate framework CLAUDE.md from agent-visible strategy files, merged strategy branch to main.
- **Topics:** token billing, xtask tokens command, OpenAI pricing, Claude vs Codex cost, branch merge
- **Files:** xtask/src/cmd/tokens.rs, CLAUDE.md, bench/CLAUDE.md, bench/SPEC.md, README.md, Dockerfile.bench
- **Changes:**
  - Built xtask tokens command for parsing Claude and Codex session token usage
  - Added OpenAI pricing model (GPT-4.1 rates)
  - Fixed CLI PATH issues for agent binaries
  - Separated framework CLAUDE.md from agent bench/CLAUDE.md
  - Merged strategy branch to main, deleted stale branches
  - Created symlink-based strategy system
- **Size:** 4.5M

---

### [thiserror Rule Update](cee71130-f75d-4f77-9f4c-18a5b53abd8a.jsonl)
- **First prompt:** "save to claude.md Use thiserror with structurally typed variants..."
- **Summary:** Updated CLAUDE.md Code Style section with strict thiserror rule requiring structurally typed variants with domain-specific fields, prohibiting String-wrapping variants.
- **Topics:** thiserror code style rule, error enum design
- **Size:** 23K

---

### [AGENTS.md Symlink](d8196de0-c842-42cc-9a37-65fbf3a01bdf.jsonl)
- **First prompt:** "please link claude.md to agents.md in project root, and add to git"
- **Summary:** Created AGENTS.md as a symlink to CLAUDE.md, staged for git on strategy branch.
- **Size:** 7.9K

---

### [CLAUDE.md Ambiguity Review (1)](02936753-fb1c-4790-9697-746571df7f3f.jsonl)
- **First prompt:** "is the claude.md clear enough for your future session? any ambigus rule?"
- **Summary:** Flagged 5 ambiguities: void/nil returns, mod.rs "thin" definition, "violations" meaning, error enum granularity, immutable-first tension with set!/call/cc.
- **Size:** 5.7K

---

### [CLAUDE.md Ambiguity Review (2)](11770d30-ea52-4a25-b553-5b7b6c485f7f.jsonl)
- **First prompt:** "is the claude.md clear enough for your future session? any ambigus rule?"
- **Summary:** Identified 10 issues: contradictory mod.rs line limits, conflicting function body length rules, undefined thresholds, unclear helpers.md registration, tension between proactive refactoring and minimal changes.
- **Size:** 6.6K

---

### [CLAUDE.md Ambiguity Review (3)](3cb692fc-a80e-4660-bbd1-5d8024dd7aa6.jsonl)
- **First prompt:** "is the claude.md clear enough for your future session? any ambigus rule?"
- **Summary:** Identified 8 ambiguities: unclear "impl line count", match arm extraction threshold overlap, missing helpers.md, eval_str void/nil semantics, .unwrap() scope for test helpers.
- **Size:** 6.6K

---

### [Disable Global Memory](80d41074-aa63-4028-bfa6-b4d79f35b4fd.jsonl)
- **First prompt:** "is this possible to disable your global memory file in new session?"
- **Summary:** Discussed disabling Claude Code auto-memory. Deleted feedback_typed_errors.md memory entry.
- **Size:** 30K

---

### [Incomplete CLAUDE.md Review](d77b9256-3d35-4397-9639-2b9f74b78136.jsonl)
- **Summary:** Session cut short before response completed.
- **Size:** 2.8K

---

### [/memory Error](9691135f-28c9-4a7d-8f10-a4cbed0bb409.jsonl)
- **Summary:** Failed /memory command (setRawMode errno 5). No interaction.
- **Size:** 2.1K

---

## 2026-03-20

### [Compliance & Compare Skills + Session Analysis](d2f6719d-a833-4e67-bf53-611e63b23b80.jsonl)
- **First prompt:** "Agent CLAUDE.md Compliance Analysis -- Method. Input: A results directory (session JSONL + produced code)..."
- **Summary:** Massive multi-day session (Mar 20-23) — the primary design cockpit for MING. Built the compliance analysis skill, compare skill, and session-turns xtask. Iterated on quality-gate vs default strategy comparisons across rounds 10-15, redesigned tech-debt punishment levels, added realworld Scheme test fixtures, built the ground-truth verification system (Chez Scheme), migrated test fixtures to shared tests.json, and reordered levels to create the L16-L18 difficulty wall.
- **Topics:** compliance skill, compare skill, session-turns xtask, quality-gate strategy refinement, tech-debt punishment, realworld test fixtures, ground-truth verification (Chez), tests.json migration, level reordering, QG nesting/file-size limits
- **Files:** CLAUDE.md, bench/SPEC.md, bench/strategies/quality-gate.md, bench/tests.json, bench/fixtures/*, xtask/src/cmd/session_turns.rs, xtask/src/cmd/test_level.rs, .claude/skills/compare/SKILL.md, .claude/skills/compliance/SKILL.md, Dockerfile.bench, Dockerfile.jvm, Dockerfile.node
- **Changes:**
  - Built /compliance skill for analyzing agent rule adherence
  - Built /compare skill for side-by-side run comparison narratives
  - Created session-turns xtask command
  - Added realworld Scheme test fixtures (alexpander, scheme-eval, RBT, etc.)
  - Built ground-truth verification system using Chez Scheme
  - Migrated all test cases to shared bench/tests.json + bench/fixtures/
  - Redesigned level ordering: moved TCO/pair-mutation/call/cc to L16-L18 difficulty wall
  - Added L25-L26 integration levels
  - Refined QG rules: nesting depth limits, function/file line limits
- **Size:** 28M

---

### [Code Quality Analysis of Running Agents](f1037761-fb02-414d-8333-a7e99ea47ebb.jsonl)
- **First prompt:** "currently those 2 agent bench are running: cl-def-lvl at L21, cl-qg-lvl at L19. please tell me the default agent code quality"
- **Summary:** Analyzed code quality of running agents. Ran clippy, counted warnings, examined function/file sizes, compared default vs quality-gate progress.
- **Topics:** code quality analysis, clippy warnings, agent monitoring, default vs QG comparison
- **Size:** 155K

---

### [Quality Gate Limits Discussion](31b2dae2-ac5a-45ef-975b-f3a4845abcf4.jsonl)
- **First prompt:** "honestly answer me, how many lines/tokens function/file are too long for you?"
- **Summary:** Discussed and adjusted quality gate code length limits. Increased thresholds to match 50% of 1M context capacity (~150 lines/function, ~300 lines/file). Updated quality-gate.md, clippy.toml, and xtask test_level.rs.
- **Topics:** quality gate limits, function length, file length, clippy configuration
- **Changes:**
  - Raised quality-gate function length limit to ~150 lines
  - Raised quality-gate file length limit to ~300 lines
  - Updated quality-gate.clippy.toml and GATE_LINT_FLAGS
- **Size:** 114K

---

### [/exit session](f0fdd208-cb96-4460-85f6-c488eb1711c5.jsonl)
- Immediate /exit. No interaction.
- **Size:** 1.8K

---

## 2026-03-21

### [Compliance Skill + Compare + Session-Turns Build](197d481a-20e7-42f3-bc4f-947a2416f6c3.jsonl)
- **First prompt:** "Agent CLAUDE.md Compliance Analysis -- Method..."
- **Summary:** Built the compliance analysis skill and compare xtask command. Designed session-turns analysis tool, implemented auto-retry for infrastructure failures, added tiered turn limits, migrated QG clippy from agent-side to orchestrator-side. Ran comparison rounds r4-r8 investigating QG over-thinking.
- **Topics:** compliance skill, compare xtask, session-turns, auto-retry, tiered turn limits, QG clippy migration
- **Files:** .claude/skills/compliance/SKILL.md, .claude/skills/compare/SKILL.md, xtask/src/cmd/session_turns.rs, xtask/src/cmd/run_agent.rs, xtask/src/cmd/test_level.rs
- **Changes:**
  - Created /compliance skill
  - Created /compare skill
  - Built session-turns xtask command
  - Implemented auto-retry (up to 2x) for infra failures
  - Added tiered turn limits: L01-L09→40, L10+→60
  - Migrated QG clippy from agent-side #![deny] to orchestrator --gate flag
  - Added "Problem-Solving Approach" guidance to reduce over-thinking
- **Size:** 3.0M

---

### [xtask Clippy Cleanup](377ae881-c2bf-43fa-a7b1-5d28621e80da.jsonl)
- **First prompt:** "run clippy on xtask, and fix all lint"
- **Summary:** Ran clippy on xtask crate and fixed all lint warnings across all command modules.
- **Topics:** clippy lint fixes, xtask code quality
- **Files:** xtask/src/cmd/analyze.rs, bench.rs, run_agent.rs, test_level.rs, verify.rs, main.rs
- **Size:** 2.1M

---

## 2026-03-22

### [MING Naming + r14 Comparison + Regression Analysis](c0952486-486d-417f-aac5-67701af16ff3.jsonl)
- **First prompt:** "help me choose name: MING-bench = Ming scheme Interpreter N___ Game bench"
- **Summary:** Chose MING acronym (Ming Interpreter Nurture Gauntlet), compared r14 Rust default vs quality-gate runs (both cleared all 24 levels), analyzed L06/L10 regression root causes (string-set! immutability, call/cc exception handling), and updated documentation.
- **Topics:** MING naming, r14 comparison, regression analysis, string-set! immutability, call/cc
- **Changes:**
  - Named the project MING (Ming Interpreter Nurture Gauntlet)
  - Analyzed default vs quality-gate r14 results (736 vs 784 turns, both 24/24)
  - Root-caused L06 string-set! and L10 call/cc regressions
  - Updated CLAUDE.md, README.md, BENCH_GUIDE.md with MING branding
- **Size:** 1.3M

---

### [Java Bench Failure Investigation](4e0cc227-561b-4e10-b2d1-81b95ac92395.jsonl)
- **First prompt:** "tell me why cl-java-def-r14 bench failed?"
- **Summary:** Discovered gradle-wrapper.jar was gitignored, preventing Java build in container. Compared r14 results across Rust default/QG, Java, TypeScript, Scala runs. Java only reached L10.
- **Topics:** Java bench failure, gradle-wrapper.jar gitignore, multi-language r14 comparison
- **Size:** 482K

---

### [r14-r25 Comparison + Anti-Cheating + Difficulty Wall](8808d5e5-0aeb-43c2-9952-38a8a1726431.jsonl)
- **First prompt:** "compare r14"
- **Summary:** Long-running session (Mar 22-24) focused on run comparison and strategic redesign. Compared r14-r25 across all languages, discovered agents pre-reading future levels (cheating), designed progressive spec revelation, reordered levels for L16-L18 difficulty wall, implemented two-pass QG strategy, raised file size limit to 1500 lines, ran compliance analysis on Scala QG.
- **Topics:** round comparisons, agent cheating detection, progressive spec revelation, level reordering, two-pass QG strategy, radical refactoring prompts, file size limits
- **Files:** CLAUDE.md, bench/SPEC.md, bench/strategies/quality-gate.md, bench/strategies/default.md, bench/hidden/SPEC_L27.md, bench/tests.json, memory/feedback_no_preread_cheating.md
- **Changes:**
  - Discovered agents pre-reading future levels as cheating; created memory note
  - Reordered levels: moved TCO/pair-mutation/call/cc to L16-L18 difficulty wall
  - Implemented two-pass QG strategy: coding pass (default.md) then cleanup pass (quality-gate.md)
  - Added radical refactoring encouragement to QG cleanup prompts
  - Raised file size limit from ~300 to 1500 lines
  - Cleaned up old results and branches (r1-r14)
  - Ran compliance analysis on Scala QG r25
- **Size:** 3.8M

---

## 2026-03-23

### [Framework Evaluation](de5506a8-77c0-4341-ac82-eedc56dd3cc3.jsonl)
- **First prompt:** "what is this shit? is this bench framework valuable? or garbarge?"
- **Summary:** User asked Claude to evaluate MING's value. Explored codebase (~10K lines Rust tooling, 5 languages, 26 levels, 238 tests), gave positive verdict. Compared to SWE-bench, Aider-bench, HumanEval — concluded MING is unique in progressive greenfield architecture testing.
- **Topics:** framework evaluation, benchmark landscape comparison, SWE-bench vs MING
- **Size:** 67K

---

## 2026-03-24

### [MING Naming + Two-Pass QG + Surprise Levels](5a1d1f38-7379-40f3-a3c2-f480f657135f.jsonl)
- **First prompt:** "help me create a better name for this project. Can we make the acronym be MING?"
- **Summary:** Named the project MING, merged coding style rules into quality-gate.md, researched opaque/newtype patterns. Ran extensive comparisons (r12-r20) across all languages, designed surprise levels L27-L28 (step-limited eval + concurrency), implemented two-pass QG strategy with strategy swap.
- **Topics:** project naming, quality-gate rules, surprise levels L27-L28, two-pass QG strategy, cross-language comparison
- **Files:** CLAUDE.md, README.md, bench/SPEC.md, bench/strategies/quality-gate.md, bench/hidden/SPEC_L27.md, bench/fixtures/l28_*
- **Changes:**
  - Renamed project to MING (Ming Interpreter Nurture Gauntlet)
  - Added immutable-first data flow rule to QG strategy
  - Designed surprise levels L27 (step-limited eval) and L28 (concurrency + perf stress)
  - Implemented two-pass QG strategy: coding pass → cleanup pass with strategy swap
  - Relaxed var/mut rules: allowed in local scope if never escaping
  - Updated all documentation for new level structure
- **Size:** 12M

---

### [Multi-Language Extension: Go/Java/TS/Scala](82900cdb-c52f-4e1f-82b4-e45daabaa01c.jsonl)
- **First prompt:** "I want to mirror this test framework. not only rust, but also bench agent in java/go/typescript/scala"
- **Summary:** Extended benchmark from Rust-only to five languages. Created per-language scaffolds with build/test scripts, test harnesses reading shared tests.json, Dockerfiles for JVM and Node, per-language strategy files. Built Scala scaffold, integrated Scalafix/Scalafmt, added pure-FP rules for Scala QG. Added output token safety caps.
- **Topics:** multi-language support, Go/Java/TypeScript/Scala scaffolds, Dockerfile.jvm, Dockerfile.node, shared test harness, token safety caps
- **Files:** bench/go/*, bench/java/*, bench/ts/*, bench/scala/*, Dockerfile.jvm, Dockerfile.node, bench/strategies/go/default.md, bench/strategies/java/default.md
- **Changes:**
  - Created Go interpreter scaffold (bench/go/)
  - Created Java interpreter scaffold (bench/java/) with Gradle + JUnit 5
  - Created TypeScript interpreter scaffold (bench/ts/) with vitest
  - Created Scala interpreter scaffold (bench/scala/) with Mill then sbt + munit
  - Added Dockerfile.jvm and Dockerfile.node
  - Built shared test harness reading tests.json + fixtures for all languages
  - Added per-language strategy files
  - Added output token safety caps per level tier (100K-500K)
  - Added Scalafix + pure-FP rules for Scala QG
- **Size:** 12M

---

## 2026-03-25

### [Session Backup & Archive](c4050e99-2147-40c2-9d6e-bb4ba1cfaf68.jsonl)
- **First prompt:** "can you backup all history sessions file to result?"
- **Summary:** Backed up all session JSONL files to results/dev-sessions/ and generated this archive document.
- **Topics:** session backup, history preservation, cross-session analysis
- **Size:** 135K

### [R22-R26 Analysis, QG Tuning, sboyer Fix, PreToolUse Hooks](current session)
- **First prompt:** "compare r14"
- **Summary:** Extended multi-round benchmark analysis session. Compared R14-R26 across all 6 Claude runs (5 languages, default + quality-gate strategies). Key deliverables: (1) Level reordering — moved TCO/pair-mutation/call-cc to L16-L18 difficulty wall, (2) Two-pass QG strategy swap — coding pass uses default.md, cleanup uses quality-gate.md (cut QG coding turns 44%), (3) Raised file size limit 500→1500 based on empirical struggle-rate analysis, (4) Diagnosed sboyer stack overflow root cause (debug vs release stack frames), (5) Added [profile.test] opt-level=2 and PreToolUse hooks blocking direct test commands, (6) Agent pre-planning analysis (zero forward design confirmed), (7) Scala-QG compliance audit (accidental 300-line file targeting).
- **Topics:** run comparison, token analysis, level reordering, QG strategy swap, file size limits, sboyer stack overflow, PreToolUse hooks, agent compliance, pre-planning analysis, difficulty wall calibration
- **Commits:** `deb3aadc` (level reorder), `a8722d35` (QG swap + 1500 limit), `60170053` (hooks + profile.test)
- **Key findings:**
  - Difficulty wall dropped 26/26 completion from 83% (r21) to 17% (r22)
  - QG strategy swap made QG coding pass competitive with default (20.4 vs 21.7 turns/level)
  - File size 1500 is data-driven: agents struggle at 67% rate above 2000 lines
  - Scala-QG set cost record: $157 for 26/26 ($6.02/level)
  - Agents never pre-plan despite seeing full SPEC — reactive coding only
  - Rust agents ignore "never run cargo test" rule after 50 turns of pressure
