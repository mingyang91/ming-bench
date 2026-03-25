# MING Benchmark — Design Evolution

A record of key design decisions, what changed, and why. Latest first.

---

## Phase 5: Validation & Reflection (Mar 23-25)

### Session Archival (This Document)
**Session:** [c4050e99](c4050e99-2147-40c2-9d6e-bb4ba1cfaf68.jsonl)
**What:** Backed up all 40 dev sessions to results/dev-sessions/ with this design evolution document and session archive.
**Why:** Preserving the design rationale and decision history for future reference.

### Java Bench Failure Root Cause
**Session:** [4e0cc227](4e0cc227-561b-4e10-b2d1-81b95ac92395.jsonl)
**What:** Discovered gradle-wrapper.jar was gitignored, preventing Java build in containers. Java runs only reached L10.
**Why:** Container builds need all artifacts committed — .gitignore rules that make sense for development can break CI.

### Framework Value Assessment
**Session:** [de5506a8](de5506a8-77c0-4341-ac82-eedc56dd3cc3.jsonl)
**What:** Self-evaluation of whether MING is valuable. Compared to SWE-bench, Aider-bench, HumanEval, MBPP.
**Conclusion:** MING is unique in testing progressive greenfield architecture — no other benchmark measures how agents handle compounding design decisions over 26+ levels. ~10K lines of tooling, 5 languages, 238 tests.

---

## Phase 4: Naming & Strategic Redesign (Mar 22-24)

### Ground-Truth Verification System
**Session:** [d2f6719d](d2f6719d-a833-4e67-bf53-611e63b23b80.jsonl)
**What:** Built `cargo xtask verify` using Chez Scheme to validate all test expected values against a real Scheme implementation.
**Why:** Test expected values were hand-written — needed automated verification that they match actual Scheme semantics.

### Output Token Safety Caps
**Session:** [82900cdb](82900cdb-c52f-4e1f-82b4-e45daabaa01c.jsonl)
**What:** Added per-level-tier output token caps (100K for foundation levels, 200K for complex levels, 500K for wall/surprise levels). Orchestrator monitors session.jsonl in real-time and sends SIGTERM when exceeded.
**Why:** Circuit breakers for dead loops or stuck agents. Set generously so legitimate work never hits them — only runaway agents.

### Multi-Language Extension
**Session:** [82900cdb](82900cdb-c52f-4e1f-82b4-e45daabaa01c.jsonl)
**What:** Extended from Rust-only to five languages: Rust, Go, Java (Gradle + JUnit 5), TypeScript (vitest), Scala (sbt + munit). Created per-language scaffolds, Dockerfiles (JVM, Node), shared test harness reading tests.json.
**Why:** Different languages test different agent capabilities. Rust tests ownership/borrowing reasoning. Go tests simplicity discipline. Java tests boilerplate tolerance. TypeScript tests dynamic typing flexibility. Scala tests FP pattern matching.

### File Size Limit Raised to 1500
**Session:** [8808d5e5](8808d5e5-0aeb-43c2-9952-38a8a1726431.jsonl)
**What:** Raised file size limit from ~300 to 1500 lines.
**Why:** After analyzing agent code at L16-L18 wall levels, found that 300-line limits forced premature splitting that created more confusion than clarity. AI agents benefit from locality — keeping related code together.

### Two-Pass Quality-Gate Strategy
**Sessions:** [5a1d1f38](5a1d1f38-7379-40f3-a3c2-f480f657135f.jsonl), [8808d5e5](8808d5e5-0aeb-43c2-9952-38a8a1726431.jsonl)
**What:** Split QG levels into two agent invocations: (1) coding pass with default.md — agent codes freely, (2) cleanup pass with quality-gate.md — agent refactors to production quality.
**Why:** Early QG experiments showed agents spending all their tokens on style compliance instead of making tests pass. Separation of concerns: first make it work, then make it beautiful. The strategy swap happens via symlink update between passes.

### Surprise Levels L27-L28
**Session:** [5a1d1f38](5a1d1f38-7379-40f3-a3c2-f480f657135f.jsonl)
**What:** Designed hidden levels injected after L26 passes. L27 (step-limited eval) punishes recursive eval — must add a step counter. L28 (concurrency + perf stress) punishes shared mutable state — must be thread-safe.
**Why:** These test accumulated tech debt. Agents that took shortcuts (deep recursion, global mutable state) in earlier levels will fail. Content lives in bench/hidden/ — invisible to agents until triggered.

### Anti-Cheating: Progressive Spec Revelation
**Session:** [8808d5e5](8808d5e5-0aeb-43c2-9952-38a8a1726431.jsonl)
**What:** Discovered agents were pre-reading future levels in SPEC.md and optimizing for them (e.g., adding Rc<RefCell> in L01 because they saw L17 would need it). Designed progressive spec revelation — agents only see levels they've reached.
**Why:** Pre-reading defeats the purpose of the benchmark. The whole point is testing how agents handle architectural surprises. If they peek ahead, they're not demonstrating adaptability — they're just following a roadmap.

### L16-L18 Difficulty Wall
**Sessions:** [d2f6719d](d2f6719d-a833-4e67-bf53-611e63b23b80.jsonl), [8808d5e5](8808d5e5-0aeb-43c2-9952-38a8a1726431.jsonl)
**What:** Reordered levels so L16 (TCO), L17 (pair mutation via Rc<RefCell>), and L18 (call/cc via CEK machine) form a consecutive "difficulty wall."
**Why:** Earlier levels are additive (add a feature, tests pass). These three force architectural rewrites against a ~2500 line codebase. Agents that built clean, modular code in L01-L15 survive; agents with accumulated tech debt stall. This creates differentiation — the wall separates good agents from great ones.

### MING: Ming Interpreter Nurture Gauntlet
**Sessions:** [c0952486](c0952486-486d-417f-aac5-67701af16ff3.jsonl), [5a1d1f38](5a1d1f38-7379-40f3-a3c2-f480f657135f.jsonl)
**What:** Named the project MING. Updated all documentation with branding.
**Why:** Needed a memorable project name. MING works as a recursive acronym (Ming Interpreter Nurture Gauntlet) and evokes the Ming dynasty — building something lasting through disciplined craft.

---

## Phase 3: Analysis Tooling & Strategy Tuning (Mar 19-21)

### Quality Gate Limits Calibration
**Session:** [31b2dae2](31b2dae2-ac5a-45ef-975b-f3a4845abcf4.jsonl)
**What:** Discussed how many lines/tokens are actually "too long" for Claude. Raised function limit to ~150 lines, file limit to ~300 lines.
**Why:** Original limits (50 lines/function) were set for human readability. AI agents with 1M context can handle much longer functions — the limits should test architectural quality, not punish normal code.

### Auto-Retry & Turn Limits
**Session:** [197d481a](197d481a-20e7-42f3-bc4f-947a2416f6c3.jsonl)
**What:** Added auto-retry (up to 2x) for infrastructure failures (timeout/529/crash). Added tiered turn limits: L01-L09→40, L10+→60. Migrated QG clippy from agent-side #![deny] to orchestrator-side --gate flag.
**Why:** Infrastructure failures (Anthropic 529s, container timeouts) were polluting results. Turn limits prevent infinite loops. Moving clippy enforcement to orchestrator means agents don't waste tokens fighting lint errors during coding — they only face clippy in the cleanup pass.

### Compliance & Compare Skills
**Sessions:** [197d481a](197d481a-20e7-42f3-bc4f-947a2416f6c3.jsonl), [d2f6719d](d2f6719d-a833-4e67-bf53-611e63b23b80.jsonl)
**What:** Built /compliance skill (analyzes whether agent followed its strategy rules from session JSONL), /compare skill (side-by-side run narratives), and session-turns xtask command (per-level turns/time/tokens/test runs).
**Why:** Needed systematic ways to understand WHY agents succeed or fail, not just whether they pass tests. Compliance answers "did the agent follow its rules?" Compare answers "how did two agents differ?"

### CLAUDE.md Iterative Refinement
**Sessions:** [3c7b9376](3c7b9376-839a-4716-860a-e402fc35fdad.jsonl), [662fe79e](662fe79e-0e1f-4714-b785-422e849b3b78.jsonl), [02936753](02936753-fb1c-4790-9697-746571df7f3f.jsonl), [11770d30](11770d30-ea52-4a25-b553-5b7b6c485f7f.jsonl), [3cb692fc](3cb692fc-a80e-4660-bbd1-5d8024dd7aa6.jsonl), [cee71130](cee71130-f75d-4f77-9f4c-18a5b53abd8a.jsonl)
**What:** Multiple rounds of Claude self-reviewing the agent CLAUDE.md for ambiguities. Found ~25 issues total: contradictory line limits, undefined thresholds, void/nil semantics gaps, immutable-first tension with set!/call/cc.
**Why:** If the rules are ambiguous, the agent can't follow them. Each ambiguity is a potential scoring artifact rather than a real quality signal.
**Key fix:** Added strict thiserror rule requiring structurally typed variants with domain-specific fields — prohibiting String-wrapping variants.

### Token Billing & Cost Analysis
**Session:** [9eb1b65d](9eb1b65d-9a4c-473f-90ca-5841961d93f3.jsonl)
**What:** Built xtask tokens command, added OpenAI pricing model alongside Anthropic. Separated framework CLAUDE.md from agent-visible bench/CLAUDE.md.
**Why:** Needed cost visibility to compare Claude vs Codex economics. Also realized agents were seeing framework documentation — needed isolation.
**Key result:** Created symlink-based strategy system where CLAUDE.md/AGENTS.md are symlinked per-run to the active strategy file.

---

## Phase 2: Framework Bootstrap (Mar 18)

### Agent Performance Diagnosis: L16 is the Wall
**Session:** [f89469f2](f89469f2-eb68-4276-8701-62cc4039a89a.jsonl)
**What:** Discovered L16 (TCO) consumed 43 minutes — 3x more than any other level. Removed full-test-run default, removed clippy from CLAUDE.md.
**Why:** L16 requires retrofitting tail-call optimization into an existing tree-walking evaluator — the first level that forces architectural change rather than additive features. This insight later drove the L16-L18 "difficulty wall" design.

### Shell Script Orchestration
**Session:** [f0e44004](f0e44004-0b00-456a-84ec-b3f6cbbda88c.jsonl)
**What:** Created run-agent.sh, test-level.sh, list-results.sh, setup.sh. Launched first 4-agent parallel bench (Claude + Codex x base + strategy).
**Why:** Needed scriptable automation for repeatable benchmark runs.

### Quality-Gate Strategy: FP Rules for Rust
**Sessions:** [2d132d3e](2d132d3e-5ca1-4946-822f-bba48e413e3f.jsonl), [a48cc1f5](a48cc1f5-5dec-47b6-8e57-f7eb2e816bf8.jsonl)
**What:** Translated functional programming rules from two existing CLAUDE.md projects into a Rust-focused quality-gate strategy. Created default.md (code freely) vs quality-gate.md (strict rules: immutable-first, iterator pipelines, helper extraction at 5 ops, structural recursion).
**Why:** Wanted to test whether strict code quality rules help or hurt agent performance. Hypothesis: QG agents produce cleaner code but spend more tokens thinking about style.

### xtask CLI & Container Isolation
**Session:** [2d132d3e](2d132d3e-5ca1-4946-822f-bba48e413e3f.jsonl)
**What:** Built the entire orchestration infrastructure in one session: xtask CLI (test/run-agent/tokens/analyze), worktree-based agent isolation, Dockerfile.bench for containerized test execution, level-by-level progression with BENCH_LEVEL env var.
**Why:** Agents needed sandboxed environments so they couldn't affect each other or the host. Level-by-level progression (vs all-at-once) was needed to test incremental development capability, not just final output.
**Key decisions:**
- Tests NEVER run on host — always containerized via podman
- Each agent gets a git worktree (isolated copy of repo)
- Agent cwd set to `bench/rust/` so agents only see their playground
- Regression checking after each level pass

---

## Phase 1: Conception & First Benchmarks (Mar 17)

### First Agent Runs: Monolithic vs Multi-File
**Sessions:** [6433449f](6433449f-301c-4a1b-b2b6-4a7095f9fc52.jsonl), [c66f28ae](c66f28ae-2c88-4d39-a267-a2cf1d41fe18.jsonl), [254341ae](254341ae-ee19-4361-b148-c02191a93136.jsonl), [378952f4](378952f4-4fee-4d81-ac61-3446b60e635a.jsonl)
**What:** Ran early benchmark rounds — some agents built monolithic mod.rs (passed all tests in one shot), others split into types/parser/eval modules (ran out of tokens).
**Why:** Needed to calibrate test difficulty and understand agent behavior patterns.
**Observation:** Monolithic approach was faster for small codebases but would create tech debt at scale — this insight later drove the quality-gate strategy design.

### The Idea: Scheme Interpreter as Agent Benchmark
**Session:** [94db3ae3](94db3ae3-cf37-42f4-9c58-2f40fdabff2e.jsonl)
**What:** Conceived a coding agent benchmark where agents build a Scheme interpreter from scratch, level by level — inspired by CS61A/SICP.
**Why:** Existing benchmarks (SWE-bench, HumanEval) test bug fixes or isolated puzzles. Nothing tested progressive greenfield architecture — building a complex system from zero, where early decisions compound. A Scheme interpreter hits the sweet spot: well-specified semantics, escalating complexity, and architectural decisions that come back to haunt you.
**Result:** Initial skeleton with Cargo.toml, 92 test cases across L01-L16, guile-3.0 as ground truth.

---

## Design Decisions by Area

### Level Design
| Decision | Why | Session |
|----------|-----|---------|
| Progressive spec revelation | Prevents agents pre-reading future levels (cheating) | [8808d5e5](8808d5e5-0aeb-43c2-9952-38a8a1726431.jsonl) |
| Surprise levels L27-L28 (hidden) | Tests accumulated tech debt; invisible to agents | [5a1d1f38](5a1d1f38-7379-40f3-a3c2-f480f657135f.jsonl) |
| L16-L18 difficulty wall (TCO/pairs/call-cc) | Forces architectural rewrites; separates good from great | [d2f6719d](d2f6719d-a833-4e67-bf53-611e63b23b80.jsonl) |
| Realworld Scheme fixtures (alexpander, RBT) | Tests real-world robustness beyond unit tests | [d2f6719d](d2f6719d-a833-4e67-bf53-611e63b23b80.jsonl) |
| Level-by-level progression (not all-at-once) | Tests incremental development, not just final output | [2d132d3e](2d132d3e-5ca1-4946-822f-bba48e413e3f.jsonl) |

### Strategy System
| Decision | Why | Session |
|----------|-----|---------|
| Two-pass QG (code then cleanup) | Agents wasted tokens on style during coding | [5a1d1f38](5a1d1f38-7379-40f3-a3c2-f480f657135f.jsonl) |
| File size limit 1500 lines (not 300) | AI agents benefit from locality; premature splitting hurts | [8808d5e5](8808d5e5-0aeb-43c2-9952-38a8a1726431.jsonl) |
| Function limit ~150 lines (not 50) | Original limits for humans, not AI with 1M context | [31b2dae2](31b2dae2-ac5a-45ef-975b-f3a4845abcf4.jsonl) |
| Symlink-based strategy swap | Clean mechanism for switching CLAUDE.md between passes | [9eb1b65d](9eb1b65d-9a4c-473f-90ca-5841961d93f3.jsonl) |
| Default vs Quality-Gate split | Tests whether code quality rules help or hurt | [2d132d3e](2d132d3e-5ca1-4946-822f-bba48e413e3f.jsonl) |

### Orchestration
| Decision | Why | Session |
|----------|-----|---------|
| Output token safety caps | Circuit breakers for dead loops/stuck agents | [82900cdb](82900cdb-c52f-4e1f-82b4-e45daabaa01c.jsonl) |
| QG clippy at orchestrator (not agent) | Agents waste tokens fighting lint during coding | [197d481a](197d481a-20e7-42f3-bc4f-947a2416f6c3.jsonl) |
| Auto-retry for infra failures (2x) | 529s/timeouts polluted results | [197d481a](197d481a-20e7-42f3-bc4f-947a2416f6c3.jsonl) |
| Regression checking after each level | Catch when new features break old tests | [2d132d3e](2d132d3e-5ca1-4946-822f-bba48e413e3f.jsonl) |
| Git worktree per agent | Parallel benchmark runs without interference | [2d132d3e](2d132d3e-5ca1-4946-822f-bba48e413e3f.jsonl) |
| Containerized tests (never host) | Sandbox isolation for agent safety | [2d132d3e](2d132d3e-5ca1-4946-822f-bba48e413e3f.jsonl) |

### Analysis Tooling
| Tool | Purpose | Session |
|------|---------|---------|
| `/compare` skill | Narrative run comparison with struggle analysis | [d2f6719d](d2f6719d-a833-4e67-bf53-611e63b23b80.jsonl) |
| `/compliance` skill | Agent rule adherence analysis | [197d481a](197d481a-20e7-42f3-bc4f-947a2416f6c3.jsonl) |
| `cargo xtask verify` | Ground-truth verification via Chez Scheme | [d2f6719d](d2f6719d-a833-4e67-bf53-611e63b23b80.jsonl) |
| `cargo xtask compare` | Side-by-side run comparison | [197d481a](197d481a-20e7-42f3-bc4f-947a2416f6c3.jsonl) |
| `cargo xtask session-turns` | Per-level turns/time/tokens/test runs | [197d481a](197d481a-20e7-42f3-bc4f-947a2416f6c3.jsonl) |
| `cargo xtask tokens` | Token billing and cost analysis | [9eb1b65d](9eb1b65d-9a4c-473f-90ca-5841961d93f3.jsonl) |
