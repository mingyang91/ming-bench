#![deny(clippy::unwrap_used)]
#![deny(clippy::result_unit_err)]
#![deny(clippy::manual_assert)]
#![warn(clippy::too_many_lines)]
#![warn(clippy::excessive_nesting)]
#![warn(clippy::manual_filter_map)]
#![warn(clippy::manual_find_map)]
#![warn(clippy::manual_flatten)]
#![warn(clippy::manual_try_fold)]
#![warn(clippy::manual_let_else)]
#![warn(clippy::needless_range_loop)]
#![warn(clippy::explicit_counter_loop)]
#![warn(clippy::explicit_iter_loop)]
#![warn(clippy::vec_init_then_push)]
#![warn(clippy::needless_collect)]
#![warn(clippy::uninlined_format_args)]

mod ast_check;
mod cmd;
mod codex;
mod model;
mod session;

use clap::{Parser, Subcommand};
use std::path::PathBuf;
use std::process;

#[derive(Parser)]
#[command(name = "xtask", about = "Benchmark analysis and orchestration tools")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// List benchmark results
    Results {
        /// Output as JSON
        #[arg(long)]
        json: bool,
    },
    /// Analyze token usage and costs
    Tokens {
        /// Run directories to analyze
        runs: Vec<PathBuf>,
        /// Analyze all runs in results/
        #[arg(long)]
        all: bool,
    },
    /// Monitor running agents
    Watch {
        /// Print once and exit
        #[arg(long)]
        once: bool,
        /// Filter to runs matching timestamp
        #[arg(long)]
        ts: Option<String>,
        /// Show all runs (default: only active + last 6h)
        #[arg(long)]
        all: bool,
    },
    /// Install dependencies and build container image
    Setup,
    /// Run tests for a level inside a container
    Test {
        /// Level number (01-23) or "all"
        level: String,
        /// Enable quality gates (clippy, mod.rs size) — off by default
        #[arg(long)]
        gate: bool,
        /// Language: rust (default), java, go, ts, scala
        #[arg(long, default_value = "rust")]
        lang: String,
    },
    /// Run full benchmark scoring pass
    Bench {
        /// Branch to benchmark
        branch: String,
        /// Run ID (default: auto-generated)
        #[arg(long)]
        run_id: Option<String>,
        /// Language: rust (default), java, go, ts, scala
        #[arg(long, default_value = "rust")]
        lang: String,
    },
    /// Run an agent on the benchmark
    RunAgent {
        /// Base branch to fork from
        #[arg(long, default_value = "main")]
        base: String,
        /// Strategy file name (default, quality-gate)
        #[arg(long, default_value = "default")]
        strategy: String,
        /// Unique run name
        #[arg(long)]
        name: String,
        /// Agent prompt
        #[arg(long)]
        prompt: Option<String>,
        /// Model override
        #[arg(long)]
        model: Option<String>,
        /// Agent: claude (default), codex, opencode
        #[arg(long, default_value = "claude")]
        agent: String,
        /// Mode: full (default) or levels
        #[arg(long, default_value = "full")]
        mode: String,
        /// Max turns per session
        #[arg(long)]
        max_turns: Option<u32>,
        /// Skip scoring after agent finishes
        #[arg(long)]
        skip_bench: bool,
        /// Resume a previous run
        #[arg(long)]
        resume: bool,
        /// Start from a specific level (levels mode)
        #[arg(long)]
        from_level: Option<String>,
        /// Clean stale worktree + branch before creating fresh
        #[arg(long)]
        clean: bool,
        /// Language: rust (default), java, go, ts, scala
        #[arg(long, default_value = "rust")]
        lang: String,
    },
    /// Analyze session request patterns and cost drivers
    Analyze {
        /// Run directories to analyze
        runs: Vec<PathBuf>,
        /// Analyze all runs in results/
        #[arg(long)]
        all: bool,
    },
    /// Verify test cases against Guile ground truth
    Verify,
    /// Dump session content (thinking, text, tool calls)
    SessionDump {
        /// Run directory or name
        run: PathBuf,
        /// Show only thinking blocks
        #[arg(long)]
        thinking: bool,
        /// Show only assistant text
        #[arg(long)]
        text: bool,
        /// Show only tool calls
        #[arg(long)]
        tools: bool,
        /// Show only user messages
        #[arg(long)]
        user: bool,
        /// Filter to a specific level (e.g., 05)
        #[arg(long)]
        level: Option<String>,
    },
    /// Search session for keywords across content types
    SessionGrep {
        /// Run directory or name
        run: PathBuf,
        /// Keywords to search for
        keywords: Vec<String>,
        /// Characters of context around each hit
        #[arg(long)]
        context: Option<usize>,
        /// Filter to a specific level
        #[arg(long)]
        level: Option<String>,
    },
    /// List tool calls in chronological order
    SessionTools {
        /// Run directory or name
        run: PathBuf,
        /// Show summary counts only
        #[arg(long)]
        summary: bool,
        /// Filter to a specific level
        #[arg(long)]
        level: Option<String>,
    },
    /// Quick stats overview of a session
    SessionStats {
        /// Run directory or name
        run: PathBuf,
    },
    /// Per-level turn/time/token/test analysis
    SessionTurns {
        /// Run directory or name
        run: PathBuf,
    },
    /// Compare two runs side-by-side
    Compare {
        /// First run directory or name
        run1: PathBuf,
        /// Second run directory or name
        run2: PathBuf,
    },
}

fn main() {
    let cli = Cli::parse();
    let result = dispatch(cli.command);
    if let Err(e) = result {
        eprintln!("error: {e}");
        process::exit(1);
    }
}

fn dispatch(command: Commands) -> model::Result<()> {
    if let Commands::RunAgent {
        base,
        strategy,
        name,
        prompt,
        model,
        agent,
        mode,
        max_turns,
        skip_bench,
        resume,
        from_level,
        clean,
        lang,
    } = command
    {
        return dispatch_run_agent(cmd::run_agent::RunAgentArgs {
            base,
            strategy,
            name,
            prompt,
            model,
            agent,
            mode,
            max_turns,
            skip_bench,
            resume,
            from_level,
            clean,
            lang,
        });
    }

    dispatch_non_agent(command)
}

fn dispatch_non_agent(command: Commands) -> model::Result<()> {
    match command {
        Commands::Results { json } => cmd::results::run(json),
        Commands::Tokens { runs, all } => cmd::tokens::run(runs, all),
        Commands::Watch { once, ts, all } => cmd::watch::run(once, ts, all),
        Commands::Setup => cmd::setup::run(),
        Commands::Test {
            ref level,
            gate,
            ref lang,
        } => cmd::test_level::run(level, gate, lang),
        Commands::Bench {
            ref branch,
            ref run_id,
            ref lang,
        } => cmd::bench::run(branch, run_id.as_deref(), lang),
        Commands::Analyze { runs, all } => cmd::analyze::run(runs, all),
        Commands::Verify => cmd::verify::run(),
        other => dispatch_session_command(other),
    }
}

fn dispatch_session_command(command: Commands) -> model::Result<()> {
    match command {
        Commands::SessionDump {
            run,
            thinking,
            text,
            tools,
            user,
            level,
        } => cmd::session_dump::run(run, thinking, text, tools, user, level),
        Commands::SessionGrep {
            run,
            keywords,
            context,
            level,
        } => cmd::session_grep::run(run, keywords, context, level),
        Commands::SessionTools {
            run,
            summary,
            level,
        } => cmd::session_tools::run(run, summary, level),
        Commands::SessionStats { run } => cmd::session_stats::run(run),
        Commands::SessionTurns { run } => cmd::session_turns::run(run),
        Commands::Compare { run1, run2 } => cmd::compare::run(run1, run2),
        Commands::RunAgent { .. } => unreachable!("run-agent handled earlier"),
        Commands::Results { .. }
        | Commands::Tokens { .. }
        | Commands::Watch { .. }
        | Commands::Setup
        | Commands::Test { .. }
        | Commands::Bench { .. }
        | Commands::Analyze { .. }
        | Commands::Verify => unreachable!("non-session command dispatched elsewhere"),
    }
}

fn dispatch_run_agent(args: cmd::run_agent::RunAgentArgs) -> model::Result<()> {
    cmd::run_agent::run(args)
}
