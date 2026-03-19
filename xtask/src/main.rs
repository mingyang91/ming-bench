#![allow(clippy::excessive_nesting)]

mod cmd;
mod model;

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
        /// Level number (01-16) or "all"
        level: String,
    },
    /// Run full benchmark scoring pass
    Bench {
        /// Branch to benchmark
        branch: String,
        /// Run ID (default: auto-generated)
        #[arg(long)]
        run_id: Option<String>,
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
}

fn main() {
    let cli = Cli::parse();
    let result = match cli.command {
        Commands::Results { json } => cmd::results::run(json),
        Commands::Tokens { runs, all } => cmd::tokens::run(runs, all),
        Commands::Watch { once, ts, all } => cmd::watch::run(once, ts, all),
        Commands::Setup => cmd::setup::run(),
        Commands::Test { ref level } => cmd::test_level::run(level),
        Commands::Bench {
            ref branch,
            ref run_id,
        } => cmd::bench::run(branch, run_id.as_deref()),
        Commands::RunAgent {
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
        } => cmd::run_agent::run(cmd::run_agent::RunAgentArgs {
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
        }),
        Commands::Analyze { runs, all } => cmd::analyze::run(runs, all),
        Commands::Verify => cmd::verify::run(),
    };
    if let Err(e) = result {
        eprintln!("error: {e}");
        process::exit(1);
    }
}
