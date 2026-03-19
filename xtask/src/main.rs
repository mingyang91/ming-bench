mod cmd;
mod model;

use clap::{Parser, Subcommand};
use std::path::PathBuf;
use std::process;

#[derive(Parser)]
#[command(name = "xtask", about = "Benchmark analysis tools")]
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
}

fn main() {
    let cli = Cli::parse();
    let result = match cli.command {
        Commands::Results { json } => cmd::results::run(json),
        Commands::Tokens { runs, all } => cmd::tokens::run(runs, all),
        Commands::Watch { once, ts, all } => cmd::watch::run(once, ts, all),
    };
    if let Err(e) = result {
        eprintln!("error: {e}");
        process::exit(1);
    }
}
