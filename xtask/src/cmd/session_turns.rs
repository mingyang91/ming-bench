use crate::codex;
use crate::model::{
    self, fmt_comma, fmt_duration, parse_iso_epoch, Color, MetaJson, Result, TokenUsage,
};
use crate::session::{self, ContentBlock, EventKind};
use std::fs;
use std::path::{Path, PathBuf};

// ---------------------------------------------------------------------------
// Per-level and per-run analysis types (reused by compare.rs)
// ---------------------------------------------------------------------------

pub struct LevelAnalysis {
    pub label: String,
    pub turns: u32,
    pub turn_limit: u32,
    pub time_secs: u64,
    pub output_tokens: u64,
    pub test_runs: u32,
    pub friction: u32,
}

pub struct RunAnalysis {
    pub name: String,
    pub short_name: String,
    pub strategy: String,
    pub mode: String,
    pub levels: Vec<LevelAnalysis>,
    pub total_tokens: u64,
    pub cost: f64,
}

// ---------------------------------------------------------------------------
// Friction keywords (case-insensitive)
// ---------------------------------------------------------------------------

const FRICTION_KEYWORDS: &[&str] = &["nesting", "clippy", "refactor"];

// ---------------------------------------------------------------------------
// Core analysis
// ---------------------------------------------------------------------------

/// Analyze a single run directory, producing per-level metrics.
pub fn analyze_run(run_dir: &Path) -> Result<RunAnalysis> {
    let meta = read_run_meta(run_dir);
    let name = run_name(run_dir, &meta);
    let short_name = extract_short_name(&name);
    let strategy = meta.strategy.clone().unwrap_or_else(|| "unknown".into());
    let mode = meta.mode.clone().unwrap_or_else(|| "unknown".into());
    let is_codex = meta.agent.as_deref() == Some("codex");

    let mut totals = RunTotals::default();
    let mut levels = Vec::new();
    for (label, path) in session::session_files(run_dir) {
        levels.push(analyze_level(
            run_dir,
            &label,
            &path,
            is_codex,
            &mut totals,
        )?);
    }

    Ok(RunAnalysis {
        name,
        short_name,
        strategy,
        mode,
        levels,
        total_tokens: totals.total_tokens(is_codex),
        cost: totals.cost(is_codex),
    })
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn compute_duration(events: &[session::SessionEvent]) -> u64 {
    let first = events.iter().filter_map(|e| e.timestamp.as_deref()).next();
    let last = events
        .iter()
        .rev()
        .filter_map(|e| e.timestamp.as_deref())
        .next();
    match (
        first.and_then(parse_iso_epoch),
        last.and_then(parse_iso_epoch),
    ) {
        (Some(s), Some(e)) if e > s => (e - s) as u64,
        _ => 0,
    }
}

fn read_run_meta(run_dir: &Path) -> MetaJson {
    let meta_path = run_dir.join("meta.json");
    if !meta_path.is_file() {
        return MetaJson::default();
    }

    let content = fs::read_to_string(&meta_path).unwrap_or_default();
    serde_json::from_str(&content).unwrap_or_default()
}

fn run_name(run_dir: &Path, meta: &MetaJson) -> String {
    meta.name.clone().unwrap_or_else(|| {
        run_dir
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| "unknown".into())
    })
}

fn analyze_level(
    run_dir: &Path,
    label: &str,
    path: &Path,
    is_codex: bool,
    totals: &mut RunTotals,
) -> Result<LevelAnalysis> {
    let events = session::parse_session(path)?;
    let turns = events
        .iter()
        .filter(|e| matches!(e.kind, EventKind::User { .. }))
        .count() as u32;
    let time_secs = compute_duration(&events);
    let output_tokens = level_output_tokens(run_dir, label, path, is_codex, totals);
    let test_runs = count_test_runs(&events);
    let friction = count_friction(&events);
    let turn_limit = parse_level_num(label)
        .map(|n| model::turns_for_level(n, None))
        .unwrap_or(0);

    Ok(LevelAnalysis {
        label: label.to_string(),
        turns,
        turn_limit,
        time_secs,
        output_tokens,
        test_runs,
        friction,
    })
}

fn level_output_tokens(
    run_dir: &Path,
    label: &str,
    path: &Path,
    is_codex: bool,
    totals: &mut RunTotals,
) -> u64 {
    if is_codex {
        return codex_output_tokens(run_dir, label, totals);
    }

    let usage = crate::cmd::tokens::parse_session(path).unwrap_or_default();
    totals.add_claude(&usage);
    usage.output_tokens
}

fn codex_output_tokens(run_dir: &Path, label: &str, totals: &mut RunTotals) -> u64 {
    let level_dir = session_dir(run_dir, label);
    let output_info = codex::parse_output_info(&level_dir.join("agent-output.txt"));
    let fallback_total_tokens = output_info.total_tokens.unwrap_or(0);
    let fallback_model = output_info.model.clone();
    let session_data = codex::load_session(&level_dir, output_info.session_id.as_deref());
    let level_model = session_data
        .as_ref()
        .and_then(|data| data.model.clone())
        .or(fallback_model);

    if let Some(data) = session_data {
        return totals.add_codex(&data.usage, level_model.as_deref());
    }

    totals.add_codex_fallback(fallback_total_tokens);
    0
}

fn count_test_runs(events: &[session::SessionEvent]) -> u32 {
    events
        .iter()
        .filter_map(|e| match &e.kind {
            EventKind::Assistant { blocks } => Some(blocks),
            _ => None,
        })
        .flat_map(|blocks| blocks.iter())
        .filter(|block| {
            matches!(block, ContentBlock::ToolUse { name, input_json }
                if session::shell_command(name, input_json)
                    .is_some_and(|cmd| cmd.contains("xtask test")))
        })
        .count() as u32
}

fn count_friction(events: &[session::SessionEvent]) -> u32 {
    events
        .iter()
        .filter(|event| {
            let EventKind::Assistant { blocks } = &event.kind else {
                return false;
            };
            blocks.iter().any(block_has_friction)
        })
        .count() as u32
}

fn block_has_friction(block: &ContentBlock) -> bool {
    let (ContentBlock::Thinking(text) | ContentBlock::Text(text)) = block else {
        return false;
    };
    let lower = text.to_lowercase();
    FRICTION_KEYWORDS.iter().any(|kw| lower.contains(kw))
}

fn parse_level_num(label: &str) -> Option<u32> {
    label.strip_prefix('L').and_then(|s| s.parse().ok())
}

fn session_dir(run_dir: &Path, label: &str) -> PathBuf {
    if label == "full" {
        run_dir.to_path_buf()
    } else {
        run_dir.join(label)
    }
}

#[derive(Default)]
struct RunTotals {
    claude_usage: TokenUsage,
    codex_total_tokens: u64,
    codex_total_cost: f64,
}

impl RunTotals {
    fn add_claude(&mut self, usage: &TokenUsage) {
        self.claude_usage.add(usage);
    }

    fn add_codex(&mut self, usage: &codex::Usage, model: Option<&str>) -> u64 {
        self.codex_total_tokens += usage.total_tokens();
        self.codex_total_cost += codex_level_cost(usage, model);
        usage.output_tokens
    }

    fn add_codex_fallback(&mut self, total_tokens: u64) {
        self.codex_total_tokens += total_tokens;
    }

    fn total_tokens(&self, is_codex: bool) -> u64 {
        if is_codex {
            self.codex_total_tokens
        } else {
            self.claude_usage.total()
        }
    }

    fn cost(&self, is_codex: bool) -> f64 {
        if is_codex {
            self.codex_total_cost
        } else {
            self.claude_usage.cost()
        }
    }
}

fn codex_level_cost(usage: &codex::Usage, model: Option<&str>) -> f64 {
    model
        .and_then(codex::pricing)
        .map(|pricing| pricing.cost_breakdown(usage).total())
        .unwrap_or(0.0)
}

/// Extract short name from run name.
/// e.g. "default_cl-def-lvl_20260320T032928" → "cl-def-lvl"
fn extract_short_name(name: &str) -> String {
    let parts: Vec<&str> = name.split('_').collect();
    if parts.len() >= 3 {
        // Middle parts between first (strategy) and last (timestamp)
        parts[1..parts.len() - 1].join("_")
    } else {
        name.to_string()
    }
}

// ---------------------------------------------------------------------------
// CLI entry point
// ---------------------------------------------------------------------------

pub fn run(run_arg: PathBuf) -> Result<()> {
    let run_dir = session::resolve_run(&run_arg)?;
    let analysis = analyze_run(&run_dir)?;

    println!(
        "{}Run:{} {} (strategy: {}, mode: {})",
        Color::BOLD,
        Color::RESET,
        analysis.name,
        analysis.strategy,
        analysis.mode
    );
    println!();

    print_level_table(&analysis);

    // Cost summary
    println!();
    println!(
        "Tokens: {} ({})  Cost: ${:.2}",
        model::fmt_tokens(analysis.total_tokens),
        fmt_comma(analysis.total_tokens),
        analysis.cost
    );

    Ok(())
}

fn print_level_table(analysis: &RunAnalysis) {
    println!(
        "{}{:<6} {:>5} {:>5} {:>7} {:>9} {:>5} {:>8}{}",
        Color::BOLD,
        "LEVEL",
        "TURNS",
        "LIMIT",
        "TIME",
        "OUTPUT",
        "TESTS",
        "FRICTION",
        Color::RESET
    );

    let mut total_turns: u32 = 0;
    let mut total_time: u64 = 0;
    let mut total_output: u64 = 0;
    let mut total_tests: u32 = 0;
    let mut total_friction: u32 = 0;

    for level in &analysis.levels {
        let limit_str = if level.turn_limit > 0 {
            level.turn_limit.to_string()
        } else {
            "\u{2014}".into()
        };
        let time_str = if level.time_secs > 0 {
            fmt_duration(level.time_secs)
        } else {
            "--".into()
        };

        println!(
            "{:<6} {:>5} {:>5} {:>7} {:>9} {:>5} {:>8}",
            level.label,
            level.turns,
            limit_str,
            time_str,
            fmt_comma(level.output_tokens),
            level.test_runs,
            level.friction
        );

        total_turns += level.turns;
        total_time += level.time_secs;
        total_output += level.output_tokens;
        total_tests += level.test_runs;
        total_friction += level.friction;
    }

    println!(
        "{}{:<6} {:>5} {:>5} {:>7} {:>9} {:>5} {:>8}{}",
        Color::BOLD,
        "TOTAL",
        total_turns,
        "\u{2014}",
        fmt_duration(total_time),
        fmt_comma(total_output),
        total_tests,
        total_friction,
        Color::RESET
    );
}
