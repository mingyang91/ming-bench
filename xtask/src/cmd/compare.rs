use crate::cmd::session_turns::{analyze_run, LevelAnalysis, RunAnalysis};
use crate::model::{self, fmt_comma, fmt_duration, Color, Result};
use crate::session;
use std::path::PathBuf;

// ---------------------------------------------------------------------------
// CLI entry point
// ---------------------------------------------------------------------------

pub fn run(run1: PathBuf, run2: PathBuf) -> Result<()> {
    let dir1 = session::resolve_run(&run1)?;
    let dir2 = session::resolve_run(&run2)?;
    let a = analyze_run(&dir1)?;
    let b = analyze_run(&dir2)?;

    println!(
        "{}Comparing:{} {} ({}) vs {} ({})",
        Color::BOLD,
        Color::RESET,
        a.short_name,
        a.strategy,
        b.short_name,
        b.strategy
    );
    println!();
    println!(
        "{}A = {}  B = {}{}",
        Color::DIM,
        a.short_name,
        b.short_name,
        Color::RESET
    );
    println!();

    let level_labels = union_levels(&a, &b);
    let (tot_a, tot_b) = print_comparison_table(&a, &b, &level_labels);
    print_comparison_summary(&a, &b, &level_labels, &tot_a, &tot_b);

    Ok(())
}

fn print_comparison_table(
    a: &RunAnalysis,
    b: &RunAnalysis,
    level_labels: &[String],
) -> (Totals, Totals) {
    print_comparison_header();

    let mut tot_a = Totals::default();
    let mut tot_b = Totals::default();
    print_level_rows(a, b, level_labels, &mut tot_a, &mut tot_b);
    print_totals_row(&tot_a, &tot_b);

    (tot_a, tot_b)
}

fn print_comparison_summary(
    a: &RunAnalysis,
    b: &RunAnalysis,
    level_labels: &[String],
    _tot_a: &Totals,
    _tot_b: &Totals,
) {
    println!();
    println!(
        "{}COST:{}  A ${:.2} ({})  vs  B ${:.2} ({})",
        Color::BOLD,
        Color::RESET,
        a.cost,
        model::fmt_tokens(a.total_tokens),
        b.cost,
        model::fmt_tokens(b.total_tokens),
    );
    println!(
        "Levels: A {}/{}  vs  B {}/{}",
        a.levels.len(),
        level_labels.len(),
        b.levels.len(),
        level_labels.len()
    );
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn union_levels(a: &RunAnalysis, b: &RunAnalysis) -> Vec<String> {
    let mut labels = Vec::new();

    // Check for "full" mode
    let has_full_a = a.levels.iter().any(|l| l.label == "full");
    let has_full_b = b.levels.iter().any(|l| l.label == "full");
    if has_full_a || has_full_b {
        labels.push("full".to_string());
    }

    // Standard levels
    for level in model::LEVELS {
        let label = format!("L{level}");
        let in_a = a.levels.iter().any(|l| l.label == label);
        let in_b = b.levels.iter().any(|l| l.label == label);
        if in_a || in_b {
            labels.push(label);
        }
    }

    labels
}

fn find_level<'a>(run: &'a RunAnalysis, label: &str) -> Option<&'a LevelAnalysis> {
    run.levels.iter().find(|l| l.label == label)
}

fn fmt_level(level: Option<&LevelAnalysis>) -> (String, String, String, String, String) {
    match level {
        Some(l) => (
            l.turns.to_string(),
            if l.time_secs > 0 {
                fmt_duration(l.time_secs)
            } else {
                "--".into()
            },
            fmt_comma(l.output_tokens),
            l.test_runs.to_string(),
            l.friction.to_string(),
        ),
        None => (
            "--".into(),
            "--".into(),
            "--".into(),
            "--".into(),
            "--".into(),
        ),
    }
}

fn print_comparison_header() {
    println!(
        "{}{:<5} {:>4} {:>4}  {:>6} {:>6}  {:>7} {:>7}  {:>3} {:>3}  {:>3} {:>3}{}",
        Color::BOLD,
        "LEVEL",
        "A",
        "B",
        "A",
        "B",
        "A",
        "B",
        "A",
        "B",
        "A",
        "B",
        Color::RESET
    );
    println!(
        "{}{:<5} {:>4} {:>4}  {:>6} {:>6}  {:>7} {:>7}  {:>3} {:>3}  {:>3} {:>3}{}",
        Color::DIM,
        "",
        "trn",
        "trn",
        "time",
        "time",
        "output",
        "output",
        "tst",
        "tst",
        "fri",
        "fri",
        Color::RESET
    );
}

fn print_level_rows(
    a: &RunAnalysis,
    b: &RunAnalysis,
    level_labels: &[String],
    tot_a: &mut Totals,
    tot_b: &mut Totals,
) {
    for label in level_labels {
        let la = find_level(a, label);
        let lb = find_level(b, label);
        print_level_row(label, la, lb);
        add_level_totals(tot_a, la);
        add_level_totals(tot_b, lb);
    }
}

fn print_level_row(label: &str, la: Option<&LevelAnalysis>, lb: Option<&LevelAnalysis>) {
    let (at, atime, aout, atest, afric) = fmt_level(la);
    let (bt, btime, bout, btest, bfric) = fmt_level(lb);
    println!(
        "{label:<5} {at:>4} {bt:>4}  {atime:>6} {btime:>6}  {aout:>7} {bout:>7}  {atest:>3} {btest:>3}  {afric:>3} {bfric:>3}"
    );
}

fn add_level_totals(totals: &mut Totals, level: Option<&LevelAnalysis>) {
    if let Some(level) = level {
        totals.add(level);
    }
}

fn print_totals_row(tot_a: &Totals, tot_b: &Totals) {
    println!(
        "{}{:<5} {:>4} {:>4}  {:>6} {:>6}  {:>7} {:>7}  {:>3} {:>3}  {:>3} {:>3}{}",
        Color::BOLD,
        "TOTAL",
        tot_a.turns,
        tot_b.turns,
        fmt_duration(tot_a.time),
        fmt_duration(tot_b.time),
        model::fmt_tokens(tot_a.output),
        model::fmt_tokens(tot_b.output),
        tot_a.tests,
        tot_b.tests,
        tot_a.friction,
        tot_b.friction,
        Color::RESET
    );
}

#[derive(Default)]
struct Totals {
    turns: u32,
    time: u64,
    output: u64,
    tests: u32,
    friction: u32,
}

impl Totals {
    fn add(&mut self, l: &LevelAnalysis) {
        self.turns += l.turns;
        self.time += l.time_secs;
        self.output += l.output_tokens;
        self.tests += l.test_runs;
        self.friction += l.friction;
    }
}
