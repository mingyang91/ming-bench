use crate::model::{
    discover_runs, fmt_comma, project_results_dir, Error, Result, TokenUsage,
    PRICE_CACHE_READ, PRICE_CACHE_WRITE, PRICE_INPUT, PRICE_OUTPUT,
};
use std::fs;
use std::path::{Path, PathBuf};

pub fn run(runs: Vec<PathBuf>, all: bool) -> Result<()> {
    let run_dirs = resolve_runs(runs, all)?;

    if run_dirs.is_empty() {
        return Err(Error::NoRuns);
    }

    let mut all_totals: Vec<(String, TokenUsage)> = Vec::new();

    for run_dir in &run_dirs {
        let label = run_dir
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| run_dir.display().to_string());
        let total = process_run(run_dir, &label)?;
        all_totals.push((label, total));
    }

    // Comparison table if multiple runs
    if all_totals.len() >= 2 {
        print_comparison(&all_totals);
    }

    Ok(())
}

fn resolve_runs(runs: Vec<PathBuf>, all: bool) -> Result<Vec<PathBuf>> {
    if all {
        let results_dir = project_results_dir();
        let discovered = discover_runs(&results_dir)?;
        Ok(discovered.into_iter().map(|(path, _)| path).collect())
    } else if runs.is_empty() {
        Err(Error::NoRuns)
    } else {
        // Verify each exists
        for r in &runs {
            if !r.is_dir() {
                return Err(Error::RunNotFound { path: r.clone() });
            }
        }
        Ok(runs)
    }
}

fn process_run(run_dir: &Path, label: &str) -> Result<TokenUsage> {
    let mut levels = find_level_dirs(run_dir);
    levels.sort();

    let separator = "=".repeat(70);
    println!("\n{separator}");
    println!("  {label}");
    println!("{separator}");
    println!(
        "{:<8} {:>10} {:>10} {:>12} {:>12} {}",
        "Level", "Input", "Output", "Cache Write", "Cache Read", "Status"
    );
    println!(
        "{} {} {} {} {} {}",
        "-".repeat(8),
        "-".repeat(10),
        "-".repeat(10),
        "-".repeat(12),
        "-".repeat(12),
        "-".repeat(10)
    );

    let mut grand_total = TokenUsage::default();

    for level_dir in &levels {
        let level_name = level_dir
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_default();

        let session_file = level_dir.join("session.jsonl");
        let status = read_status(level_dir);

        match parse_session(&session_file) {
            Some(tokens) => {
                grand_total.add(&tokens);
                println!(
                    "{:<8} {:>10} {:>10} {:>12} {:>12} {}",
                    level_name,
                    fmt_comma(tokens.input_tokens),
                    fmt_comma(tokens.output_tokens),
                    fmt_comma(tokens.cache_creation_input_tokens),
                    fmt_comma(tokens.cache_read_input_tokens),
                    status,
                );
            }
            None => {
                println!(
                    "{:<8} {:>10} {:>10} {:>12} {:>12} {}",
                    level_name, "—", "—", "—", "—", status,
                );
            }
        }
    }

    // Totals
    println!(
        "{} {} {} {} {}",
        "-".repeat(8),
        "-".repeat(10),
        "-".repeat(10),
        "-".repeat(12),
        "-".repeat(12)
    );
    println!(
        "{:<8} {:>10} {:>10} {:>12} {:>12}",
        "TOTAL",
        fmt_comma(grand_total.input_tokens),
        fmt_comma(grand_total.output_tokens),
        fmt_comma(grand_total.cache_creation_input_tokens),
        fmt_comma(grand_total.cache_read_input_tokens),
    );

    // Cost breakdown
    let cost_input = grand_total.input_tokens as f64 * PRICE_INPUT;
    let cost_output = grand_total.output_tokens as f64 * PRICE_OUTPUT;
    let cost_cache_w = grand_total.cache_creation_input_tokens as f64 * PRICE_CACHE_WRITE;
    let cost_cache_r = grand_total.cache_read_input_tokens as f64 * PRICE_CACHE_READ;
    let total_cost = cost_input + cost_output + cost_cache_w + cost_cache_r;

    println!();
    println!("  Total tokens: {}", fmt_comma(grand_total.total()));
    println!("  Estimated cost: ${total_cost:.2}");
    println!(
        "    Input: ${cost_input:.2} | Output: ${cost_output:.2} | Cache Write: ${cost_cache_w:.2} | Cache Read: ${cost_cache_r:.2}"
    );

    Ok(grand_total)
}

fn print_comparison(all_totals: &[(String, TokenUsage)]) {
    let separator = "=".repeat(70);
    println!("\n{separator}");
    println!("  COMPARISON");
    println!("{separator}");

    // Header: Metric, then each run label
    print!("{:<25}", "Metric");
    for (label, _) in all_totals {
        // Truncate label to 15 chars
        let short: String = label.chars().take(15).collect();
        print!(" {:>15}", short);
    }
    println!();

    print!("{}", "-".repeat(25));
    for _ in all_totals {
        print!(" {}", "-".repeat(15));
    }
    println!();

    let fields: &[(&str, fn(&TokenUsage) -> u64)] = &[
        ("Input Tokens", |t| t.input_tokens),
        ("Output Tokens", |t| t.output_tokens),
        ("Cache Write", |t| t.cache_creation_input_tokens),
        ("Cache Read", |t| t.cache_read_input_tokens),
        ("Total", |t| t.total()),
    ];

    for (name, extract) in fields {
        print!("{:<25}", name);
        for (_, tokens) in all_totals {
            print!(" {:>15}", fmt_comma(extract(tokens)));
        }
        println!();
    }

    // Cost row
    print!("{:<25}", "Est. Cost");
    for (_, tokens) in all_totals {
        print!(" {:>14}$", format!("{:.2}", tokens.cost()));
    }
    println!();
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn find_level_dirs(run_dir: &Path) -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    let entries = match fs::read_dir(run_dir) {
        Ok(e) => e,
        Err(_) => return dirs,
    };
    for entry in entries.flatten() {
        let name = entry.file_name();
        let name_str = name.to_string_lossy();
        if name_str.starts_with('L') && entry.path().is_dir() {
            dirs.push(entry.path());
        }
    }
    dirs
}

fn read_status(level_dir: &Path) -> String {
    let status_file = level_dir.join("status.txt");
    fs::read_to_string(status_file)
        .map(|s| s.trim().to_string())
        .unwrap_or_else(|_| "running...".to_string())
}

fn parse_session(jsonl_path: &Path) -> Option<TokenUsage> {
    let content = fs::read_to_string(jsonl_path).ok()?;
    let mut totals = TokenUsage::default();
    let mut found = false;

    for line in content.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let obj: serde_json::Value = match serde_json::from_str(line) {
            Ok(v) => v,
            Err(_) => continue,
        };

        let usage = obj
            .get("message")
            .and_then(|m| m.as_object())
            .and_then(|m| m.get("usage"))
            .and_then(|u| u.as_object());

        if let Some(usage) = usage {
            found = true;
            totals.input_tokens += usage
                .get("input_tokens")
                .and_then(|v| v.as_u64())
                .unwrap_or(0);
            totals.output_tokens += usage
                .get("output_tokens")
                .and_then(|v| v.as_u64())
                .unwrap_or(0);
            totals.cache_creation_input_tokens += usage
                .get("cache_creation_input_tokens")
                .and_then(|v| v.as_u64())
                .unwrap_or(0);
            totals.cache_read_input_tokens += usage
                .get("cache_read_input_tokens")
                .and_then(|v| v.as_u64())
                .unwrap_or(0);
        }
    }

    if found {
        Some(totals)
    } else {
        None
    }
}
