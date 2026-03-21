use crate::model::{
    self, discover_runs, elapsed_secs, fmt_duration, project_results_dir, score_display, Result,
};
use std::fs;

pub fn run(json: bool) -> Result<()> {
    let results_dir = project_results_dir();
    let runs = discover_runs(&results_dir)?;

    if runs.is_empty() {
        println!("No results found.");
        return Ok(());
    }

    if json {
        print_json(&runs)?;
    } else {
        print_table(&runs);
    }
    Ok(())
}

fn print_table(runs: &[(std::path::PathBuf, model::MetaJson)]) {
    println!(
        "{:<40} {:<6} {:<10} {:<8} {:<10} {:<8} MODE",
        "RUN", "LANG", "BASE", "AGENT", "SCORE", "DURATION"
    );
    println!(
        "{:<40} {:<6} {:<10} {:<8} {:<10} {:<8} ----",
        "---", "----", "----", "-----", "-----", "--------"
    );

    for (run_dir, meta) in runs {
        let run_name = run_dir
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_default();

        let lang = meta.lang.as_deref().unwrap_or("rust");
        let base = meta.base.as_deref().unwrap_or("?");
        let agent = meta.agent.as_deref().unwrap_or("?");
        let mode = meta.mode.as_deref().unwrap_or("?");

        let mut score = score_display(meta);
        // Fallback: grep bench.log for Score: N/M
        if score == "?" || score == "null" {
            score = bench_log_score(run_dir).unwrap_or_else(|| "?".to_string());
        }

        let duration = elapsed_secs(meta)
            .map(fmt_duration)
            .unwrap_or_else(|| "?".to_string());

        println!("{run_name:<40} {lang:<6} {base:<10} {agent:<8} {score:<10} {duration:<8} {mode}");
    }
}

fn print_json(runs: &[(std::path::PathBuf, model::MetaJson)]) -> Result<()> {
    let entries: Vec<serde_json::Value> = runs
        .iter()
        .map(|(run_dir, meta)| {
            let run_name = run_dir
                .file_name()
                .map(|n| n.to_string_lossy().into_owned())
                .unwrap_or_default();

            let mut score = score_display(meta);
            if score == "?" || score == "null" {
                score = bench_log_score(run_dir).unwrap_or_else(|| "?".to_string());
            }

            let duration = elapsed_secs(meta);

            serde_json::json!({
                "run": run_name,
                "lang": meta.lang.as_deref().unwrap_or("rust"),
                "base": meta.base,
                "agent": meta.agent,
                "score": score,
                "duration_secs": duration,
                "mode": meta.mode,
            })
        })
        .collect();

    println!(
        "{}",
        serde_json::to_string_pretty(&entries).expect("json serialization failed")
    );
    Ok(())
}

/// Try to extract "Score: N/M" from bench.log in the run directory.
fn bench_log_score(run_dir: &std::path::Path) -> Option<String> {
    let log_path = run_dir.join("bench.log");
    let content = fs::read_to_string(log_path).ok()?;
    content.lines().find_map(|line| {
        let pos = line.find("Score: ")?;
        let rest = &line[pos + 7..];
        let score: String = rest
            .chars()
            .take_while(|c| c.is_ascii_digit() || *c == '/')
            .collect();
        if score.is_empty() {
            None
        } else {
            Some(score)
        }
    })
}
