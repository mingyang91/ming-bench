use std::env;
use std::fs;
use std::process::ExitCode;

fn main() -> ExitCode {
    let mut args = env::args().skip(1);
    let Some(path) = args.next() else {
        eprintln!("usage: eval_fixture <path> [output]");
        return ExitCode::from(2);
    };

    let input = match fs::read_to_string(&path) {
        Ok(input) => input,
        Err(err) => {
            eprintln!("read error for {path}: {err}");
            return ExitCode::from(1);
        }
    };

    if matches!(args.next().as_deref(), Some("output")) {
        match ming::scheme::eval_str_with_output(input.trim()) {
            Ok((result, output)) => {
                println!("RESULT={result}");
                print!("OUTPUT={output}");
                ExitCode::SUCCESS
            }
            Err(err) => {
                eprintln!("ERR={err}");
                ExitCode::from(1)
            }
        }
    } else {
        match ming::scheme::eval_str(input.trim()) {
            Ok(result) => {
                println!("{result}");
                ExitCode::SUCCESS
            }
            Err(err) => {
                eprintln!("ERR={err}");
                ExitCode::from(1)
            }
        }
    }
}
