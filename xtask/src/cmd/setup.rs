use crate::model::{command_exists, project_dir, run_cmd, Result};

pub fn run() -> Result<()> {
    let proj = project_dir();

    println!("=== MING Setup ===");

    // 1. Install podman + jq if missing
    for pkg in &["podman", "jq"] {
        if !command_exists(pkg) {
            println!("Installing {pkg}...");
            run_cmd("sudo", &["apt-get", "update", "-qq"], &proj)?;
            run_cmd("sudo", &["apt-get", "install", "-y", "-qq", pkg], &proj)?;
        }
    }

    // Show podman version
    let _ = run_cmd("podman", &["--version"], &proj);

    // 2. Build bench container image
    println!("Building bench container image 'ming'...");
    let exit = run_cmd(
        "sudo",
        &[
            "podman",
            "build",
            "-t",
            "ming",
            "-f",
            "Dockerfile.bench",
            ".",
        ],
        &proj,
    )?;

    if exit != 0 {
        return Err(crate::model::Error::CommandFailed {
            cmd: "podman build".to_string(),
            exit_code: exit,
        });
    }

    println!("Image 'ming' built successfully.");
    println!("=== Setup complete ===");
    println!("Run benchmarks with: cargo xtask bench <branch>");

    Ok(())
}
