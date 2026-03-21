use crate::model::{command_exists, project_dir, run_cmd, Result};

#[allow(clippy::excessive_nesting)]
pub fn run() -> Result<()> {
    let proj = project_dir();
    let mut failures: Vec<String> = vec![];

    println!("=== MING Setup ===");

    // 1. Install required packages
    let mut need_update = true;
    for pkg in &["podman", "jq", "openjdk-21-jdk-headless"] {
        if !command_exists(pkg) {
            if need_update {
                println!("Updating package index...");
                let _ = run_cmd("sudo", &["apt-get", "update", "-qq"], &proj);
                need_update = false;
            }
            println!("Installing {pkg}...");
            let exit =
                run_cmd("sudo", &["apt-get", "install", "-y", "-qq", pkg], &proj).unwrap_or(1);
            if exit != 0 {
                eprintln!("WARNING: Failed to install {pkg} (exit {exit})");
                failures.push(format!("apt install {pkg}"));
            }
        }
    }

    // Show podman version
    let _ = run_cmd("podman", &["--version"], &proj);

    // 2. Build container images
    let images = [
        ("ming", "Dockerfile.bench"),
        ("ming-jvm", "Dockerfile.jvm"),
        ("ming-node", "Dockerfile.node"),
    ];

    for (name, dockerfile) in &images {
        let dockerfile_path = proj.join(dockerfile);
        if !dockerfile_path.is_file() {
            println!("Skipping {name} (no {dockerfile})");
            continue;
        }
        println!("Building container image '{name}' from {dockerfile}...");
        let exit = run_cmd(
            "sudo",
            &["podman", "build", "-t", name, "-f", dockerfile, "."],
            &proj,
        )
        .unwrap_or(1);
        if exit != 0 {
            eprintln!("WARNING: Failed to build image '{name}' (exit {exit})");
            failures.push(format!("podman build {name}"));
            continue;
        }
        println!("Image '{name}' built successfully.");
    }

    if failures.is_empty() {
        println!("\n=== Setup complete ===");
    } else {
        eprintln!("\n=== Setup completed with errors ===");
        for f in &failures {
            eprintln!("  FAILED: {f}");
        }
    }

    Ok(())
}
