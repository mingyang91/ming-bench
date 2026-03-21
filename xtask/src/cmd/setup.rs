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
        )?;
        if exit != 0 {
            return Err(crate::model::Error::CommandFailed {
                cmd: format!("podman build {name}"),
                exit_code: exit,
            });
        }
        println!("Image '{name}' built successfully.");
    }

    println!("=== Setup complete ===");
    println!("Run benchmarks with: cargo xtask bench <branch>");

    Ok(())
}
