use crate::model::{command_exists, project_dir, run_cmd, Result};
use std::path::Path;

pub fn run() -> Result<()> {
    let proj = project_dir();
    let mut failures = Vec::new();

    println!("=== MING Setup ===");
    install_missing_packages(&proj, &mut failures);
    let _ = run_cmd("podman", &["--version"], &proj);
    build_container_images(&proj, &mut failures);
    print_summary(&failures);
    Ok(())
}

fn install_missing_packages(proj: &Path, failures: &mut Vec<String>) {
    let mut package_index_updated = false;
    for pkg in REQUIRED_PACKAGES
        .iter()
        .copied()
        .filter(|pkg| !command_exists(pkg))
    {
        update_package_index(proj, &mut package_index_updated);
        install_package(proj, pkg, failures);
    }
}

fn update_package_index(proj: &Path, package_index_updated: &mut bool) {
    if *package_index_updated {
        return;
    }
    println!("Updating package index...");
    let _ = run_cmd("sudo", &["apt-get", "update", "-qq"], proj);
    *package_index_updated = true;
}

fn install_package(proj: &Path, pkg: &str, failures: &mut Vec<String>) {
    println!("Installing {pkg}...");
    let exit = run_cmd("sudo", &["apt-get", "install", "-y", "-qq", pkg], proj).unwrap_or(1);
    if exit == 0 {
        return;
    }
    eprintln!("WARNING: Failed to install {pkg} (exit {exit})");
    failures.push(format!("apt install {pkg}"));
}

fn build_container_images(proj: &Path, failures: &mut Vec<String>) {
    for (name, dockerfile) in IMAGES {
        build_container_image(proj, name, dockerfile, failures);
    }
}

fn build_container_image(proj: &Path, name: &str, dockerfile: &str, failures: &mut Vec<String>) {
    let dockerfile_path = proj.join(dockerfile);
    if !dockerfile_path.is_file() {
        println!("Skipping {name} (no {dockerfile})");
        return;
    }

    println!("Building container image '{name}' from {dockerfile}...");
    let exit = run_cmd(
        "sudo",
        &["podman", "build", "-t", name, "-f", dockerfile, "."],
        proj,
    )
    .unwrap_or(1);
    if exit != 0 {
        eprintln!("WARNING: Failed to build image '{name}' (exit {exit})");
        failures.push(format!("podman build {name}"));
        return;
    }
    println!("Image '{name}' built successfully.");
}

fn print_summary(failures: &[String]) {
    if failures.is_empty() {
        println!("\n=== Setup complete ===");
    } else {
        eprintln!("\n=== Setup completed with errors ===");
        for f in failures {
            eprintln!("  FAILED: {f}");
        }
    }
}

const REQUIRED_PACKAGES: &[&str] = &["podman", "jq", "openjdk-21-jdk-headless"];
const IMAGES: &[(&str, &str)] = &[
    ("ming", "Dockerfile.bench"),
    ("ming-jvm", "Dockerfile.jvm"),
    ("ming-node", "Dockerfile.node"),
];
