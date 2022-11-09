use xshell::{cmd, Shell};

fn main() -> anyhow::Result<()> {
    // let sh = Shell::new()?;
    // for path in sh.read_dir(".")? {
    //     if path.to_str().unwrap_or_default().ends_with(".adoc") {
    //         let contents = sh.read_file(&path)?;
    //         sh.remove_path(&path)?;
    //         sh.write_file(path.with_extension("djot"), &contents)?
    //     }
    // }
    std::process::Command::new("cargo")
        .arg("build")
    Ok(())
}
