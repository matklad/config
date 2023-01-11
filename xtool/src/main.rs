use std::path::PathBuf;

use xshell::Shell;

mod amend;
mod autopatchelf;
mod autostart;
mod autowatch;
mod commit;
mod gbda;
mod gbors;
mod gcf;
mod git_refresh;
mod git_spinoff;
mod go;
mod gpr;
mod n;
mod new_crate;
mod nix;
mod prf;
mod script;
mod swm;
mod use_nix;

const TOOLS: &[(&str, fn(&Shell) -> anyhow::Result<()>)] = &[
    ("amend", amend::run),
    ("autopatchelf", autopatchelf::run),
    ("autostart", autostart::run),
    ("autowatch", autowatch::run),
    ("commit", commit::run),
    ("gbda", gbda::run),
    ("gbors", gbors::run),
    ("gcf", gcf::run),
    ("git-refresh", git_refresh::run),
    ("git-spinoff", git_spinoff::run),
    ("go", go::run),
    ("gpr", gpr::run),
    ("n", n::run),
    ("new-crate", new_crate::run),
    ("nixgc", nix::gc),
    ("nixup", nix::up),
    ("prf", prf::run),
    ("script", script::run),
    ("swm", swm::run),
    ("use-nix", use_nix::run),
];

fn main() -> anyhow::Result<()> {
    let progn: PathBuf = std::env::args_os().next().unwrap_or_default().into();
    let progn = progn.file_stem().unwrap_or_default().to_str().unwrap_or_default();

    let (_name, run) = TOOLS
        .iter()
        .find(|&&(name, _run)| name == progn)
        .ok_or_else(|| anyhow::format_err!("unknown tool: `{progn}`"))?;
    let sh = Shell::new()?;
    run(&sh)
}

#[test]
fn link_me_up() {
    use xshell::{cmd, Shell};

    let sh = Shell::new().unwrap();
    let bin = std::path::Path::new("/home/matklad/bin");
    sh.create_dir(&bin).unwrap();
    cmd!(sh, "cargo build --release").run().unwrap();

    for &(tool, _) in TOOLS {
        let dst = bin.join(tool);
        sh.remove_path(&dst).unwrap();
        let _ = cmd!(sh, "git rm {dst} -f").ignore_stderr().quiet().run();
        sh.hard_link("./target/release/xtool", &dst).unwrap();
    }

    let home: PathBuf = "/home/matklad/".into();
    let config_home = home.join("config/home");
    for abs_path in walkdir(config_home.clone()).unwrap() {
        let rel_path = abs_path.strip_prefix(&config_home).unwrap();
        let dest = home.join(rel_path);
        sh.remove_path(&dest).unwrap();
        sh.create_dir(dest.parent().unwrap()).unwrap();
        std::os::unix::fs::symlink(abs_path, dest).unwrap();
    }

    fn walkdir(path: PathBuf) -> anyhow::Result<Vec<PathBuf>> {
        let mut res = Vec::new();
        let mut work = vec![path];
        while let Some(dir) = work.pop() {
            for entry in std::fs::read_dir(&dir)? {
                let entry = entry?;
                let file_type = entry.file_type()?;
                if file_type.is_file() {
                    res.push(entry.path())
                } else if file_type.is_dir() {
                    work.push(entry.path());
                }
            }
        }
        Ok(res)
    }
}
