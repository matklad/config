use std::path::PathBuf;

use xshell::Shell;

mod autopatchelf;
mod autostart;
mod autowatch;
mod gbda;
mod gcf;
mod git_spinoff;
mod n;
mod new_crate;
mod nix;
mod prf;
mod script;
mod swm;
mod use_nix;

const TOOLS: &[(&str, fn(&Shell) -> anyhow::Result<()>)] = &[
    ("autopatchelf", autopatchelf::run),
    ("autostart", autostart::run),
    ("autowatch", autowatch::run),
    ("gbda", gbda::run),
    ("gcf", gcf::run),
    ("git-spinoff", git_spinoff::run),
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
#[cfg(nope)]
fn link_me_up() {
    use xshell::{cmd, Shell};

    let sh = Shell::new().unwrap();
    let home: PathBuf = "/home/matklad/".into();
    let config_home = home.join("config/home");
    for abs_path in walkdir(config_home.clone()).unwrap() {
        let rel_path = abs_path.strip_prefix(&config_home).unwrap();
        let dest = home.join(rel_path);
        sh.remove_path(&dest).unwrap();
        sh.create_dir(dest.parent().unwrap()).unwrap();
        std::os::unix::fs::symlink(abs_path, dest).unwrap();
    }

    let vm_shared = home.join("vms/shared");
    sh.create_dir(&vm_shared).unwrap();
    sh.copy_file(home.join("config/init.ps1"), &vm_shared).unwrap();

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
