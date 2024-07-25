use std::path::PathBuf;

use xshell::Shell;

mod autopatchelf;
mod autostart;
mod autowatch;
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
