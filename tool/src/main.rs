use std::path::PathBuf;

mod commit;
mod gbors;
mod git_spinoff;
mod t;
mod use_nix;

const TOOLS: &[(&str, fn() -> anyhow::Result<()>)] = &[
    ("commit", commit::run),
    ("gbors", gbors::run),
    ("git-spinoff", git_spinoff::run),
    ("t", t::run),
    ("use-nix", use_nix::run),
];

fn main() -> anyhow::Result<()> {
    let progn: PathBuf = std::env::args_os().next().unwrap_or_default().into();
    let progn = progn.file_stem().unwrap_or_default().to_str().unwrap_or_default();

    let (_name, run) = TOOLS
        .iter()
        .find(|&&(name, _run)| name == progn)
        .ok_or_else(|| anyhow::format_err!("unknown tool: `{}`", progn))?;

    run()
}

#[test]
fn link_me_up() {
    let bin = std::path::Path::new("../bin");

    xshell::cmd!("cargo build --release").run().unwrap();

    for &(tool, _) in TOOLS {
        let dst = bin.join(tool);
        xshell::rm_rf(&dst).unwrap();
        xshell::hard_link("./target/release/tool", dst).unwrap();
    }

    let ignore = TOOLS.iter().map(|&(name, _)| name).collect::<Vec<_>>().join("\n");
    xshell::write_file("../bin/.gitignore", ignore).unwrap();
}
