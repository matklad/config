use std::path::PathBuf;

mod amend;
mod commit;
mod gbda;
mod gbors;
mod gcf;
mod git_refresh;
mod git_spinoff;
mod gpr;
mod t;
mod use_nix;

const TOOLS: &[(&str, fn() -> anyhow::Result<()>)] = &[
    ("amend", amend::run),
    ("commit", commit::run),
    ("gbda", gbda::run),
    ("gbors", gbors::run),
    ("gcf", gcf::run),
    ("git-refresh", git_refresh::run),
    ("git-spinoff", git_spinoff::run),
    ("gpr", gpr::run),
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

fn single_arg() -> anyhow::Result<String> {
    let mut args = std::env::args();
    let _progn = args.next();
    let arg = args.next();
    let next_arg = args.next();
    match (arg, next_arg) {
        (Some(arg), None) => Ok(arg),
        _ => anyhow::bail!("expected one argument"),
    }
}

#[test]
fn link_me_up() {
    use xshell::cmd;

    let bin = std::path::Path::new("../bin");

    cmd!("cargo build --release").run().unwrap();

    for &(tool, _) in TOOLS {
        let dst = bin.join(tool);
        xshell::rm_rf(&dst).unwrap();
        let _ = cmd!("git rm {dst} -f").echo_cmd(false).ignore_stderr().run();
        xshell::hard_link("./target/release/tool", &dst).unwrap();
    }

    let ignore = TOOLS.iter().map(|&(name, _)| name).collect::<Vec<_>>().join("\n");
    xshell::write_file("../bin/.gitignore", ignore).unwrap();
}
