use std::path::PathBuf;

mod amend;
mod commit;
mod gbda;
mod gbors;
mod gcf;
mod git_refresh;
mod git_spinoff;
mod gpr;
mod nix;
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
    ("nixgc", nix::gc),
    ("nixsw", nix::sw),
    ("nixup", nix::up),
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
    match single_arg_impl() {
        Ok(Some(arg)) => Ok(arg),
        _ => anyhow::bail!("expected one argument"),
    }
}

fn opt_single_arg() -> anyhow::Result<Option<String>> {
    single_arg_impl().map_err(|()| anyhow::format_err!("expected at most one argument"))
}

fn single_arg_impl() -> Result<Option<String>, ()> {
    let mut args = std::env::args();
    let _progn = args.next();
    let arg = args.next();
    let next_arg = args.next();
    match (arg, next_arg) {
        (None, None) => Ok(None),
        (Some(arg), None) => Ok(Some(arg)),
        _ => Err(()),
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

    let home: PathBuf = "/home/matklad/".into();
    let config_home = home.join("config/home");
    for abs_path in walkdir(config_home.clone()).unwrap() {
        let rel_path = abs_path.strip_prefix(&config_home).unwrap();
        let dest = home.join(rel_path);
        xshell::rm_rf(&dest).unwrap();
        xshell::mkdir_p(dest.parent().unwrap()).unwrap();
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
        Ok(work)
    }
}
