use std::{os::unix::process::CommandExt, path::PathBuf};

use xshell::{cmd, Shell};

mod flags {
    xflags::xflags! {
        cmd config {
            cmd make {}
            cmd brew {

            }
            cmd link {

            }
            cmd edit {}
        }
    }
}

fn main() -> anyhow::Result<()> {
    let arg0 = std::env::args().next().unwrap();
    if arg0 != "config" {
        return main_multicall(&arg0);
    }

    let flags = flags::Config::from_env_or_exit();
    let sh = xshell::Shell::new()?;
    match flags.subcommand {
        flags::ConfigCmd::Make(flags::Make) => make(&sh)?,
        flags::ConfigCmd::Edit(flags::Edit) => {
            Err(std::process::Command::new("code")
                .arg("/Users/matklad/config")
                .exec())?;
        }
        flags::ConfigCmd::Brew(flags::Brew) => {
            cmd!(sh, "brew bundle install --cleanup --file=~/config/Brewfile").run_echo()?;
            cmd!(sh, "brew upgrade").run_echo()?;
        }
        flags::ConfigCmd::Link(flags::Link) => symlink(&sh)?,
    }
    Ok(())
}

fn make(sh: &Shell) -> anyhow::Result<()> {
    cmd!(sh, "cargo install --path /Users/matklad/config/tools/gg").run_echo()?;
    cmd!(
        sh,
        "cargo install --path /Users/matklad/config/tools/config"
    )
    .run_echo()?;
    Ok(())
}

fn symlink(sh: &Shell) -> anyhow::Result<()> {
    let home: PathBuf = "/Users/matklad/".into();
    let config_home = home.join("config/home");

    for abs_path in walkdir(config_home.clone()).unwrap() {
        let rel_path = abs_path.strip_prefix(&config_home).unwrap();
        let dest = home.join(rel_path);
        sh.remove_path(&dest).unwrap();
        sh.create_dir(dest.parent().unwrap()).unwrap();
        if dest.ends_with("DefaultKeyBinding.Dict") {
            std::fs::copy(abs_path, dest).unwrap();
        } else {
            std::os::unix::fs::symlink(abs_path, dest).unwrap();
        }
    }

    return Ok(());

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

fn main_multicall(arg0: &str) -> anyhow::Result<()> {
    let sh = xshell::Shell::new()?;
    match arg0 {
        "n" => multicall::n::run(&sh),
        _ => anyhow::bail!("unknown command: `{arg0}`"),
    }
}

mod multicall {
    pub mod n;
}
