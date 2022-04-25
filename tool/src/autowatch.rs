use std::path::PathBuf;

use xshell::{cmd, Shell};

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let files = std::env::args_os()
        .skip(1)
        .map(PathBuf::from)
        .filter(|it| it.exists())
        .map(|it| format!("{}\n", it.display()))
        .collect::<String>();

    let args = std::env::args_os().skip(1);
    cmd!(sh, "entr -r {args...}").stdin(&files).run()?;
    Ok(())
}
