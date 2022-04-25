use xshell::{cmd, Shell};

use crate::opt_single_arg;

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let message = opt_single_arg()?;
    let message = message.as_deref().unwrap_or(".");

    cmd!(sh, "git add .").run()?;
    cmd!(sh, "git --no-pager diff --cached --color=always").run()?;
    cmd!(sh, "git commit -m {message}").run()?;

    Ok(())
}
