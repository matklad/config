use xshell::cmd;

use crate::opt_single_arg;

pub(crate) fn run() -> anyhow::Result<()> {
    let message = opt_single_arg()?;
    let message = message.as_deref().unwrap_or(".");

    cmd!("git add .").run()?;
    cmd!("git --no-pager diff --cached --color=always").run()?;
    cmd!("git commit -m {message}").run()?;

    Ok(())
}
