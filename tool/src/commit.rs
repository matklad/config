use xshell::cmd;

use crate::opt_single_arg;

pub(crate) fn run() -> anyhow::Result<()> {
    let message = opt_single_arg()?;
    match message {
        Some(message) => {
            cmd!("git add .").run()?;
            if cmd!("git --no-pager diff --cached --color=always").run().is_err() {
                return Ok(());
            }
            cmd!("git commit -m {message}").run()?;
        },
        None => cmd!("git commit -a").run()?,
    }
    Ok(())
}
