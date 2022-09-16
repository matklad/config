use xshell::{cmd, Shell};

use crate::opt_single_arg;

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    cmd!(sh, "git fetch upstream").run()?;
    match opt_single_arg()? {
        None => cmd!(sh, "git rebase upstream/master"),
        Some(commit) => cmd!(sh, "git rebase --onto upstream/master {commit}^"),
    }
    .run()?;
    Ok(())
}
