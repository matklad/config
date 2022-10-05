use xshell::{cmd, Shell};

use crate::opt_single_arg;

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let commit = opt_single_arg()?;

    cmd!(sh, "git fetch upstream").run()?;
    match commit {
        None => cmd!(sh, "git rebase upstream/master"),
        Some(commit) => cmd!(sh, "git rebase --onto upstream/master {commit}^"),
    }
    .run()?;
    Ok(())
}
