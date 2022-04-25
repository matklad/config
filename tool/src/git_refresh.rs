use xshell::{cmd, Shell};

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    cmd!(sh, "git fetch upstream").run()?;
    cmd!(sh, "git rebase upstream/master").run()?;
    Ok(())
}
