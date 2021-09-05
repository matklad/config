use xshell::cmd;

pub(crate) fn run() -> anyhow::Result<()> {
    cmd!("git fetch upstream").run()?;
    cmd!("git rebase upstream/master").run()?;
    Ok(())
}
