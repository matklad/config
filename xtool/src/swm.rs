use xshell::{cmd, Shell};

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let branches = cmd!(sh, "git branch").read()?;
    let target = if branches.contains("master") { "master" } else { "main" };
    cmd!(sh, "git switch {target}").run()?;
    Ok(())
}
