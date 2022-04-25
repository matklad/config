use xshell::{cmd, Shell};

use crate::single_arg;

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let pr = single_arg()?;
    cmd!(sh, "git fetch upstream refs/pull/{pr}/head").run()?;
    cmd!(sh, "git switch --detach FETCH_HEAD").run()?;
    Ok(())
}
