use xshell::cmd;

use crate::single_arg;

pub(crate) fn run() -> anyhow::Result<()> {
    let pr = single_arg()?;
    cmd!("git fetch upstream refs/pull/{pr}/head").run()?;
    cmd!("git switch --detach FETCH_HEAD").run()?;
    Ok(())
}
