use xshell::cmd;

use crate::single_arg;

pub(crate) fn run() -> anyhow::Result<()> {
    let newbranch = single_arg()?;

    let fullbranch = cmd!("git symbolic-ref -q HEAD").read()?;
    let u = "{u}";
    let upstream = cmd!("git rev-parse -q --verify @{u}").read()?;

    cmd!("git switch -c {newbranch}").run()?;
    cmd!("git update-ref -m 'git spinoff' {fullbranch} {upstream}").echo_cmd(false).run()?;
    Ok(())
}
