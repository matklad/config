use xshell::{cmd, Shell};

use crate::single_arg;

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let newbranch = single_arg()?;

    let fullbranch = cmd!(sh, "git symbolic-ref -q HEAD").read()?;
    let u = "{u}";
    let upstream = cmd!(sh, "git rev-parse -q --verify @{u}").read()?;

    cmd!(sh, "git switch -c {newbranch}").run()?;
    cmd!(sh, "git update-ref -m 'git spinoff' {fullbranch} {upstream}").quiet().run()?;
    Ok(())
}
