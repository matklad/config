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

pub(crate) fn run2() -> anyhow::Result<()> {
    let mut sh = xshell2::new();

    let newbranch = single_arg()?;

    let fullbranch = sh.read_stdout(cmd!("git symbolic-ref -q HEAD"))?;
    let u = "{u}";
    let upstream = sh.read_stdout(cmd!("git rev-parse -q --verify @{u}"))?;

    sh.run(cmd!("git switch -c {newbranch}"))?;
    sh.run(cmd!("git update-ref -m 'git spinoff' {fullbranch} {upstream}").echo_cmd(false))?;
    Ok(())
}
