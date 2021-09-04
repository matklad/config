use xshell::cmd;

pub(crate) fn run() -> anyhow::Result<()> {
    let newbranch = args1()?;

    let fullbranch = cmd!("git symbolic-ref -q HEAD").read()?;
    let u = "{u}";
    let upstream = cmd!("git rev-parse -q --verify @{u}").read()?;

    cmd!("git switch -c {newbranch}").run()?;
    cmd!("git update-ref -m 'git spinoff' {fullbranch} {upstream}").echo_cmd(false).run()?;
    Ok(())
}

fn args1() -> anyhow::Result<String> {
    let mut args = std::env::args();
    let _progn = args.next();
    let arg = args.next();
    let next_arg = args.next();
    match (arg, next_arg) {
        (Some(arg), None) => Ok(arg),
        _ => anyhow::bail!("expected one argument"),
    }
}
