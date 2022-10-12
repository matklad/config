use xshell::{cmd, Shell};

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let flags = xflags::parse_or_exit! { required branch: String };
    let newbranch = flags.branch;

    let fullbranch = cmd!(sh, "git symbolic-ref -q HEAD").read()?;
    let u = "{u}";
    let upstream = cmd!(sh, "git rev-parse -q --verify @{u}").read()?;

    cmd!(sh, "git switch -c {newbranch}").run()?;
    cmd!(sh, "git update-ref -m 'git spinoff' {fullbranch} {upstream}").quiet().run()?;
    Ok(())
}
