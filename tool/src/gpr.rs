use xshell::{cmd, Shell};

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let flags = xflags::parse_or_exit! { required pr: String };
    let pr = flags.pr;
    cmd!(sh, "git fetch upstream refs/pull/{pr}/head").run()?;
    cmd!(sh, "git switch --detach FETCH_HEAD").run()?;
    Ok(())
}
