use xshell::{cmd, Shell};

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let flags = xflags::parse_or_exit! {
        required pr: String
        optional --review
    };

    let remote =
        if cmd!(sh, "git remote").read()?.contains("upstream") { "upstream" } else { "origin" };

    let pr = flags.pr;
    cmd!(sh, "git fetch {remote} refs/pull/{pr}/head").run()?;
    cmd!(sh, "git switch --detach FETCH_HEAD").run()?;
    if flags.review {
        let base = cmd!(sh, "git merge-base HEAD main").read()?;
        cmd!(sh, "git reset {base}").run()?;
    }
    Ok(())
}
