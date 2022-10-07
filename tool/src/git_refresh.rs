use xshell::{cmd, Shell};

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let flags = xflags::parse_or_exit! { optional commit: String };

    let remote =
        if cmd!(sh, "git remote").read()?.contains("upstream") { "upstream" } else { "origin" };

    cmd!(sh, "git fetch {remote}").run()?;
    match flags.commit {
        None => cmd!(sh, "git rebase {remote}/master"),
        Some(commit) => cmd!(sh, "git rebase --onto {remote}/master {commit}^"),
    }
    .run()?;
    Ok(())
}
