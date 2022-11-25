use xshell::{cmd, Shell};

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let flags = xflags::parse_or_exit! { optional commit: String };

    let remote =
        if cmd!(sh, "git remote").read()?.contains("upstream") { "upstream" } else { "origin" };

    let branch = if cmd!(sh, "git branch").read()?.contains("master") { "master" } else { "main" };

    cmd!(sh, "git fetch {remote}").run()?;
    match flags.commit {
        None => cmd!(sh, "git rebase {remote}/{branch}"),
        Some(commit) => cmd!(sh, "git rebase --onto {remote}/{branch} {commit}^"),
    }
    .run()?;
    Ok(())
}
