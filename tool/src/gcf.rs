use anyhow::Context;
use xshell::{cmd, Shell};

use crate::single_arg;

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let userrepo = single_arg()?;
    let (user, repo) = userrepo.split_once('/').context("invalid user/repo")?;

    cmd!(sh, "git clone git@github.com:matklad/{repo}.git").run()?;
    let _p = sh.push_dir(repo);

    cmd!(sh, "git remote add upstream git@github.com:{user}/{repo}.git").run()?;
    cmd!(sh, "git fetch upstream").run()?;
    let branch = cmd!(sh, "git rev-parse --abbrev-ref HEAD").read()?;
    cmd!(sh, "git switch {branch}").run()?;
    cmd!(sh, "git reset --hard upstream/{branch}").run()?;
    cmd!(sh, "git branch --set-upstream-to=upstream/{branch}").run()?;

    Ok(())
}
