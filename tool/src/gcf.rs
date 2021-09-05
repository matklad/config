use anyhow::Context;
use xshell::{cmd, pushd};

use crate::single_arg;

pub(crate) fn run() -> anyhow::Result<()> {
    let userrepo = single_arg()?;
    let (user, repo) = userrepo.split_once('/').context("invalid user/repo")?;

    cmd!("git clone git@github.com:matklad/{repo}.git").run()?;
    let _p = pushd(repo)?;

    cmd!("git remote add upstream git@github.com:{user}/{repo}.git").run()?;
    cmd!("git fetch upstream").run()?;
    let branch = cmd!("git rev-parse --abbrev-ref HEAD").read()?;
    cmd!("git switch {branch}").run()?;
    cmd!("git reset --hard upstream/{branch}").run()?;
    cmd!("git branch --set-upstream-to=upstream/{branch}").run()?;

    Ok(())
}
