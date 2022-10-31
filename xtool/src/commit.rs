use xshell::{cmd, Shell};

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let flags = xflags::parse_or_exit! {
        /// Commit message.
        optional message: String
        /// Move all changes out of the way as a commit onto a new branch.
        optional -b,--branch branch: String
    };
    let message = flags.message.as_deref().unwrap_or(".");

    cmd!(sh, "git add --all").run()?;
    cmd!(sh, "git --no-pager diff --cached --color=always").run()?;
    match flags.branch {
        Some(branch) => {
            cmd!(sh, "git switch -c {branch}").run()?;
            cmd!(sh, "git commit -m {message}").run()?;
            cmd!(sh, "git switch -").run()?;
        }
        None => cmd!(sh, "git commit -m {message}").run()?,
    }

    Ok(())
}
