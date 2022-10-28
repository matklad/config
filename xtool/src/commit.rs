use xshell::{cmd, Shell};

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let flags = xflags::parse_or_exit! {
        /// Commit message.
        optional message: String
    };
    let message = flags.message.as_deref().unwrap_or(".");

    cmd!(sh, "git add --all").run()?;
    cmd!(sh, "git --no-pager diff --cached --color=always").run()?;
    cmd!(sh, "git commit -m {message}").run()?;

    Ok(())
}
