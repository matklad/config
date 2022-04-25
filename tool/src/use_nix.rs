use xshell::{cmd, Shell};

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    sh.hard_link("/home/matklad/config/shell.nix", "shell.nix")?;
    sh.write_file(".envrc", "use nix\n")?;
    cmd!(sh, "direnv allow").run()?;
    Ok(())
}
