use xshell::cmd;

pub(crate) fn run() -> anyhow::Result<()> {
    xshell::hard_link("/home/matklad/config/shell.nix", "shell.nix")?;
    xshell::write_file(".envrc", "use nix\n")?;
    cmd!("direnv allow").run()?;
    Ok(())
}
