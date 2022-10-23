use xshell::{cmd, Shell};

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let flags = xflags::parse_or_exit! { required name: String };
    let name = flags.name;
    if name.contains('_') {
        anyhow::bail!("only dashes, please")
    }
    cmd!(sh, "cp -r /home/matklad/config/templates/crate {name}").run()?;
    sh.change_dir(&name);
    cmd!(sh, "git init .").run()?;
    Ok(())
}
