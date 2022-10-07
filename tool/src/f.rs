use xshell::Shell;

use crate::single_arg;

pub(crate) fn t(sh: &Shell) -> anyhow::Result<()> {
    let path = single_arg()?;
    sh.write_file(path, b"")?;
    Ok(())
}

pub(crate) fn d(sh: &Shell) -> anyhow::Result<()> {
    let path = single_arg()?;
    sh.remove_path(&path)?;
    Ok(())
}
