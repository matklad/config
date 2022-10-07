use xshell::Shell;

use std::path::PathBuf;

pub(crate) fn t(sh: &Shell) -> anyhow::Result<()> {
    let flags = xflags::parse_or_exit! { required path: String };
    if flags.path.ends_with('/') {
        sh.create_dir(flags.path)?;
    } else {
        sh.write_file(flags.path, b"")?;
    }
    Ok(())
}

pub(crate) fn d(sh: &Shell) -> anyhow::Result<()> {
    let flags = xflags::parse_or_exit! { required path: PathBuf };
    sh.remove_path(&flags.path)?;
    Ok(())
}
