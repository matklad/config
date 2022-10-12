use std::path::{Path, PathBuf};

use xshell::{cmd, Shell};

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let flags = xflags::parse_or_exit! { required path: PathBuf };
    let bin = flags.path;
    let nix_cc = cmd!(sh, "nix-shell -p hello --run 'echo $NIX_CC'").read()?;
    let linker = sh.read_file(Path::new(&nix_cc).join("nix-support/dynamic-linker"))?;
    let linker = linker.trim_end();
    cmd!(sh, "patchelf --set-interpreter {linker} {bin}").run()?;
    Ok(())
}
