use std::path::Path;

use xshell::{cmd, Shell};

use crate::single_arg;

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let bin = single_arg()?;
    let nix_cc = cmd!(sh, "nix-shell -p hello --run 'echo $NIX_CC'").read()?;
    let linker = sh.read_file(Path::new(&nix_cc).join("nix-support/dynamic-linker"))?;
    let linker = linker.trim_end();
    cmd!(sh, "patchelf --set-interpreter {linker} {bin}").run()?;
    Ok(())
}
