use xshell::{cmd, Shell};

pub(crate) fn gc(sh: &Shell) -> anyhow::Result<()> {
    cmd!(sh, "nix-collect-garbage -d").run()?;
    cmd!(sh, "/nix/var/nix/profiles/system/bin/switch-to-configuration switch").run()?;
    Ok(())
}

pub(crate) fn sw(sh: &Shell) -> anyhow::Result<()> {
    cmd!(sh, "nixos-rebuild switch").run()?;
    Ok(())
}

pub(crate) fn up(sh: &Shell) -> anyhow::Result<()> {
    cmd!(sh, "nixos-rebuild switch --upgrade").run()?;
    Ok(())
}
