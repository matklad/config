use xshell::cmd;

pub(crate) fn gc() -> anyhow::Result<()> {
    cmd!("nix-collect-garbage -d").run()?;
    cmd!("/nix/var/nix/profiles/system/bin/switch-to-configuration switch").run()?;
    Ok(())
}

pub(crate) fn sw() -> anyhow::Result<()> {
    cmd!("nixos-rebuild switch").run()?;
    Ok(())
}

pub(crate) fn up() -> anyhow::Result<()> {
    cmd!("nixos-rebuild switch --upgrade").run()?;
    Ok(())
}
