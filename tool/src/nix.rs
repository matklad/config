use xshell::{cmd, Shell};

pub(crate) fn gc(sh: &Shell) -> anyhow::Result<()> {
    cmd!(sh, "doas nix-collect-garbage -d").run()?;
    cmd!(sh, "doas /nix/var/nix/profiles/system/bin/switch-to-configuration switch").run()?;
    Ok(())
}

pub(crate) fn sw(sh: &Shell) -> anyhow::Result<()> {
    sh.change_dir("/home/matklad/config");
    if !cmd!(sh, "git status --porcelain").read()?.is_empty() {
        cmd!(sh, "git add .").run()?;
        cmd!(sh, "git --no-pager diff --cached --color=always").run()?;
        cmd!(sh, "git commit -m .").run()?;
    }
    cmd!(sh, "doas nixos-rebuild switch").run()?;
    cmd!(sh, "doas rm -rf /var/lib/sddm/.cache").run()?;
    Ok(())
}

pub(crate) fn up(sh: &Shell) -> anyhow::Result<()> {
    cmd!(sh, "nixos-rebuild switch --upgrade").run()?;
    Ok(())
}
