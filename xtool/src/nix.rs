use xshell::{cmd, Shell};

pub(crate) fn gc(sh: &Shell) -> anyhow::Result<()> {
    cmd!(sh, "doas nix-collect-garbage -d").run()?;
    cmd!(sh, "doas /nix/var/nix/profiles/system/bin/switch-to-configuration switch").run()?;
    Ok(())
}

pub(crate) fn up(sh: &Shell) -> anyhow::Result<()> {
    cmd!(sh, "doas true").quiet().run()?;

    let flags = xflags::parse_or_exit! { optional --offline };

    sh.change_dir("/home/matklad/config");
    if !flags.offline {
        cmd!(sh, "nix flake update").run()?;
    }
    let mut committed = false;
    if !cmd!(sh, "git status --porcelain").read()?.is_empty() {
        cmd!(sh, "git add .").run()?;
        cmd!(sh, "git --no-pager diff --cached --color=always").run()?;
        cmd!(sh, "git commit -m .").run()?;
        committed = true;
    }
    if !flags.offline {
        cmd!(sh, "git pull --rebase").run()?;
    }
    cmd!(sh, "doas rm -rf /var/lib/sddm/.cache").run()?;
    cmd!(sh, "doas nixos-rebuild switch").run()?;
    if committed && !flags.offline {
        cmd!(sh, "git push").run()?;
    }
    Ok(())
}
