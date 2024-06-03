use xshell::{cmd, Shell};

pub(crate) fn gc(sh: &Shell) -> anyhow::Result<()> {
    cmd!(sh, "doas nix-collect-garbage -d").run()?;
    cmd!(sh, "doas /nix/var/nix/profiles/system/bin/switch-to-configuration switch").run()?;
    Ok(())
}

pub(crate) fn up(sh: &Shell) -> anyhow::Result<()> {
    let flags = xflags::parse_or_exit! {
        optional --update
        optional --no-pp
    };

    sh.change_dir("/home/matklad/config");
    if flags.update {
        cmd!(sh, "nix flake update").run()?;
    }
    let mut committed = false;
    if !cmd!(sh, "git status --porcelain").read()?.is_empty() {
        cmd!(sh, "git add .").run()?;
        cmd!(sh, "git --no-pager diff --cached --color=always").run()?;
        cmd!(sh, "git commit -m .").run()?;
        committed = true;
    }
    if !flags.no_pp {
        cmd!(sh, "git pull --rebase").run()?;
    }

    {
        cmd!(sh, "doas rm -rf /var/lib/sddm/.cache").run()?;
        cmd!(sh, "rm -rf /home/matklad/.config/Code/GPUCache/").run()?;
    }

    let host = cmd!(sh, "hostname").read()?;
    let system = cmd!(
        sh,
        "
        nix build --no-link --print-out-paths
            path:/home/matklad/config#nixosConfigurations.{host}.config.system.build.toplevel"
    )
    .read()?;
    cmd!(sh, "doas nix-env -p /nix/var/nix/profiles/system --set {system}").run()?;
    cmd!(sh, "doas {system}/bin/switch-to-configuration switch").run()?;
    if committed && !flags.no_pp {
        cmd!(sh, "git push").run()?;
    }
    Ok(())
}
