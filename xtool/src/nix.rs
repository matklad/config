use xshell::{cmd, Shell};

pub(crate) fn gc(sh: &Shell) -> anyhow::Result<()> {
    cmd!(sh, "doas nix-collect-garbage -d").run()?;
    cmd!(sh, "doas /nix/var/nix/profiles/system/bin/switch-to-configuration switch").run()?;
    Ok(())
}

pub(crate) fn up(sh: &Shell) -> anyhow::Result<()> {
    cmd!(sh, "doas true").quiet().run()?;

    let mut upgdate = false;
    for arg in std::env::args().skip(1) {
        match arg.as_str() {
            "--up" => upgdate = true,
            arg => anyhow::bail!("unexpected arg: {:?}", arg),
        };
    }

    sh.change_dir("/home/matklad/config");
    if upgdate {
        cmd!(sh, "nix flake update").run()?;
    }
    let mut push = false;
    if !cmd!(sh, "git status --porcelain").read()?.is_empty() {
        cmd!(sh, "git add .").run()?;
        cmd!(sh, "git --no-pager diff --cached --color=always").run()?;
        cmd!(sh, "git commit -m .").run()?;
        push = true;
    }
    cmd!(sh, "doas rm -rf /var/lib/sddm/.cache").run()?;
    cmd!(sh, "doas nixos-rebuild switch").run()?;
    if push {
        cmd!(sh, "git push").run()?;
    }
    Ok(())
}
