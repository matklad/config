use xshell::{cmd, Shell};

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    sh.write_file(
        "./shell.nix",
        "\
with import <nixpkgs> {}; mkShell {
  packages = [ pkg-config ];
}
",
    )?;
    sh.write_file("./.envrc", "use nix\n")?;
    cmd!(sh, "direnv allow").run()?;
    Ok(())
}
