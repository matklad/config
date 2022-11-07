use xshell::{cmd, Shell};

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let args = std::env::args().skip(1);
    cmd!(sh, "cargo run -q --manifest-path /home/matklad/config/script/Cargo.toml -- {args...}")
        .run()?;
    Ok(())
}
