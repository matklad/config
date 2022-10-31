use xshell::{Shell, cmd};

fn main() -> anyhow::Result<()> {
    let sh = Shell::new();
    cmd!(sh, "echo 'hello world'");
    Ok(())
}
