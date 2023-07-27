use xshell::{cmd, Shell};

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let branches = cmd!(sh, "git branch --merged").read()?;
    let branches: Vec<_> = branches
        .lines()
        .map(str::trim)
        .filter(|&it| {
            !(it == "master" || it == "main" || it.starts_with('*') || it.starts_with('+'))
        })
        .collect();
    if branches.is_empty() {
        println!("no merged branches");
        return Ok(());
    }

    cmd!(sh, "git branch -D {branches...}").run()?;
    Ok(())
}
