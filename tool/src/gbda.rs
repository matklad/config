use xshell::cmd;

pub(crate) fn run() -> anyhow::Result<()> {
    let branches = cmd!("git branch --merged").read()?;
    let branches: Vec<_> = branches
        .lines()
        .map(str::trim)
        .filter(|&it| !(it == "master" || it.starts_with('*')))
        .collect();
    if branches.is_empty() {
        println!("no merged branches");
        return Ok(());
    }

    cmd!("git branch -D {branches...}").run()?;
    Ok(())
}
