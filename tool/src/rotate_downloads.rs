use xshell::{cwd, mkdir_p, read_dir, rm_rf};

pub(crate) fn run() -> anyhow::Result<()> {
    mkdir_p("/home/matklad/downloads")?;
    let _p = xshell::pushd("/home/matklad/downloads")?;
    rm_rf(".old")?;

    let new = read_dir(".")?;
    if !new.is_empty() {
        mkdir_p(".old")?;
        let cwd = cwd()?;
        for path in new {
            std::fs::rename(cwd.join(&path), cwd.join(".old").join(&path))?
        }
    }
    Ok(())
}
