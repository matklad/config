use xshell::{cmd, cwd, mkdir_p, read_dir, rm_rf};

pub(crate) fn run() -> anyhow::Result<()> {
    let mut res = Ok(());

    if let Err(err) = rotate_downloads() {
        eprintln!("failed to rotate downloads");
        res = Err(err);
    }

    if let Err(err) = set_shortcuts() {
        eprintln!("failed to set_shortcuts");
        res = Err(err);
    }

    res
}

pub(crate) fn rotate_downloads() -> anyhow::Result<()> {
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

pub(crate) fn set_shortcuts() -> anyhow::Result<()> {
    cmd!("xbindkeys").run()?;
    Ok(())
}
