use xshell::Shell;

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let mut res = Ok(());

    if let Err(err) = rotate_downloads(sh) {
        eprintln!("failed to rotate downloads");
        res = Err(err);
    }

    res
}

fn rotate_downloads(sh: &Shell) -> anyhow::Result<()> {
    sh.create_dir("/home/matklad/downloads")?;
    let _p = sh.push_dir("/home/matklad/downloads");
    sh.remove_path(".old")?;

    let new = sh.read_dir(".")?;
    if !new.is_empty() {
        sh.create_dir(".old")?;
        let cwd = sh.current_dir();
        for path in new {
            let new_path = cwd.join(".old").join(path.strip_prefix(&cwd).unwrap());
            std::fs::rename(path, new_path)?
        }
    }
    Ok(())
}
