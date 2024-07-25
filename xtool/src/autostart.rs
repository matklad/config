use anyhow::Ok;
use xshell::{cmd, Shell};

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let mut res = Ok(());

    if let Err(err) = rotate_downloads(sh) {
        eprintln!("failed to rotate downloads");
        res = Err(err);
    }

    if let Err(err) = plasma_shortcuts(sh) {
        eprintln!("failed to update shortcuts");
        res = Err(err);
    }

    if let Err(err) = symlink(sh) {
        eprintln!("failed to symlik");
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

fn plasma_shortcuts(sh: &Shell) -> anyhow::Result<()> {
    // cmd!(
    //     sh,
    //     "
    //     kwriteconfig6 --file kglobalshortcutsrc
    //         --group services
    //         --group org.kde.dolphin.desktop
    //         --key _launch
    //         'none,none,Dolphin'
    //     "
    // )
    // .run()?;
    cmd!(
        sh,
        "
        kwriteconfig6 --file kglobalshortcutsrc
            --group plasmashell
            --key 'manage activities'
            'none,none,Show Activity Switcher'
        "
    )
    .run()?;
    cmd!(
        sh,
        "
        kwriteconfig6 --file kglobalshortcutsrc
            --group kwin
            --key 'Window Close'
            'Meta+Q,none,Close Window'
        "
    )
    .run()?;
    cmd!(
        sh,
        "
        kwriteconfig6 --file kglobalshortcutsrc
            --group kwin
            --key 'Window Maximize'
            'Meta+Up,Meta+Up,Maximize Window'
        "
    )
    .run()?;
    cmd!(
        sh,
        "
        kwriteconfig6 --file kglobalshortcutsrc
            --group kwin
            --key 'Window Maximize'
            'Meta+Up,Meta+Up,Maximize Window'
        "
    )
    .run()?;
    cmd!(
        sh,
        "
        kwriteconfig6 --file kglobalshortcutsrc
            --group kwin
            --key 'Window Quick Tile Top'
            'none,none,Quick Tile Window to the Top'
        "
    )
    .run()?;

    Ok(())
}

fn symlink(sh: &Shell) -> anyhow::Result<()> {
    let home: PathBuf = "/home/matklad/".into();
    let config_home = home.join("config/home");

    for abs_path in walkdir(config_home.clone()).unwrap() {
        let rel_path = abs_path.strip_prefix(&config_home).unwrap();
        let dest = home.join(rel_path);
        sh.remove_path(&dest).unwrap();
        sh.create_dir(dest.parent().unwrap()).unwrap();
        std::os::unix::fs::symlink(abs_path, dest).unwrap();
    }

    return Ok(());

    fn walkdir(path: PathBuf) -> anyhow::Result<Vec<PathBuf>> {
        let mut res = Vec::new();
        let mut work = vec![path];
        while let Some(dir) = work.pop() {
            for entry in std::fs::read_dir(&dir)? {
                let entry = entry?;
                let file_type = entry.file_type()?;
                if file_type.is_file() {
                    res.push(entry.path())
                } else if file_type.is_dir() {
                    work.push(entry.path());
                }
            }
        }
        Ok(res)
    }
}
