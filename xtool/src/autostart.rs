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
