use std::{env, time::Duration};

use anyhow::Context;
use xshell::{cwd, mkdir_p, read_dir, rm_rf, cmd};

pub(crate) fn run() -> anyhow::Result<()> {
    let mut res = Ok(());

    if let Err(err) = rotate_downloads() {
        eprintln!("failed to rotate downloads");
        res = Err(err);
    }

    std::thread::sleep(Duration::from_millis(1500));
    if let Err(err) = set_keymap() {
        eprintln!("failed to set keymap");
        res = Err(err);
    }

    res
}

fn set_keymap() -> anyhow::Result<()> {
    let display = env::var("DISPLAY")
        .context("failed to read $DISPLAY")?;
    cmd!("xkbcomp /home/matklad/config/home-row.xkb {display}").run()?;
    Ok(())
}

fn rotate_downloads() -> anyhow::Result<()> {
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
