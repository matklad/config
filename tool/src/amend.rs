use xshell::cmd;

pub(crate) fn run() -> anyhow::Result<()> {
    cmd!("git add .").run()?;
    cmd!("git --no-pager diff --cached --color=always").run()?;
    cmd!("git --no-pager show -s --oneline --abbrev-commit --color=always HEAD").run()?;

    if yes_or_no("Continue?") {
        cmd!("git commit --amend --no-edit").run()?;
    }
    Ok(())
}

fn yes_or_no(msg: &str) -> bool {
    println!("{}", msg);
    let mut buf = String::new();
    match std::io::stdin().read_line(&mut buf) {
        Err(_) | Ok(0) => false,
        Ok(_) => {
            let resp = buf.trim();
            matches!(resp, "" | "y" | "Y")
        }
    }
}
