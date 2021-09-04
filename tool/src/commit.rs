use xshell::cmd;

pub(crate) fn run() -> anyhow::Result<()> {
    let message = std::env::args().nth(1);
    match message {
        Some(message) => {
            cmd!("git add .").run()?;
            if cmd!("git --no-pager diff --cached --color=always").run().is_err() {
                return Ok(());
            }
            cmd!("git commit -m {message}").run()?;
        },
        None => cmd!("git commit -a").run()?,
    }
    Ok(())
}
