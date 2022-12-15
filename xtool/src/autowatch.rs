use std::path::Path;

use xshell::{cmd, Shell};

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let mut args = Vec::new();
    let mut shell_arg = None;
    let mut args_iter = std::env::args().skip(1);
    while let Some(arg) = args_iter.next() {
        if arg.contains(' ') {
            shell_arg = Some(arg);
            if args_iter.next().is_some() || !args.is_empty() {
                anyhow::bail!("bad shell arg");
            }
            break;
        }
        args.push(arg);
    }
    let files = shell_arg
        .iter()
        .flat_map(|it| it.split_ascii_whitespace())
        .chain(args.iter().map(|it| it.as_str()))
        .map(Path::new)
        .filter(|it| it.exists())
        .map(|it| format!("{}\n", it.display()))
        .collect::<String>();
    match shell_arg {
        Some(shell_arg) => {
            cmd!(sh, "entr -r -c fish -c {shell_arg}").stdin(&files).run()?;
        }
        None => {
            cmd!(sh, "entr -r -c {args...}").stdin(&files).run()?;
        }
    }

    Ok(())
}
