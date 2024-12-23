use anyhow::Context;
use xshell::{cmd, Shell};

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let mut n: Option<u32> = None;
    let mut ignore_status = false;
    let mut args = Vec::new();
    let mut iter = std::env::args().skip(1);
    while let Some(flag) = iter.next() {
        match flag.as_str() {
            "-i" | "--ignore-status" => ignore_status = true,
            value if n.is_none() => n = Some(value.parse()?),
            _ => {
                args.push(flag);
                args.extend(iter);
                break;
            }
        }
    }
    let n = n.context("number of iterations expected")?;
    if args.is_empty() {
        anyhow::bail!("expected command");
    }

    let progn = args.remove(0);

    let task = if args.is_empty() && progn.contains(' ') {
        Task::Shell { script: progn }
    } else {
        Task::Command { progn, args }
    };

    for i in 1..n + 1 {
        eprintln!("Run {i}");
        let mut cmd = match &task {
            Task::Command { progn, args } => cmd!(sh, "{progn} {args...}"),
            Task::Shell { script } => cmd!(sh, "fish -c {script}"),
        };
        if ignore_status {
            cmd = cmd.ignore_status();
        }
        cmd.run_echo()?;
    }

    Ok(())
}

enum Task {
    Command { progn: String, args: Vec<String> },
    Shell { script: String },
}
