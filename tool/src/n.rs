use anyhow::Context;
use xshell::{cmd, Shell};

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let mut args = std::env::args();
    let _progn = args.next();

    let mut ignore_status = false;
    let mut shell = false;
    let n: u32;

    loop {
        let arg = args.next().context("expected program name")?;
        match arg.as_str() {
            "-i" => ignore_status = true,
            "-s" | "--sh" => shell = true,
            _ => {
                n = arg.parse::<u32>()?;
                break;
            }
        }
    }

    let task = if shell {
        let script = args.next().context("expected a script")?;
        if let Some(arg) = args.next() {
            anyhow::bail!("unexpected argument: {}", arg)
        }
        Task::Shell { script }
    } else {
        let progn = args.next().context("expected program name")?;
        let args = args.collect();
        Task::Command { progn, args }
    };

    for i in 1..n + 1 {
        eprintln!("Run {i}");
        let mut cmd = match &task {
            Task::Command { progn, args } => cmd!(sh, "{progn} {args...}"),
            Task::Shell { script } => cmd!(sh, "fish -c {script}"),
        }
        .quiet();
        if ignore_status {
            cmd = cmd.ignore_status();
        }
        cmd.run()?;
    }

    Ok(())
}

enum Task {
    Command { progn: String, args: Vec<String> },
    Shell { script: String },
}
