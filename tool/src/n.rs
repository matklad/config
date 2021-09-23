use anyhow::Context;
use xshell::cmd;

pub(crate) fn run() -> anyhow::Result<()> {
    let mut args = std::env::args();
    let _progn = args.next();

    let n = args.next().context("expected a number")?;
    let n = n.parse::<u32>()?;

    let mut ignore_status = false;
    let mut shell = false;

    let mut arg;
    loop {
        arg = args.next().context("expected program name")?;
        match arg.as_str() {
            "-i" => ignore_status = true,
            "-s" => shell = true,
            _ => break,
        }
    }

    let task = if shell {
        if let Some(arg) = args.next() {
            anyhow::bail!("unexpected argument: {}", arg)
        }
        Task::Shell { script: arg }
    } else {
        let args = args.collect();
        Task::Command { progn: arg, args }
    };

    for i in 0..n {
        eprintln!("Run {}", i + 1);
        let mut cmd = match &task {
            Task::Command { progn, args } => cmd!("{progn} {args...}"),
            Task::Shell { script } => cmd!("fish -c {script}"),
        }
        .echo_cmd(false);
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
