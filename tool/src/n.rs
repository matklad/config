use xshell::{cmd, Shell};

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let mut flags = xflags::parse_or_exit! {
        optional -i,--ignore-status
        /// Number of iterations.
        required n: u32
        repeated args: String
    };
    if flags.args.is_empty() {
        anyhow::bail!("expected command");
    }

    let progn = flags.args.remove(0);

    let task = if flags.args.is_empty() && progn.contains(' ') {
        Task::Shell { script: progn }
    } else {
        Task::Command { progn, args: flags.args }
    };

    for i in 1..flags.n + 1 {
        eprintln!("Run {i}");
        let mut cmd = match &task {
            Task::Command { progn, args } => cmd!(sh, "{progn} {args...}"),
            Task::Shell { script } => cmd!(sh, "fish -c {script}"),
        }
        .quiet();
        if flags.ignore_status {
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
