use xshell::cmd;

pub(crate) fn run() -> anyhow::Result<()> {
    let temp_dir = xshell::mktemp_d()?;
    let f = temp_dir.path().join("timings");

    let mut args = std::env::args_os();
    args.next();

    let _ = cmd!("time --output {f} --format '%e %U %S %M' {args...}").echo_cmd(false).run();

    let timings = xshell::read_file(&f)?
        .split_ascii_whitespace()
        .map(|it| it.parse::<f64>().unwrap())
        .collect::<Vec<_>>();
    let real = timings[0];
    let user = timings[1];
    let sys = timings[2];
    let rsskb = timings[3];

    eprintln!(
        "
real {:.2}s
cpu  {:.2}s ({:.2}s user + {:.2}s sys)
rss  {:.2}mb
",
        real,
        (user + sys),
        user,
        sys,
        (rsskb / 1024.0)
    );
    Ok(())
}
