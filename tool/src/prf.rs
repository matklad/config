use xshell::{cmd, Shell};

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let mut args = std::env::args();
    let _progn = args.next();
    cmd!(sh, "perf record -F500 -g --call-graph=dwarf,65500 {args...}").run()?;
    let data = cmd!(sh, "perf script -F +pid").read()?;
    let output = "perf_profile";
    sh.write_file(output, &data)?;
    eprintln!("output is in {output}");
    sh.remove_path("perf.data")?;
    Ok(())
}
