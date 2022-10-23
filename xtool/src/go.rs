use xshell::{cmd, Shell};

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let flags = xflags::parse_or_exit! { required dest: String };
    match flags.dest.as_str() {
        "style" => cmd!(sh, "code /home/matklad/p/fun/style.adoc").run()?,
        "notes" => cmd!(sh, "code /home/matklad/notes.adoc").run()?,
        dest => anyhow::bail!("unknown dest: `{}`", dest),
    }
    Ok(())
}
