mod gbors;

const TOOLS: &[(&str, fn() -> anyhow::Result<()>)] = &[("gbors", gbors::run)];

fn main() -> anyhow::Result<()> {
    let progn = std::env::args_os().next().unwrap_or_default();
    let progn = progn.to_str().unwrap_or_default();

    let (_name, run) = TOOLS
        .iter()
        .find(|&&(name, _run)| name == progn)
        .ok_or_else(|| anyhow::format_err!("unknown tool: `{}`", progn))?;

    run()
}

#[test]
fn link_me_up() {
    let bin = std::path::Path::new("../bin");

    xshell::cmd!("cargo build --release").run().unwrap();

    for &(tool, _) in TOOLS {
        let dst = bin.join(tool);
        xshell::rm_rf(&dst).unwrap();
        xshell::hard_link("./target/release/tool", dst).unwrap();
    }
}
