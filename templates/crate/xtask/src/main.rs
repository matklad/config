#[cfg(test)]
mod tidy;

use std::time::Instant;

use xshell::{cmd, Shell};

fn main() -> xshell::Result<()> {
    let sh = Shell::new()?;

    cmd!(sh, "rustup toolchain install stable --no-self-update").run()?;
    let _e = sh.push_env("RUSTUP_TOOLCHAIN", "stable");
    cmd!(sh, "rustc --version").run()?;

    {
        let _s = section("BUILD");
        cmd!(sh, "cargo test --workspace --no-run").run()?;
    }

    {
        let _s = section("TEST");
        cmd!(sh, "cargo test --workspace -- --nocapture").run()?;
    }

    {
        let _s = section("PUBLISH");

        let version = cmd!(sh, "cargo pkgid").read()?.rsplit_once('#').unwrap().1.to_string();
        let tag = format!("v{version}");

        let current_branch = cmd!(sh, "git branch --show-current").read()?;
        let tag_exists =
            cmd!(sh, "git tag --list").read()?.split_ascii_whitespace().any(|it| it == &tag);

        if current_branch == "master" && !tag_exists {
            cmd!(sh, "git tag v{version}").run()?;
            cmd!(sh, "cargo publish").run()?;
            cmd!(sh, "git push --tags").run()?;
        }
    }
    Ok(())
}

fn section(name: &'static str) -> impl Drop {
    println!("::group::{name}");
    let start = Instant::now();
    defer(move || {
        let elapsed = start.elapsed();
        eprintln!("{name}: {elapsed:.2?}");
        println!("::endgroup::");
    })
}

fn defer<F: FnOnce()>(f: F) -> impl Drop {
    struct D<F: FnOnce()>(Option<F>);
    impl<F: FnOnce()> Drop for D<F> {
        fn drop(&mut self) {
            if let Some(f) = self.0.take() {
                f()
            }
        }
    }
    D(Some(f))
}
