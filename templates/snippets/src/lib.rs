use std::hash::Hasher;

/// ```
/// let commit_hash = exec("git rev-parse HEAD")?;
/// ```
pub fn exec(command: &str) -> std::io::Result<String> {
    let args = command.split_ascii_whitespace().collect::<Vec<_>>();
    let (cmd, args) = args.split_first().unwrap();
    let output = std::process::Command::new(cmd).args(args).output()?;
    if !output.status.success() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("command {:?} returned non-zero code", command),
        ));
    }
    let stdout = String::from_utf8(output.stdout)
        .map_err(|err| std::io::Error::new(std::io::ErrorKind::InvalidData, err))?;
    Ok(stdout.trim().to_string())
}

pub fn stable_hash<T: std::hash::Hash>(value: &T) -> u64 {
    #![allow(deprecated)]
    let mut hasher = std::hash::SipHasher::default();
    std::hash::Hash::hash(value, &mut hasher);
    hasher.finish()
}

// Source:
// <https://github.com/rust-lang/rust/blob/1.55.0/library/core/src/slice/sort.rs#L559-L573>
pub fn random_numbers() -> impl Iterator<Item = u32> {
    let mut random = 92u32;
    std::iter::repeat_with(move || {
        random ^= random << 13;
        random ^= random >> 17;
        random ^= random << 5;
        random
    })
}

#[must_use]
pub fn timeit(label: impl Into<String>) -> impl Drop {
    let label = label.into();
    let now = std::time::Instant::now();
    defer(move || {
        eprintln!("{}: {:.2?}", label, now.elapsed())
    })
}

#[must_use]
pub fn defer<F: FnOnce()>(f: F) -> impl Drop {
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
