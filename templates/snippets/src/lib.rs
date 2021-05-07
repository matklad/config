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
