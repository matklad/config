use xshell::cmd;

pub(crate) fn run() -> anyhow::Result<()> {
    cmd!("git fetch upstream").read()?;
    let commits = cmd!("git cherry upstream/master").read()?;
    let commits: Vec<&str> = commits.lines().filter_map(|it| it.strip_prefix("+ ")).collect();

    let (title, mut body) = if commits.len() == 1 {
        let message = get_message(commits[0])?;
        split_subject(&message)
    } else {
        let all_messages =
            commits.into_iter().map(get_message).collect::<anyhow::Result<Vec<_>>>()?.join("\n\n");

        let edited_message = {
            let tmpd = xshell::mktemp_d()?;
            let message_file = tmpd.path().join("PR_EDIT_MESSAGE");
            xshell::write_file(&message_file, all_messages)?;
            cmd!("micro {message_file}").run()?;
            xshell::read_file(&message_file)?
        };

        split_subject(edited_message.trim())
    };

    if !body.is_empty() {
        body.push_str("\n\n");
    }
    body.push_str("bors r+\n🤖");

    cmd!("git push --set-upstream origin").run()?;
    cmd!("gh pr create --title {title} --body {body}").run()?;

    Ok(())
}

fn get_message(commit_hash: &str) -> anyhow::Result<String> {
    let res = cmd!("git show -s --format='%s\n%b' {commit_hash}").read()?;
    Ok(res)
}

fn split_subject(message: &str) -> (String, String) {
    let subject = message.lines().next().unwrap().to_string();
    let body = message.lines().skip(1).collect::<Vec<_>>().join("\n").trim().to_string();
    (subject, body)
}
