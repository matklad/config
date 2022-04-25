use xshell::{cmd, Shell};

use crate::opt_single_arg;

pub(crate) fn run(sh: &Shell) -> anyhow::Result<()> {
    let title_arg = opt_single_arg()?;

    cmd!(sh, "git fetch upstream").read()?;
    let commits = cmd!(sh, "git cherry upstream/master").read()?;
    let commits: Vec<&str> = commits.lines().filter_map(|it| it.strip_prefix("+ ")).collect();

    let (title, mut body) = if let Some(title) = title_arg {
        (title, String::new())
    } else if commits.len() == 1 {
        let message = get_message(sh, commits[0])?;
        split_subject(&message)
    } else {
        let all_messages = commits
            .into_iter()
            .map(|it| get_message(sh, it))
            .collect::<anyhow::Result<Vec<_>>>()?
            .join("\n\n");

        let edited_message = {
            let tmpd = sh.create_temp_dir()?;
            let message_file = tmpd.path().join("PR_EDIT_MESSAGE");
            sh.write_file(&message_file, all_messages)?;
            cmd!(sh, "micro {message_file}").run()?;
            sh.read_file(&message_file)?
        };

        split_subject(edited_message.trim())
    };

    if !body.is_empty() {
        body.push_str("\n\n");
    }
    body.push_str("bors r+\n🤖");

    cmd!(sh, "git push --set-upstream origin").run()?;
    cmd!(sh, "gh pr create --title {title} --body {body}").run()?;

    Ok(())
}

fn get_message(sh: &Shell, commit_hash: &str) -> anyhow::Result<String> {
    let res = cmd!(sh, "git show -s --format='%s\n%b' {commit_hash}").read()?;
    Ok(res)
}

fn split_subject(message: &str) -> (String, String) {
    let subject = message.lines().next().unwrap().to_string();
    let body = message.lines().skip(1).collect::<Vec<_>>().join("\n").trim().to_string();
    (subject, body)
}
