use xshell::{cmd, Shell};

mod flags {
    xflags::xflags! {
        cmd gg {
            cmd worktree {
                cmd add {
                    required name: String
                }
            }
            cmd amend {

            }
            cmd branch {
                required name: String
            }
            cmd sync {

            }
        }
    }
}

type Result<T = ()> = anyhow::Result<T>;

fn main() -> Result {
    let flags = flags::Gg::from_env_or_exit();

    let sh = Shell::new()?;
    let default_branch = {
        let branches = cmd!(sh, "git branch").read()?;
        if branches.contains(" master") {
            "master"
        } else {
            "main"
        }
    };

    let remote = {
        let remotes = cmd!(sh, "git remote").read()?;
        if remotes.contains("upstream") {
            "upstream"
        } else {
            "origin"
        }
    };

    let context = Context {
        sh: &sh,
        default_branch,
        remote,
    };

    match flags.subcommand {
        flags::GgCmd::Worktree(worktree) => match worktree.subcommand {
            flags::WorktreeCmd::Add(add) => context.worktree_add(&add.name),
        },
        flags::GgCmd::Amend(flags::Amend) => context.amend(),
        flags::GgCmd::Branch(branch) => context.branch(&branch.name),
        flags::GgCmd::Sync(flags::Sync) => context.sync(),
    }
}

struct Context<'a> {
    sh: &'a Shell,
    default_branch: &'static str,
    remote: &'static str,
}

impl<'a> Context<'a> {
    fn worktree_add(&self, name: &str) -> Result {
        let commit = self.default_branch;
        cmd!(self.sh, "git worktree add ./{name} {commit} --detach").run()?;
        Ok(())
    }

    fn amend(&self) -> Result {
        cmd!(self.sh, "git add .").run()?;
        cmd!(self.sh, "git --no-pager diff --cached --color=always").run()?;
        cmd!(
            self.sh,
            "git --no-pager show -s --oneline --abbrev-commit --color=always HEAD"
        )
        .run()?;

        if yes_or_no("Continue?") {
            cmd!(self.sh, "git commit --amend --no-edit").run()?;
        }
        Ok(())
    }

    fn branch(&self, name: &str) -> Result {
        let remote = self.remote;
        let default_branch = self.default_branch;
        cmd!(self.sh, "git fetch {remote} {default_branch}").run()?;
        cmd!(
            self.sh,
            "git switch --create {name} {remote}/{default_branch}"
        )
        .run()?;
        Ok(())
    }

    fn sync(&self) -> Result {
        let remote = self.remote;
        let branch = cmd!(self.sh, "git rev-parse --abbrev-ref HEAD").read()?;
        cmd!(self.sh, "git fetch {remote} {branch}").run()?;
        cmd!(self.sh, "git reset --hard {remote}/{branch}").run()?;
        Ok(())
    }
}

fn yes_or_no(msg: &str) -> bool {
    println!("{msg}");
    let mut buf = String::new();
    match std::io::stdin().read_line(&mut buf) {
        Err(_) | Ok(0) => false,
        Ok(_) => {
            let resp = buf.trim();
            matches!(resp, "" | "y" | "Y")
        }
    }
}
