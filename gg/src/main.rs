use xshell::{cmd, Shell};

mod flags {
    xflags::xflags! {
        cmd gg {
            cmd worktree {
                cmd add {
                    required name: String
                }
            }
            /// Adds changes to the topmost commit.
            cmd amend {
            }
            /// Creates a new branch in a synced state.
            cmd branch {
                required name: String
            }
            cmd commit {
                /// Commit message.
                optional message: String
                /// Move all changes out of the way as a commit onto a new branch.
                optional -b,--branch branch: String
            }
            /// Syncs the branch to the remote counterpart.
            cmd sync {
            }
            /// Rebases the branch on top of the main branch.
            cmd refresh {
                /// Don't rebase commits before this one (for stacked PRs).
                optional --from commit: String
            }
        }
    }
}

type Result<T = ()> = anyhow::Result<T>;

fn main() -> Result {
    let flags = flags::Gg::from_env_or_exit();

    let sh = Shell::new()?;
    let main_branch = {
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
        main_branch,
        remote,
    };

    match flags.subcommand {
        flags::GgCmd::Worktree(worktree) => match worktree.subcommand {
            flags::WorktreeCmd::Add(add) => context.worktree_add(&add.name),
        },
        flags::GgCmd::Amend(flags::Amend) => context.amend(),
        flags::GgCmd::Branch(branch) => context.branch(&branch.name),
        flags::GgCmd::Commit(commit) => {
            context.commit(commit.message.as_deref(), commit.branch.as_deref())
        }
        flags::GgCmd::Sync(flags::Sync) => context.sync(),
        flags::GgCmd::Refresh(refresh) => context.refresh(refresh.from.as_deref()),
    }
}

struct Context<'a> {
    sh: &'a Shell,
    main_branch: &'static str,
    remote: &'static str,
}

impl<'a> Context<'a> {
    fn worktree_add(&self, name: &str) -> Result {
        let commit = self.main_branch;
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
        let main_branch = self.main_branch;
        cmd!(self.sh, "git fetch {remote} {main_branch}").run()?;
        cmd!(self.sh, "git switch --create {name} {remote}/{main_branch}").run()?;
        Ok(())
    }

    fn commit(&self, message: Option<&str>, branch: Option<&str>) -> Result {
        let message = message.unwrap_or(".");
        cmd!(self.sh, "git add --all").run()?;
        cmd!(self.sh, "git --no-pager diff --cached --color=always").run()?;
        match branch {
            Some(branch) => {
                cmd!(self.sh, "git switch -c {branch}").run()?;
                cmd!(self.sh, "git commit -m {message}").run()?;
                cmd!(self.sh, "git switch -").run()?;
            }
            None => cmd!(self.sh, "git commit -m {message}").run()?,
        }
        Ok(())
    }

    fn sync(&self) -> Result {
        let remote = self.remote;
        let branch = cmd!(self.sh, "git rev-parse --abbrev-ref HEAD").read()?;
        cmd!(self.sh, "git fetch {remote} {branch}").run()?;
        cmd!(self.sh, "git reset --hard {remote}/{branch}").run()?;
        Ok(())
    }

    fn refresh(&self, from: Option<&str>) -> Result {
        let remote = self.remote;
        let branch = self.main_branch;
        cmd!(self.sh, "git fetch {remote} {branch}").run()?;
        match from {
            None => cmd!(self.sh, "git rebase {remote}/{branch}"),
            Some(commit) => cmd!(self.sh, "git rebase --onto {remote}/{branch} {commit}^"),
        }
        .run()?;
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
