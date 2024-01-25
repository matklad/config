use anyhow::Context as _;
use xshell::{cmd, Shell};

mod flags {
    xflags::xflags! {
        cmd gg {
            cmd worktree {
                cmd add {
                    required name: String
                }
                cmd clone {
                    required remote: String
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
                optional --to branch: String
            }
            /// Rebases the branch on top of the main branch.
            cmd refresh {
                /// Don't rebase commits before this one (for stacked PRs).
                optional --from commit: String
                /// Use this branch instead of main.
                optional --to branch: String
            }
        }
    }
}

type Result<T = ()> = anyhow::Result<T>;

fn main() -> Result {
    let flags = flags::Gg::from_env_or_exit();

    let sh = Shell::new()?;
    let main_branch = {
        let branches = cmd!(sh, "git branch")
            .read()
            .unwrap_or_else(|_| format!("master"));
        if branches.contains(" master") {
            "master"
        } else {
            "main"
        }
    };

    let remote = {
        let remotes = cmd!(sh, "git remote")
            .read()
            .unwrap_or_else(|_| format!("origin"));
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
            flags::WorktreeCmd::Clone(clone) => context.worktree_clone(&clone.remote),
        },
        flags::GgCmd::Amend(flags::Amend) => context.amend(),
        flags::GgCmd::Branch(branch) => context.branch(&branch.name),
        flags::GgCmd::Commit(commit) => {
            context.commit(commit.message.as_deref(), commit.branch.as_deref())
        }
        flags::GgCmd::Sync(sync) => context.sync(sync.to.as_deref()),
        flags::GgCmd::Refresh(refresh) => {
            context.refresh(refresh.from.as_deref(), refresh.to.as_deref())
        }
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

    fn worktree_clone(&self, remote: &str) -> Result {
        let (_, dir_name) = remote
            .rsplit_once('/')
            .with_context(|| format!("can't determine directory name"))?;
        let dir_name = dir_name.rsplit_once('.').map_or(dir_name, |it| it.0);

        self.sh.create_dir(dir_name)?;
        let _dir = self.sh.push_dir(dir_name);
        cmd!(self.sh, "git clone --bare {remote} .bare").run()?;
        self.sh.write_file(".git", "gitdir: ./.bare")?;
        cmd!(
            self.sh,
            "git config remote.origin.fetch '+refs/heads/*:refs/remotes/origin/*'"
        )
        .run()?;
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

    fn sync(&self, branch: Option<&str>) -> Result {
        let remote = self.remote;
        let branch = match branch {
            Some(branch) => branch.to_string(),
            None => cmd!(self.sh, "git rev-parse --abbrev-ref HEAD").read()?,
        };
        cmd!(self.sh, "git fetch {remote} {branch}").run()?;
        cmd!(self.sh, "git reset --hard {remote}/{branch}").run()?;
        Ok(())
    }

    fn refresh(&self, from: Option<&str>, to: Option<&str>) -> Result {
        let to = match to {
            Some(to) => to.to_string(),
            None => {
                let remote = self.remote;
                let branch = self.main_branch;
                cmd!(self.sh, "git fetch {remote} {branch}").run()?;
                format!("{remote}/{branch}")
            }
        };
        match from {
            None => cmd!(self.sh, "git rebase {to}"),
            Some(commit) => cmd!(self.sh, "git rebase --onto {to} {commit}^"),
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
