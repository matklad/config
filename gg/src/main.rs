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

    let context = Context {
        sh: &sh,
        default_branch,
    };

    match flags.subcommand {
        flags::GgCmd::Worktree(worktree) => match worktree.subcommand {
            flags::WorktreeCmd::Add(add) => context.worktree_add(&add.name),
        },
        flags::GgCmd::Amend(flags::Amend) => context.amend(),
    }
}

struct Context<'a> {
    sh: &'a Shell,
    default_branch: &'static str,
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
