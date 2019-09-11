function __fish_git_branches
    # This is much faster than using `git branch`,
    # and avoids having to deal with localized "detached HEAD" messages.
    command git for-each-ref --format='%(refname)' refs/heads/ refs/remotes/ 2>/dev/null \
        | string replace -r '^refs/heads/(.*)$' '$1\tLocal Branch' \
        | string replace -r '^refs/remotes/(.*)$' '$1\tRemote Branch'
end

function __fish_git_using_command
    set -l cmd (__fish_git_needs_command)
    test -z "$cmd"
    and return 1
    contains -- $cmd $argv
    and return 0
    # Check aliases.
    set -l varname __fish_git_alias_(string escape --style=var -- $cmd)
    set -q $varname
    and contains -- $$varname $argv
    and return 0
    return 1
end

function __fish_git_needs_command
    # Figure out if the current invocation already has a command.
    # Git has tons of options, but fortunately only a few can appear before the command.
    # They are listed here.
    set -l opts h-help p P-paginate N-no-pager b-bare o-no-replace-objects \
        l-literal-pathspecs g-glob-pathspecs O-noglob-pathspecs i-icase-pathspecs \
        e-exec-path= G-git-dir= c= C= v-version H-html-path \
        m-man-path I-info-path w-work-tree= a-namespace= s-super-prefix=
    set cmd (commandline -opc)
    set -e cmd[1]
    argparse -s $opts -- $cmd 2>/dev/null
    or return 0
    # These flags function as commands, effectively.
    set -q _flag_version; and return 1
    set -q _flag_html_path; and return 1
    set -q _flag_man_path; and return 1
    set -q _flag_info_path; and return 1
    if set -q argv[1]
        # Also print the command, so this can be used to figure out what it is.
        echo $argv[1]
        return 1
    end
    return 0
end

complete -f -c git -n '__fish_git_needs_command' -a switch -d 'Switch to a branch'
complete -k -f -c git -n '__fish_git_using_command switch; and not contains -- -- (commandline -op)' -a '(__fish_git_branches)'
complete -f -c git -n '__fish_git_using_command switch' -s c -l create -d 'Create a new branch'
complete -f -c git -n '__fish_git_using_command switch' -s C -l force-create -d 'Force create a new branch'
complete -f -c git -n '__fish_git_using_command switch' -s d -l detach -d 'Switch to a commit for inspection and discardable experiment'
complete -f -c git -n '__fish_git_using_command switch' -l guess -d 'Guess branch name from remote branch (default)'
complete -f -c git -n '__fish_git_using_command switch' -l no-guess -d 'Do not guess branch name from remote branch'
complete -f -c git -n '__fish_git_using_command switch' -s f -l force -l discard-changes -d 'Proceed even if the index or the working tree differs from HEAD'
complete -f -c git -n '__fish_git_using_command switch' -s m -l merge -d 'Merge the current branch and contents of the working tree into a new branch'
complete -f -c git -n '__fish_git_using_command switch' -s t -l track -d 'Track remote branch when creating a new branch'
complete -f -c git -n '__fish_git_using_command switch' -l no-track -d 'Do not track remote branch when creating a new branch'
complete -f -c git -n '__fish_git_using_command switch' -l orphan -d 'Create a new orphan branch'
complete -f -c git -n '__fish_git_using_command switch' -l ignore-other-worktrees -d 'Force check out of the reference'
complete -f -c git -n '__fish_git_using_command switch' -l recurse-submodules -d 'Update the work trees of submodules'
complete -f -c git -n '__fish_git_using_command switch' -l no-recurse-submodules -d 'Do not update the work trees of submodules'
