function gpr
    set -l pr $argv[1]
    git fetch upstream pull/$pr/head && git switch --detach FETCH_HEAD
end
