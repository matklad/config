function gpr
    set -l pr $argv[1]
    git fetch upstream pull/$pr/head:pr-$pr
    git checkout pr-$pr
end
