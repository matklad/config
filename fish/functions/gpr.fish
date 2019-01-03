function gpr
    set -l pr $argv[1]
    if git remote | rg upstream
        git fetch upstream pull/$pr/head:pr-$pr
    else
        git fetch origin pull/$pr/head:pr-$pr
    end
    git checkout pr-$pr
end
