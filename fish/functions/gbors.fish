function gbors
    set -l pr (git-hub pull new -m $argv[1] | rg '.*\[(\d+)\].*' -r '$1')
    echo "created pr #$pr"
    git-hub pull comment -m"bors r+
🤖" $pr
end
