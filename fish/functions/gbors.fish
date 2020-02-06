function gbors
    set -l message (git show -s --format=%B HEAD)
    if set -q argv[1]
        set message $argv
    end

    set -l pr (git-hub pull new -m $message | rg '.*\[(\d+)\].*' -r '$1')
    echo "created pr #$pr"
    git-hub pull comment -m"bors r+
🤖" $pr
end
