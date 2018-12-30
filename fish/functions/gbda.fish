function gbda
    set -l merged (git branch --merged | rg -v 'master|\*')
    if test -z "$merged"
        echo "no merged branches"
    else
        echo $merged | xargs git branch -d
    end
end
