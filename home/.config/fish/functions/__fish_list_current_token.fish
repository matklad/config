# Defined in /tmp/fish.2594D5/__fish_list_current_token.fish @ line 2
function __fish_list_current_token --description 'List contents of token under the cursor if it is a directory, otherwise list the contents of the current directory'
	alias ls 'exa -l'
    set val (eval echo (commandline -t))
    printf "\n"
    if test -d $val
        ls $val
    else
        set dir (dirname $val)
        if test $dir != . -a -d $dir
            ls $dir
        else
            ls
        end
    end

    set -l line_count (count (fish_prompt))
    if test $line_count -gt 1
        for x in (seq 2 $line_count)
            printf "\n"
        end
    end

    commandline -f repaint
end
