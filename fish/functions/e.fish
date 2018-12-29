function e
    emacsclient --create-frame \
        --socket-name=/tmp/emacs1000/server \
        --alternate-editor='' \
        $argv
end
