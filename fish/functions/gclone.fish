function gclone
    set -l userrepo $argv[1]
    git clone git@github.com:$userrepo.git
end
