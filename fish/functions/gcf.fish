function gcf
    set -l userrepo $argv[1]
    set -l repo (basename $userrepo)
    git clone git@github.com:matklad/$repo.git
    and begin
        pushd $repo
        git remote add upstream git@github.com:$userrepo.git
        and git fetch upstream
        and git checkout master
        and git reset --hard upstream/master
        and git branch --set-upstream-to=upstream/master
	and printf "[hub]
\tupstream = \"$userrepo\"
\tforkrepo = \"matklad/$repo\"
\tforkremote = \"origin\"
" >> .git/config
        popd
    end
end
