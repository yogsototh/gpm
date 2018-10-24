#!/usr/bin/env zsh

title(){
    set +x
    echo
    print -- "  $*  "|sed 's/./=/g'
    print -- "  $*  "
    print -- "  $*  "|sed 's/./=/g'
    echo
    set -x
}
subtitle() {
    set +x
    echo
    print -- "$*"
    print -- "$*"|sed 's/./-/g'
    echo
    set -x
}

prjname="testproj"
playgrounddir="/tmp/gpm-playground"
testproj="$playgrounddir/$prjname"
clonedproj="$playgrounddir/cloned-$prjname"

# display all commands
set -x

# ------------------------------------------------------------------------------
title "INIT"

subtitle "Cleaning Up"
[[ -d $testproj ]] && rm -rf $testproj
[[ -d $clonedproj ]] && rm -rf $clonedproj
[[ -d ~/.local/share/gpm ]] && rm -rf ~/.local/share/gpm
gpm serve stop

subtitle "Create Project $testproj"
mkdir -p $testproj
pushd $testproj
echo "README 1" > README
git init .
git add README
git commit -m "initial commit"

subtitle "gpm init"
gpm init

# ------------------------------------------------------------------------------
title "ISSUES"

subtitle "gpm new-issue"
gpm new-issue -t "better README" -p "A" -b better-readme -t doc

# ------------------------------------------------------------------------------
title "HOOKS"

subtitle "Change Some Hooks"
git co gpm
cp hooks/prepare-commit-msg{.sample,}
git add hooks
git commit -m "updated the prepare-commit-msg git hook"
git co master

subtitle "gpm hooks sync"
gpm hooks sync

# ------------------------------------------------------------------------------
title "SERVE"

subtitle "gpm serve start"
gpm serve update
gpm serve start
popd

# ------------------------------------------------------------------------------
title "CLONE"

mkdir -p ${clonedproj:h}
pushd ${clonedproj:h}
subtitle "git clone"
git clone git://localhost:9418/${prjname}.git $clonedproj

# - - - - - - - - - - - - - - - - - - - - - - - -
pushd $clonedproj # Cloned project
subtitle "gpm init (into the cloned repo)"
gpm init

subtitle "Pull request"
git checkout -b better-readme
echo "Some Edit" >> README
git add README
git commit -m "made a better README"

popd
# - - - - - - - - - - - - - - - - - - - - - - - -

subtitle "Review"

pushd $testproj
git remote add dev $clonedproj

git fetch dev
git checkout better-readme

# look into README
# better with org-anotate-file `SPC o a`
# gpm review start
# gpm review end
gpm review request-change -t "add more infos please"

popd

# - - - - - - - - - - - - - - - - - - - - - - - -
pushd $clonedproj
gpm review retrieve
echo "More infos" >> README
git add README
git commit -m "added more infos"
popd
# - - - - - - - - - - - - - - - - - - - - - - - -

pushd $testproj
git checkout better-readme
git pull
git review accept -t "LGTM"
git co master
git merge better-readme
gpm serve update
popd

# - - - - - - - - - - - - - - - - - - - - - - - -
pushd $clonedproj
git co master
git pull
popd
# - - - - - - - - - - - - - - - - - - - - - - - -

# ------------------------------------------------------------------------------
title "STOP SERVE"

subtitle "gpm serve stop"
gpm serve stop
popd

set +x
echo "------------------------------------"
echo "$testproj"
echo "$clonedproj"
