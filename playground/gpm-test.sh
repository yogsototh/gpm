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
testproj2="$playgrounddir/testproj2"

# display all commands
set -x

# ------------------------------------------------------------------------------
title "INIT"

subtitle "Cleaning Up"
[[ -d $testproj ]] && rm -rf $testproj
[[ -d $testproj2 ]] && rm -rf $testproj2
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
gpm new-issue -t "issue-1" -p "A"

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
mkdir $testproj2
pushd $testproj2

subtitle "git clone"
git clone git://localhost:9418/${prjname}.git $testproj2

subtitle "gpm init (into the cloned repo)"
gpm init

subtitle "gpm serve stop"
gpm serve stop
popd

set +x
echo "------------------------------------"
echo "$testproj"
echo "$testproj2"
