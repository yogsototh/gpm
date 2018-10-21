#!/usr/bin/env zsh

set -x

title(){
    set +x
    echo
    print -- "----------------------------------------"
    print -- "          $*"
    print -- "----------------------------------------"
    echo
    set -x
}

prjname="testproj"
testproj="/tmp/$prjname"
testproj2="/tmp/testproj2"
mkdir -p $testproj
pushd $testproj

title "CLEANING UP"
[[ -d $testproj ]] && rm -rf $testproj
[[ -d $testproj2 ]] && rm -rf $testproj2
title "CREATE PROJECT $testproj"
mkdir $testproj
pushd $testproj
echo "README 1" > README
git init .
git add README
git commit -m "initial commit"
title "gpm init"
gpm init
title "gpm new-issue"
gpm new-issue -t "issue-1" -p "A"
title "HOOKS"
git co gpm
cp hooks/prepare-commit-msg{.sample,}
git add hooks
git commit -m "updated the prepare-commit-msg git hook"
git co master
title "gpm hooks sync"
gpm hooks sync
title "gpm serve start"
gpm serve start
popd
mkdir $testproj2
pushd $testproj2
git clone http://localhost:3000/${prjname}.git $testproj2
gpm serve stop
popd

popd

echo
echo "--------"
echo "$testproj"
echo "$testproj2"
