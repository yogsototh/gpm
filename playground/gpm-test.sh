#!/usr/bin/env zsh

[[ -d testproj ]] && rm -rf testproj
mkdir testproj
pushd testproj
echo "README 1" > README
git init .
git add README
git commit -m "initial commit"
gpm init
gpm new-issue -t "issue-1" -p "A"
git co gpm
cp hooks/prepare-commit-msg{.sample,}
git add hooks
git commit -m "updated the prepare-commit-msg git hook"
git co master
gpm hooks sync
popd
