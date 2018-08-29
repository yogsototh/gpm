#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "ghc.withPackages (ps: [ ps.protolude ps.turtle ])"
#! nix-shell -I nixpkgs="https://github.com/NixOS/nixpkgs/archive/16d475334409f7fa632929b2838421b4ffe34927.tar.gz"
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import Protolude hiding (stdout,(%))
import Turtle

main :: IO ()
main = do
  echo "# <GPM> -- Git Project Manager"
  mkNewEmptyBranch "gpm"
  initIssues
  initDocs

mkNewEmptyBranch :: Text -> IO ()
mkNewEmptyBranch br = do
  putText $ "create a new branch " <> br <> " (be sure the branch " <> br <> " doesn't already exists)"
  debug $ "git checkout --orphan " <> br
  echo "cleanup the branch"
  debug "git rm --cached -r ."

debug :: Text -> IO ()
debug cmd = do
  putText cmd
  stdout $ inshell cmd empty

initIssues :: IO ()
initIssues = do
  echo "* issue.org"
  input ("templates" </> "issues.org")
    & output "issues.org"

initDocs :: IO ()
initDocs = do
  echo "* wiki.org"
  input ("templates" </> "wiki.org")
    & output "wiki.org"

initReview :: IO ()
initReview = do
  let fic = "reviews" </> "write-contributing-yogsototh.org"
  mktree "reviews"
  putText $ format ("* "%fp) fic
  input ("templates" </> "review.org")
    & output fic
