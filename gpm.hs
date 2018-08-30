#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "ghc.withPackages (ps: [ ps.protolude ps.turtle ])"
#! nix-shell -I nixpkgs="https://github.com/NixOS/nixpkgs/archive/16d475334409f7fa632929b2838421b4ffe34927.tar.gz"
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import Protolude hiding (stdout,(%),die)
import Turtle

main :: IO ()
main = do
  x <- options "Git Project Manager" parser
  case x of
   Init -> init
   NewIssue -> newIssue
   StartReview br -> startReview br
   EndReview br -> endReview br

data Command = Init
             | NewIssue
             | StartReview (Maybe Text)
             | EndReview (Maybe Text)
             deriving (Eq)

parser :: Parser Command
parser = subcommand "init" "Initialize gpm" (pure Init)
         <|> subcommand "new-issue" "Create a new Issue" (pure NewIssue)
         <|> StartReview <$> subcommand "start-review" "Start review (use current branch by default)"
                             (optional (argText "branch" "The git branch to review"))
         <|> EndReview   <$> subcommand "end-review" "End review (use current branch by default)"
                             (optional (argText "branch" "The git branch to end review"))

newIssue :: IO ()
newIssue = die "TODO"

startReview :: Maybe Text -> IO ()
startReview br = die "TODO"

endReview :: Maybe Text -> IO ()
endReview br = die "TODO"

init :: IO ()
init = do
  echo "# <GPM> -- Git Project Manager"
  mkNewEmptyBranch "gpm"
  initIssues
  initDocs
  debug "git commit -m 'gpm initialized'"
  debug "git checkout master"

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
  debug "git add issues.org"

initDocs :: IO ()
initDocs = do
  echo "* wiki.org"
  input ("templates" </> "wiki.org")
    & output "wiki.org"
  debug "git add wiki.org"

initReview :: IO ()
initReview = do
  let fic = "reviews" </> "write-contributing-yogsototh.org"
  mktree "reviews"
  putText $ format ("* "%fp) fic
  input ("templates" </> "review.org")
    & output fic
  debug "git add reviews"
