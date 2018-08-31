{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module GPM
where

import Protolude hiding (stdout,(%),die)
import Turtle

import GPM.Review (handleReview,ReviewCommand(..),parseReviewCmd)

gpm :: IO ()
gpm = do
  subcmd <- options "Git Project Manager" parser
  case subcmd of
   Init -> init
   NewIssue -> newIssue
   Review reviewCmd -> handleReview reviewCmd

data Command = Init
             | NewIssue
             | Review ReviewCommand
             deriving (Eq)

parser :: Parser Command
parser = subcommand "init" "Initialize gpm" (pure Init)
         <|> subcommand "new-issue" "Create a new Issue" (pure NewIssue)
         <|> Review <$> subcommand "review"
                         "Review (use current branch by default)"
                         parseReviewCmd

newIssue :: IO ()
newIssue = die "TODO"

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
