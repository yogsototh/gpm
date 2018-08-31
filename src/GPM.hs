{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GPM
where

import           Data.FileEmbed (embedStringFile)
import           Protolude      hiding (die, stdout, (%),fold)
import           Turtle
import qualified Control.Foldl as Fold
import Control.Exception.Base (bracket)

import           GPM.Review     (ReviewCommand (..), handleReview,
                                 parseReviewCmd)

gpm :: IO ()
gpm = do
  subcmd <- options "Git Project Manager" parser
  case subcmd of
   Init             -> init
   NewIssue         -> inGPM newIssue
   Review reviewCmd -> inGPM (handleReview reviewCmd)

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

inGPM :: MonadIO io => IO a -> io ()
inGPM actions = sh $ do
  res <- fold (inshell "git rev-parse --abbrev-ref HEAD" empty) Fold.head
  oldbr <- case res of
    Nothing -> die "Cannot retrieve current branch"
    Just br -> do
           void $ inshell "git stash --all" empty
           void $ inshell "git checkout gpm" empty
           return br
  liftIO $ bracket (return ())
                   (const $ sh $ do
                       void $ inshell ("git checkout " <> lineToText oldbr) empty
                       void $ inshell "git stash pop" empty)
                   (const actions)

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
  output "issues.org" $(embedStringFile "templates/issues.org")
  mktree "templates"
  output ("templates" </> "new-issue.tpl") $(embedStringFile "templates/new-issue.tpl")
  debug "git add issues.org"

initDocs :: IO ()
initDocs = do
  echo "* wiki.org"
  output "wiki.org" $(embedStringFile "templates/wiki.org")
  debug "git add wiki.org"

initReview :: IO ()
initReview = do
  let fic = "reviews" </> "write-contributing-yogsototh.org"
  mktree "reviews"
  putText $ format ("* "%fp) fic
  output fic $(embedStringFile "templates/review.org")
  debug "git add reviews"
