{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
module      : GPM.Init
Description : GPM init command
License     : Public Domain
Maintainer  : yann.esposito@gmail.com
-}
module GPM.Init
  (init)
where

import           Protolude     hiding (die,fold)
import           Turtle

import qualified Control.Foldl as Fold

import qualified GPM.Docs      as Docs
import           GPM.Helpers   (debug_)
import qualified GPM.Issue     as Issue
import qualified GPM.Review    as Review

-- | Init a repository with a new empty branch named @gpm@
init :: IO ()
init = do
  echo "# <GPM> -- Git Project Manager"
  mkNewEmptyBranch "gpm"
  Issue.init
  Docs.init
  Review.init
  debug_ "git commit -m 'gpm initialized'"
  debug_ "git checkout master"

-- | Create a new empty branch, fail if the branch already exists
mkNewEmptyBranch :: Text -> IO ()
mkNewEmptyBranch br = sh $ do
  clean <- fold (inshell "git status --porcelain" empty) Fold.null
  if clean
    then liftIO $ do
      putText $ "create a new branch " <> br <> " (be sure the branch " <> br <> " doesn't already exists)"
      debug_ $ "git checkout --orphan " <> br
      echo "cleanup the branch"
      debug_ "git rm --cached -r ."
      debug_ "git clean -fd"
    else die "Please take care of pending modification and untracked files (you can use git stash --all)"

