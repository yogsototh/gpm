{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
module      : GPM.Init
Description : GPM init command
License     : Public Domain
Maintainer  : yann.esposito@gmail.com
-}
module GPM.Init
  (init)
where

import           Protolude
import           Turtle

import           GPM.Helpers (debug)
import qualified GPM.Docs    as Docs
import qualified GPM.Issue   as Issue
import qualified GPM.Review  as Review

init :: IO ()
init = do
  echo "# <GPM> -- Git Project Manager"
  mkNewEmptyBranch "gpm"
  Issue.init
  Docs.init
  Review.init
  debug "git commit -m 'gpm initialized'"
  debug "git checkout master"

mkNewEmptyBranch :: Text -> IO ()
mkNewEmptyBranch br = do
  putText $ "create a new branch " <> br <> " (be sure the branch " <> br <> " doesn't already exists)"
  debug $ "git checkout --orphan " <> br
  echo "cleanup the branch"
  debug "git rm --cached -r ."

