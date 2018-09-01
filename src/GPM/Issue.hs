{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
module      : GPM.Issue
Description : GPM new issue related command
License     : Public Domain
Maintainer  : yann.esposito@gmail.com
-}
module GPM.Issue
  ( init
  , newIssue
  )
where

import           Protolude      hiding (die)
import           Turtle

import           Data.FileEmbed (embedStringFile)

newIssue :: IO ()
newIssue = die "TODO"

init :: IO ()
init = do
  echo "* issue.org"
  output "issues.org" $(embedStringFile "templates/issues.org")

