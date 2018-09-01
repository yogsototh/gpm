{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-|
module      : GPM.Docs
Description : GPM docs commands
License     : Public Domain
Maintainer  : yann.esposito@gmail.com
-}
module GPM.Docs
  (init)
where

import           Protolude
import           Turtle

import           Data.FileEmbed (embedStringFile)

import           GPM.Helpers    (debug_)

init :: IO ()
init = do
  echo "* wiki.org"
  output "wiki.org" $(embedStringFile "templates/wiki.org")
  debug_ "git add wiki.org"
