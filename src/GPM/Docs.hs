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

import           Data.FileEmbed (embedStringFile)

import           GPM.Helpers    (debug_,green)

init :: IO ()
init = do
  green "* wiki.org"
  writeFile "wiki.org" $(embedStringFile "templates/wiki.org")
  debug_ "git add wiki.org"
