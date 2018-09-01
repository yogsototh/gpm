{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-|
module      : GPM.Helpers
Description : GPM helper functions
License     : Public Domain
Maintainer  : yann.esposito@gmail.com
-}
module GPM.Helpers
  (debug,inGPM)
where

import           Protolude     hiding (stdout,fold,die)
import           Turtle

import qualified Control.Foldl as Fold

debug :: Text -> IO ()
debug cmd = do
  putText cmd
  stdout $ inshell cmd empty

-- | Ensure actions occurs in the @gpm@ branch
-- and returns to current branch with also all untracked files
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
