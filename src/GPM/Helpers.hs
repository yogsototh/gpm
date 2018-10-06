{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
module      : GPM.Helpers
Description : GPM helper functions
License     : Public Domain
Maintainer  : yann.esposito@gmail.com
-}
module GPM.Helpers
  ( debug
  , debug_
  , getCurrentGitBranch
  , getGPMCacheDir
  , getGitUser
  , inGPM
  )
where

import qualified Control.Foldl    as Fold
import           Protolude        hiding (die)
import qualified System.Directory as Directory
import           Turtle

-- | execute a shell script and return the last line as text
-- but also log the command to the console to minimize surprise
debug :: Text -> IO (Maybe Text)
debug cmd = do
  putText cmd
  fmap lineToText <$> _foldIO (inshell cmd empty) (Fold.generalize Fold.last)

-- | execute a shell script without stdin and without handling output
debug_ :: Text -> IO ()
debug_ = void . debug

getCurrentGitBranch :: IO (Maybe Text)
getCurrentGitBranch = debug "git rev-parse --abbrev-ref HEAD"

getGitUser :: IO (Maybe Text)
getGitUser = debug "git config user.name"

-- | Ensure actions occurs in the @gpm@ branch
-- and returns to current branch with also all untracked files
inGPM :: (Text -> IO a) -> IO a
inGPM = bracket safeChangeBranch safeReturnBranch
  where
    safeChangeBranch = do
      res <- getCurrentGitBranch
      case res of
        Nothing -> die "Cannot retrieve current branch"
        Just br -> do
               debug_ "git stash --all"
               debug_ "git checkout gpm"
               return br
    safeReturnBranch oldbr = do
      debug_ ("git checkout " <> oldbr)
      debug_ "git stash pop"

-- | Retrieve the cache directory to save temporary files in gpm
getGPMCacheDir :: IO Turtle.FilePath
getGPMCacheDir = fromString <$> Directory.getXdgDirectory Directory.XdgCache "gpm"
