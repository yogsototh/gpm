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
  , getGPMDataDir
  , getGitUser
  , inGPM
  , inDir
  , red
  , green
  , yellow
  )
where

import           Protolude           hiding (die)
import           Turtle

import qualified Control.Foldl       as Fold
import qualified System.Console.ANSI as Console
import qualified System.Directory    as Directory
import qualified System.IO

-- | execute a shell script and return the last line as text
-- but also log the command to the console to minimize surprise
debug :: Text -> IO (Maybe Text)
debug cmd = do
  Console.setSGR [ Console.SetColor Console.Foreground Console.Dull Console.Cyan
                 , Console.SetItalicized True
                 ]
  putErrText ("    " <> cmd)
  Console.setSGR [ Console.Reset ]
  System.IO.hFlush System.IO.stderr
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
getGPMDataDir :: IO Turtle.FilePath
getGPMDataDir = fromString <$> Directory.getXdgDirectory Directory.XdgData "gpm"

-- | Perform some actions in some directory.
-- Take care of returning the the previous directory after the action are finished
inDir :: MonadIO m => Turtle.FilePath -> m a -> m a
inDir workDir action = do
  currPwd <- pwd
  cd workDir
  res <- action
  cd currPwd
  return res


putTextColor :: Console.Color -> Text -> IO ()
putTextColor color t = do
  Console.setSGR [ Console.SetColor Console.Foreground Console.Dull color ]
  putText t
  Console.setSGR [ Console.Reset ]
  System.IO.hFlush System.IO.stdout

green :: Text -> IO ()
green = putTextColor Console.Green

yellow :: Text -> IO ()
yellow = putTextColor Console.Yellow

red :: Text -> IO ()
red = putTextColor Console.Red
