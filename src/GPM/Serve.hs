{-# LANGUAGE NoImplicitPrelude #-}
{-|
module      : GPM.Serve
Description : GPM review related commands
License     : Public Domain
Maintainer  : yann.esposito@gmail.com
-}
module GPM.Serve
  ( init
  )
where

import           Protolude      hiding (ask, die, (%),stdout)
import           Turtle

import           GPM.Helpers    (getGPMDataDir, debug, debug_)

getPublicDir :: IO Turtle.FilePath
getPublicDir = do
  gpmDataDir <- getGPMDataDir
  let publicdir = gpmDataDir </> "public"
  return publicdir

getPublicPrjDir :: IO Turtle.FilePath
getPublicPrjDir = do
  publicdir <- getPublicDir
  mReporoot <- debug "git rev-parse --show-toplevel"
  case mReporoot of
    Just reporoot -> do
      let projectName = dirname (fromString (toS reporoot))
      return (publicdir </> projectName)
    Nothing -> die "You don't appear to be in a git repository."

inDir :: MonadIO m => Turtle.FilePath -> m a -> m a
inDir workDir action = do
  currPwd <- pwd
  cd workDir
  res <- action
  cd currPwd
  return res


-- | init gpm branch to handle reviews
init :: IO ()
init = do
  echo "* server init"
  publicdir <- getPublicDir
  mktree publicdir
  debug_ "git init ."
  output (publicdir </> ".git" </> "description") "Main repositories"
  publicProjectDir <- getPublicPrjDir
  inDir publicProjectDir $ do
    mv ("hooks" </> "post-update.sample") ("hooks" </> "post-update")
    _ <- chmod executable ("hooks" </> "post-update")
    debug_ "git update-server-info"

handleUpdate :: IO ()
handleUpdate = do
  pubPrjDir <- getPublicPrjDir
  inDir pubPrjDir $
    debug_ "git pull"

handleServe :: IO ()
handleServe = do
  pubDir <- getPublicDir
  inDir pubDir $
    debug_ "git instaweb --http=webrick"
