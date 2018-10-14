{-# LANGUAGE NoImplicitPrelude #-}
{-|
module      : GPM.Serve
Description : GPM review related commands
License     : Public Domain
Maintainer  : yann.esposito@gmail.com
-}
module GPM.Serve
  ( init
  , handleServe
  , parseServeCommand
  , ServeCommand
  )
where

import           Protolude   hiding (die, (%))
import           Turtle

import           GPM.Helpers (debug, debug_, getGPMDataDir, inDir, green)

getPublicDir :: IO Turtle.FilePath
getPublicDir = do
  gpmDataDir <- getGPMDataDir
  let publicdir = gpmDataDir </> "public"
  return publicdir

getProjectRoot :: IO Turtle.FilePath
getProjectRoot = do
  mReporoot <- debug "git rev-parse --show-toplevel"
  case mReporoot of
    Nothing       -> die "You don't appear to be in a git repository."
    Just reporoot -> return (fromString (toS reporoot))

getPublicPrjDir :: IO Turtle.FilePath
getPublicPrjDir = do
  publicdir <- getPublicDir
  reporoot <- getProjectRoot
  let projectName = basename reporoot
  return (publicdir </> projectName)

-- | init gpm branch to handle reviews
init :: IO ()
init = do
  green "* server init"
  publicdir <- getPublicDir
  putText (format ("create dir: "%fp) publicdir)
  mktree publicdir
  inDir publicdir $ do
    debug_ "git init ."
    let descriptionFile = publicdir </> ".git" </> "description"
    output descriptionFile "Main repositories"
  repoRoot <- getProjectRoot
  publicProjectDir <- getPublicPrjDir
  whenM (testdir publicProjectDir) $ do
    putText (format ("rmtree " % fp) publicProjectDir)
    rmtree publicProjectDir
  debug_ (format ("git clone --bare "%fp%" "%fp)
                 repoRoot
                 publicProjectDir)
  inDir publicProjectDir $ do
    mv ("hooks" </> "post-update.sample") ("hooks" </> "post-update")
    _ <- chmod executable ("hooks" </> "post-update")
    debug_ "git update-server-info"

-- | Serve command

data ServeCommand = ServeStart
                   | ServeStop
                   | ServeUpdate
                   deriving (Eq)

parseServeCommand :: Parser ServeCommand
parseServeCommand =
  subcommand "start" "Start to serve all gpm tracked repositories" (pure ServeStart)
  <|> subcommand "stop" "Stop to serve all gpm tracked repositories" (pure ServeStop)
  <|> subcommand "update" "Update the served git repository" (pure ServeUpdate)

handleServe :: ServeCommand -> Text -> IO ()
handleServe ServeStart  _ = handleServeStart
handleServe ServeStop   _ = handleServeStop
handleServe ServeUpdate _ = handleUpdate

handleUpdate :: IO ()
handleUpdate = do
  pubPrjDir <- getPublicPrjDir
  inDir pubPrjDir $
    debug_ "git pull"

handleServeStart :: IO ()
handleServeStart = do
  pubDir <- getPublicDir
  inDir pubDir $
    debug_ "git instaweb --http=webrick start"
    -- TODO: Do not forget to also git serve

handleServeStop :: IO ()
handleServeStop = do
  pubDir <- getPublicDir
  inDir pubDir $
    debug_ "git instaweb --http=webrick stop"
