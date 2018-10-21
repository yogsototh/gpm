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

-- | Generic Import
import           Protolude                      hiding (die, stdout, (%), (<.>))
import           Turtle

-- | Local Imports
import           GPM.Helpers                    (debug, debug_, getGPMDataDir,
                                                 green, inDir, inGPM)

-- | External Lib Imports
import qualified Data.Text                      as Text
import qualified Network.Wai.Application.Static as WaiStatic
import qualified Network.Wai.Handler.Warp       as Warp
import qualified System.Posix.Process           as Process

-- | Retrieve a public dir to serve git repositories
getPublicDir :: IO Turtle.FilePath
getPublicDir = do
  gpmDataDir <- getGPMDataDir
  let publicdir = gpmDataDir </> "public"
  return publicdir

-- | Retrieve the git project root directory
getProjectRoot :: IO Turtle.FilePath
getProjectRoot = do
  mReporoot <- debug "git rev-parse --show-toplevel"
  case mReporoot of
    Nothing       -> die "You don't appear to be in a git repository."
    Just reporoot -> return (fromString (toS reporoot))

-- | Retrieve the git public directory for the project
getPublicPrjDir :: IO Turtle.FilePath
getPublicPrjDir = do
  publicdir <- getPublicDir
  reporoot <- getProjectRoot
  let projectName = basename reporoot
  return (publicdir </> (projectName <.> "git"))

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
                   | ProjectDir
                   deriving (Eq)

parseServeCommand :: Parser ServeCommand
parseServeCommand =
  subcommand "start" "Start to serve all gpm tracked repositories" (pure ServeStart)
  <|> subcommand "stop" "Stop to serve all gpm tracked repositories" (pure ServeStop)
  <|> subcommand "update" "Update the served git repository" (pure ServeUpdate)
  <|> subcommand "path" "Show the path of the bare repository" (pure ServeUpdate)

handleServe :: ServeCommand -> IO ()
handleServe ServeStart  = handleServeStart
handleServe ServeStop   = handleServeStop
handleServe ServeUpdate = inGPM handleUpdate
handleServe ProjectDir  = handleProjectDir

handleUpdate :: Text -> IO ()
handleUpdate _ = do
  pubPrjDir <- getPublicPrjDir
  inDir pubPrjDir $ do
    pwd >>= putText . format fp
    debug_ "git fetch"

handleServeStart :: IO ()
handleServeStart = do
  pubDir <- getPublicDir
  inDir pubDir $ do
    pwd >>= putText . format fp
    debug_ "git instaweb --http=webrick start"
    dirServe

handleServeStop :: IO ()
handleServeStop = do
  pubDir <- getPublicDir
  inDir pubDir $ do
    pwd >>= putText . format fp
    debug_ "git instaweb --http=webrick stop"
    dirStopServe

handleProjectDir :: IO ()
handleProjectDir = getPublicDir >>= putText . format fp

dirServe :: IO ()
dirServe = do
  processId <- Process.forkProcess $ Warp.run 3000 (WaiStatic.staticApp (WaiStatic.defaultWebAppSettings "."))
  gpmDataDir <- getGPMDataDir
  inDir gpmDataDir $ do
    mktree "procs"
    writeTextFile ("procs" </> "gitServePID") (show processId)

dirStopServe :: IO ()
dirStopServe = do
  gpmDataDir <- getGPMDataDir
  inDir gpmDataDir $ do
    pidtxt <- readTextFile ("procs" </> "gitServePID")
    if Text.null pidtxt
      then putErrText "The git server doesn't appear to be running"
      else debug_ ("kill " <> pidtxt)

