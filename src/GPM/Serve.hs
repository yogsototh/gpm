{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
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
import           Protolude                      hiding (die, (%), (<.>))
import           Turtle

-- | Local Imports
import           GPM.Helpers                    (debug, debug_, getGPMDataDir,
                                                 green, inDir, inGPM)

-- | External Lib Imports
import qualified Data.Text                      as Text
import           Data.FileEmbed (embedStringFile)

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
  debug_ (format ("git clone --mirror "%fp%" "%fp)
                 repoRoot
                 publicProjectDir)
  inDir (publicProjectDir </> "hooks") $ do
    mv "post-update.sample" "post-update"
    _ <- chmod executable "post-update"
    writeFile "pre-receive-hooks" $(embedStringFile "templates/pre-receive-hooks")
    _ <- chmod executable "pre-receive-hooks"
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
    debug_ "git fetch --all"

handleServeStart :: IO ()
handleServeStart = do
  pubDir <- getPublicDir
  inDir pubDir $ do
    pwd >>= putText . format fp
    dirServe pubDir
    debug_ "git instaweb --http=webrick start"

handleServeStop :: IO ()
handleServeStop = do
  pubDir <- getPublicDir
  inDir pubDir $ do
    pwd >>= putText . format fp
    dirStopServe
    debug_ "git instaweb --http=webrick stop"

handleProjectDir :: IO ()
handleProjectDir = getPublicDir >>= putText . format fp

dirServe :: Turtle.FilePath -> IO ()
dirServe pubdir = do
  gpmDataDir <- getGPMDataDir
  debug_ $
    format ("git daemon --detach --pid-file="%fp
            %" --reuseaddr --export-all --base-path="%fp%" "%fp)
    (gpmDataDir </> "git-daemon-pid")
    pubdir
    pubdir

dirStopServe :: IO ()
dirStopServe = do
  gpmDataDir <- getGPMDataDir
  inDir gpmDataDir $ do
    pidtxt <- readTextFile (gpmDataDir </> "git-daemon-pid")
    if Text.null pidtxt
      then putErrText "git daemon doesn't appear to be running"
      else debug_ ("kill " <> pidtxt)

