{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
module      : GPM.Helpers
Description : GPM helper functions
License     : Public Domain
Maintainer  : yann.esposito@gmail.com
-}
module GPM.Hooks
  ( init
  , handleHooks
  , parseHooksCommand
  , HooksCommand(..)
  )
where

import           Protolude      hiding ((%))
import           Turtle

import           GPM.Helpers

data HooksCommand = SyncHooks

parseHooksCommand :: Parser HooksCommand
parseHooksCommand = subcommand "sync" "Synchronize hooks from gpm branch" (pure SyncHooks)

-- | Init a hook directory with the git hooks
init :: IO ()
init = do
  green "* hooks/"
  putText "    Copyings default hooks into the hooks directory"
  let hooksDir = "hooks"
  cptree (".git" </> "hooks") hooksDir
  debug_ (toS (format ("git add "%fp) hooksDir))

-- | Handle hooks related commands
handleHooks :: HooksCommand -> Text -> IO ()
handleHooks SyncHooks _ = do
  let githooksdir = ".git" </> "hooks"
      gpmhooksdir = "hooks"
  putErrText (format ("    deleting "%fp) githooksdir)
  rmtree githooksdir
  putErrText (format ("    cp -r "%fp%" "%fp) gpmhooksdir githooksdir)
  cptree gpmhooksdir githooksdir
