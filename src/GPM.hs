{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
module      : GPM
Description : GPM command line function
License     : Public Domain
Maintainer  : yann.esposito@gmail.com
-}
module GPM
where

import           Protolude
import           Turtle


import           GPM.Helpers (inGPM)
import qualified GPM.Hooks   as Hooks
import qualified GPM.Init    as Init
import qualified GPM.Issue   as Issue
import qualified GPM.Review  as Review
import qualified GPM.Serve   as Serve

gpm :: IO ()
gpm = do
  subcmd <- options "Git Project Manager" parser
  case subcmd of
   Init              -> Init.init
   NewIssue issueOpt -> inGPM (Issue.handleNewIssue issueOpt)
   Review reviewCmd  -> inGPM (Review.handleReview reviewCmd)
   Serve serveCmd    -> Serve.handleServe serveCmd
   Hooks hooksCmd    -> inGPM (Hooks.handleHooks hooksCmd)

data Command = Init
             | NewIssue Issue.IssueOptions
             | Review Review.ReviewCommand
             | Serve Serve.ServeCommand
             | Hooks Hooks.HooksCommand

parser :: Parser Command
parser = subcommand "init" "Initialize gpm" (pure Init)
         <|> NewIssue <$> subcommand "new-issue"
                                     "Create a new Issue"
                                     Issue.parseIssueOptions
         <|> Review <$> subcommand "review"
                         "Review (use current branch by default)"
                         Review.parseReviewCmd
         <|> Serve <$> subcommand "serve"
                         "Serve the git to the web"
                         Serve.parseServeCommand
         <|> Hooks <$> subcommand "hooks"
                         "Handle hooks for this git repository"
                         Hooks.parseHooksCommand
