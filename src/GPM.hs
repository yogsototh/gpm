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

import           Protolude   hiding (stdout)
import           Turtle


import           GPM.Helpers (inGPM)
import qualified GPM.Init    as Init
import qualified GPM.Issue   as Issue
import qualified GPM.Review  as Review

gpm :: IO ()
gpm = do
  subcmd <- options "Git Project Manager" parser
  case subcmd of
   Init              -> Init.init
   NewIssue issueOpt -> inGPM (Issue.handleNewIssue issueOpt)
   Review reviewCmd  -> inGPM (Review.handleReview reviewCmd)

data Command = Init
             | NewIssue Issue.IssueOptions
             | Review Review.ReviewCommand

parser :: Parser Command
parser = subcommand "init" "Initialize gpm" (pure Init)
         <|> NewIssue <$> subcommand "new-issue"
                                     "Create a new Issue"
                                     Issue.parseIssueOptions
         <|> Review <$> subcommand "review"
                         "Review (use current branch by default)"
                         Review.parseReviewCmd

debug :: Text -> IO ()
debug cmd = do
  putText cmd
  stdout $ inshell cmd empty
