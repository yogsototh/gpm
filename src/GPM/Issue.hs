{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
module      : GPM.Issue
Description : GPM new issue related command
License     : Public Domain
Maintainer  : yann.esposito@gmail.com
-}
module GPM.Issue
  ( init
  , handleNewIssue
  )
where

import           Protolude      hiding (die)
import           Turtle

import           Data.FileEmbed (embedStringFile)
import           GPM.Helpers
import           Text.Mustache

data NewIssue =
  NewIssue { priority    :: Priority
           , status      :: Text
           , title       :: Text
           , user        :: User
           , branch      :: Maybe Text
           , tags        :: [Text]
           , assignee    :: Maybe User
           , reviewers   :: [User]
           , description :: Text
           }
data Priority = PriorityA | PriorityB | PriorityC
type User = Text

priorityToText :: Priority -> Text
priorityToText PriorityA = "[#A]"
priorityToText PriorityB = "[#B]"
priorityToText PriorityC = "[#C]"

instance ToMustache NewIssue where
  toMustache NewIssue{..} = object
    [ "priority"    ~> priorityToText priority
    , "status"      ~> status
    , "title"       ~> title
    , "user"        ~> user
    , "branch"      ~> branch
    , "tags"        ~> tags
    , "assignee"    ~> assignee
    , "reviewers"   ~> reviewers
    , "description" ~> description
    ]

createTmpNewIssue :: NewIssue -> IO ()
createTmpNewIssue ni = do
  ecompiled <- automaticCompile ["./templates"] "new-issue.org"
  case ecompiled of
    Left pe -> do
      print pe
      die "Parse ERROR, check your template ./templates/new-issue.org"
    Right compiled -> writeFile ".issues.org.tmp" (substitute compiled ni)

handleNewIssue :: IO ()
handleNewIssue = do
  newIssue <- gatherNewIssueInfos
  createTmpNewIssue newIssue
  validTmpNewIssue

validTmpNewIssue :: IO ()
validTmpNewIssue = do
  tmpIssue <- readFile ".issues.org.tmp"
  appendFile "issues.org" ("\n\n" <> tmpIssue)

defaultNewIssue :: NewIssue
defaultNewIssue = NewIssue
  { priority    = PriorityB
  , status      = "TODO"
  , title       = "Issue Title"
  , user        = "Unknown User"
  , branch      = Nothing
  , tags        = []
  , assignee    = Nothing
  , reviewers   = []
  , description = "Write a comprehensive issue description..."
  }

gatherNewIssueInfos :: IO NewIssue
gatherNewIssueInfos = do
  user <- getGitUser
  branch <- getCurrentGitBranch
  return $ defaultNewIssue { user = fromMaybe "Unknown User" user
                           , branch = branch }

init :: IO ()
init = do
  echo "* issue.org"
  output "issues.org" $(embedStringFile "templates/issues.org")

