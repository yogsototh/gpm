{-# LANGUAGE ApplicativeDo     #-}
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
  , parseIssueOptions
  , IssueOptions(..)
  )
where

import           Protolude      hiding (ask, die)
import           Turtle

import           Data.FileEmbed (embedStringFile)
import qualified Data.Text      as T
import           Text.Mustache

import           GPM.Helpers

data IssueOptions = IssueOptions
                    { interactive :: Bool
                    , newIssue    :: NewIssue
                    }


parseIssueOptions :: Parser IssueOptions
parseIssueOptions = IssueOptions
                    <$> switch "interactive" 'i' "Interactive mode"
                    <*> parseNewIssue

parseNewIssue :: Parser NewIssue
parseNewIssue = do
  isPriority    <- optional $ optText "priority"  'p' "Priority A,B,C"
  isStatus      <- optional $ optText "status"    's' "The status of the issue (TODO, QUESTION, ...)"
  isTitle       <- optional $ optText "title"     't' "The status title"
  isUser        <- optional $ optText "creator"   'c' "The user that created the issue"
  isBranch      <- optional $ optText "branch"    'b' "The branch related to the issue"
  isTags        <- optional $ optText "tags"      'g' "comma separated tags"
  isAssignee    <- optional $ optText "assignee"  'a' "Assignee"
  isReviewers   <- optional $ optText "reviewers" 'r' "comma separated reviewers"
  isDescription <- optional $ optText "descr"     'd' "Long issue description"
  pure NewIssue { priority = maybe PriorityB toPriority isPriority
                , status = fromMaybe "TODO" isStatus
                , title  = fromMaybe "Issue Title" isTitle
                , user        = isUser
                , branch      = isBranch
                , tags        = maybe [] (T.splitOn ",") isTags
                , assignee    = isAssignee
                , reviewers   = maybe [] (T.splitOn ",") isReviewers
                , description = isDescription
                }

toPriority :: Text -> Priority
toPriority "A" = PriorityA
toPriority "B" = PriorityB
toPriority "C" = PriorityC
toPriority _   = PriorityB

data NewIssue =
  NewIssue { priority    :: Priority
           , status      :: Text
           , title       :: Text
           , user        :: Maybe User
           , branch      :: Maybe Text
           , tags        :: [Text]
           , assignee    :: Maybe User
           , reviewers   :: [User]
           , description :: Maybe Text
           }

data Priority = PriorityA
              | PriorityB
              | PriorityC
              deriving (Eq,Ord)

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

handleNewIssue :: IssueOptions -> Text -> IO ()
handleNewIssue opts br = do
  newIssueTmp <- gatherNewIssueInfos (newIssue opts) br
  newIssue <- if interactive opts
              then interactiveNewIssue newIssueTmp
              else return newIssueTmp
  createTmpNewIssue newIssue
  validTmpNewIssue

interactiveNewIssue :: NewIssue -> IO NewIssue
interactiveNewIssue ni =
  NewIssue
    <$> (fromMaybe (priority ni)
          <$> ask "priority" (ptot (priority ni)) toPriority)
    <*> (fromMaybe (status ni)
         <$> ask "status" (status ni) identity)
    <*> (fromMaybe (title ni)
         <$> ask "title" (title ni) identity)
    <*> (maybe (user ni) Just
         <$>
         ask "user" (fromMaybe "your name" (user ni)) identity)
    <*> (maybe (branch ni) Just
         <$> ask "branch" (fromMaybe "related branch" (branch ni)) identity)
    <*> (fromMaybe (tags ni)
         <$> ask "tags" "comma separated tags" (T.splitOn ","))
    <*> (maybe (assignee ni) Just
         <$> ask "assignee" "a single nick" identity)
    <*> (fromMaybe (tags ni)
         <$> ask "reviewers" "comma separated nicks" (T.splitOn ","))
    <*> (maybe (description ni) Just
         <$> ask "description" "the long description" identity)
  where
    ptot :: Priority -> Text
    ptot PriorityA = "A"
    ptot PriorityB = "B"
    ptot PriorityC = "C"
    ask :: Text -> Text -> (Text -> a) -> IO (Maybe a)
    ask field ex tr = do
      putText $ "Please enter " <> field <> "("<> ex <>"): "
      fmap (tr . lineToText) <$> readline

validTmpNewIssue :: IO ()
validTmpNewIssue = do
  tmpIssue <- readFile ".issues.org.tmp"
  appendFile "issues.org" ("\n\n" <> tmpIssue)

gatherNewIssueInfos :: NewIssue -> Text -> IO NewIssue
gatherNewIssueInfos iss br = do
  user <- if isNothing (user iss)
          then getGitUser
          else return (user iss)
  branch <- if isNothing (branch iss)
            then return (Just br)
            else return (branch iss)
  return $ iss { user   = user
               , branch = branch }

init :: IO ()
init = do
  echo "* issue.org"
  writeFile "issues.org" $(embedStringFile "templates/issues.org")
  debug_ "git add issues.org"
  mktree "templates"
  writeFile "templates/new-issue.org" $(embedStringFile "templates/new-issue.org")
  debug_ "git add templates"

