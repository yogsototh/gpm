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

import           Protolude      hiding (ask,die)
import           Turtle

import           Data.FileEmbed (embedStringFile)
import qualified Data.Text      as T
import           GPM.Helpers
import           Text.Mustache

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

handleNewIssue :: IssueOptions -> IO ()
handleNewIssue opts = do
  newIssueTmp <- gatherNewIssueInfos (newIssue opts)
  newIssue <- if interactive opts
              then interactiveNewIssue newIssueTmp
              else return newIssueTmp
  createTmpNewIssue newIssue
  validTmpNewIssue

interactiveNewIssue :: NewIssue -> IO NewIssue
interactiveNewIssue _ =
  NewIssue <$> (fromMaybe PriorityB <$> ask "priority" "ex: A,B,C" toPriority)
    <*> (fromMaybe "TODO" <$> ask "status" "ex: TODO, QUESTION" identity)
    <*> (fromMaybe "Issue title" <$> ask "title" "Short Description" identity)
    <*> ask "user" "your nick" identity
    <*> ask "branch" "related branch" identity
    <*> (fromMaybe [] <$> ask "tags" "comma separated tags" (T.splitOn ","))
    <*> ask "assignee" "a single nick" identity
    <*> (fromMaybe [] <$> ask "reviewers" "comma separated nicks" (T.splitOn ","))
    <*> ask "description" "the long description" identity
  where
    ask :: Text -> Text -> (Text -> a) -> IO (Maybe a)
    ask field ex tr = do
      putText $ "Please enter " <> field <> "("<> ex <>"): "
      fmap (tr . lineToText) <$> readline

validTmpNewIssue :: IO ()
validTmpNewIssue = do
  tmpIssue <- readFile ".issues.org.tmp"
  appendFile "issues.org" ("\n\n" <> tmpIssue)

gatherNewIssueInfos :: NewIssue -> IO NewIssue
gatherNewIssueInfos iss = do
  user <- if isNothing (user iss)
          then getGitUser
          else return (user iss)
  branch <- if isNothing (branch iss)
            then getCurrentGitBranch
            else return (branch iss)
  return $ iss { user   = user
               , branch = branch }

init :: IO ()
init = do
  echo "* issue.org"
  output "issues.org" $(embedStringFile "templates/issues.org")

