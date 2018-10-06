{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
module      : GPM.Review
Description : GPM review related commands
License     : Public Domain
Maintainer  : yann.esposito@gmail.com
-}
module GPM.Review
  ( init
  , parseReviewCmd
  , handleReview
  , ReviewCommand(..)
  )
where

import           Protolude      hiding (ask, die, (%))
import           Turtle

import           Data.FileEmbed (embedStringFile)
import           GPM.Helpers    (getGPMCacheDir, debug_, getGitUser)
import           Text.Mustache

data ReviewCommand = ReviewStart ReviewOptions
                   | ReviewCommit
                   | ReviewAccept
                   | ReviewFeedback
                   | ReviewQuestion
                   | ReviewReject
                   deriving (Eq)

-- | init gpm branch to handle reviews
init :: IO ()
init = do
  let fic = "reviews" </> "write-contributing-yogsototh.org"
  mktree "reviews"
  putText $ format ("* "%fp) fic
  writeFile (toS (format fp fic)) $(embedStringFile "templates/review.org")
  debug_ "git add reviews"


data ReviewOptions = ReviewOptions
                    { interactive :: Bool
                    , newReview    :: NewReview
                    } deriving (Eq)

parseReviewOptions :: Parser ReviewOptions
parseReviewOptions = ReviewOptions
                    <$> switch "interactive" 'i' "Interactive mode"
                    <*> parseNewReview


parseNewReview :: Parser NewReview
parseNewReview = do
  nrStatus      <- optional $ optText "status"    's' "The status of the review (TODO, QUESTION, ...)"
  nrTitle       <- optional $ optText "title"     't' "The status title"
  nrUser        <- optional $ optText "creator"   'c' "The user that created the review"
  nrBranch      <- optional $ optText "branch"    'b' "The branch related to the review"
  nrDescription <- optional $ optText "descr"     'd' "Long review description"
  pure NewReview { status = fromMaybe "TODO" nrStatus
                 , title  = fromMaybe "Review Title" nrTitle
                 , user        = nrUser
                 , branch      = nrBranch
                 , reviewer    = nrUser
                 , description = nrDescription
                 }

parseReviewCmd :: Parser ReviewCommand
parseReviewCmd =
  subcommand "accept" "Accept the merge" (pure ReviewAccept)
  <|> subcommand "feedback" "Provide a feedback" (pure ReviewFeedback)
  <|> subcommand "question" "Ask a question" (pure ReviewQuestion)
  <|> subcommand "reject" "Reject the merge" (pure ReviewReject)
  <|> subcommand "start" "Create a new review" (ReviewStart <$> parseReviewOptions)
  <|> subcommand "end" "End a review" (pure ReviewCommit)


gatherNewReviewInfos :: NewReview -> Text -> IO NewReview
gatherNewReviewInfos iss br = do
  user <- if isNothing (user iss)
          then getGitUser
          else return (user iss)
  branch <- if isNothing (branch iss)
            then return (Just br)
            else return (branch iss)
  return $ iss { user   = user
               , branch = branch }

handleReview :: ReviewCommand -> Text -> IO ()
handleReview (ReviewStart opts) br = do
  newReviewTmp <- gatherNewReviewInfos (newReview opts) br
  newReview <- if interactive opts
               then interactiveNewReview newReviewTmp
               else return newReviewTmp
  createTmpNewReview newReview
handleReview ReviewCommit   _ = validTmpNewReview
handleReview ReviewAccept   _ = die "TODO"
handleReview ReviewFeedback _ = die "TODO"
handleReview ReviewQuestion _ = die "TODO"
handleReview ReviewReject   _ = die "TODO"

validTmpNewReview :: IO ()
validTmpNewReview = do
  tmpReviewFile <- getTmpReviewFile
  tmpIssue <- readFile (toS (format fp tmpReviewFile))
  appendFile "issues.org" ("\n\n" <> tmpIssue)

data NewReview =
  NewReview { status      :: Text
            , title       :: Text
            , user        :: Maybe User
            , branch      :: Maybe Text
            , reviewer    :: Maybe User
            , description :: Maybe Text
            } deriving (Eq)

type User = Text

instance ToMustache NewReview where
  toMustache NewReview{..} = object
    [ "status"      ~> status
    , "title"       ~> title
    , "user"        ~> user
    , "branch"      ~> branch
    , "reviewer"    ~> reviewer
    , "description" ~> description
    ]

getTmpReviewFile :: IO Turtle.FilePath
getTmpReviewFile = do
  cacheDir <- getGPMCacheDir
  return $ cacheDir </> "review-feedback.org"

createTmpNewReview :: NewReview -> IO ()
createTmpNewReview nr = do
  ecompiled <- automaticCompile ["./templates"] "new-review.org"
  case ecompiled of
    Left pe -> do
      print pe
      die "Parse ERROR, check your template ./templates/new-review.org"
    Right compiled -> do
      reviewName <- getTmpReviewFile
      writeFile (toS (format fp reviewName)) (substitute compiled nr)

interactiveNewReview :: NewReview -> IO NewReview
interactiveNewReview nr =
  NewReview
    <$> (fromMaybe (status nr)
         <$> ask "status" (status nr) identity)
    <*> (fromMaybe (title nr)
         <$> ask "title" (title nr) identity)
    <*> (maybe (user nr) Just
         <$>
         ask "user" (fromMaybe "your name" (user nr)) identity)
    <*> (maybe (branch nr) Just
         <$> ask "branch" (fromMaybe "related branch" (branch nr)) identity)
    <*> (maybe (reviewer nr) Just
         <$> ask "reviewer" "a single nick" identity)
    <*> (maybe (description nr) Just
         <$> ask "description" "the long description" identity)
  where
    ask :: Text -> Text -> (Text -> a) -> IO (Maybe a)
    ask field ex tr = do
      putText $ "Please enter " <> field <> "("<> ex <>"): "
      fmap (tr . lineToText) <$> readline
