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
import           GPM.Helpers    (getGPMCacheDir, debug_)
import           Text.Mustache

data ReviewCommand = ReviewStart (Maybe Text)
                   | ReviewStop (Maybe Text)
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


parseReviewCmd :: Parser ReviewCommand
parseReviewCmd =
  subcommand "accept" "Accept the merge" (pure ReviewAccept)
  <|> subcommand "feedback" "Provide a feedback" (pure ReviewFeedback)
  <|> subcommand "question" "Ask a question" (pure ReviewQuestion)
  <|> subcommand "reject" "Reject the merge" (pure ReviewReject)

handleReview :: ReviewCommand -> Text -> IO ()
handleReview (ReviewStart _br) _ = die "TODO"
handleReview (ReviewStop _br ) _ = die "TODO"
handleReview ReviewAccept      _ = die "TODO"
handleReview ReviewFeedback    _ = die "TODO"
handleReview ReviewQuestion    _ = die "TODO"
handleReview ReviewReject      _ = die "TODO"

data NewReview =
  NewReview { status      :: Text
            , title       :: Text
            , user        :: Maybe User
            , branch      :: Maybe Text
            , reviewer    :: Maybe User
            , description :: Maybe Text
            }

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

createTmpNewReview :: NewReview -> IO ()
createTmpNewReview nr = do
  ecompiled <- automaticCompile ["./templates"] "new-review.org"
  case ecompiled of
    Left pe -> do
      print pe
      die "Parse ERROR, check your template ./templates/new-review.org"
    Right compiled -> do
      cacheDir <- getGPMCacheDir
      let reviewName = cacheDir </> "review-feedback.org"
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
