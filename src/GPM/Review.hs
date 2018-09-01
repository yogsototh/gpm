{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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

import           Protolude      hiding (die, (%))
import           Turtle

import           Data.FileEmbed (embedStringFile)
import           GPM.Helpers    (debug_)

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
  output fic $(embedStringFile "templates/review.org")
  debug_ "git add reviews"


parseReviewCmd :: Parser ReviewCommand
parseReviewCmd =
  subcommand "accept" "Accept the merge" (pure ReviewAccept)
  <|> subcommand "feedback" "Provide a feedback" (pure ReviewFeedback)
  <|> subcommand "question" "Ask a question" (pure ReviewQuestion)
  <|> subcommand "reject" "Reject the merge" (pure ReviewReject)

handleReview :: ReviewCommand -> IO ()
handleReview (ReviewStart _br) = die "TODO"
handleReview (ReviewStop _br ) = die "TODO"
handleReview ReviewAccept      = die "TODO"
handleReview ReviewFeedback    = die "TODO"
handleReview ReviewQuestion    = die "TODO"
handleReview ReviewReject      = die "TODO"
