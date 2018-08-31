{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module GPM.Review where

import Protolude hiding (stdout,die)
import Turtle

data ReviewCommand = ReviewStart (Maybe Text)
                   | ReviewStop (Maybe Text)
                   | ReviewAccept
                   | ReviewFeedback
                   | ReviewQuestion
                   | ReviewReject
                   deriving (Eq)

parseReviewCmd :: Parser ReviewCommand
parseReviewCmd = subcommand "accept" "Accept the merge" (pure ReviewAccept)
                 <|> subcommand "feedback" "Provide a feedback" (pure ReviewFeedback)
                 <|> subcommand "question" "Ask a question" (pure ReviewQuestion)
                 <|> subcommand "reject" "Ask a question" (pure ReviewReject)

handleReview :: ReviewCommand -> IO ()
handleReview (ReviewStart _br) = die "TODO"
handleReview (ReviewStop _br ) = die "TODO"
handleReview ReviewAccept      = die "TODO"
handleReview ReviewFeedback    = die "TODO"
handleReview ReviewQuestion    = die "TODO"
handleReview ReviewReject      = die "TODO"
