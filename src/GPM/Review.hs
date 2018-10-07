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

import           Protolude      hiding (ask, die, (%),stdout)
import           Turtle

import           Data.FileEmbed (embedStringFile)
import           GPM.Helpers    (getGPMDataDir, debug_, getGitUser)
import           Text.Mustache
import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified System.Directory as Directory

data ReviewCommand = ReviewStart ReviewOptions
                   | ReviewCommit
                   | ReviewAccept ReviewOptions
                   | ReviewChangeRequest ReviewOptions
                   | ReviewFeedback ReviewOptions
                   | ReviewQuestion ReviewOptions
                   | ReviewReject ReviewOptions
                   | ReviewShow
                   | ReviewRetrieve
                   deriving (Eq)

-- | init gpm branch to handle reviews
init :: IO ()
init = do
  let fic = "reviews" </> "write-contributing-yogsototh.org"
  mktree "reviews"
  putText $ format ("* "%fp) fic
  writeFile (toS (format fp fic)) $(embedStringFile "templates/review.org")
  debug_ "git add reviews"
  mktree "templates"
  writeFile "templates/new-review.org" $(embedStringFile "templates/new-review.org")
  debug_ "git add templates"


-- | Command Line Options
data ReviewOptions = ReviewOptions
                    { interactive :: Bool
                    , newReview    :: NewReview
                    } deriving (Eq)

data NewReview =
  NewReview { status      :: Text
            , title       :: Text
            , user        :: Maybe User
            , branch      :: Maybe Text
            , description :: Maybe Text
            } deriving (Eq)

type User = Text

instance ToMustache NewReview where
  toMustache NewReview{..} = object
    [ "status"      ~> status
    , "title"       ~> title
    , "user"        ~> user
    , "branch"      ~> branch
    , "description" ~> description
    ]

parseFullReviewOptions :: Parser ReviewOptions
parseFullReviewOptions =
  ReviewOptions
  <$> switch "interactive" 'i' "Interactive mode"
  <*> parseFullNewReview

parseFullNewReview :: Parser NewReview
parseFullNewReview = do
  nrStatus      <- optional $ optText "status"    's' "The status of the review (TODO, QUESTION, ...)"
  nrTitle       <- optional $ optText "title"     't' "The status title"
  nrUser        <- optional $ optText "creator"   'c' "The user that created the review"
  nrBranch      <- optional $ optText "branch"    'b' "The branch related to the review"
  nrDescription <- optional $ optText "descr"     'd' "Long review description"
  pure NewReview { status = fromMaybe "TODO" nrStatus
                 , title  = fromMaybe "Review Title" nrTitle
                 , user        = nrUser
                 , branch      = nrBranch
                 , description = nrDescription
                 }

parsePartialReviewOptions :: Text -> Parser ReviewOptions
parsePartialReviewOptions status =
  ReviewOptions
  <$> switch "interactive" 'i' "Interactive mode"
  <*> parsePartialNewReview status

parsePartialNewReview :: Text -> Parser NewReview
parsePartialNewReview status = do
  nrTitle       <- optional $ optText "title"     't' "The status title"
  nrUser        <- optional $ optText "creator"   'c' "The user that created the review"
  nrBranch      <- optional $ optText "branch"    'b' "The branch related to the review"
  nrDescription <- optional $ optText "descr"     'd' "Long review description"
  pure NewReview { status = status
                 , title  = fromMaybe "Review Title" nrTitle
                 , user        = nrUser
                 , branch      = nrBranch
                 , description = nrDescription
                 }

parseReviewCmd :: Parser ReviewCommand
parseReviewCmd =
  subcommand "accept" "Accept the merge"
  (ReviewAccept <$> parsePartialReviewOptions "ACCEPTED")
  <|> subcommand "feedback" "Provide a feedback"
  (ReviewFeedback <$> parsePartialReviewOptions "FEEDBACK")
  <|> subcommand "question" "Ask a question"
  (ReviewQuestion <$> parsePartialReviewOptions "QUESTION")
  <|> subcommand "request-change" "Request some Changes to merge"
  (ReviewChangeRequest <$> parsePartialReviewOptions "CHANGE_REQUESTED")
  <|> subcommand "reject" "Reject the merge"
  (ReviewReject <$> parsePartialReviewOptions "REFUSED")
  <|> subcommand "start" "Start a new review"
  (ReviewStart <$> parseFullReviewOptions)
  <|> subcommand "end" "End a review"
  (pure ReviewCommit)
  <|> subcommand "show" "Show the review"
  (pure ReviewShow)
  <|> subcommand "retrieve" "Retrieve all the reviews for current branch"
  (pure ReviewRetrieve)


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

handleNewReview :: ReviewOptions -> Text -> IO ()
handleNewReview opts br = do
  newReviewTmp <- gatherNewReviewInfos (newReview opts) br
  newReview <- if interactive opts
               then interactiveNewReview newReviewTmp
               else return newReviewTmp
  createTmpNewReview newReview

setStatus :: ReviewOptions -> Text -> ReviewOptions
setStatus ro status = ro { newReview =  (newReview ro) { status = status } }

handleReview :: ReviewCommand -> Text -> IO ()
handleReview (ReviewStart opts)    br =
  handleNewReview opts br
handleReview ReviewCommit          br =
  validTmpNewReview br
handleReview (ReviewAccept opts)   br =
  handleNewReview (setStatus opts "ACCEPTED") br
handleReview (ReviewFeedback opts) br =
  handleNewReview (setStatus opts "FEEDBACK") br
handleReview (ReviewQuestion opts) br =
  handleNewReview (setStatus opts "QUESTION") br
handleReview (ReviewChangeRequest opts)   br =
  handleNewReview (setStatus opts "CHANGE_REQUESTED") br
handleReview (ReviewReject opts)   br =
  handleNewReview (setStatus opts "REJECTED") br
handleReview ReviewShow br = showReview br
handleReview ReviewRetrieve br = retrieveReview br

communicateFp :: MonadIO m => Turtle.FilePath -> m ()
communicateFp filepath = do
  let fptxt = format fp filepath
  putText $ "Review file: " <> fptxt
  export "GPM_REVIEW_FILE" fptxt
  putText $ "export GPM_REVIEW_FILE=" <> fptxt

retrieveReview :: Text -> IO ()
retrieveReview br = do
  reviewName <- getTmpReviewFile br
  let gpmReviewFile = "reviews" </>
         fromString (toS ("review-" <> protectStr br <> ".org"))
  mktree (directory reviewName)
  cp gpmReviewFile reviewName
  communicateFp reviewName

showReview :: Text -> IO ()
showReview br = do
  reviewName <- getTmpReviewFile br
  stdout (input reviewName)

protectStr :: Text -> Text
protectStr =
  Text.map (\c -> if Char.isAscii c then c else '-')

validTmpNewReview :: Text -> IO ()
validTmpNewReview br = do
  tmpReviewFile <- getTmpReviewFile br
  tmpReview <- readFile (toS (format fp tmpReviewFile))
  Directory.createDirectoryIfMissing True "reviews"
  let dstReview = "reviews" </>
         fromString (toS ("review-" <> protectStr br <> ".org"))
  appendFile (toS (format fp dstReview)) ("\n\n" <> tmpReview)
  debug_ $ "git add " <> toS (format fp dstReview)
  debug_ $ "git commit -m \"review for " <> br <> "\""

getTmpReviewFile :: Text -> IO Turtle.FilePath
getTmpReviewFile br = do
  gpmDataDir <- getGPMDataDir
  let reviewFilename = "review-" <> protectStr br <> ".org"
  return $ gpmDataDir </> fromString (toS reviewFilename)

createTmpNewReview :: NewReview -> IO ()
createTmpNewReview nr = do
  putText "DEBUG: create temporary file for the new review"
  ecompiled <- automaticCompile ["./templates"] "new-review.org"
  case ecompiled of
    Left pe -> do
      print pe
      die "Parse ERROR, check your template ./templates/new-review.org"
    Right compiled -> do
      reviewName <- getTmpReviewFile (fromMaybe "no-name" (branch nr))
      let tmpReviewFilename = format fp reviewName
      mktree (directory reviewName)
      writeFile (toS tmpReviewFilename) (substitute compiled nr)
      communicateFp reviewName

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
    <*> (maybe (description nr) Just
         <$> ask "description" "the long description" identity)
  where
    ask :: Text -> Text -> (Text -> a) -> IO (Maybe a)
    ask field ex tr = do
      putText $ "Please enter " <> field <> "("<> ex <>"): "
      fmap (tr . lineToText) <$> readline
