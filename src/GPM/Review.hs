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

import           Protolude        hiding (ask, die, stdout, (%))
import           Turtle

import           GPM.Helpers      (debug_, getGPMDataDir, getGitUser, green)

import qualified Data.Char        as Char
import           Data.FileEmbed   (embedStringFile)
import qualified Data.Text        as Text
import qualified System.Directory as Directory
import           Text.Mustache

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
  green "* reviews.org"
  putText "    create some example review for inspiration"
  let reviewDir = "reviews"
  mktree reviewDir
  let fic = reviewDir </> "write-contributing-yogsototh.org"
  putText $ format ("      "%fp) fic
  writeFile (toS (format fp fic)) $(embedStringFile "templates/review.org")
  debug_ (toS (format ("git add "%fp) reviewDir))
  putText "    create some review templates"
  let templateDir = "templates"
      templateFic = templateDir </> "new-review.org"
  mktree templateDir
  writeFile (toS (format fp templateFic)) $(embedStringFile "templates/new-review.org")
  putText $ format ("      "%fp) templateFic
  debug_ (toS (format ("git add "%fp) templateDir))

-- | Command Line Options
data ReviewOptions = ReviewOptions
                    { interactive :: Bool
                    , newReview   :: NewReview
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
  pure NewReview { status      = fromMaybe "TODO" nrStatus
                 , title       = fromMaybe "Review Title" nrTitle
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
  createTmpNewReview newReview br

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

communicateFp :: Turtle.FilePath -> IO ()
communicateFp filepath = do
  let fptxt = format fp filepath
  mainReviewName <- getMainReviewFile
  putErrText $ "Main review file: " <> format fp mainReviewName
  putErrText $ "Review file: " <> fptxt
  export "GPM_REVIEW_FILE" fptxt
  putErrText $ "export GPM_REVIEW_FILE=" <> fptxt

retrieveReview :: Text -> IO ()
retrieveReview br = do
  reviewName <- getTmpReviewFile br
  let gpmReviewFile = "reviews" </>
         fromString (toS ("review-" <> protectStr br <> ".org"))
  mktree (directory reviewName)
  cp gpmReviewFile reviewName
  communicateFp reviewName

getMainReviewFile :: IO Turtle.FilePath
getMainReviewFile = do
  gpmDataDir <- getGPMDataDir
  return $ gpmDataDir </> "current-review.org"

-- | Use a single main name for current review
linkReviewFile :: Turtle.FilePath -> IO Turtle.FilePath
linkReviewFile reviewFile = do
  mainReviewFile <- getMainReviewFile
  debug_ $ format ("ln -s "%fp%" "%fp) reviewFile mainReviewFile
  return mainReviewFile

showReview :: Text -> IO ()
showReview br = do
  reviewName <- getTmpReviewFile br
  mainReviewName <- getMainReviewFile
  putText "--------------------------------------------------------------------------------"
  green $ format ("Review file: "%fp) reviewName
  green $ format ("Main Review file: "%fp) mainReviewName
  putText "--------------------------------------------------------------------------------"
  stdout (input reviewName)
  putText "--------------------------------------------------------------------------------"

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

createTmpNewReview :: NewReview -> Text -> IO ()
createTmpNewReview nr br = do
  putErrText "Create temporary file for the new review"
  ecompiled <- automaticCompile ["./templates"] "new-review.org"
  case ecompiled of
    Left pe -> do
      print pe
      die "Parse ERROR, check your template ./templates/new-review.org"
    Right compiled -> do
      reviewName <- getTmpReviewFile (fromMaybe br (branch nr))
      mktree (directory reviewName)
      writeFile (toS (format fp reviewName)) (substitute compiled nr)
      linkReviewFile reviewName
      communicateFp reviewName

interactiveNewReview :: NewReview -> IO NewReview
interactiveNewReview nr =
  NewReview
    <$> ask "status"                     (status nr) identity
    <*> ask "title"                       (title nr) identity
    <*> ask "user"          (fromMaybe "" (user nr)) notEmpty
    <*> ask "branch"      (fromMaybe "" (branch nr)) notEmpty
    <*> ask "description"                        ""  notEmpty
  where
    notEmpty :: Text -> Maybe Text
    notEmpty ""  = Nothing
    notEmpty str = Just str
    ask :: Text -> Text -> (Text -> a) -> IO a
    ask field ex tr = do
      putText $ "Please enter " <> field
        <> (if ex /= "" then " ("<> ex <>"): " else "")
      mline <- readline
      case mline of
        Nothing -> return (tr "")
        Just line -> if line == ""
                        then return (tr "")
                        else return . tr . lineToText $ line
