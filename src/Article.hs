{-# LANGUAGE OverloadedStrings #-}

module Article (Article(..), getAricles) where
import Data.Text
import Data.Time (Day)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import System.Directory

data Article = Article
  { _articleId :: Int,
    _articleAuthorId :: Int,
    _articleTitle :: Text,
    _articleRating :: Int,
    _articleCreatedAt :: Day
  } deriving (Show)

instance ToJSON Article where
  toJSON (Article _articleId _articleAuthorId _articleTitle _articleRating _articleCreatedAt) =
    object ["id" .= _articleId,
            "author_id" .= _articleAuthorId,
            "title" .= _articleTitle,
            "rating" .= _articleRating,
            "created_at" .= _articleCreatedAt]

instance FromJSON Article where
  parseJSON (Object v) = do
    _articleId <- v .: "id"
    _articleAuthorId <- v .: "author_id"
    _articleTitle <- v .: "title"
    _articleRating <- v .: "rating"
    _articleCreatedAt <- v .: "created_at"
    return $ Article _articleId _articleAuthorId _articleTitle _articleRating _articleCreatedAt
  parseJSON _ = fail "Unable to parse Article"

getAricles :: String -> IO (Maybe [Article])
getAricles filename = do
  fileExists <- doesFileExist filename
  if fileExists
    then do article_contents <- B.readFile filename
            let (Just articles) = decode article_contents :: Maybe [Article]
            return $ Just articles
    else do putStrLn $ "The file " ++ filename ++ " does not exist!"
            return Nothing