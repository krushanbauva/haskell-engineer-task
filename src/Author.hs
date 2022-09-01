{-# LANGUAGE OverloadedStrings #-}

module Author(Author(..), getAuthors, AuthorWithArticles(..)) where

import Data.Text
import Data.Time (Day)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Article
import System.Directory

data Author = Author
  { _authorId :: Int,
    _authorName :: Text
  } deriving (Show)

instance ToJSON Author where
  toJSON (Author _authorId _authorName) =
    object ["id" .= _authorId,
            "name" .= _authorName]

instance FromJSON Author where
  parseJSON (Object v) = do
    _authorId <- v .: "id"
    _authorName <- v .: "name"
    return $ Author _authorId _authorName
  parseJSON _ = fail "Unable to parse Author"

getAuthors :: String -> IO (Maybe [Author])
getAuthors filename = do
  fileExists <- doesFileExist filename
  if fileExists
    then do author_contents <- B.readFile filename
            let (Just authors) = decode author_contents :: Maybe [Author]
            return $ Just authors
    else do putStrLn $ "The file " ++ filename ++ " does not exist!"
            return Nothing

data AuthorWithArticles = AuthorWithArticles
  { _authorWithArticlesId :: Int,
    _authorWithArticlesName :: Text,
    articles :: [Article]
  } deriving (Show)

instance ToJSON AuthorWithArticles where
  toJSON (AuthorWithArticles _authorWithArticlesId _authorWithArticlesName articles) =
    object ["id" .= _authorWithArticlesId,
            "name" .= _authorWithArticlesName,
            "articles" .= articles]