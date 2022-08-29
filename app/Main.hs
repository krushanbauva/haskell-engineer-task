{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Text
import Data.Time (Day)
import Data.Aeson
import Data.Data (typeOf)
import qualified Data.ByteString.Lazy as B
import System.Environment

-- decode :: FromJSON a => ByteString -> Maybe a
-- encode :: ToJSON a => a -> ByteString

data Author = Author
  { id :: Int,
    name :: Text
  } deriving (Show)

instance ToJSON Author where
  toJSON (Author id name) = 
    object ["id" .= id,
            "name" .= name]

instance FromJSON Author where
  parseJSON (Object v) = do
    id <- v .: "id"
    name <- v .: "name"
    return $ Author id name
  parseJSON _ = fail "Unable to parse Author"

data Article = Article
  { article_id :: Int,
    author_id :: Int,
    title :: Text,
    rating :: Int,
    created_at :: Day
  } deriving (Show)

instance ToJSON Article where
  toJSON (Article article_id author_id title rating created_at) = 
    object ["id" .= article_id,
            "author_id" .= author_id,
            "title" .= title,
            "rating" .= rating,
            "created_at" .= created_at]

instance FromJSON Article where
  parseJSON (Object v) = do
    article_id <- v .: "id"
    author_id <- v .: "author_id"
    title <- v .: "title"
    rating <- v .: "rating"
    created_at <- v .: "created_at"
    return $ Article article_id author_id title rating created_at
  parseJSON _ = fail "Unable to parse Article"

printOneByOne :: Show a => [a] -> IO ()
printOneByOne [] = return ()
printOneByOne (x:xs) = do 
  print x
  printOneByOne xs

main :: IO ()
main = do
  args <- getArgs
  print args
  
  author_contents <- B.readFile "authors.json"
  let (Just authors) = decode author_contents :: Maybe [Author]
  printOneByOne authors

  article_contents <- B.readFile "articles.json"
  let (Just articles) = decode  article_contents :: Maybe [Article]
  printOneByOne articles
