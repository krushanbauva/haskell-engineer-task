{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Text
import Data.Time (Day)
import Data.Aeson
import Data.Data (typeOf)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import System.Environment
import Data.List

-- decode :: FromJSON a => ByteString -> Maybe a
-- encode :: ToJSON a => a -> ByteString

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

printOneByOne :: Show a => [a] -> IO ()
printOneByOne [] = return ()
printOneByOne (x:xs) = do
  print x
  printOneByOne xs

getAuthors :: String -> IO [Author]
getAuthors filename = do
  author_contents <- B.readFile filename
  let (Just authors) = decode author_contents :: Maybe [Author]
  return authors

getAricles :: String -> IO [Article]
getAricles filename = do
  article_contents <- B.readFile filename
  let (Just articles) = decode article_contents :: Maybe [Article]
  return articles

getArticlesByAuthorID :: [Article] -> Int -> [Article]
getArticlesByAuthorID articles authorId = [x | x <- articles, _articleAuthorId x == authorId]

sortByCreationDate :: [Article] -> [Article]
sortByCreationDate = sortOn _articleCreatedAt

filterArticles :: [Article] -> [String] -> [Article]
filterArticles articles filters
  | "ratingGreater3" `elem` filters = filterArticles [x | x <- articles, _articleRating x > 3] (delete "ratingGreater3" filters)
  | "first5" `elem` filters         = filterArticles (Prelude.take 5 $ sortByCreationDate articles) (delete "first5" filters)
  | otherwise                       = articles

createAuthorWithArticles :: Author -> [Article] -> [String] -> AuthorWithArticles
createAuthorWithArticles author articles filters = AuthorWithArticles (_authorId author) (_authorName author) (filterArticles (getArticlesByAuthorID articles (_authorId author)) filters)

createAuthorsWithArticles :: [Author] -> [Article] -> [String] -> [AuthorWithArticles]
createAuthorsWithArticles authors articles filters = [createAuthorWithArticles author articles filters | author <- authors]

getFilters :: [String] -> [String]
getFilters = Data.List.map (\x -> if Data.List.isInfixOf "filter=" x then Data.List.drop 7 x else x)

writeJSONOneByOne :: [AuthorWithArticles] -> IO ()
writeJSONOneByOne [] = return ()
writeJSONOneByOne (x:xs) = do
  appendFile "authors_with_articles.json" (C.unpack (encode x) ++ "\n")
  writeJSONOneByOne xs

writeJSON :: [AuthorWithArticles] -> IO ()
writeJSON xs = do
  writeFile "authors_with_articles.json" ""
  writeJSONOneByOne xs

main :: IO ()
main = do
  args <- getArgs
  authors <- getAuthors (Data.List.head args)
  articles <- getAricles (args !! 1)
  let filters = getFilters $ Data.List.drop 2 args
  let author_with_articles = createAuthorsWithArticles authors articles filters
  writeJSON author_with_articles