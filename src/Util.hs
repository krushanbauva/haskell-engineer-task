module Util(getFilters, createAuthorsWithArticles, writeJSON) where
import Author
import Article
import Data.List
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Aeson

printOneByOne :: Show a => [a] -> IO ()
printOneByOne [] = return ()
printOneByOne (x:xs) = do
  print x
  printOneByOne xs

getArticlesByAuthorID :: [Article] -> Int -> [Article]
getArticlesByAuthorID articles authorId = [x | x <- articles, _articleAuthorId x == authorId]

sortByCreationDate :: [Article] -> [Article]
sortByCreationDate = sortOn _articleCreatedAt

filterArticles :: [Article] -> [String] -> [Article]
filterArticles articles filters
  | "ratingGreater3" `elem` filters = filterArticles [x | x <- articles, _articleRating x > 3] (delete "ratingGreater3" filters)
  | "first5" `elem` filters         = filterArticles (take 5 $ sortByCreationDate articles) (delete "first5" filters)
  | otherwise                       = articles

createAuthorWithArticles :: Author -> [Article] -> [String] -> AuthorWithArticles
createAuthorWithArticles author articles filters = AuthorWithArticles (_authorId author) (_authorName author) (filterArticles (getArticlesByAuthorID articles (_authorId author)) filters)

createAuthorsWithArticles :: [Author] -> [Article] -> [String] -> [AuthorWithArticles]
createAuthorsWithArticles authors articles filters = [createAuthorWithArticles author articles filters | author <- authors]

getFilters :: [String] -> [String]
getFilters = Data.List.map (\x -> if "filter=" `isInfixOf` x then Data.List.drop 7 x else x)

writeJSONOneByOne :: [AuthorWithArticles] -> IO ()
writeJSONOneByOne [] = return ()
writeJSONOneByOne (x:xs) = do
  appendFile "authors_with_articles.json" (C.unpack (encode x) ++ "\n")
  writeJSONOneByOne xs

writeJSON :: [AuthorWithArticles] -> IO ()
writeJSON xs = do
  writeFile "authors_with_articles.json" ""
  writeJSONOneByOne xs