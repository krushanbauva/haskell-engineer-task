{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Environment
import Data.List
import Article
import Author
import Util

main :: IO ()
main = do
  args <- getArgs
  Just authors <- getAuthors (head args)
  Just articles <- getAricles (args !! 1)
  let filters = getFilters $ drop 2 args
  let author_with_articles = createAuthorsWithArticles authors articles filters
  writeJSON author_with_articles