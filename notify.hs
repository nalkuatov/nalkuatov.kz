#!/usr/bin/env stack
{- stack script
  --resolver lts-16.20
  --package directory
  --package process
-}

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (forM_, when)
import Text.Printf (printf)
import System.IO (hGetLine, openFile, IOMode(ReadMode))
import System.Directory (doesFileExist)
import System.Process (readCreateProcess, callCommand, shell)
import Data.Maybe (fromMaybe)
import Data.List (stripPrefix)
import Control.Exception (catch, SomeException)

getTitle :: FilePath -> IO String
getTitle filename = do
  let title = stripPrefix "title: "
  handle <- openFile filename ReadMode
  line   <- hGetLine handle *> hGetLine handle
  pure $ fromMaybe "" $ title line

curl filename title =
  unwords
  [ "curl -H \"Content-Type: application/json\" -X POST"
  , printf -- ^ 'printf' is truly dangerous...
      "-d '{\"filename\": \"%s\", \"title\": \"%s\"}' https://nalkuatov.kz/telegram-notifier/api/posts"
      filename title
  ]

gitLog = "git log --name-status -n 1 | grep A[[:space:]]posts/"

main :: IO ()
main = do
  res <- readCreateProcess (shell gitLog) []
    `catch` (\(_ :: SomeException) -> putStrLn "No new posts" *> pure "")
  forM_ (words res) $ \filename -> do
    exists <- doesFileExist filename
    when exists $ do
      title <- getTitle filename
      callCommand $ curl filename title
