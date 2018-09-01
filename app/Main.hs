module Main where

import Control.Arrow
import Lib

loadLines :: FilePath -> IO [Line]
loadLines = readFile >>> fmap (lines >>> fmap parseLine)

main :: IO ()
main = do
  patterns <- loadLines "./patterns.txt"
  let outputLines = fmap evolveUntilPattern patterns
  mapM_ putStrLn outputLines
