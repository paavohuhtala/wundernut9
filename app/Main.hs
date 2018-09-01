module Main where

import Lib

main :: IO ()
main = do
  patterns <- loadLines "./patterns.txt"
  let outputLines = fmap evolveUntilPattern patterns
  mapM_ putStrLn outputLines
