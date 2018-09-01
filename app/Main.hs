module Main where

import Lib

main :: IO ()
main = do
  patterns <- loadLines "./patterns.txt"
  let outputLines = fmap (iterateUntilPattern []) patterns
  mapM_ putStrLn outputLines
