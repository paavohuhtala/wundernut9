module Main where

import qualified Data.Set as Set
import Lib

main :: IO ()
main = do
  patterns <- loadLines "./patterns.txt"
  let outputLines = fmap (iterateUntilPattern Set.empty) patterns
  mapM_ putStrLn outputLines
