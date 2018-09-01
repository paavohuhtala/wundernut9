module Main where

import qualified Data.Set as Set
import Lib

main :: IO ()
main = do
  patterns <- loadLines "./patterns.txt"
  let outputLines = fmap (\p -> iterateUntilPattern Set.empty p >>= putStrLn) patterns
  sequence_ outputLines
