module Lib
    ( 
        Cell(..),
        Pattern(..),
        loadLines,
        countNeighbors,
        nextLine,
        shiftLeft,
        shiftRight,
        detectPattern,
        iterateUntilPattern
    ) where

import Data.List
import qualified Data.Set as Set
import Control.Arrow

data Cell = Filled | Blank deriving (Show, Eq, Ord)
type Line = [Cell] 

isFilled Filled = True
isFilled Blank = False

decodeCell :: Char -> Cell
decodeCell '#' = Filled
decodeCell '.' = Blank

decodeLine :: String -> Line
decodeLine = fmap decodeCell

loadLines :: FilePath -> IO [Line]
loadLines = readFile >>> fmap (lines >>> fmap decodeLine)

countNeighbors :: Line -> [Int]
countNeighbors xs = [
  length [ () | i <- [n - 2..n + 2], inRange i, i /= n, isFilled (xs !! i) ]
              | n <- [0..len - 1] ]
  where
    len = length xs
    inRange i = i >= 0 && i < len

nextCell :: (Cell, Int) -> Cell
nextCell (_, 2)      = Filled
nextCell (Blank, 3)  = Filled
nextCell (Filled, 4) = Filled
nextCell _           = Blank

nextLine :: Line -> Line
nextLine xs = fmap nextCell $ zip xs $ countNeighbors xs

data Pattern = Blinking | Gliding | Vanishing deriving (Show, Eq)

shiftLeft :: Line -> Int -> Line
shiftLeft xs n = take (length xs) $ drop n xs ++ repeat Blank

shiftRight :: Line -> Int -> Line
shiftRight xs n = take (length xs) $ replicate n Blank ++ xs

detectPattern :: Set.Set Line -> Line -> Maybe Pattern
detectPattern _     xs | all (==Blank) xs                 = Just Vanishing
detectPattern prevs xs | Set.member xs prevs              = Just Blinking
detectPattern prevs xs | not $ Set.disjoint prevs shifted = Just Gliding
  where 
    shifted = Set.fromList $ [0..length xs - 1] >>= (\n -> [shiftLeft xs n, shiftRight xs n])
detectPattern _ _ = Nothing

depthLimit :: Int
depthLimit = 100

debugPrintLine = putStrLn . formatLine
  where
    formatLine = fmap formatCell
    formatCell Blank = '.'
    formatCell Filled = '#'

iterateUntilPattern :: Set.Set Line -> Line -> IO String
iterateUntilPattern prevs _ | length prevs >= depthLimit = return "other"
iterateUntilPattern prevs line = do
  debugPrintLine line
  case pattern of
    Just Vanishing                     -> return "vanishing"
    Just Blinking                      -> return "blinking"
    Just Gliding                       -> return "gliding" 
    _                                  -> iterateUntilPattern nextPrevs next
  where
    next      = nextLine line
    nextPrevs = Set.insert line prevs
    pattern   = detectPattern prevs line