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
decodeLine = map decodeCell

loadLines :: FilePath -> IO [Line]
loadLines = readFile >>> fmap (lines >>> map decodeLine)

countNeighbors :: Line -> [Int]
countNeighbors xs = map (filter isFilled >>> length) neighbors
  where
    len = length xs
    inRange i = i >= 0 && i < len
    indices = map (\i -> filter inRange [i - 2, i -1, i + 1, i + 2]) [0 .. len - 1]
    neighbors = map (map (xs!!)) indices

nextCell :: (Cell, Int) -> Cell
nextCell (Filled, 2) = Filled
nextCell (Filled, 4) = Filled
nextCell (Blank, 2)  = Filled
nextCell (Blank, 3)  = Filled
nextCell _           = Blank

nextLine :: Line -> Line
nextLine xs = map nextCell $ zip xs $ countNeighbors xs

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

iterateUntilPattern :: Set.Set Line -> Line -> String
iterateUntilPattern prevs line =
  case pattern of
    Just Vanishing                     -> "vanishing"
    Just Blinking                      -> "blinking"
    Just Gliding                       -> "gliding" 
    _ | length nextPrevs == depthLimit -> "other"
    _                                  -> iterateUntilPattern nextPrevs next
  where
    next      = nextLine line
    nextPrevs = Set.insert line prevs
    pattern   = detectPattern nextPrevs next