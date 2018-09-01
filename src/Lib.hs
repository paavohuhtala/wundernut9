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
import Control.Arrow

data Cell = Filled | Blank deriving (Show, Eq)

isFilled Filled = True
isFilled Blank = False

decodeCell :: Char -> Cell
decodeCell '#' = Filled
decodeCell '.' = Blank

decodeLine :: String -> [Cell]
decodeLine = map decodeCell

loadLines :: FilePath -> IO [[Cell]]
loadLines = readFile >>> fmap (lines >>> map decodeLine)

countNeighbors :: [Cell] -> [Int]
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

nextLine :: [Cell] -> [Cell]
nextLine xs = map nextCell $ zip xs $ countNeighbors xs

data Pattern = Blinking | Gliding | Vanishing deriving (Show, Eq)

shiftLeft :: [Cell] -> Int -> [Cell]
shiftLeft xs n = take (length xs) $ drop n xs ++ repeat Blank

shiftRight :: [Cell] -> Int -> [Cell]
shiftRight xs n = take (length xs) $ replicate n Blank ++ xs

detectPattern :: [[Cell]] -> [Cell] -> Maybe Pattern
detectPattern _     xs | all (==Blank) xs                     = Just Vanishing
detectPattern prevs xs | elem xs prevs                        = Just Blinking
detectPattern prevs xs | any (\sxs -> elem sxs prevs) shifted = Just Gliding
  where 
    shifted = nub . concat $ [[shiftLeft xs n, shiftRight xs n] | n <- [0..length xs - 1]]
detectPattern _     _                                         = Nothing

depthLimit :: Int
depthLimit = 100

iterateUntilPattern :: [[Cell]] -> [Cell] -> String
iterateUntilPattern prevs line =
  case pattern of
    Just Vanishing                     -> "vanishing"
    Just Blinking                      -> "blinking"
    Just Gliding                       -> "gliding" 
    _ | length nextPrevs == depthLimit -> "other"
    _                                  -> iterateUntilPattern nextPrevs next
  where
    next      = nextLine line
    nextPrevs = line : prevs
    pattern   = detectPattern nextPrevs next 