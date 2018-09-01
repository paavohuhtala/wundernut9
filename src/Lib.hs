module Lib
    ( 
        Cell(..),
        Line,
        Pattern(..),
        parseLine,
        countNeighbors,
        nextLine,
        shiftLeft,
        shiftRight,
        detectPattern,
        evolveUntilPattern
    ) where

import Data.List
import qualified Data.Set as Set
import Control.Arrow

data Cell = Filled | Blank deriving (Show, Eq, Ord)

parseCell :: Char -> Cell
parseCell '#' = Filled
parseCell '.' = Blank

type Line = [Cell]

parseLine :: String -> Line
parseLine = fmap parseCell

countNeighbors :: Line -> [Int]
countNeighbors xs = fmap countWindow $ windowed $ [Blank, Blank] ++ xs ++ [Blank, Blank]
  where
    windowed = tails >>> fmap (take 5) >>> takeWhile (length >>> (==5))
    countWindow [a, b, _, c, d] = length . filter (==Filled) $ [a, b, c, d]

nextCell :: (Cell, Int) -> Cell
nextCell (_, 2)      = Filled
nextCell (Blank, 3)  = Filled
nextCell (Filled, 4) = Filled
nextCell _           = Blank

nextLine :: Line -> Line
nextLine xs = fmap nextCell $ zip xs $ countNeighbors xs

data Pattern = Blinking | Gliding | Vanishing | Other deriving (Show, Eq)

shiftLeft :: Line -> Int -> Line
shiftLeft xs n = take (length xs) $ drop n xs ++ repeat Blank

shiftRight :: Line -> Int -> Line
shiftRight xs n = take (length xs) $ replicate n Blank ++ xs

detectPattern :: Set.Set Line -> Line -> Pattern
detectPattern _     xs | all (==Blank) xs                 = Vanishing
detectPattern prevs xs | Set.member xs prevs              = Blinking
detectPattern prevs xs | not $ Set.disjoint prevs shifted = Gliding
  where 
    shifted = Set.fromList $ [1..length xs - 1] >>= (\n -> [shiftLeft xs n, shiftRight xs n])
detectPattern _ _ = Other

evolveUntilPattern :: Line -> String
evolveUntilPattern = evolve Set.empty >>> show
  where
    evolve :: Set.Set Line -> Line -> Pattern
    evolve prevs line =
        case detectPattern prevs line of
        Other | length prevs < depthLimit -> evolve nextPrevs next
        x                                 -> x
      where
        nextPrevs = Set.insert line prevs
        next      = nextLine line
        depthLimit = 100
