{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List (group, sort)
import Data.List.Split (splitOn)
import AOC2021 ( load, readInt )
import Data.List.Unique (uniq)
import Control.Arrow ((&&&))
import Debug.Trace (traceShowId)

type Coordinate = (Int, Int)
data Line = Line Coordinate Coordinate deriving Show

parse = map readInt . splitOn ","

main = do
        initial <- load parse "data/day7"
        print $ part1 initial
        print $ part2 initial

fuelRate n = sum [1..n]

part1 list = minimum . map (sum . map abs) . (<*>) (map f [min'..max']) . pure $ list
    where
        max' = maximum list
        min' = minimum list
        f x  = map (x -)


part2 list = minimum . map (sum . map (fuelRate . abs)) . (<*>) (map f [min'..max']) . pure $ list
    where
        max' = maximum list
        min' = minimum list
        f x  = map (x -)