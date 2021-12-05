{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List (transpose, tails, intercalate, group, sort)
import Data.List.Split (splitOn)
import Data.Char(digitToInt)
import AOC2021 ( load )
import Control.Applicative ((<*>))
import Data.List.Unique (uniq)
import Control.Arrow ((&&&))
import Debug.Trace (traceShowId)

type Coordinate = (Int, Int)
data Line = Line Coordinate Coordinate deriving Show

parseLine [x1, y1,  _, x2, y2] = Line (read x1, read y1) (read x2, read y2)

parse = map (parseLine . (concatMap (splitOn ",") . words)) . lines

main = do
        list <- load parse "data/day5"
        print $ part1 list
        print $ part2 list

activePoints' a b = if a <= b then [a..b] else [a,a-1..b]
activePoints (Line (x1,y1) (x2,y2)) = take k (zip (cycle x) (cycle y))
        where
                x = activePoints' x1 x2
                y = activePoints' y1 y2
                k = maximum [length x, length y]

frequency =  map (length &&& head) . group . sort

part1 = part2 . filter f
        where f (Line (x1,y1) (x2,y2)) = (x1==x2) || (y1==y2)

part2 = length . filter (\p -> fst p > 1) . frequency . concatMap activePoints