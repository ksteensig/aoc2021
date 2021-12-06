{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List (group, sort)
import Data.List.Split (splitOn)
import AOC2021 ( load, readInt )
import Data.List.Unique (uniq)
import Control.Arrow ((&&&))
import Debug.Trace (traceShowId)

type Coordinate = (Int, Int)
data Line = Line Coordinate Coordinate deriving Show

parse = map readInt . concatMap (concatMap (splitOn ",") . words) . lines

main = do
        initial <- load parse "data/day6"
        print $ part1 initial


transform n list = case n of
    0 -> list
    _ -> transform (n-1) $ ns ++ zs
    where
        ns =  map (+ (-1)) . filter (/=0) $ list
        zs = take (2*(length . filter (== 0) $ list)) $ cycle [6,8]

part1 = length . transform 80

--part1 = length . transform 80