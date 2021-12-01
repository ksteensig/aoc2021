import System.IO ( openFile )
import Data.List ( tails, transpose )
import AOC2021 ( load, readInt ) 

parse = map readInt . words

main = do
        list <- load parse "data/day1"
        print $ part1 list
        print $ part2 list

differences fs = zipWith (-) fs $ tail fs

windows n = Data.List.transpose . take n . tails

part1 = length . filter (< 0) . differences

part2 = part1 . map sum . windows 3

