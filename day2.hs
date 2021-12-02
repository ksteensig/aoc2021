{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List ( tails, transpose, foldl' )
import AOC2021 ( load, readInt )

data Direction = Forward Int | Up Int | Down Int

strToDirection "forward" x = Forward x
strToDirection "up" x      = Up x
strToDirection "down" x    = Down x

parseDirection :: [String] -> Direction
parseDirection [s, x] = strToDirection s $ readInt x

parse = map (parseDirection . words) . lines

type Depth = Int
type Height = Int
type Aim = Int
data Position = Position Depth Height Aim

instance Show Position where
  show (Position d h a) = "Depth: " ++ show d ++ " - Height: " ++ show h ++ " - Aim: " ++ show a

initial = Position 0 0 0

main = do
        list <- load parse "data/day2"
        print $ part1 list
        print $ part2 list

part1transform (Position d h a) (Forward x)  = Position d (h+x) a
part1transform (Position d h a) (Up x)       = Position (d-x) h a
part1transform (Position d h a) (Down x)     = Position (d+x) h a

part2transform (Position d h a) (Forward x)  = Position (d + x*a) (h+x) a
part2transform (Position d h a) (Up x)       = Position d h (a-x)
part2transform (Position d h a) (Down x)     = Position d h (a+x)

part1 = foldl' part1transform initial
part2 = foldl' part2transform initial