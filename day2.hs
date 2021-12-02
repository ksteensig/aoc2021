{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List ( tails, transpose )
import AOC2021 ( load, readInt )
import Data.Bifunctor (bimap)

parseWord s = let x = words s in (head x, readInt . last $ x)

parse = map parseWord . lines

type Depth = Int
type Height = Int
type Aim = Int
data Position = Position Depth Height Aim

instance Show Position where
  show (Position d h a) = "Depth: " ++ show d ++ " - Height: " ++ show h ++ " - Aim: " ++ show a

initialPos = Position 0 0 0

main = do
        list <- load parse "data/day2"
        print $ part1 list
        print $ part2 list initialPos

directions = ["forward", "up", "down"]
direction d = sum . map snd . filter ((==d) . fst)

apply' = zipWith ($)

part1 list = Position f (d-u) 0
    where
        [f,u,d] = map ($ list) $ apply' [direction, direction, direction] directions

transform ("forward", x) (Position d h a) = Position (d + x*a) (h+x) a
transform ("up", x) (Position d h a)      = Position d h (a-x)
transform ("down", x) (Position d h a)    = Position d h (a+x)

part2 [x] (Position d h a) = transform x (Position d h a)
part2 (x:xs) (Position d h a) = part2 xs $ transform x (Position d h a)