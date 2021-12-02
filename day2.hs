{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List ( tails, transpose )
import AOC2021 ( load, readInt )

parseWord s = let x = words s in (head x, readInt . last $ x)

parse = map parseWord . lines

data Position = Position Int Int Int deriving Show

initialPos = Position 0 0 0

main = do
        list <- load parse "data/day2"
        print $ part1 list
        print $ part2 list initialPos

direction d = map snd . filter ((==d) . fst)

forward = direction "forward"

up = direction "up"

down = direction "down"

part1 list = Position f (d-u) 0
    where
        f = sum $ forward list
        u = sum $ up list
        d = sum $ down list

transform ("forward", x) (Position d h a) = Position (d + x*a) (h+x) a
transform ("up", x) (Position d h a)      = Position d h (a-x)
transform ("down", x) (Position d h a)    = Position d h (a+x)

part2 [x] (Position d h a) = transform x (Position d h a)
part2 (x:xs) (Position d h a) = part2 xs $ transform x (Position d h a)