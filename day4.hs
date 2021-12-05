{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List (group, sort, transpose)
import Data.List.Split (splitOn, splitWhen)
import AOC2021 ( load, readInt )
import Data.List.Unique (uniq)
import Control.Arrow ((&&&))

type Row = [Int]
type Column = [Int]
data Board = Board [Row] [Column] deriving Show

parse = (<*>) [(:[]) . head, (:[]) . head . tail, tail . tail] . pure . lines

parseRow = map readInt . words
parseBoard r = Board r (transpose r)
parseBoards = map (parseBoard . map parseRow) . splitWhen (=="")

parseNumbers = map readInt . splitOn ","

main = do
        p <- load parse "data/day4_short"
        let [numbers,_,boards] = p
        let bs = parseBoards boards
        let ns = parseNumbers $ head numbers
        print $ part1 ns bs


--checkWin (Board r c) n = elem 0 $ map (length . filter (\x -> all ($ x))) 

p n x = not $ any (($x) . (==)) n

checkWin ns (Board r c) = win r || win c
    where win x = [] `elem` map (filter (p ns)) x

draw ns' (n:ns) bs = case filter (checkWin (n:ns')) bs of
    []  -> draw (n:ns') ns bs
    [Board r _] -> (n,sum $ concatMap (filter (p (n:ns'))) r)

part1 = draw []

draw' ns' (n:ns) bs = case filter (checkWin (n:ns')) bs of
    []  -> draw' (n:ns') ns bs
    [Board r c] -> (n,sum $ concatMap (filter (p (n:ns'))) r)