{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List (transpose, tails, intercalate)
import Data.Char(digitToInt)
import AOC2021 ( load )
import Control.Applicative ((<*>))

parse = lines

main = do
        list <- load parse "data/day3"
        let mat = transpose . inputTrans $ list
        print $ part1 mat
        print $ part2 mat


inputTrans = map (map digitToInt)

calcColBit comp = fromEnum
        . uncurry comp
        . head
        . (\xs -> [ (x,y) | (x:rest) <- tails xs , y <- rest ])
        . (<*>) [zeros',ones']
        where
                ones' = sum
                zeros' x = length x - ones' x

solve f' = map (concatMap show) . (<*>) f' . pure

part1 = solve (map f [(<), (>)])
        where f comp = map (calcColBit comp . pure)

part2 = solve (map f [oxygen, scrubber])
        where
                oxygen = (<=)
                scrubber a1 a2 = not $ oxygen a1 a2
                f _ [] = []
                f comp (x:xs) = case length . transpose $ (x:xs) of
                                1 -> head . transpose $ (x:xs)
                                _ -> b : f comp xs'
                        where   b = fromEnum $ calcColBit comp $ pure x
                                xs' = case transpose . filterRows b . transpose $ (x:xs) of
                                        [] -> []
                                        (_:xs') -> xs'
                                filterRows b = filter ((==b) . head)