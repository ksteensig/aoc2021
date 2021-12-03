import Data.List (transpose, intercalate )
import Data.Char(digitToInt)
import AOC2021 ( load )

parse = lines

main = do
        list <- load parse "data/day3"
        print $ part1 list

part1 list = let gamma = map (fromEnum . (\d -> d*2 > length list) . (sum . map digitToInt)) $ Data.List.transpose list
             in (intercalate "" $
                    map show gamma,
                 intercalate "" $
                    map show $
                        zipWith (-) (replicate (length $ Data.List.transpose list) 1) gamma)