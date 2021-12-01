import System.IO
    ( hClose, hGetContents, openFile, IOMode(ReadMode) )
import Data.List ( tails, transpose )

main = do
        let list = []
        handle <- openFile "input" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            list = map readInt singlewords
        print $ part1 list
        print $ part2 list
        hClose handle

readInt :: String -> Int
readInt = read

part1 = length . filter (< 0) . differences

part2 = part1 . map sum . windows 3

differences fs = zipWith (-) fs (tail fs)
windows n xs = Data.List.transpose (take n (tails xs))