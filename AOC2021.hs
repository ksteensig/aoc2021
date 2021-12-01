module AOC2021 where

load parse file = do
    contents <- readFile file
    return $ parse contents

readInt :: String -> Int
readInt = read