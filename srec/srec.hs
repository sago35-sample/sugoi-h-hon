{-# OPTIONS -Wall -Werror #-}

import Data.Char
import Data.Bits

main :: IO ()
main = interact p

p :: String -> String
p = unlines .
    map (\xs -> parseS0Record xs) .
    filter (\line -> isS0Record line) .
    lines

isS0Record :: String -> Bool
isS0Record [] = False
isS0Record [_] = False
isS0Record (x:y:_) = x == 'S' && y == '0'

parseS0Record :: String -> String
parseS0Record [] = ""
parseS0Record [_] = ""
parseS0Record (_:_:xs) = do
    let len = getLength (take 2 xs)
    printLength len

--getAddress :: String -> Int
--getAddress _ = 0

getLength :: String -> Int
getLength [x, y] = (shiftL (hex2dec x) 4) + (hex2dec y)
getLength _ = 0

hex2dec :: Char -> Int
hex2dec x = if ord 'A' <= ord x && ord x <= ord 'F'
                then ord x - ord 'A' + 10
                else ord x - ord '0'

printLength :: Int -> String
printLength x = show x
