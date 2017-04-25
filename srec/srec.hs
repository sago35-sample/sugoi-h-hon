--{-# OPTIONS -Wall -Werror #-}

import Data.Char
import Data.Bits

main :: IO ()
main = interact q

q :: String -> String
q = unlines .
    map (\xs -> parseRecord xs) .
    filter (\line -> isS0Record line) .
    lines

p :: String -> String
p = unlines .
    map (\xs -> parseS0Record xs) .
    filter (\line -> isS0Record line) .
    lines

parseRecord :: String -> String
parseRecord ('S':'0':xs) = parseS0Record xs
parseRecord _ = "1"

isS0Record :: String -> Bool
isS0Record [] = False
isS0Record [_] = False
isS0Record (x:y:_) = x == 'S' && y == '0'

parseS0Record :: String -> String
parseS0Record [] = ""
parseS0Record [_] = ""
parseS0Record a@(_:_:xs) = do
    let len = getLength (take 2 xs)
        addr = getAddress $ take 4 $ drop 2 xs
    a ++ "\n" ++ show len ++ " " ++ show addr

hexString2dec :: String -> Int
hexString2dec xs = foldl (\acc x -> (shiftL acc 4) + hex2dec x) 0 xs

getAddress :: String -> Int
getAddress = hexString2dec

getLength :: String -> Int
getLength [x, y] = (shiftL (hex2dec x) 4) + (hex2dec y)
getLength _ = 0

hex2dec :: Char -> Int
hex2dec x = if ord 'A' <= ord x && ord x <= ord 'F'
                then ord x - ord 'A' + 10
                else ord x - ord '0'
