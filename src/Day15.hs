module Day15 where

import Data.Bits

part1 = length $ filter agree $ take 40000000 $ tail $ iterate next input
part2 = length $ filter agree $ take 5000000 $ tail $ iterate next2 input

nextX :: Int -> Int
nextX x = 16807 * x `mod` 2147483647
nextY :: Int -> Int
nextY y = 48271 * y `mod` 2147483647

next :: (Int,Int) -> (Int,Int)
next (x,y) = (nextX x, nextY y)

next2 :: (Int,Int) -> (Int,Int)
next2 (x,y) = (f 4 (tail $ iterate nextX x), f 8 (tail $ iterate nextY y))
    where f m= head . filter (\x' -> x' `mod` m == 0)

agree :: (Int,Int) -> Bool
agree (x,y) = countTrailingZeros (x `xor` y) >= 16

test :: (Int, Int)
test=(65,8921)
input :: (Int, Int)
input= (634,301)