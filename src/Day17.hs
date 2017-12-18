{-# LANGUAGE BangPatterns #-}

module Day17 where

import Data.Bool
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as V

part1 = xs ! (i+1) `mod` V.length xs
    where (xs,i,_) = last $ take 2018 $ iterate (step input) (V.singleton 0,0,1)

part2 = wrapArounds input

step :: Int -> (V.Vector Int, Int,Int) -> (V.Vector Int, Int,Int)
step s (xs,i,v) = (xs',idx,v+1)
    where 
        !xs' = (V.++) x $ v `V.cons` y
        (x,y) = V.splitAt idx xs
        idx = 1 + (s+i) `mod` V.length xs

wrapArounds :: Int -> Int
wrapArounds s = insert 0 1 0 where
    insert pos len next
        | len' > 50000000 = next
        | otherwise = insert pos' len' $ if pos' == 1 then  len + q else next
        where
            q = (len - pos) `div` (s + 1)
            pos' = (pos + (q + 1) * (s + 1) - 1) `mod` (len + q) + 1
            len' = len + q + 1

test = 3
input = 335