{-# LANGUAGE BangPatterns #-}

module Day17 where

import Data.Bool
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as V

part1 = xs ! (i+1) `mod` V.length xs
    where (xs,i,_) = last $ take 2018 $ iterate (step input) (V.singleton 0,0,1)
part2 = countWraps input 0

step :: Int -> (V.Vector Int, Int,Int) -> (V.Vector Int, Int,Int)
step s (xs,i,v) = (xs',idx,v+1)
    where 
        !xs' = (V.++) x $ v `V.cons` y
        (x,y) = V.splitAt idx xs
        idx = 1 + (s+i) `mod` V.length xs

countWraps :: Int -> Int -> Int
countWraps stepSize wrapVal = insert 0 1 0 where
    insert pos len next
        | len' > 50000000 = next - 1
        | otherwise = insert (pos' + 1) len' next'
        where
           pos' = (pos + stepSize) `mod` len
           len' = len + 1
           !next' = if pos' == wrapVal then len' else next

test :: Int
test = 3
input :: Int
input = 335