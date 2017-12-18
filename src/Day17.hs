{-# LANGUAGE BangPatterns #-}

module Day17 where

import Data.List

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Set as S
import Control.Monad.Primitive

part1 = (V.!) xs $ (i+1) `mod` V.length xs
    where (xs,i,_) = last $ take 2018 $ iterate (step input) (V.singleton 0, 0,1)

part2 =  (V.!) xs 1
    where (xs,i,_) = last $ take 50000000 $ iterate (step input) (V.singleton 0, 0,1)

step :: Int -> (V.Vector Int, Int,Int) -> (V.Vector Int, Int,Int)
step s (xs,i,v) = (xs',idx,v+1)
    where 
        !xs' = (V.++) x $ v `V.cons` y
        (x,y) = V.splitAt idx xs
        idx = 1 + (s+i) `mod` V.length xs

test = 3
input = 335