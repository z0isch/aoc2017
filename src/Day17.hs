module Day17 where

import Data.List

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Set as S
import Control.Monad.Primitive

part1 = xs !! (i+1) `mod` length xs
    where (xs,i,_) = last $ take 2018 $ iterate (step input) ([0],0,1)
part2 _ = xs !! 1
    where (xs,i,_) = last $ take 2018 $ iterate (step input) ([0],0,1)

part2' _ = (V.!) xs 1
    where (xs,i,_) = last $ take 2018 $ iterate (step' input) (V.singleton 0, 0,1)

step' :: Int -> (V.Vector Int, Int,Int) -> (V.Vector Int, Int,Int)
step' s (xs,i,v) = (xs', idx `mod` V.length xs',v+1)
    where 
        xs' = (V.++) x $ (V.++) (V.singleton v) y
        (x,y) = V.splitAt idx xs
        idx = 1 + (s+i) `mod` V.length xs

step :: Int -> ([Int],Int,Int) -> ([Int],Int,Int)
step s (xs,i,v) = (xs', idx `mod` length xs',v+1)
    where 
        xs' = x ++ v:y
        (x,y) = splitAt idx xs
        idx = 1 + (s+i) `mod` length xs

test = 3
input = 335