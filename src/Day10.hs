module Day10 where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Char
import Data.List.Split
import Data.Bits
import Data.List
import Numeric

part1 :: [Int]
part1 = hash [0..255] $ parseInput input

part2 :: String
part2 = concatMap (hexRep . xord) $ chunksOf 16 $ hash [0..255] $ concat $ replicate 64 $ (++ [17, 31, 73, 47, 23]) $ map ord input
    where 
        hexRep x
            | length (showHex x "") == 1 = "0" ++ showHex x ""
            | otherwise = showHex x ""
        xord (x:xs) = foldl' xor x xs

hash :: [Int] -> [Int] -> [Int]
hash inp xs = runST $ do
    n <- V.thaw $ V.fromList inp
    go 0 0 0 n
    where
        go ix curr skip v
            | ix == length xs = V.toList <$> V.freeze v
            | otherwise = do
                zipWithM_ (M.swap v) (halfOf range) (halfOf $ reverse range)
                go (ix + 1) ((curr + size + skip) `mod` length inp) (skip + 1) v 
            where
                halfOf = take (length range `div` 2)
                range
                    | curr + size < length inp = [curr..(curr+size - 1)]
                    | otherwise = [curr..(length inp -1)] ++ [0..(size - ((length inp + 1) - curr))]
                size = xs !! ix

parseInput :: String -> [Int]
parseInput = map read . splitOn ","

input :: String
input = "31,2,85,1,80,109,35,63,98,255,0,13,105,254,128,33"
test :: String
test = "3,4,1,5"