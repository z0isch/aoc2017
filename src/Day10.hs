module Day10 where

import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Text.Trifecta

part1 = hash [0..255] m 
    where (Success m) = parseString inputP mempty input
part2 = undefined

hash :: [Int] -> [Int] -> [Int]
hash inp xs = runST $ do
    n <- V.thaw $ V.fromList inp
    go 0 0 0 n
    where
        go ix curr skip v
            | ix == length xs = V.freeze v >>= return . V.toList
            | otherwise = do
                mapM_ (uncurry (M.swap v)) skips
                go (ix + 1) ((curr + size + skip) `mod` length inp) (skip + 1) v 
            where
                skips = take (length range `div` 2) $ zip range (reverse range)
                range
                    | curr + size < length inp = [curr..(curr+size - 1)]
                    | otherwise = [curr..(length inp -1)] ++ [0..(size - ((length inp + 1) - curr))]
                size = xs !! ix

inputP :: Parser [Int]
inputP = map read <$> commaSep1 (many digit)

input = "31,2,85,1,80,109,35,63,98,255,0,13,105,254,128,33"
test = "3,4,1,5"