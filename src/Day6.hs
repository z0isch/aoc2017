module Day6 where   

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List

part1 = (+) (-2) $ length $ takeWhileInclusive (all (<=1) . M.elems) $ solve $ map read $ words input
part2 = (+) (- part1) $ (+) (-2) $ length $ takeWhileInclusive (all (<=2) . M.elems) $ solve $ map read $ words input

solve :: [Int] -> [Map [Int] Integer]
solve = scanl (flip $ M.alter foo) M.empty . iterate doStep
    where 
        foo Nothing = Just 1
        foo (Just x) = Just (x + 1)


doStep :: [Int] -> [Int]
doStep ns = zipWith (+) incByOne $ map (+ incrementAll) $ updateEl (const 0) maxIndex ns
    where
        numEl = length ns
        (maxIndex,max) = getMaxAndIndex ns
        incrementAll = max `div` numEl
        rest = max `rem` numEl
        incByOne = shift (numEl - (maxIndex + 1)) $ replicate rest 1 ++ replicate (numEl - rest) 0
        
getMaxAndIndex :: [Int] -> (Int,Int)
getMaxAndIndex = snd . foldl' (\(i,(mI,m)) n -> if n > m then (i+1,(i,n)) else (i+1,(mI,m))) (0,(0,0))

updateEl :: (a -> a) -> Int -> [a] -> [a]
updateEl f x xs = h ++ [f (xs !! x)] ++ t
    where (h,_:t) = splitAt x xs

shift :: Int -> [a] -> [a]
shift i xs = map (\ a -> xs !! ((i + a) `mod` length xs)) [0,1..length xs - 1]

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []

test = "0 2 7 0"
input = "2 8 8 5 4 2 3 1 5 5 1 2 15 13 5 14"