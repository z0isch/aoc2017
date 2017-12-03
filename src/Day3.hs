module Day3 where

import Data.List

input = 325489

part1 = (manhatten . snd) <$> findArr input
part2 = last $ takeWhile (\xs -> all (<= input) $ map fst xs) $ mkSums


circ = map (\(d,(x,y)) -> (zip (map (+ offset d) [2..(d +(d-1)*3)]) $ mkMoves d (x,y)))
    $ zip [3,5..]
    $ map (\x -> (x+1,-x)) [0..]
    where 
        offset 3 = 0
        offset x = (x-2) + ((x-3)*3) - 1 + offset (x-2)
        

findArr x = find (\(d,_) -> d == x) $ last $ takeWhileInclusive (\xs -> x `notElem` map fst xs) circ

mkSums = scanl (\xs c -> xs ++ [(getSumOfNeighbors xs c, c)]) [(1,(0,0))] $ map snd $ concat circ

getSumOfNeighbors xs c = sum $ map fst $ intersectBy (\(_,c1) (_,c2) -> c1 == c2) xs $ zip (repeat 0) $ neighbors c

manhatten (x,y) = abs x + abs y

data Dir = U | L | R | D
    deriving (Eq, Ord, Show)

mkMove (x,y) U = (x,y+1)
mkMove (x,y) D = (x,y-1)
mkMove (x,y) L = (x-1,y)
mkMove (x,y) R = (x+1,y)

neighbors (x,y) = [(x+1,y), (x-1,y), (x,y+1),(x,y-1),(x-1,y-1), (x-1,y+1), (x+1,y-1), (x+1,y+1)]

moves d = replicate (d-2) U ++ replicate (d-1) L ++ replicate (d-1) D ++ replicate (d-1) R

mkMoves d c = scanl mkMove c $ moves d

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []