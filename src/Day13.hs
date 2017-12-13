module Day13 where

import Text.Trifecta
import Data.List
import Data.Function

type Layer = (Integer, Integer)

part1 = severity layers 0
    where 
        layers = fillIn $ sortOn fst m
        (Success m) = parseInput input
part2 = (-1) + length (takeWhileInclusive (/= 0) severitys)
    where
        severitys = map (severity layers) [0..]
        layers = fillIn $ sortOn fst m
        (Success m) = parseInput input

severity :: [Layer] -> Integer  -> Integer
severity ls d = sum $ map (\(x,y) -> if isAtTop d (x,y) then (x+d)*y else 0) ls

isAtTop :: Integer -> Layer -> Bool
isAtTop _ (_,0) = False
isAtTop d (x,y) = (x+d) `mod` m == 0
        where m = 2*y-2

fillIn :: [Layer] -> [Layer]
fillIn (x:xs) = concat $ zipWith mkBetween (x:xs) $ map head (init $ tails xs) ++ [last xs]
    where
        mkBetween p1@(x,_) (x',_)
            | x' == x+1 = [p1]
            | otherwise = p1:zip nextOnes (repeat 0)
            where nextOnes =  [x+1..x'-1]

parseInput :: String -> Result [Layer]
parseInput = parseString (some (layerP <* skipOptional newline)) mempty

layerP :: Parser Layer
layerP = (,) <$> (read <$> some digit) <*> (string ": " *> (read <$> some digit))

test = "0: 3\n1: 2\n4: 4\n6: 4"
input = "0: 3\n1: 2\n2: 4\n4: 6\n6: 5\n8: 6\n10: 6\n12: 4\n14: 8\n16: 8\n18: 9\n20: 8\n22: 6\n24: 14\n26: 12\n28: 10\n30: 12\n32: 8\n34: 10\n36: 8\n38: 8\n40: 12\n42: 12\n44: 12\n46: 12\n48: 14\n52: 14\n54: 12\n56: 12\n58: 12\n60: 12\n62: 14\n64: 14\n66: 14\n68: 14\n70: 14\n72: 14\n80: 18\n82: 14\n84: 20\n86: 14\n90: 17\n96: 20\n98: 24"

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []