module Day14 where

import Day10 (knotHash)
import Numeric 
import Data.Char
import Data.Graph
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List

part1 = sum $ map (length . filter ('1' ==)) $ grid input
part2 = length $ filter (\(Node a _) -> a `S.member` c) $ dff g
  where
    g = buildG (0,128*128) (mkEdges c) 
    mkEdges = foldl' (\xs x -> xs ++ map (\y -> (y,x)) (filter (`S.member` c) (neighbors x))) []
    c = usedCoords input

usedCoords :: String -> S.Set Int
usedCoords = mkSet . concat . zipWith (\x c -> zipWith (\y n -> ((x*128)+y,n == '1')) [0..127] c) [0..127] . grid
  where mkSet = foldl' (\s (x,b) -> if b then S.insert x s else s) S.empty 

grid :: String -> [String]
grid i =  map (concatMap hexToBinary . knotHash . (\y -> i ++ "-" ++ show y)) [0..127] 

coord :: Int -> (Int,Int)
coord x = x `divMod` 128

neighbors :: Int -> [Int]
neighbors x = concat 
  [ if m == 0 then [] else [x-1]
  , if m == 127 then [] else [x+1]
  , if d == 0 then [] else [x-128]
  , if d == 127 then [] else [x+128]
  ] 
  where (d,m) = x `divMod` 128

hexToBinary :: Char -> String
hexToBinary s = replicate (4 - length bin) '0' ++ bin
  where bin = showIntAtBase 2 intToDigit (fst $ head $ readHex [s]) ""

test="flqrgnkx"
input="stpzcrnm"