{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Day24 where

import Data.List.Split
import Data.List
import Control.Lens
import Control.Monad
import Data.Functor.Foldable
import Text.Show.Deriving
import Data.Function

data TreeF a r 
    = LeafF a
    | NodeF a [r]
    deriving (Show, Functor)
$(deriveShow1 ''TreeF)

type Component = (Int,Int)
type Tree a = Fix (TreeF a)

part1 = strongest input
part2 = longestStrongest input

treeCoalg :: ([Component], Component) -> TreeF Component ([Component], Component)
treeCoalg x
    | null xs = LeafF $ snd x
    | otherwise = NodeF (snd x) xs
    where xs = getNext x

longestStrongest :: [Component] -> Int
longestStrongest cs = snd $ maximumBy longestThenStrongest $ hylo alg treeCoalg (cs,(0,0))
    where
        longestThenStrongest (l,s) (l',s')
            | l == l' = compare s s'
            | otherwise = compare l l'
        alg :: TreeF Component [(Int,Int)] -> [(Int,Int)]
        alg (LeafF (x,y)) = [(1,x + y)]
        alg (NodeF (x,y) ls) = concatMap (map (\(l,s) -> (l+1,x+y+s))) ls

strongest :: [Component] -> Int
strongest cs = maximum $ hylo alg treeCoalg (cs,(0,0))
    where
        alg :: TreeF Component [Int] -> [Int]
        alg (LeafF (x,y)) = [x + y]
        alg (NodeF (x,y) ls) = concatMap (map ((x+y) +)) ls

getNext :: ([Component],Component) -> [([Component],Component)]
getNext (cs,(_,y)) = [(delete (x',y') cs, if x'== y then (x',y') else (y',x')) | (x',y') <- cs, x' == y || y' == y]

strength :: [Component] -> Int
strength = sum . map (sumOf both)

parseInput :: String -> [Component]
parseInput = map ((\[x,y] -> (read x,read y)) . splitOn "/") . splitOn "\n"

test = parseInput "0/2\n2/2\n2/3\n3/4\n3/5\n0/1\n10/1\n9/10"
input = parseInput "24/14\n30/24\n29/44\n47/37\n6/14\n20/37\n14/45\n5/5\n26/44\n2/31\n19/40\n47/11\n0/45\n36/31\n3/32\n30/35\n32/41\n39/30\n46/50\n33/33\n0/39\n44/30\n49/4\n41/50\n50/36\n5/31\n49/41\n20/24\n38/23\n4/30\n40/44\n44/5\n0/43\n38/20\n20/16\n34/38\n5/37\n40/24\n22/17\n17/3\n9/11\n41/35\n42/7\n22/48\n47/45\n6/28\n23/40\n15/15\n29/12\n45/11\n21/31\n27/8\n18/44\n2/17\n46/17\n29/29\n45/50"