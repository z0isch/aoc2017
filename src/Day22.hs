{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Day22 where

import Text.Trifecta hiding (dot)
import Linear.Matrix
import Linear.V2
import Linear.Vector
import Linear.Metric 
import Data.Maybe
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Control.Lens

data Dir = U | D | L | R
  deriving (Eq,Show)

data VirusState = VirusState
  { _infected :: HashSet (V2 Int)
  , _weakened :: HashSet (V2 Int)
  , _flagged :: HashSet (V2 Int)
  , _pos :: V2 Int
  , _facing :: Dir
  , _infections :: Int
  }
  deriving (Eq,Show)
makeLenses ''VirusState

part1 = view infections $ last $ take 10001 $ iterate step initS
part2 = view infections $ last $ take 10000001 $ iterate step' initS
initS = let (Success i) = parseString stateP mempty input in i

turn :: Dir -> Dir -> Dir 
turn L = \case
  U -> L
  D -> R
  L -> D
  R -> U
turn R = \case
  U -> R
  D -> L
  L -> U
  R -> D

dirV :: Dir -> V2 Int
dirV U = V2 0 1
dirV D = V2 0 (-1)
dirV L = V2 (-1) 0
dirV R = V2 1 0

move :: Dir -> V2 Int -> V2 Int
move d = (^+^) (dirV d)

reverseDir :: Dir -> Dir
reverseDir U = D
reverseDir R = L
reverseDir L = R
reverseDir D = U

step' :: VirusState -> VirusState
step' (VirusState i w fl p f nI) = VirusState i' w' fl' p' f' nI'
  where 
    isInfected = p `S.member` i
    isWeakened = p `S.member` w
    isFlagged = p `S.member` fl
    p' = move f' p
    (!i',!w',!fl') 
      | isInfected = (S.delete p i, w, S.insert p fl)
      | isWeakened = (S.insert p i, S.delete p w, fl)
      | isFlagged = (i, w, S.delete p fl)
      | otherwise = (i, S.insert p w, fl)
    nI' = if isWeakened then nI+1 else nI
    f'
      | isInfected = turn R f 
      | isWeakened = f
      | isFlagged = reverseDir f
      | otherwise = turn L f

step :: VirusState -> VirusState
step (VirusState i w fl p f nI) = VirusState i' w fl p' f' nI'
  where 
    isInfected = p `S.member` i
    p' = move f' p
    i' = if isInfected then S.delete p i else S.insert p i
    nI' = if isInfected then nI else nI+1
    f' = if isInfected then turn R f else turn L f

stateP :: Parser VirusState
stateP = mkState . over _1 (\s -> V2 (s`div`2) (-s`div` 2)) . over _2 (S.fromList . catMaybes) . mV <$> sepBy1 (some (oneOf ".#")) (char '\n')
  where
    mkState (x,y) = VirusState y S.empty S.empty x U 0
    mV :: [String] -> (Int,[Maybe (V2 Int)])
    mV ls = (length ls, concatMap (\(y,cs) -> zipWith (\x c -> if c == '.' then Nothing else Just (V2 x y)) [0..(length cs-1)] cs) $ zip [0,-1..(-(length ls-1))] ls)


test = "..#\n#..\n..."
input = "...###.#.#.##...##.#..##.\n.#...#..##.#.#..##.#.####\n#..#.#...######.....#####\n.###.#####.#...#.##.##...\n.#.#.##......#....#.#.#..\n....##.##.#..##.#...#....\n#...###...#.###.#.#......\n..#..#.....##..####..##.#\n#...#..####.#####...#.##.\n###.#.#..#..#...##.#..#..\n.....##..###.##.#.....#..\n#.....#...#.###.##.##...#\n.#.##.##.##.#.#####.##...\n##.#.###..#.####....#.#..\n#.##.#...#.###.#.####..##\n#.##..#..##..#.##.####.##\n#.##.#....###.#.#......#.\n.##..#.##..###.#..#...###\n#..#.#.#####.....#.#.#...\n.#####..###.#.#.##..#....\n###..#..#..##...#.#.##...\n..##....##.####.....#.#.#\n..###.##...#..#.#####.###\n####.########.#.#..##.#.#\n#####.#..##...####.#..#.."