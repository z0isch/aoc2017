module Day21 where

import Text.Trifecta hiding (dot)
import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.Vector
import Linear.Metric 
import Linear.Quaternion
import Data.Maybe
import Data.Map.Strict (Map,(!))
import qualified Data.Map.Strict as M
import Control.Lens
import Data.List

type Image = (Int,[V2 Int])
type Box = (V2 Int, V2 Int)

t::Image
t = (6,[V2 0 0,V2 1 0,V2 0 (-1),V2 3 0,V2 4 0,V2 3 (-1),V2 0 (-3),V2 1 (-3),V2 0 (-4),V2 3 (-3),V2 4 (-3),V2 3 (-4)])

part1 = iterate (step recipes) start
    
part2 = undefined

recipes = let (Success rs) = parseInput test in rs

printBox :: (Box, Image) -> IO ()
printBox ((sv,_),img) = printImage $ translate img (negate sv)

printImage :: Image -> IO ()
printImage (b,ons) = mapM_ putStrLn bar
  where 
    foo y x = if V2 x y `elem` ons then "#" else "."
    bar = map (\y -> concatMap (foo y) [0..(b-1)]) [0,-1..(1-b)]

step :: Map Image Image -> Image -> Image
step rs imgs = collide (length divides) $ map (snd . (\((sv,_),im@(b,_)) -> (b+1, translate (findRecipe im sv) (goBack sv)))) divides
  where
    --fix me!
    goBack (V2 x y) = V2 (if x == 0 then 0 else x +1) (if y == 0 then 0 else y-1)
    findRecipe im sv = rs ! translate im (negate sv)
    divides = divide imgs
    
inBox :: Box -> V2 Int -> Bool
inBox (a@(V2 ax ay),c@(V2 cx _)) m = 0 <= dabm && dabm <= dot ab ab && 0 <= dbcm && dbcm <= dot bc bc
    where
      b = V2 ((cx - ax)+ax) ay
      ab = b ^-^ a
      am = m ^-^ a
      dabm = dot ab am
      bc = c ^-^ b
      bm = m ^-^ b
      dbcm = dot bc bm

collide :: Int -> [Image] -> Image
collide numBoxes imgs@((b,_):_) = (b + numBoxes - 1, foldMap snd imgs)
      where b' = floor $ sqrt $ fromIntegral $ numBoxes*b*b

divide :: Image -> [(Box, Image)]
divide (s,vs) = map (\b -> (b, (divInto,filter (inBox b) vs))) (divBoxes s)
  where 
    divInto = if s `mod` 2 == 0 then 2 else 3
    b = (V2 0 0, V2 divInto (-divInto))

divBoxes :: Int -> [Box]
divBoxes s = concatMap (\y -> map (\x -> translate' b (V2 (x*divInto) (-y*divInto))) pts) pts
  where 
    divInto = if s `mod` 2 == 0 then 2 else 3
    pts = [0..((s`div`divInto)-1)]
    b = (V2 0 0, V2 (divInto-1) (1-divInto))

perms :: Image -> [Image]
perms i@(s,_) = i:rotated ++ reflected
  where
    reflected = zipWith translate rfs rfts 
    rotated = zipWith translate rs rts 
    rts = [V2 (s-1) 0, V2 (s-1) (1-s), V2 0 (1-s)]
    rs = map (mult i) rotations
    rfts = [V2 0 (1-s), V2 (s-1) 0]
    rfs = map (mult i) reflections

translate' :: Box -> V2 Int -> Box
translate' (s,e) v = (s ^+^ v, e ^+^ v)

translate :: Image -> V2 Int -> Image
translate (s,v) t = (s, liftU2 (^+^) v (replicate (length v) t))

mult :: Image -> M22 Int -> Image
mult (s,v) r = (s, map (r !*) v)

reflections :: [M22 Int]
reflections = [V2 (V2 1 0) (V2 0 (-1)), V2 (V2 (-1) 0) (V2 0 1)]

rotations :: [M22 Int]
rotations = [V2 (V2 0 1) (V2 (-1) 0), V2 (V2 (-1) 0) (V2 0 (-1)), V2 (V2 0 (-1)) (V2 1 0)]

start :: Image
start = i
  where (Success i) = parseString imageP mempty ".#./..#/###"

parseInput :: String -> Result (Map Image Image)
parseInput = parseString (M.fromList . concat <$> some (rulesP <* skipOptional newline)) mempty

rulesP :: Parser [(Image,Image)]
rulesP = (\pre post -> map (\p -> (p,post)) $ perms pre) <$> token imageP <*> (token (string "=>") *> imageP)

imageP :: Parser Image
imageP = over _2 catMaybes . mV <$> sepBy1 (some (oneOf ".#")) (char '/')
    where
      mV :: [String] -> (Int,[Maybe (V2 Int)])
      mV ls = (length ls, concatMap (\(y,cs) -> zipWith (\x c -> if c == '.' then Nothing else Just (V2 x y)) [0..(length cs-1)] cs) $ zip [0,-1..(-(length ls-1))] ls)


test="../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"
input=""