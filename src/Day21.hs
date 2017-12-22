{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Day21 where

import Text.Trifecta hiding (dot)
import Linear.Matrix
import Linear.V2
import Linear.Vector
import Linear.Metric 
import Data.Maybe
import Data.HashMap.Strict (HashMap,(!))
import qualified Data.HashMap.Strict as M
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Control.Lens
import Control.Parallel.Strategies hiding (dot)
import Data.Hashable

data Image = Image 
  { _size :: Int
  , _ons :: HashSet (V2 Int)
  }
  deriving (Show,Eq)
makeLenses ''Image
instance Hashable Image where
  hashWithSalt salt (Image s o) = hashWithSalt salt o + s

type Box = (V2 Int, V2 Int)

part1 = S.size $ view ons $ last $ take 6 $ iterate (step recipes) start
part2 = S.size $ view ons $ last $ take 19 $ iterate (step recipes) start
recipes = let (Success rs) = parseInput input in rs

printRecipes :: HashMap Image Image -> IO ()
printRecipes = mapM_ (\(k,v) -> printImage k >> putStrLn "=>" >> printImage v >> putStrLn "") . M.toList 

printBox :: (Box, Image) -> IO ()
printBox ((sv,_),img) = printImage $ translate img (negate sv)

printImage :: Image -> IO ()
printImage (Image b ons) = mapM_ putStrLn bar
  where 
    foo y x = if V2 x y `elem` ons then "#" else "."
    bar = map (\y -> concatMap (foo y) [0..(b-1)]) [0,-1..(1-b)]

step :: HashMap Image Image -> Image -> Image
step rs img@(Image b0 _) = collided
  where
    !collided = collide b' $ map getNext $ zip [0..] divides
    b' = floor $ sqrt $ fromIntegral $ length divides * l * l
    l = 1 + head divides ^. _2.size
    getNext (i,((sv,_),im@(Image b _))) = set size (b+1) replaced
      where
        replaced = translate (rs ! translate im (negate sv)) (sv ^+^ offset)
        boxesPerRow = b0 `div` b
        xMv = i `rem` boxesPerRow
        yMv = i `div` boxesPerRow
        offset = V2 xMv (-yMv)
    divides = divide img

collide :: Int -> [Image] -> Image
collide b' = Image b' . S.unions . map (view ons)

divide :: Image -> [(Box, Image)]
divide (Image s vs) = parMap rpar (\b -> (b, Image divInto $ S.filter (inBox b) vs)) (divBoxes s)
  where 
    divInto = if s `mod` 2 == 0 then 2 else 3

divBoxes :: Int -> [Box]
divBoxes s = concatMap (\y -> map (\x -> over both (^+^ V2 (x*divInto) (-y*divInto)) b) pts) pts
  where 
    divInto = if s `mod` 2 == 0 then 2 else 3
    pts = [0..((s`div`divInto)-1)]
    b = (V2 0 0, V2 (divInto-1) (1-divInto))

inBox :: Box -> V2 Int -> Bool
inBox (a@(V2 ax ay),c@(V2 cx cy)) m@(V2 mx my) = mx >= ax && mx <= cx && -my >= -ay && -my <= -cy 
  -- generic formula for point in rectangle
  --0 <= dabm && dabm <= dot ab ab && 0 <= dbcm && dbcm <= dot bc bc
  -- where
  --   b = V2 ((cx - ax)+ax) ay
  --   ab = b ^-^ a
  --   am = m ^-^ a
  --   dabm = dot ab am
  --   bc = c ^-^ b
  --   bm = m ^-^ b
  --   dbcm = dot bc bm
    
perms :: Image -> [Image]
perms i@(Image s _) = i:total
  where
    total = rotated ++ reflected ++ bth
    reflected = zipWith translate rfs rfts 
    rotated = zipWith translate rs rts 
    bth = zipWith translate bfs (cycle rfts)
    rts = [V2 (s-1) 0, V2 (s-1) (1-s), V2 0 (1-s)]
    rs = map (mult i) rotations
    rfts = [V2 0 (1-s), V2 (s-1) 0]
    rfs = map (mult i) reflections
    bfs = zipWith mult rotated (cycle reflections)

translate :: Image -> V2 Int -> Image
translate (Image s v) t = Image s $ S.fromList $ liftU2 (^+^) (S.toList v) (replicate (S.size v) t)

mult :: Image -> M22 Int -> Image
mult i r = over ons (S.map (r !*)) i

reflections :: [M22 Int]
reflections = [V2 (V2 1 0) (V2 0 (-1)), V2 (V2 (-1) 0) (V2 0 1)]

rotations :: [M22 Int]
rotations = [V2 (V2 0 1) (V2 (-1) 0), V2 (V2 (-1) 0) (V2 0 (-1)), V2 (V2 0 (-1)) (V2 1 0)]

start :: Image
start = i
  where (Success i) = parseString imageP mempty ".#./..#/###"

parseInput :: String -> Result (HashMap Image Image)
parseInput = parseString (M.fromList . concat <$> some (rulesP <* skipOptional newline)) mempty

rulesP :: Parser [(Image,Image)]
rulesP = (\pr post -> map (\p -> (p,post)) $ perms pr) <$> token imageP <*> (token (string "=>") *> imageP)

imageP :: Parser Image
imageP = uncurry Image . over _2 (S.fromList . catMaybes) . mV <$> sepBy1 (some (oneOf ".#")) (char '/')
  where
    mV :: [String] -> (Int,[Maybe (V2 Int)])
    mV ls = (length ls, concatMap (\(y,cs) -> zipWith (\x c -> if c == '.' then Nothing else Just (V2 x y)) [0..(length cs-1)] cs) $ zip [0,-1..(-(length ls-1))] ls)

test2 = "#./.. => #../.../...\n.../.../..# => #.../..../...."
test="../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..\n##/#. => ##./#../...\n.#/.# => .#./.#./...\n../.. => .../.../..."
input= "../.. => .##/..#/##.\n#./.. => ##./#../#..\n##/.. => ###/#.#/..#\n.#/#. => .../#../##.\n##/#. => ###/#../###\n##/## => .##/.##/#.#\n.../.../... => #.##/#.##/###./..##\n#../.../... => ##.#/..##/#.#./##.#\n.#./.../... => ###./.#.#/.#../.###\n##./.../... => ##.#/###./..../##..\n#.#/.../... => ##.#/.###/.##./#.#.\n###/.../... => #..#/.##./#.../.#.#\n.#./#../... => .##./####/#..#/###.\n##./#../... => ##../..#./#.##/..##\n..#/#../... => #.##/.#.#/##../..##\n#.#/#../... => #.../##../..#./.##.\n.##/#../... => #.#./.#.#/#.##/#..#\n###/#../... => .#../.#../...#/##..\n.../.#./... => ..#./..#./##../.#.#\n#../.#./... => ##../####/##../.###\n.#./.#./... => ..../#..#/#.#./....\n##./.#./... => ..##/####/..../##..\n#.#/.#./... => #.##/##../#.../..#.\n###/.#./... => ..../..../####/#..#\n.#./##./... => ..../####/##.#/....\n##./##./... => ####/#.../.###/#.##\n..#/##./... => .#.#/.#../###./.#..\n#.#/##./... => .#.#/###./..../..##\n.##/##./... => #.../.#.#/.#.#/...#\n###/##./... => #.##/.#../.#../#...\n.../#.#/... => ###./..#./.#../..##\n#../#.#/... => #..#/#.##/.#../...#\n.#./#.#/... => ####/..#./..../..#.\n##./#.#/... => #.#./..../.###/..#.\n#.#/#.#/... => #..#/.#../#.#./.###\n###/#.#/... => .##./#..#/.#.#/..#.\n.../###/... => .#../#..#/...#/.##.\n#../###/... => .##./##../###./##.#\n.#./###/... => ...#/..##/###./...#\n##./###/... => .#.#/##.#/.###/.#..\n#.#/###/... => #.#./##../#.#./..#.\n###/###/... => .#.#/####/###./####\n..#/.../#.. => .#../#.##/..../..#.\n#.#/.../#.. => ..../.#.#/##../#..#\n.##/.../#.. => #.##/.#.#/#..#/.#.#\n###/.../#.. => #..#/.#.#/#.#./##.#\n.##/#../#.. => ##../##.#/##.#/#..#\n###/#../#.. => ..../#..#/###./#.##\n..#/.#./#.. => ..../.#../..../.##.\n#.#/.#./#.. => #..#/#.##/.###/....\n.##/.#./#.. => ###./..../##.#/#.#.\n###/.#./#.. => #.../###./.#.#/..#.\n.##/##./#.. => ..../.#../..#./#.#.\n###/##./#.. => ...#/.###/###./####\n#../..#/#.. => ..../.##./..##/..##\n.#./..#/#.. => .#.#/#.../#..#/###.\n##./..#/#.. => #.#./.##./.##./....\n#.#/..#/#.. => #..#/..##/##.#/##..\n.##/..#/#.. => ..#./#.../.##./##.#\n###/..#/#.. => ##../.##./####/.##.\n#../#.#/#.. => ###./#.#./###./.#.#\n.#./#.#/#.. => .##./#.#./#..#/..#.\n##./#.#/#.. => .#.#/#.#./#.../##.#\n..#/#.#/#.. => .##./##.#/.#.#/.#.#\n#.#/#.#/#.. => .#../.##./###./#...\n.##/#.#/#.. => ####/##../.##./##.#\n###/#.#/#.. => ###./.##./##.#/#...\n#../.##/#.. => ...#/#.#./..##/####\n.#./.##/#.. => #.../##.#/.##./###.\n##./.##/#.. => ##.#/.#.#/..../#.#.\n#.#/.##/#.. => ..../#.../.#.#/..#.\n.##/.##/#.. => ##../..../..#./#.##\n###/.##/#.. => ..#./...#/#..#/...#\n#../###/#.. => ..../.#../#.../###.\n.#./###/#.. => ..../#.#./.#.#/...#\n##./###/#.. => ###./###./..#./.###\n..#/###/#.. => #.##/..#./..##/#...\n#.#/###/#.. => ##.#/.#.#/##../#..#\n.##/###/#.. => ###./..##/#.../....\n###/###/#.. => .###/###./#.../..#.\n.#./#.#/.#. => ..##/##.#/.##./####\n##./#.#/.#. => ..../.#.#/#.../###.\n#.#/#.#/.#. => ##.#/###./..#./.#..\n###/#.#/.#. => .###/##../.###/....\n.#./###/.#. => ####/.###/.###/....\n##./###/.#. => #.#./#..#/#..#/###.\n#.#/###/.#. => #.#./.#.#/#.##/####\n###/###/.#. => #.#./.###/..#./#.#.\n#.#/..#/##. => ###./.#.#/##../##..\n###/..#/##. => #.../.###/#.../..#.\n.##/#.#/##. => #..#/.#.#/...#/.#..\n###/#.#/##. => ...#/###./..##/.#.#\n#.#/.##/##. => ###./...#/..../#...\n###/.##/##. => ...#/#.../#.##/##..\n.##/###/##. => .###/.###/..#./#...\n###/###/##. => #.../##../##.#/.###\n#.#/.../#.# => ##../#.##/..#./.###\n###/.../#.# => #.#./.##./.##./#..#\n###/#../#.# => #.../##../####/..##\n#.#/.#./#.# => #.../.#../#.../..##\n###/.#./#.# => #..#/###./####/#...\n###/##./#.# => ##../..##/#.#./##..\n#.#/#.#/#.# => .#../.#.#/#.#./.#.#\n###/#.#/#.# => ..##/####/####/.###\n#.#/###/#.# => .###/##../#..#/..#.\n###/###/#.# => ##../#.../##.#/##..\n###/#.#/### => ###./...#/####/..#.\n###/###/### => .##./##../..../..#."