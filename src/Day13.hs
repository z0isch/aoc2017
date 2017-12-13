module Day13 where

import Text.Trifecta

part1 = sum $ map (\(x,y) -> if x `mod` y == 0 then x * y else 0) $ sortm
    where (Success m) = parseInput test
part2 = undefined

type Layer = (Integer, Integer)

parseInput :: String -> Result [Layer]
parseInput = parseString (some (layerP <* skipOptional newline)) mempty

--fillIn (l:ls) = zipWith (\(x,y) -> ) l ls
layerP :: Parser Layer
layerP = (,) <$> (read <$> some digit) <*> (string ": " *> (read <$> some digit))

test = "0: 3\n1: 2\n4: 4\n6: 4"
input = ""