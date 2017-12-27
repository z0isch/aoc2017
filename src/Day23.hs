{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Day23 where

import Text.Trifecta
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Maybe
import Data.List
import qualified Data.Vector as V
import Data.Char
import Control.Monad.Primitive
import qualified Data.HashSet as H

type RegOrInt = Either Char Int

data Inst = Set Char RegOrInt | Sub Char RegOrInt | Mul Char RegOrInt | Jgz RegOrInt RegOrInt
  deriving (Eq,Show)
  
part1 = runST $ runP i $ UV.fromList (replicate 8 0)
  where
    (Success i) = parseInput input
part2 = 1 + (122700 - 105700) `div` 17 - numPrimes
  where numPrimes = length $ filter (`H.member` vals) $ takeWhile (<= 122700) $ dropWhile (<= 105700) primes
        vals :: H.HashSet Int
        vals = H.fromList $ takeWhile (<= 122700) [105700,(105700+17)..]
        primes = 2 : primes'
          where isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
                primes' = 3 : filter (isPrime primes') [5, 7 ..]

runP :: PrimMonad m => V.Vector Inst -> UV.Vector Int -> m (Int, Int)
runP inst st = do
  rs <- UV.thaw st
  go rs (0,0)
  where
    go rs (idx,snds)
      | idx >= 0 && idx < fromIntegral (V.length inst) = do
        nxt@(!x,!y) <- doInst rs (idx,snds) ((V.!) inst (fromIntegral idx))
        go rs nxt
      | otherwise = return (idx,snds)

charToIndex :: Char -> Int
charToIndex c = ord c - 97

val :: PrimMonad m => MV.MVector (PrimState m) Int -> RegOrInt -> m Int
val m = \case
  Left c -> MV.read m (charToIndex c)
  Right i -> return i

doInst :: PrimMonad m => MV.MVector (PrimState m) Int -> (Int, Int) -> Inst -> m (Int, Int)
doInst rs (idx, snds) = \case
  Set x y -> do
    vy <- val rs y
    MV.write rs (charToIndex x) vy 
    return (idx+1,snds)
  Sub x y -> do
    vy <- val rs y
    MV.modify rs (\m -> m - vy) (charToIndex x)
    return (idx+1,snds)
  Mul x y -> do
    vy <- val rs y
    MV.modify rs (* vy) (charToIndex x)
    return (idx+1,snds + 1)
  Jgz x y -> do
    vx <- val rs x
    if vx /= 0
    then do
      vy <- val rs y
      return (vy+idx,snds)
    else return (idx+1,snds)

parseInput :: String -> Result (V.Vector Inst)
parseInput = parseString (V.fromList <$> some (instP <* skipOptional newline)) mempty

instP :: Parser Inst  
instP = try setP <|> try addP <|> try mulP <|> jgzP
  where
    cmd = token . string
    regOrInt :: Parser (Either Char Int)
    regOrInt = try (Left <$> letter) <|> (Right . fromIntegral <$> integer)
    setP = (\_ x y -> Set x y) <$> cmd "set" <*> token letter <*> regOrInt
    addP = (\_ x y -> Sub x y) <$> cmd "sub" <*> token letter <*> regOrInt
    mulP = (\_ x y -> Mul x y) <$> cmd "mul" <*> token letter <*> regOrInt
    jgzP = (\_ x y -> Jgz x y) <$> cmd "jnz" <*> token regOrInt <*> regOrInt

test=""
input="set b 57\nset c b\njnz a 2\njnz 1 5\nmul b 100\nsub b -100000\nset c b\nsub c -17000\nset f 1\nset d 2\nset e 2\nset g d\nmul g e\nsub g b\njnz g 2\nset f 0\nsub e -1\nset g e\nsub g b\njnz g -8\nsub d -1\nset g d\nsub g b\njnz g -13\njnz f 2\nsub h -1\nset g b\nsub g c\njnz g 2\njnz 1 3\nsub b -17\njnz 1 -23"