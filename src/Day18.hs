{-# LANGUAGE LambdaCase #-}

module Day18 where

import Text.Trifecta
import Control.Applicative
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List
import Control.Concurrent.Chan
import Data.IORef
import Control.Concurrent.Async
import Control.Monad
import qualified Control.Exception.Base as E

type RegOrInt = Either Char Integer

data Inst = Snd RegOrInt | Set Char RegOrInt | Add Char RegOrInt | Mul Char RegOrInt | Mod Char RegOrInt | Rcv Char | Jgz RegOrInt RegOrInt
  deriving (Eq,Show)

part1 = (\(_,_,x:_,_) -> x) $ last $ takeWhile (\(_,_,_,x) -> x) $ iterate (next i) (0,mempty,[],True)
  where (Success i) = parseInput input

part2 = do
  q1 <- newChan
  q2 <- newChan
  t1 <- newIORef (0,M.singleton 'p' 0,0)
  t2 <- newIORef (0,M.singleton 'p' 1,0)
  let (Success i) = parseInput input
      r1 = runP i q1 q2 t1
      r2 = runP i q2 q1 t2
      readVars = (,) <$> readIORef t1 <*> readIORef t2
  (_,(_,_,r)) <- E.catch (concurrently r1 r2 >> readVars) $ \E.BlockedIndefinitelyOnMVar -> readVars
  return r

val :: Map Char Integer -> RegOrInt -> Integer
val m = \case
  Left c -> M.findWithDefault 0 c m
  Right i -> i

next :: [Inst] -> (Integer, Map Char Integer, [Integer], Bool) -> (Integer, Map Char Integer, [Integer], Bool)
next inst s@(idx,rs,snds,_)
  | idx >= 0 && idx < genericLength inst = doInst s (inst `genericIndex` idx)
  | otherwise = (idx,rs,snds,False)

doInst :: (Integer, Map Char Integer, [Integer], Bool) -> Inst -> (Integer, Map Char Integer, [Integer], Bool)
doInst (idx, rs, snds,t) = \case
  Snd x -> (idx+1,rs,val rs x:snds,t)
  Set x y -> (idx+1,M.alter (const $ Just (val rs y)) x rs,snds,t)
  Add x y -> (idx+1,M.alter (\m -> Just (val rs y + fromMaybe 0 m)) x rs,snds,t)
  Mul x y -> (idx+1,M.alter (\m -> Just (val rs y * fromMaybe 0 m)) x rs,snds,t)
  Mod x y -> (idx+1,M.alter (\m -> Just (fromMaybe 0 m `mod` val rs y)) x rs,snds,t)
  Rcv x
    | M.findWithDefault 0 x rs /= 0 -> (idx+1,rs,snds,False)
    | otherwise -> (idx+1,rs,snds,t)
  Jgz x y 
    | val rs x > 0 -> (val rs y+idx,rs,snds,t)
    | otherwise -> (idx+1,rs,snds,t)

runP ::
  [Inst]
  -> Chan Integer
  -> Chan Integer
  -> IORef (Integer, Map Char Integer, Integer)
  -> IO ()
runP inst mine theirs v = go
  where
    go = do
      (idx,rs,snds) <- readIORef v
      when (idx >= 0 && idx < genericLength inst) $ 
        doStep (idx, rs, snds) (inst `genericIndex` idx) >> go
      where
        doStep (idx,rs,snds) = \case
            Snd x -> writeChan theirs (val rs x) >> writeIORef v (idx+1, rs, snds+1)
            Set x y -> writeIORef v (idx+1,M.alter (const $ Just (val rs y)) x rs,snds)
            Add x y -> writeIORef v (idx+1,M.alter (\m -> Just (val rs y + fromMaybe 0 m)) x rs,snds)
            Mul x y -> writeIORef v (idx+1,M.alter (\m -> Just (val rs y * fromMaybe 0 m)) x rs,snds)
            Mod x y -> writeIORef v (idx+1,M.alter (\m -> Just (fromMaybe 0 m `mod` val rs y)) x rs,snds)
            Rcv x -> do
              q <- readChan mine
              writeIORef v (idx+1,M.alter (const $ Just q) x rs,snds)
            Jgz x y 
              | val rs x > 0 -> writeIORef v (val rs y+idx,rs,snds)
              | otherwise -> writeIORef v (idx+1,rs,snds)

parseInput :: String -> Result [Inst]
parseInput = parseString (some (instP <* skipOptional newline)) mempty

instP :: Parser Inst  
instP = try sndP <|> try setP <|> try addP <|> try mulP <|> try modP <|> try rcvP <|> jgzP
  where
    cmd = token . string
    regOrInteger :: Parser (Either Char Integer)
    regOrInteger = try (Left <$> letter) <|> (Right <$> integer)
    sndP = Snd <$> (cmd "snd" *> regOrInteger)
    setP = (\_ x y -> Set x y) <$> cmd "set" <*> token letter <*> regOrInteger
    addP = (\_ x y -> Add x y) <$> cmd "add" <*> token letter <*> regOrInteger
    mulP = (\_ x y -> Mul x y) <$> cmd  "mul" <*> token letter <*> regOrInteger
    modP = (\_ x y -> Mod x y) <$> cmd "mod" <*> token letter <*> regOrInteger
    rcvP = Rcv <$> (cmd "rcv" *> letter)
    jgzP = (\_ x y -> Jgz x y) <$> cmd "jgz" <*> token regOrInteger <*> regOrInteger

test = "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2"
test2 = "snd 1\nsnd 2\nsnd p\nrcv a\nrcv b\nrcv c\nrcv d"
input = "set i 31\nset a 1\nmul p 17\njgz p p\nmul a 2\nadd i -1\njgz i -2\nadd a -1\nset i 127\nset p 316\nmul p 8505\nmod p a\nmul p 129749\nadd p 12345\nmod p a\nset b p\nmod b 10000\nsnd b\nadd i -1\njgz i -9\njgz a 3\nrcv b\njgz b -1\nset f 0\nset i 126\nrcv a\nrcv b\nset p a\nmul p -1\nadd p b\njgz p 4\nsnd a\nset a b\njgz 1 3\nsnd b\nset f 1\nadd i -1\njgz i -11\nsnd a\njgz f -16\njgz a -19"