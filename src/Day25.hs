{-# LANGUAGE BangPatterns #-}

module Day25 where
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Data.Foldable
import Data.Monoid

type Instruction = (Bool, Bool, Char)
type State = (Instruction, Instruction)

part1 = getSum 
        $ foldMap (Sum . const 1) 
        $ M.filter id 
        $ (\(x,_,_) -> x) 
        $ last 
        $ take 12794429
        $ iterate (step input) (mempty, 'a', 0)

step :: HashMap Char State -> (HashMap Int Bool, Char, Int) -> (HashMap Int Bool, Char, Int) 
step states (t,s,i) = (t',s',i')
    where
        !t' = M.insert i w t
        i' = if m then i + 1 else i - 1
        n = M.lookupDefault False i t
        (w,m,s') = (if n then snd else fst) $ states ! s

test :: HashMap Char State
test = M.fromList [('a',((True,True,'b'),(False,False,'b'))), ('b',((True,False,'a'),(True,True,'a')))]
input = M.fromList 
    [ ('a',((True,True,'b'),(False,False,'f')))
    , ('b',((False,True,'c'),(False,True,'d')))
    , ('c',((True,False,'d'),(True,True,'e')))
    , ('d',((False,False,'e'),(False,False,'d')))
    , ('e',((False,True,'a'),(True,True,'c')))
    , ('f',((True,False,'a'),(True,True,'a')))
    ]
