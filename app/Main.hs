module Main where

import Day21
import Criterion.Main
import Control.Parallel.Strategies

main = print part2
-- main :: IO ()
-- main = defaultMain 
--   [ bgroup ""
--     [ bench "part1" $ nf (const part2) undefined
--     --, bench "part1'" $ nf (const part2) (parMap rpar)
--     ]
--   ]
