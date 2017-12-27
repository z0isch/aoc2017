module Main where

import Day23
import Criterion.Main
import Control.Parallel.Strategies

main = print part1
-- main :: IO ()
-- main = defaultMain 
--   [ bgroup ""
--     [ bench "part1" $ nf (const part2) undefined
--     --, bench "part1'" $ nf (const part2) (parMap rpar)
--     ]
--   ]
