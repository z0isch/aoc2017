module Main where

import Day21
import Criterion.Main
import Control.Parallel.Strategies

main :: IO ()
main = print part2'
-- main = defaultMain 
--   [ bgroup ""
--     [ bench "part2" $ nf (const part2) undefined
--     , bench "part2'" $ nf (const part2') (parMap rpar)
--     ]
--   ]
