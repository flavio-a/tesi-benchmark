module Main where

import Criterion.Main
import Queens.Queens

queens = bgroup "queens - 13"
    [ bench "seq" $ whnf bseq 13
    , bench "strat" $ whnf bstrat 13
    , bench "repa" $ whnf brepa 13
    , bench "mpar" $ whnf bmpar 13
    ]


main :: IO ()
main = defaultMain
    [ queens
    ]
