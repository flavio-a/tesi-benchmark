module Main where

import Criterion.Main
import Queens.Queens as Queens
import Minimax.Minimax as Minimax

queens = bgroup "queens - 13"
    [ bench "seq" $ whnf Queens.bseq 13
    , bench "strat" $ whnf Queens.bstrat 13
    , bench "repa" $ whnf Queens.brepa 13
    , bench "mpar" $ whnf Queens.bmpar 13
    ]

minimax = bgroup "minimax - 4 4"
    [ bench "seq" $ nf (Minimax.bseq 4) 4
    , bench "strat" $ nf (Minimax.bstrat 4) 4
    -- , bench "repa" $ nf Queens.brepa 13
    , bench "mpar" $ nf (Minimax.bmpar 4) 4
    ]


main :: IO ()
main = defaultMain
    [ queens
    , minimax
    ]
