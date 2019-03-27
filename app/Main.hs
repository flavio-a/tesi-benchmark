module Main where

import Criterion.Main
import Control.DeepSeq
import Queens.Queens as Queens
import Minimax.Minimax as Minimax
import Matmult.Matmult as Matmult

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

matmult = bgroup (concat ["matmult - ", show dim, "x", show dim])
    [ bench "seq" $ nf (Matmult.bseq m) m
    , bench "strat" $ nf (Matmult.bstrat m) m
    , bench "repa" $ nf (Matmult.brepa m) m
    , bench "mpar" $ nf (Matmult.bmpar m) m
    ]
    where
        dim = 400
        size = dim^2 `div` 2
        m' = Matmult.splitGroup dim [(1 - size)..size]
        m = rnf m' `seq` m'


main :: IO ()
main = defaultMain [
    -- queens,
    -- minimax,
    matmult
    ]
