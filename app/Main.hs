module Main where

import Criterion.Main
import Control.DeepSeq
import Data.Array.Repa as R

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
    , bench "repa" $ nf (R.toList . Matmult.brepa mrepa) mrepa
    , bench "mpar" $ nf (Matmult.bmpar m) m
    ]
    where
        dim = 500
        m = Matmult.splitGroup dim [-dim .. dim^2 - dim - 1]  :: Matrix
        -- Repa Arrays are already strict. Using m in it's definition force its
        -- evaluation too
        mrepa = R.fromListUnboxed (R.Z R.:. dim R.:. dim) $ concat m :: R.Array R.U DIM2 Int


main :: IO ()
main = defaultMain [
    queens,
    minimax,
    matmult
    ]
