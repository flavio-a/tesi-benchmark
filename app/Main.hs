module Main where

import Criterion.Main
import Queens

main :: IO ()
main = defaultMain
    [ bgroup "queens-seq"   [ bench "13" $ whnf bseq 13 ]
    , bgroup "queens-strat" [ bench "13" $ whnf bstrat 13 ]
    , bgroup "queens-repa"  [ bench "13" $ whnf brepa 13 ]
    , bgroup "queens-mpar"  [ bench "13" $ whnf bmpar 13 ]
    ]
