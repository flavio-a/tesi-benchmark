module Main where

import Criterion.Main
import Control.DeepSeq
import qualified Data.Array.Repa as R

import Queens.Queens as Queens
import Minimax.Minimax as Minimax
import Matmult.Matmult as Matmult
import Coins.Coins as Coins
import Transclos.Transclos as Transclos
import Nbody.Nbody as Nbody
import Sphere.Sphere as Sphere
import Gatesim.Gatesim as Gatesim

queens = bgroup "queens - 13"
    [ bench "seq" $ whnf Queens.bseq 13
    , bench "strat" $ whnf Queens.bstrat 13
    , bench "repa" $ whnf Queens.brepa 13
    , bench "mpar" $ whnf Queens.bmpar 13
    ]

minimax = bgroup (concat ["minimax - ", show startPieces, " ", show depth])
    [ bench "seq" $ nf (Minimax.bseq startPieces) depth
    , bench "strat" $ nf (Minimax.bstrat startPieces) depth
    , bench "repa" $ nf (Minimax.brepa startPieces) depth
    , bench "mpar" $ nf (Minimax.bmpar startPieces) depth
    ]
    where
        startPieces = 4 :: Int
        depth = 5 :: Int

matmult = bgroup (concat ["matmult - ", show dim, "x", show dim])
    [ bench "seq" $ nf (Matmult.bseq m) m
    , bench "strat" $ nf (Matmult.bstrat m) m
    , bench "repa" $ nf (R.toUnboxed . Matmult.brepa mrepa) mrepa
    , bench "mpar" $ nf (Matmult.bmpar m) m
    ]
    where
        dim = 500
        m = Matmult.splitGroup dim [-dim .. dim^2 - dim - 1]  :: Matrix
        -- Repa Arrays are already strict. Using m in it's definition force its
        -- evaluation too
        mrepa :: R.Array R.U R.DIM2 Int
        mrepa = R.fromListUnboxed (R.Z R.:. dim R.:. dim) $ concat m

coins = bgroup ("coins - " ++ show val)
    [ bench "seq" $ nf (Coins.bseq val) coins
    , bench "strat" $ nf (Coins.bstrat val) coins
    , bench "repa" $ nf (Coins.brepa val) coins
    , bench "mpar" $ nf (Coins.bmpar val) coins
    ]
    where
        vals, quants :: [Int]
        vals = [250, 100, 25, 10, 5, 1]
        quants = [55, 88, 88, 99, 122, 177]
        coins = zip vals quants :: [(Int, Int)]
        val = 1163 :: Int

transclos = bgroup ("transclos - " ++ show val)
    [ bench "seq" $ nf (Transclos.bseq r val) seed
    , bench "strat" $ nf (Transclos.bstrat r val) seed
    -- , bench "repa" $ nf (Transclos.brepa r val) seed
    , bench "mpar" $ nf (Transclos.bmpar r val) seed
    ]
    where
        base = 2 * 10^6 :: Int
        val = base + 25 :: Int
        seed = map (*(base `div` 100)) [100 .. 109] :: [Int]
        r = Transclos.r2

nbody = bgroup ("nbody - " ++ show val)
    [ bench "seq" $ nf Nbody.bseq val
    , bench "strat" $ nf Nbody.bstrat val
    , bench "repa" $ nf (R.toUnboxed . Nbody.brepa) val
    , bench "mpar" $ nf Nbody.bmpar val
    ]
    where
        val = 10000 :: Int

sphere = bgroup ("sphere - " ++ show winsize)
    [ bench "seq" $ nf Sphere.bseq winsize
    , bench "strat" $ nf Sphere.bstrat winsize
    , bench "repa" $ nf (R.toUnboxed . Sphere.brepa) winsize
    , bench "mpar" $ nf Sphere.bmpar winsize
    ]
    where
        winsize = 1500 :: Int

gatesim = bgroup ("gatesim - " ++ show num ++ "E" ++ show e)
    [ bench "seq" $ nf (Gatesim.bseq gates) 0
    , bench "strat" $ nf (Gatesim.bstrat gates) 0
    -- , bench "repa" $ nf (Gatesim.brepa gatesrepa) 0
    , bench "mpar" $ nf (Gatesim.bmpar gates) 0
    ]
    where
        num = 32 :: Int
        e = 7 :: Int
        eval = 10 ^ e
        input = toInput [eval .. eval + num - 1]
        sleeps = map Sleep [0 .. num - 1]
        sums = take (num - 1) $ zipWith Sum [num, num + 2..] [num + 1, num + 3..]
        gates = fromList $ input ++ sleeps ++ sums
        -- gatesrepa = vectorToRepa gates

main :: IO ()
main = defaultMain [
    -- queens,
    -- minimax,
    -- matmult,
    -- coins,
    -- nbody,
    -- sphere,
    -- gatesim,
    transclos
    ]
