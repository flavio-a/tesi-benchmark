module Queens.Queens
    ( bseq
    , brepa
    , bstrat
    , bmpar
    ) where

import Control.Monad
import Data.Functor.Identity
import System.Environment

import Control.Parallel
import Control.Parallel.Strategies

import Control.Monad.Par

import Data.Array.Repa.Index
import qualified Data.Array.Repa as R


-- version of N-queens originally from nofib/imaginary/queens, parallelised
-- by Simon Marlow 03/2010.

-- A chessboard is represented as a list of Int, in which the index is the
-- column and the value is the row of the queen in that column
type Chessboard = [Int]

-- Check if a position is safe given a list of queens
safe :: Int -> Int -> Chessboard -> Bool
safe x d []    = True
safe x d (q:l) = x /= q && x /= q+d && x /= q-d && safe x (d+1) l

-- Takes a list of chessboards and extends each one of them with all possible
-- positions
gen :: Int -> [Chessboard] -> [Chessboard]
gen nq bs = [ q:b | b <- bs, q <- [1..nq], safe q 1 b ]

-- Threshold for parallelisation
threshold = 3

-- Sequential computation of solution from a single Chessboard with already n
-- queens
seqgen :: Int -> Int -> Chessboard -> [Chessboard]
seqgen nq n b = iterate (gen nq) [b] !! (nq - n)

-- Sequential version, for comparison
bseq :: Int -> Int
bseq nq = length $ seqgen nq 0 []

-- Repa
brepa :: Int -> Int
brepa nq = runIdentity result R.! Z
    where
    -- Creates the array, apply three step sequentially, then maps in parallel
    -- seqgen
    threshold' = threshold - 1
    seqcbs = seqgen nq (nq - threshold') [] :: [Chessboard]
    lengen :: [Int] -> Int
    lengen x = length $ seqgen nq threshold' x
    resarr = R.fromFunction (Z :. length seqcbs) (\(Z :. i) -> lengen $ seqcbs !! i) :: R.Array R.D DIM1 Int
    ress = runIdentity $ R.computeP resarr :: R.Array R.U DIM1 Int
    result = R.sumP ress

-- Strategies
bstrat :: Int -> Int
bstrat nq = length $ pargen 0 []
    where
    pargen :: Int -> Chessboard -> [Chessboard]
    pargen n b
      | n >= threshold = seqgen nq n b
      | otherwise      = concat bs
        where bs = map (pargen (n+1)) (gen nq [b]) `using` parList rdeepseq

-- Monad par
bmpar :: Int -> Int
bmpar nq = length $ runPar $ recmpar 0 []
    where
    recmpar :: Int -> Chessboard -> Par [Chessboard]
    recmpar n b
        | n >= threshold = return $ seqgen nq n b
        | otherwise      = do
            bs <- parMapM (recmpar (n + 1)) $ gen nq [b]
            return $ concat bs
            -- Equivalent to:
            -- liftM concat $ parMapM (recmpar (n + 1)) $ gen nq [b]
