module Queens.Queens
    ( bseq
    , brepa
    , bstrat
    , bmpar
    ) where

import Control.Monad
import Data.Functor.Identity

import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad.Par
import Data.Array.Repa.Index
import qualified Data.Array.Repa as R

-- ================================== Common ==================================
-- A chessboard is represented as a list of Int, in which the index is the
-- column and the value is the row of the queen in that column
type Chessboard = [Int]

-- A partial chessboard is a chessboard that contains only the queens of the
-- first q columns

-- Check if a position is safe given a partial chessboard
-- TODO: tail-recursive this
safe :: Int -> Int -> Chessboard -> Bool
safe x d []    = True
safe x d (q:l) = x /= q && x /= q+d && x /= q-d && safe x (d+1) l

-- Takes a list of partial chessboards and extends each one of them with all
-- possible positions. First parameter is the total size of the chessboard
gen :: Int -> [Chessboard] -> [Chessboard]
gen nq bs = [ q:b | b <- bs, q <- [1..nq], safe q 1 b ]

-- Sequential computation: given a partial chessboard (the tail's starting
-- point) computes all the possible chessboards obtained adding (nq - n) queens
-- to that chessboard
seqgen :: Int -> Int -> Chessboard -> [Chessboard]
seqgen nq n b = iterate (gen nq) [b] !! (nq - n)

-- ================================ Sequential ================================
bseq :: Int -> Int
bseq nq = length $ seqgen nq 0 []

-- ================================ Strategies ================================
{-
Divide-and-conquer parallelized with threshold. Recursive function that at each
call adds one queens to the partial chessboard it computed up to that point,
then calls itself on each one of the extended partial chessboards. This
recursive call happens in parallel until a certain threshold is reached, that
is when a certain number of queens has already been placed. Beyond the
threshold we have a "tail" computation that is a recursive call to place the
remaining queens, each one with its starting point that is the partial
chessboard on which happens the recursive call. A single tail is then evaluated
sequentially (doesn't sparks anything, is a "single thread"), even though all
the tails are sparked in parallel, so many tails are evaluated at the same time.
-}
bstrat :: Int -> Int
bstrat nq = length $ pargen 0 []
    where
    pargen :: Int -> Chessboard -> [Chessboard]
    pargen n b
      | n >= threshold = seqgen nq n b
      | otherwise      = concat bs
        where bs = map (pargen (n+1)) (gen nq [b]) `using` parList rdeepseq
    threshold = 4

-- ================================ Monad Par ================================
-- Same as strategies but using monad par
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
    threshold = 3

-- =================================== Repa ===================================
{-
Sequential + parallelized sequential tails.

seqcbs computes sequentially all possible partial chessboards with 3 queens.
Then using Repa I create array resarr with an element for each of those
chessboards, containing at position i the number of possible chessboards that
extends the i-th partial chessboard computed by seqcbs. The computation of one
element of this array is a tail, and is sequential (because seqgen is). All
these sequential tails are then evaluated in parallel via computeP.
-}
brepa :: Int -> Int
brepa nq = runIdentity result R.! Z
    where
    threshold = 3
    lengen :: [Int] -> Int
    lengen x = length $ seqgen nq threshold x
    -- Apply three steps sequentially
    seqcbs = seqgen nq (nq - threshold) [] :: [Chessboard]
    -- Maps in parallel lengen
    resarr = R.fromFunction (Z :. length seqcbs) (\(Z :. i) -> lengen $ seqcbs !! i) :: R.Array R.D DIM1 Int
    result = R.sumP resarr :: Identity (R.Array R.U DIM0 Int)
