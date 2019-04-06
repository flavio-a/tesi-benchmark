{-# LANGUAGE CPP, DeriveGeneric, DeriveAnyClass #-}
module Minimax.Game
    ( alternateSeq
    , alternateStrat
    , alternateMpar
    , alternateRepa
    -- , showMove
    , Player
    , Move
    ) where

import Minimax.Board

import Control.Parallel
import Control.Parallel.Strategies
import GHC.Generics (Generic)
import Control.Monad.Par
import qualified Data.Array.Repa as R
import Data.Array.Repa.Index
import Data.Functor.Identity

-- ================================== Common ==================================
data Tree a = Branch a [Tree a] deriving (Show, Generic, NFData)

-- Given two functions and a starting point creates the tree that start at that
-- point build by applying f and g alternated to get the list of children
repTree :: (a -> [a]) -> (a -> [a]) -> a -> Tree a
repTree f g a = Branch a (map (repTree g f) (f a))

-- Prune any node deeper than the passed level
prune :: Int -> Tree a -> Tree a
prune 0 (Branch a l) = Branch a []
prune n (Branch a l) = Branch a (map (prune (n-1)) l)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Branch a l) = Branch (f a) (map (mapTree f) l)

-- A player is a total order on evaluations (a comparator)
type Player = Evaluation -> Evaluation -> Evaluation
type Move = (Board,Evaluation)

opposite :: Piece -> Piece
opposite X = O
opposite O = X

-- Computes the best move for a player given a list of board and evaluations
best :: Player -> [Board] -> [Evaluation] -> Move
best f bs ss = foldr1 moveComp $ zip bs ss
    where
    moveComp :: Move -> Move -> Move
    moveComp m1@(_, e1) m2@(_, e2) | f e1 e2 == e1 = m1
                                   | otherwise     = m2

-- showMove :: Move -> String
-- showMove (b,e) = show e ++ "\n" ++ showBoard b

-- Removes useless branches from a tree of evaluations, that are branches below
-- a Win evaluation (they are filled with the same win evaluation)
cropTree :: Tree Evaluation -> Tree Evaluation
cropTree t@(Branch a []) = t
cropTree (Branch a@(Score _) l) = Branch a (map cropTree l)
cropTree (Branch a _) = Branch a []

-- Creates the tree of all possible games from a given board and piece to place
-- at that turn
searchTree :: Piece -> Board -> Tree Board
searchTree p = repTree (newPositions p) (newPositions (opposite p))

-- Given two players and a tree of evaluations get the resulting evaluation
mise :: Player -> Player -> Tree Evaluation -> Evaluation
mise f g (Branch a []) = a
mise f g (Branch _ l) = foldr (f . mise g f) (g OWin XWin) l

-- ================================ Sequential ================================
-- Computes the list of best moves from here to the end of the game exploring
-- the tree with Minmax strategy. Lazyness automatically adds alpha-beta
-- pruning.
alternateSeq :: Int -> Piece -> Player -> Player -> Board -> [Move]
alternateSeq _ _ _ _ b | fullBoard b = []
alternateSeq _ _ _ _ b | static b == XWin = []
alternateSeq _ _ _ _ b | static b == OWin = []
alternateSeq depth player f g board = move : alternateSeq depth opponent g f board'
    where
    move@(board',eval) = best f possibles scores
    scores = map (bestMoveSeq depth opponent g f) possibles
    possibles = newPositions player board
    opponent = opposite player

    bestMoveSeq :: Int -> Piece -> Player -> Player -> Board -> Evaluation
    bestMoveSeq depth p f g
      = mise f g
      . cropTree
      . mapTree static
      . prune depth
      . searchTree p

-- ================================ Strategies ================================
alternateStrat :: Int -> Piece -> Player -> Player -> Board -> [Move]
alternateStrat _ _ _ _ b | fullBoard b = []
alternateStrat _ _ _ _ b | static b == XWin = []
alternateStrat _ _ _ _ b | static b == OWin = []
alternateStrat depth player f g board = move : alternateStrat depth opponent g f board'
    where
    move@(board',eval) = best f possibles scores
    scores = map (bestMoveStrat depth opponent g f) possibles `using` parList rseq
    possibles = newPositions player board
    opponent = opposite player

    th = 3 -- threshold parameter

    bestMoveStrat :: Int -> Piece -> Player -> Player -> Board -> Evaluation
    bestMoveStrat depth p f g
      = parMiseStrat th f g
      . cropTree
      . mapTree static
      . prune depth
      . searchTree p

    parMiseStrat :: Int -> Player -> Player -> Tree Evaluation -> Evaluation
    parMiseStrat 0 f g t = mise f g t
    parMiseStrat n f g (Branch a []) = a
    parMiseStrat n f g (Branch _ l) = foldr f (g OWin XWin) (map (parMiseStrat (n-1) g f) l `using` parList rseq)

-- ================================ Monad Par ================================
{-
Explore the tree in parallel with monad par. There's no lazyness, so no
alpha-beta pruning on that and this degrades performances a lot. To cope with
this problem parallelism threshold is kept at 1 because sequential tails does
apply alpha-beta pruning. Of course this limit parallelism to the number of
moves that can be done from a certain situation, but in tic-tac-toe few moves
means few turns left so the computation is fast anyway (even without
parallelism).

Probably a better implementation should decide the threshold dynamically
considering the number of processors and the number of moves.
-}
alternateMpar :: Int -> Piece -> Player -> Player -> Board -> Par [Move]
alternateMpar _ _ _ _ b | fullBoard b = return []
alternateMpar _ _ _ _ b | static b == XWin = return []
alternateMpar _ _ _ _ b | static b == OWin = return []
alternateMpar depth player f g board = do
    scores' <- scores
    let move@(board',_) = best f possibles scores'
    moves <- alternateMpar depth opponent g f board'
    return $ move : moves
    where
    scores = parMapM (bestMoveMpar depth opponent g f) possibles
    possibles = newPositions player board
    opponent = opposite player

    th = 1 -- threshold parameter, see initial comment for an explanation

    bestMoveMpar :: Int -> Piece -> Player -> Player -> Board -> Par Evaluation
    bestMoveMpar depth p f g
        = parMiseMpar th f g
        . cropTree
        . mapTree static
        . prune depth
        . searchTree p

    parMiseMpar :: Int -> Player -> Player -> Tree Evaluation -> Par Evaluation
    parMiseMpar 0 f g t = return $ mise f g t
    parMiseMpar n f g (Branch a []) = return a
    parMiseMpar n f g (Branch _ l) =
        foldr f (g OWin XWin) <$> parMapM (parMiseMpar (n-1) g f) l

-- =================================== Repa ===================================
-- Unboxed version of Evaluation, to put inside Repa array
type EvaluationUnboxed = (Int, Int)

evUnbox :: Evaluation -> EvaluationUnboxed
evUnbox OWin = (-1, 0)
evUnbox XWin = (1, 0)
evUnbox (Score a) = (0, a)

evBox :: EvaluationUnboxed -> Evaluation
evBox (-1, 0) = OWin
evBox (1, 0) = XWin
evBox (0, a) = Score a

{- Same problem as monad par, thershold fixed to 1 -}
alternateRepa :: Int -> Piece -> Player -> Player -> Board -> [Move]
alternateRepa _ _ _ _ b | fullBoard b = []
alternateRepa _ _ _ _ b | static b == XWin = []
alternateRepa _ _ _ _ b | static b == OWin = []
alternateRepa depth player f g board = move : alternateRepa depth opponent g f board'
    where
    scores = map (bestMoveRepa depth opponent g f) possibles
    move@(board',_) = best f possibles scores
    possibles = newPositions player board
    opponent = opposite player


    bestMoveRepa :: Int -> Piece -> Player -> Player -> Board -> Evaluation
    bestMoveRepa depth p f g
        = parMiseRepa f g
        . cropTree
        . mapTree static
        . prune depth
        . searchTree p

    -- threshold fixed to 1: parMiseRepa always call mise (sequential)
    -- see initial comment of mpar for an explanation
    parMiseRepa :: Player -> Player -> Tree Evaluation -> Evaluation
    parMiseRepa f g (Branch a []) = a
    parMiseRepa f g (Branch _ l) = evBox . (R.! Z) . runIdentity $
        R.foldP f' (evUnbox $ g OWin XWin) $
        R.fromFunction (Z :. length l) (\(Z :. i) -> evUnbox $ mise g f (l !! i))
        where
            f' a b = evUnbox $ f (evBox a) (evBox b)
