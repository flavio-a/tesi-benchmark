{-# LANGUAGE CPP, DeriveGeneric, DeriveAnyClass #-}

module Minimax.Game
    ( alternateSeq
    , alternateStrat
    , alternateMpar
    , showMove
    , Player
    , Move
    ) where

import Minimax.Board

import Control.Parallel
import Control.Parallel.Strategies
import GHC.Generics (Generic)
import Control.Monad.Par

-- ================================== Common ==================================

type Player = Evaluation -> Evaluation -> Evaluation
type Move = (Board,Evaluation)

opposite :: Piece -> Piece
opposite X = O
opposite O = X

best :: Player -> [Board] -> [Evaluation] -> Move
best f (b:bs) (s:ss) = best' b s bs ss
    where
    best' b s [] [] = (b,s)
    best' b s (b':bs) (s':ss) | s == f s s' = best' b s bs ss
                  | otherwise     = best' b' s' bs ss

showMove :: Move -> String
showMove (b,e) = show e ++ "\n" ++ showBoard b

cropTree :: Tree Evaluation -> Tree Evaluation
cropTree (Branch a []) = Branch a []
cropTree (Branch (Score x) l) = Branch (Score x) (map cropTree l)
cropTree (Branch x l) = Branch x []

searchTree :: Piece -> Board -> Tree Board
searchTree p = repTree (newPositions p) (newPositions (opposite p))

mise :: Player -> Player -> Tree Evaluation -> Evaluation
mise f g (Branch a []) = a
mise f g (Branch _ l) = foldr (f . mise g f) (g OWin XWin) l


data Tree a = Branch a [Tree a] deriving (Show, Generic, NFData)

repTree :: (a->[a]) -> (a->[a])-> a -> Tree a
repTree f g a = Branch a (map (repTree g f) (f a))

prune :: Int -> Tree a -> Tree a
prune 0 (Branch a l) = Branch a []
prune n (Branch a l) = Branch a (map (prune (n-1)) l)


-- ================================ Sequential ================================

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
      = parMiseSeq 2 f g
      . cropTree
      . mapTreeSeq static
      . prune depth
      . searchTree p

    parMiseSeq :: Int -> Player -> Player -> Tree Evaluation -> Evaluation
    parMiseSeq 0 f g t = mise f g t
    parMiseSeq n f g (Branch a []) = a
    parMiseSeq n f g (Branch _ l) = foldr (f . parMiseSeq (n-1) g f) (g OWin XWin) l


    mapTreeSeq :: (a -> b) -> Tree a -> Tree b
    mapTreeSeq f (Branch a l) = Branch (f a) (map (mapTreeSeq f) l)

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

    bestMoveStrat :: Int -> Piece -> Player -> Player -> Board -> Evaluation
    bestMoveStrat depth p f g
      = parMiseStrat 2 f g
      . cropTree
      . mapTreeStrat static
      . prune depth
      . searchTree p

    parMiseStrat :: Int -> Player -> Player -> Tree Evaluation -> Evaluation
    parMiseStrat 0 f g t = mise f g t
    parMiseStrat n f g (Branch a []) = a
    parMiseStrat n f g (Branch _ l) = foldr f (g OWin XWin) (map (parMiseStrat (n-1) g f) l `using` parList rseq)


#define SEQ

#ifndef SEQ

    mapTreeStrat :: (a -> b) -> Tree a -> Tree b
    mapTreeStrat f (Branch a l)
       = fa `par` Branch fa (map (mapTreeStrat f) l `using` myParList)
       where fa = f a

#else /* SEQ */

    mapTreeStrat :: (a -> b) -> Tree a -> Tree b
    mapTreeStrat f (Branch a l) = Branch (f a) (map (mapTreeStrat f) l)

#endif

    myParList [] = ()
    myParList (x:xs) = foldr par () xs

-- ================================ Monad Par ================================

alternateMpar :: Int -> Piece -> Player -> Player -> Board -> [Move]
alternateMpar depth player f g board = runPar $ alternateMpar' depth player f g board
    where
    alternateMpar' :: Int -> Piece -> Player -> Player -> Board -> Par [Move]
    alternateMpar' _ _ _ _ b | fullBoard b = return []
    alternateMpar' _ _ _ _ b | static b == XWin = return []
    alternateMpar' _ _ _ _ b | static b == OWin = return []
    alternateMpar' depth player f g board = do
        scores' <- scores
        let move@(board',_) = best f possibles scores' in do
            moves <- alternateMpar' depth opponent g f board'
            return $ move : moves
        where
        scores = parMapM (bestMoveMpar depth opponent g f) possibles
        possibles = newPositions player board
        opponent = opposite player

    bestMoveMpar :: Int -> Piece -> Player -> Player -> Board -> Par Evaluation
    bestMoveMpar depth p f g b = do
        evtree <- mapTreeMpar static . prune depth . searchTree p $ b
        parMiseMpar 2 f g . cropTree $ evtree

    parMiseMpar :: Int -> Player -> Player -> Tree Evaluation -> Par Evaluation
    parMiseMpar 0 f g t = return $ mise f g t
    parMiseMpar n f g (Branch a []) = return a
    parMiseMpar n f g (Branch _ l) = do
        evlist <- parMapM (parMiseMpar (n-1) g f) l
        return $ foldr f (g OWin XWin) evlist


    mapTreeMpar :: (a -> b) -> Tree a -> Par (Tree b)
    mapTreeMpar f (Branch a l) = do
        childs <- mapM (mapTreeMpar f) l
        return $ Branch (f a) childs

-- =================================== Repa ===================================

-- alternateRepa :: Int -> Piece -> Player -> Player -> Board -> [Move]
-- alternateRepa _ _ _ _ b | fullBoard b = []
-- alternateRepa _ _ _ _ b | static b == XWin = []
-- alternateRepa _ _ _ _ b | static b == OWin = []
-- alternateRepa depth player f g board = move : alternateRepa depth opponent g f board'
--     where
--     move@(board',eval) = best f possibles scores
--     scores = map (bestMoveRepa depth opponent g f) possibles `using` parList rseq
--     possibles = newPositions player board
--     opponent = opposite player
--
--     bestMoveRepa :: Int -> Piece -> Player -> Player -> Board -> Evaluation
--     bestMoveRepa depth p f g
--       = parMiseRepa 2 f g
--       . cropTree
--       . mapTreeRepa static
--       . prune depth
--       . searchTree p
--
--     parMiseRepa :: Int -> Player -> Player -> Tree Evaluation -> Evaluation
--     parMiseRepa 0 f g t = mise f g t
--     parMiseRepa n f g (Branch a []) = a
--     parMiseRepa n f g (Branch _ l) = foldr f (g OWin XWin) (map (parMiseRepa (n-1) g f) l `using` parList rseq)
--
--
--     mapTreeRepa :: (a -> b) -> Tree a -> Tree b
--     mapTreeRepa f (Branch a l) = Branch (f a) (map (mapTreeRepa f) l)
