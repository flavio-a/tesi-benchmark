{-
Module describing a board of tic-tac-toe and its heuristic evaluation.
-}
{-# LANGUAGE BangPatterns, DeriveGeneric, DeriveAnyClass #-}
module Minimax.Board where

import Data.List
import Control.DeepSeq
import GHC.Generics (Generic)

boardDim = 4

data Piece = X | O | Empty deriving (Eq,Show,Generic,NFData)
type Row = [Piece]
type Board = [Row]


initialBoard :: Board
initialBoard = replicate boardDim (replicate boardDim Empty)

isEmpty Empty = True
isEmpty _     = False

empty :: (Int,Int) -> Board -> Bool
empty (x,y) board = isEmpty ((board !! (y-1)) !! (x-1))

fullBoard :: Board -> Bool
fullBoard b = all (not.isEmpty) (concat b)


showBoard :: Board -> String
showBoard board = intercalate "\n--------\n" (map showRow board) ++ "\n"
    where
        showRow r = intercalate "|" (map showPiece r)

        showPiece :: Piece -> String
        showPiece X = "X"
        showPiece O = "O"
        showPiece Empty = " "


placePiece :: Piece -> Board -> (Int,Int) -> Board
placePiece new board pos
  = [[ if (x,y) == pos then new else old
     | (x,old) <- zip [1..] row ]
     | (y,row) <- zip [1..] board ]

-- Adds piece "p" to board "b" in an empty position, and returns the list of
-- all possible such boards
newPositions :: Piece -> Board -> [Board]
newPositions piece = goRows piece id
    where
        goRows :: Piece -> (Board -> a) -> Board -> [a]
        goRows p rowsL [] = []
        goRows p rowsL (row:rowsR) = goRow p rowsL id row rowsR ++ goRows p (rowsL . (row:)) rowsR

        goRow :: Piece -> ([c] -> a) -> (Row -> c) -> Row -> [c] -> [a]
        goRow p rowsL psL [] rowsR = []
        goRow p rowsL psL (Empty:psR) rowsR = rowsL (psL (p:psR) : rowsR) : goRow p rowsL (psL . (Empty:)) psR rowsR
        goRow p rowsL psL (p':psR) rowsR = goRow p rowsL (psL . (p':)) psR rowsR

        --  [ placePiece piece board (x,y) | (x,y) <- empties board ]
        -- empties board = [ (x,y) | (y,row)   <- zip [1..] board,
        --                           (x,Empty) <- zip [1..] row ]

-- Evaluation of a board (for the heuristic)
data Evaluation = OWin | Score !Int | XWin
    -- higher scores denote a board in X's favour
    deriving (Show,Eq,Generic,NFData)

-- Maximum and minimum of two Evaluations
maxE :: Evaluation -> Evaluation -> Evaluation
maxE XWin _ = XWin
maxE _ XWin = XWin
maxE b OWin = b
maxE OWin b = b
maxE a@(Score x) b@(Score y) | x > y = a
                             | otherwise = b

minE :: Evaluation -> Evaluation -> Evaluation
minE OWin _ = OWin
minE _ OWin = OWin
minE b XWin = b
minE XWin b = b
minE a@(Score x) b@(Score y) | x < y = a
                             | otherwise = b

-- Statically evalutes a board
static :: Board -> Evaluation
static board = interpret 0 (score board)
    where
        interpret :: Int -> [Evaluation] -> Evaluation
        interpret x [] = Score x
        interpret x (Score y:l) = interpret (x+y) l
        interpret x (XWin:l) = XWin
        interpret x (OWin:l) = OWin

        score :: Board -> [Evaluation]
        score board =
           [ eval (scoreString 0 row) | row <- board ] ++
           [ eval (scoreString 0 col) | col <- transpose board ] ++
           [ eval (scoreString 0 (zipWith (!!) board [0..])),
             eval (scoreString 0 (zipWith (!!) board [boardDim-1,boardDim-2 ..])) ]

        -- Transform an integer into an Evaluation
        eval :: Int -> Evaluation
        eval n | n  == boardDim = XWin
               | -n == boardDim = OWin
               | otherwise      = Score n

        scoreString !n [] = n
        scoreString !n (X:ps)     = scoreString (n+1) ps
        scoreString !n (O:ps)     = scoreString (n-1) ps
        scoreString !n (Empty:ps) = scoreString n ps
