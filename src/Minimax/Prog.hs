module Minimax.Prog (randomBoard, solve) where

import System.Random
import Data.List

import Minimax.Board
import Minimax.Wins
import Minimax.Game (Player, Move, showMove)

-- X to play: find the best move
solve :: (Int -> Piece -> Player -> Player -> Board -> [Move]) -> Int -> Board -> String
solve alternate depth
    = unlines
    . map showMove
    . take 1
    . alternate depth X maxE minE

randomBoard :: StdGen -> Int -> Board
randomBoard g moves =  do
  let (g1,g2) = split g
      xs = randomRs (1, boardDim) g1
      ys = randomRs (1, boardDim) g2

  let
    play 0 _ _ board = board
    play n (pos:poss) (p:ps) board
     | not (empty pos board) = play n poss (p:ps) board
     | otherwise             = play (n-1) poss ps (placePiece p board pos)

  play moves (zip xs ys) (cycle [X,O]) initialBoard
