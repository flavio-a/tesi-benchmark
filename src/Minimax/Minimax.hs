module Minimax.Minimax
    ( bseq
    , bstrat
    , bmpar
    , brepa
    ) where

import System.Random

import Minimax.Game
import Minimax.Board

import Control.Monad.Par

randomBoard :: StdGen -> Int -> Board
randomBoard g moves = play moves (zip xs ys) (cycle [X,O]) initialBoard
    where
        (g1,g2) = split g
        xs = randomRs (1, boardDim) g1
        ys = randomRs (1, boardDim) g2

        play 0 _ _ board = board
        play n (pos:poss) (p:ps) board
            | not (empty pos board) = play n poss (p:ps) board
            | otherwise             = play (n-1) poss ps (placePiece p board pos)

bseq :: Int -> Int -> [Move]
bseq n depth =
    let b = randomBoard (mkStdGen 99999) n in
        alternateSeq depth X maxE minE b

bstrat :: Int -> Int -> [Move]
bstrat n depth =
    let b = randomBoard (mkStdGen 99999) n in
        alternateStrat depth X maxE minE b

bmpar :: Int -> Int -> [Move]
bmpar n depth = runPar $
    let b = randomBoard (mkStdGen 99999) n in
        alternateMpar depth X maxE minE b

brepa :: Int -> Int -> [Move]
brepa n depth =
    let b = randomBoard (mkStdGen 99999) n in
        alternateRepa depth X maxE minE b
