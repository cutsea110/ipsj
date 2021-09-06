{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Sudoku where

import Data.List (nub, minimumBy, transpose, (\\))

import Puzzle

type Sudoku = Int

instance Puzzle Sudoku where
  puzzleElm     = [1..sudokuSize]
  vacant        = 0
  candidate b p = puzzleElm \\ nub (concatMap (\c -> c b p) [col, row, box])

sudokuBase :: Int
sudokuBase = 3

sudokuSize :: Int
sudokuSize = sudokuBase^2

col :: Board Sudoku -> Position -> [Sudoku]
col b (i, _) = transpose b !! i
row :: Board Sudoku -> Position -> [Sudoku]
row b (_, j) = b !! j
box :: Board Sudoku -> Position -> [Sudoku]
box b (i, j) = concat
               $ map (take sudokuBase) $ map (drop $ (i `div` sudokuBase) * sudokuBase)
               $ take sudokuBase $ drop ((j `div` sudokuBase) * sudokuBase) b

sudoku :: Board Sudoku -> [Board Sudoku]
sudoku = solve

showSudokuBoard :: Board Sudoku -> String
showSudokuBoard b = unlines $ map (unwords . map show) b

readSudokuBoard :: String -> Board Sudoku
readSudokuBoard s = map (map read . words) $ lines s

instance {-# OVERLAPPING #-} Show (Board Sudoku) where
  show = showSudokuBoard

instance {-# OVERLAPPING #-} Read (Board Sudoku) where
  readsPrec _ str = [(readSudokuBoard str, "")]

sample :: Board Sudoku
sample = read sampleData
sampleData :: String
sampleData = unlines ["8 0 0 0 3 4 0 5 0"
                     ,"0 0 2 0 0 0 0 0 1"
                     ,"0 1 0 9 0 0 0 0 0"
                     ,"0 0 8 0 0 9 0 0 6"
                     ,"5 0 0 0 1 0 0 0 8"
                     ,"6 0 0 4 0 0 7 0 0"
                     ,"0 0 0 0 0 1 0 7 0"
                     ,"2 0 0 0 0 0 1 0 0"
                     ,"0 9 0 5 6 0 0 0 2"
                     ]
