module Sudoku where

sudokuBase :: Int
sudokuBase = 3

sudokuSize :: Int
sudokuSize = sudokuBase^2

type Sudoku = Int
sudokuElm :: [Sudoku]
sudokuElm = [1..sudokuSize]
vacant :: Sudoku
vacant = 0

type SudokuBoard = [[Sudoku]]

sample :: SudokuBoard
sample = [ [8, 0, 0, 0, 3, 4, 0, 5, 0]
         , [0, 0, 2, 0, 0, 0, 0, 0, 1]
         , [0, 1, 0, 9, 0, 0, 0, 0, 0]
         , [0, 0, 8, 0, 0, 9, 0, 0, 6]
         , [5, 0, 0, 0, 1, 0, 0, 0, 8]
         , [6, 0, 0, 4, 0, 0, 7, 0, 0]
         , [0, 0, 0, 0, 0, 1, 0, 7, 0]
         , [2, 0, 0, 0, 0, 0, 1, 0, 0]
         , [0, 9, 0, 5, 6, 0, 0, 0, 7]
         ]
