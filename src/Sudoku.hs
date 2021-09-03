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

type Position = (Int, Int) -- 列 x 行

sudoku :: SudokuBoard -> [SudokuBoard]
sudoku b
  = case vacantPositions b of
      [] -> [b]
      ps -> case nextVacant b ps of
              (p, xs) -> concatMap (sudoku . putCell b) [(p, x) | x <- xs]

vacantPositions :: SudokuBoard -> [Position]
vacantPositions = error "(vacantPositions) is not yet implemented"

nextVacant :: SudokuBoard -> [Position] -> (Position, [Sudoku])
nextVacant = error "(nextVacant) is not yet implemented"

putCell :: SudokuBoard -> (Position, Sudoku) -> SudokuBoard
putCell = error "(putCell) is not yet implemented"
