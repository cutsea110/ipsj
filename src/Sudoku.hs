{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Sudoku where

import Data.List (nub, minimumBy, transpose, (\\))

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
sample = [[8,0,0,0,3,4,0,5,0]
         ,[0,0,2,0,0,0,0,0,1]
         ,[0,1,0,9,0,0,0,0,0]
         ,[0,0,8,0,0,9,0,0,6]
         ,[5,0,0,0,1,0,0,0,8]
         ,[6,0,0,4,0,0,7,0,0]
         ,[0,0,0,0,0,1,0,7,0]
         ,[2,0,0,0,0,0,1,0,0]
         ,[0,9,0,5,6,0,0,0,2]
         ]

type Position = (Int, Int) -- 列 x 行

sudoku :: SudokuBoard -> [SudokuBoard]
sudoku b
  = case vacantPositions b of
      [] -> [b]
      ps -> case nextVacant b ps of
              (p, xs) ->
                concatMap sudoku
                $ map (putCell b)
                $ [(p, x) | x <- xs]

vacantPositions :: SudokuBoard -> [Position]
vacantPositions b = map fst
                    $ filter (\(p, x) -> vacant == x)
                    $ concatMap (\(j, xs) -> zipWith (\i x -> ((i, j), x)) [0..] xs)
                    $ zip [0..] b

nextVacant :: SudokuBoard -> [Position] -> (Position, [Sudoku])
nextVacant b ps = minimumBy cmp [(p, candidate b p) | p <- ps]
  where (_, xs) `cmp` (_, ys) = length xs `compare` length ys

candidate :: SudokuBoard -> Position -> [Sudoku]
candidate b p = sudokuElm \\ nub (concat $ map (\c -> c b p) [col, row, box])

col :: SudokuBoard -> Position -> [Sudoku]
col b (i, _) = transpose b !! i
row :: SudokuBoard -> Position -> [Sudoku]
row b (_, j) = b !! j
box :: SudokuBoard -> Position -> [Sudoku]
box b (i, j) = concat
               $ map (take sudokuBase) $ map (drop $ (i `div` sudokuBase) * sudokuBase)
               $ take sudokuBase $ drop ((j `div` sudokuBase) * sudokuBase) b


putCell :: SudokuBoard -> (Position, Sudoku) -> SudokuBoard
putCell b ((i, j), x) = ls0 ++ (xs0 ++ x:xs1):ls1
  where (ls0, l:ls1) = splitAt j b
        (xs0, _:xs1) = splitAt i l

showSudokuBoard :: SudokuBoard -> String
showSudokuBoard b = unlines $ map (unwords . map show) b

readSudokuBoard :: String -> SudokuBoard
readSudokuBoard s = map (map read . words) $ lines s

instance {-# OVERLAPPING #-} Show SudokuBoard where
  show = showSudokuBoard

instance {-# OVERLAPPING #-} Read SudokuBoard where
  readsPrec _ str = [(readSudokuBoard str, "")]
