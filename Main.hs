module Main where

import CustomDataTypes
import Printer
import Backtracking
import TestData

main :: IO ()
main = do
    putStrLn "Original Sudoku:"
    printSudokuMatrix sudokuMatrix
    putStrLn "\nLösung:"
    let result = backtracking sudokuMatrix (MatrixPosition (0,0))
    printSudokuMatrix result
