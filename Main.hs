module Main where

import CustomDataTypes
import Printer
import Backtracking
import SudokuCreator

main :: IO ()
main = do
    putStrLn "Generiertes Sudoku:"
    sudoku <- generateUniqueSudoku
    printSudokuMatrix sudoku

    putStrLn "\nDrücken Sie Enter, um das gelöste Sudoku anzuzeigen..."
    _ <- getLine

    putStrLn "\nGelöstes Sudoku:"
    let solvedSudoku = backtracking sudoku (MatrixPosition (0, 0))
    printSudokuMatrix solvedSudoku