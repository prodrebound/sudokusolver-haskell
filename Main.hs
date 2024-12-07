module Main where

import CustomDataTypes
import Printer
import Backtracking
import SudokuCreator
import CustomMatrixOperations

main :: IO ()
main = do
    putStrLn "Generiertes Sudoku:"
    sudoku <- generateUniqueSudoku
    printSudokuMatrix sudoku

    putStrLn "\nDrücken Sie Enter, um das gelöste Sudoku anzuzeigen..."
    _ <- getLine

    putStrLn "\nGelöstes Sudoku:"
    let solvedSudoku = backtracking sudoku (MatrixPosition (0, 0)) False
    printSudokuMatrix solvedSudoku

    putStrLn "\nDrücken Sie Enter, um ein neues Sudoku zu generieren oder Strg+C zum Beenden..."
    _ <- getLine
    main