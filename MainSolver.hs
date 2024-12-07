module MainSolver where

import CustomDataTypes
import Printer
import Backtracking
import CustomMatrixOperations
import TestData (sudokuMatrix)

-- Used for single presentation of the backtracking algorithm. Works all the time.
main :: IO ()
main = do
    putStrLn "Eingegebenes Sudoku:"
    printSudokuMatrix sudokuMatrix

    putStrLn "\nDrücken Sie Enter, um das gelöste Sudoku anzuzeigen..."
    _ <- getLine

    putStrLn "\nGelöstes Sudoku:"
    let solvedSudoku = backtracking sudokuMatrix (MatrixPosition (0, 0)) False
    printSudokuMatrix solvedSudoku

    putStrLn "\nDrücken Sie Enter, um das Programm zu beenden..."
    _ <- getLine
    return ()
    