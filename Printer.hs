module Printer where

import CustomDataTypes
import SudokuLogic

printSudokuMatrix :: Matrix -> IO()
printSudokuMatrix m = do
    putStrLn "-------------------------"
    printRows 0
    where
        printRows 9 = return ()
        printRows y = do
            putStr "| "
            printCols y 0
            if y `mod` 3 == 2
                then putStrLn "-------------------------"
                else return ()
            printRows (y + 1)

        printCols y 9 = putStrLn ""
        printCols y x = do
            let val = getValueAtMatrixPosition m (MatrixPosition (x,y))
            putStr (if val == -1 then " " else show val)
            putStr (if (x + 1) `mod` 3 == 0 then " | " else " ")
            printCols y (x + 1)
