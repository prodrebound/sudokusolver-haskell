{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Use notElem" #-}
{-# HLINT ignore "Redundant ==" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use newtype instead of data" #-}



-- TODO delete matrix.hs if everything works




sudokuList = [5,1,8,6,-1,-1,4,-1,-1,-1,-1,6,-1,-1,-1,-1,-1,7,4,3,-1,2,-1,-1,-1,-1,6,7,-1,-1,-1,-1,-1,-1,3,-1,-1,6,4,-1,1,8,2,-1,5,-1,5,-1,7,2,6,9,4,8,-1,-1,9,-1,-1,1,8,2,3,-1,2,1,-1,-1,-1,7,-1,-1,3,-1,-1,-1,4,-1,-1,6,9]
sudokuMatrix = listToMatrix sudokuList

printIteratedMatrix :: Matrix -> MatrixPosition -> IO()
printIteratedMatrix m p =
    if checkIfMatrixRowOutOfBound p
    then print "finished"
    else do
        print (getValueAtMatrixPosition m p)
        printIteratedMatrix m (incrementMatrixPosition p)








main = do
    putStrLn "Original Sudoku:"
    printSudokuMatrix sudokuMatrix
    putStrLn "\nLösung:"
    let result = backtracking sudokuMatrix (MatrixPosition (0,0))
    printSudokuMatrix result