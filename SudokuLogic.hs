module SudokuLogic where 

import CustomDataTypes ( MatrixPosition(..), Matrix )
import CustomMatrixOperations
import PositionOperations

checkIfNumberIn3x3 :: Matrix -> MatrixPosition -> Int -> Bool
checkIfNumberIn3x3 m (MatrixPosition(x,y)) num =
    let startX = (x `div` 3) * 3
        startY = (y `div` 3) * 3
        block = [ getListItem (getRowIntListFromMatrix m row) col
            | row <- [startY..startY+2]
            , col <- [startX..startX+2]
            ]
    in elem num (filterOutEmptyPositions block)

checkIfPositionLegal :: Matrix -> MatrixPosition -> Int -> Bool
checkIfPositionLegal m (MatrixPosition(x, y)) i =   not (elem i (filterOutEmptyPositions (getColumnIntListFromMatrix m x))) 
    && not (elem i (filterOutEmptyPositions (getRowIntListFromMatrix m y))) && not (checkIfNumberIn3x3 m (MatrixPosition (x,y)) i)

--all the initial given values in this Matrix are 1, all values which are not given are 0
checkIfPositionEmpty :: Matrix -> MatrixPosition -> Bool
checkIfPositionEmpty m (MatrixPosition(x, y)) = getListItem (matrixRowToIntList (getRowFromRowList (matrixToRowList m) y)) x == -1

filterOutEmptyPositions :: [Int] -> [Int]
filterOutEmptyPositions arr = [x | x <- arr, x /= -1]

getValueAtMatrixPosition :: Matrix -> MatrixPosition -> Int
getValueAtMatrixPosition m (MatrixPosition(x,y)) = getElementFromRow (getRowFromRowList (matrixToRowList m) y) x
