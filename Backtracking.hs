module Backtracking where

import CustomDataTypes
import CustomMatrixOperations
import PositionOperations
import SudokuLogic

getFixValueMatrix :: Matrix -> [Bool]
getFixValueMatrix m = [getValueAtMatrixPosition m (MatrixPosition (x, y)) /= -1 | y <- [0..8], x <- [0..8]]

checkIfMatrixPosFix :: MatrixPosition -> [Bool] -> Bool
checkIfMatrixPosFix p fixvalues = getListItem fixvalues (matrixPosToListPos p) == True

backtracking :: Matrix -> MatrixPosition -> Matrix
backtracking m pos = backtrackingIteration m pos (getFixValueMatrix m)

backtrackingIteration :: Matrix -> MatrixPosition -> [Bool] -> Matrix
backtrackingIteration m pos fixvalues 
    | checkIfMatrixRowOutOfBound pos = m
    | checkIfMatrixPosFix pos fixvalues = backtrackingIteration m (getNextMatrixPosition pos fixvalues) fixvalues
    | not (checkIfPositionEmpty m pos) &&  checkIfPositionLegal m pos (getValueAtMatrixPosition m pos) =
        backtrackingIteration m (getNextMatrixPosition pos fixvalues) fixvalues
    | otherwise = 
        let currentVal = getValueAtMatrixPosition m pos
        in if currentVal == -1 
            then evaluateTryValueResult m pos fixvalues (tryValues m pos 1 fixvalues) 
            else evaluateTryValueResult m pos fixvalues (tryValues m pos (currentVal+1) fixvalues)

checkPositionValue :: Matrix -> MatrixPosition -> [Bool] -> Bool
checkPositionValue m pos fixvalues =
    let val = getValueAtMatrixPosition m pos
    in val /= -1 && checkIfPositionLegal m pos val

tryValues :: Matrix -> MatrixPosition -> Int -> [Bool] -> Int
tryValues m pos val fixvalues 
    | val > 9 = -1
    | checkIfPositionLegal m pos val = val
    | otherwise = tryValues m pos (val+1) fixvalues

evaluateTryValueResult :: Matrix -> MatrixPosition -> [Bool] -> Int -> Matrix
evaluateTryValueResult m pos fixvalues val 
    | val == -1 = backtrackingIteration (setValueAtMatrixPos m pos val) (getPreviousMatrixPosition pos fixvalues) fixvalues
    | otherwise = backtrackingIteration (setValueAtMatrixPos m pos val) (getNextMatrixPosition pos fixvalues) fixvalues

getNextMatrixPosition :: MatrixPosition -> [Bool] -> MatrixPosition
getNextMatrixPosition pos fixvalues
    | x == 8 && y == 8 = incrementMatrixPosition pos
    | checkIfMatrixPosFix (incrementMatrixPosition pos) fixvalues = getNextMatrixPosition (incrementMatrixPosition pos) fixvalues
    | otherwise = incrementMatrixPosition pos
    where MatrixPosition(x,y) = pos

getPreviousMatrixPosition :: MatrixPosition -> [Bool] -> MatrixPosition
getPreviousMatrixPosition pos fixvalues
    |  x == 0 && y == 0 = error "No valid position!"
    |  checkIfMatrixPosFix (decrementMatrixPosition pos) fixvalues = getPreviousMatrixPosition (decrementMatrixPosition pos) fixvalues
    |  otherwise = decrementMatrixPosition pos
    where MatrixPosition(x,y) = pos

increaseBacktrackingValue :: Matrix -> MatrixPosition -> Int -> Matrix
increaseBacktrackingValue m pos (-1) = setValueAtMatrixPos m pos 1
increaseBacktrackingValue m pos n = setValueAtMatrixPos m pos (n+1)

backtrackingFinished :: Matrix -> Bool
backtrackingFinished m = all (/= -1) [getValueAtMatrixPosition m (MatrixPosition (x,y)) | x <- [0..8], y <- [0..8]] && all (== True) [checkIfPositionLegal m (MatrixPosition (x,y)) (getValueAtMatrixPosition m (MatrixPosition (x,y))) | x <- [0..8], y <- [0..8]]

setValueAtMatrixPos :: Matrix -> MatrixPosition -> Int -> Matrix
setValueAtMatrixPos m (MatrixPosition(x,y)) value =
    let rowList = matrixToRowList m
        targetRow = getRowFromRowList rowList y
        oldIntList = matrixRowToIntList targetRow
        newRowList = take x oldIntList ++ [value] ++ drop (x+1) oldIntList
        newRow = listToMatrixRow newRowList
        newMatrixRowList = take y rowList ++ [newRow] ++ drop (y+1) rowList
    in rowListToMatrix newMatrixRowList