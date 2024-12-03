module SudokuCreator where

import Printer
import SudokuLogic
import CustomDataTypes
import CustomMatrixOperations
import Backtracking
import System.Random
import Data.List (transpose)


tryFixSudoku :: Matrix -> Int -> IO Matrix
tryFixSudoku m 81 = return m
tryFixSudoku m n = do
    let emptyPos = findEmptyPos m
    randomVal <- randomRIO (1, 9)
    let updatedMatrix = setValueAtMatrixPos m emptyPos randomVal
    let solvedSudoku = backtracking updatedMatrix (MatrixPosition (0, 0))
    if solvedSudoku /= updatedMatrix
        then tryFixSudoku updatedMatrix (n + 1)
        else do
            let matrixWithoutValue = setValueAtMatrixPos m emptyPos (-1)
            tryFixSudoku matrixWithoutValue n

findEmptyPos :: Matrix -> MatrixPosition
findEmptyPos m = head [(MatrixPosition (x, y)) | x <- [0..8], y <- [0..8], getValueAtMatrixPosition m (MatrixPosition (x, y)) == -1]

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex idx val list = take idx list ++ [val] ++ drop (idx + 1) list


-- | Setzt zufällige Werte in die Matrix
generateUniqueSudoku :: IO Matrix
generateUniqueSudoku = do
    let emptyMatrix = listToMatrix (replicate 81 (-1))
    sudoku <- fillRandomValues emptyMatrix 20
    if isSolvable sudoku
        then return sudoku
        else generateUniqueSudoku -- Rekursiv erneut generieren, falls das Sudoku nicht lösbar ist

fillRandomValues :: Matrix -> Int -> IO Matrix
fillRandomValues m 0 = return m
fillRandomValues m n = do
    randomPos <- randomRIO (0, 80)
    randomVal <- randomRIO (1, 9)
    let (x, y) = (randomPos `mod` 9, randomPos `div` 9)
        pos = MatrixPosition (x, y)
    if getValueAtMatrixPosition m pos == -1 && isValid m pos randomVal
        then do
            let newMatrix = setValueAtMatrixPos m pos randomVal
            fillRandomValues newMatrix (n - 1)
        else fillRandomValues m n

isValid :: Matrix -> MatrixPosition -> Int -> Bool
isValid m pos val =
    not (inRow m pos val || inColumn m pos val || inBox m pos val)

isSolvable :: Matrix -> Bool
isSolvable m = backtracking m (MatrixPosition (0, 0)) /= m

inRow, inColumn, inBox :: Matrix -> MatrixPosition -> Int -> Bool
inRow m (MatrixPosition (x, _)) val =
    any (== val) [getValueAtMatrixPosition m (MatrixPosition (x, y)) | y <- [0..8]]

inColumn m (MatrixPosition (_, y)) val =
    any (== val) [getValueAtMatrixPosition m (MatrixPosition (x, y)) | x <- [0..8]]

inBox m (MatrixPosition (x, y)) val =
    let startX = (x `div` 3) * 3
        startY = (y `div` 3) * 3
    in any (== val) [getValueAtMatrixPosition m (MatrixPosition (x', y')) | x' <- [startX..startX+2], y' <- [startY..startY+2]]

-- | Invertiert die Reihenfolge der Werte in der Matrix
reverseMatrix :: Matrix -> Matrix
reverseMatrix m = 
    let intList = matrixToIntList m
        reversedList = reverse intList
    in listToMatrix reversedList


-- | Vergleicht zwei Matrizen auf Gleichheit
matricesEqual :: Matrix -> Matrix -> Bool
matricesEqual m1 m2 =
    all (\(x, y) -> getValueAtMatrixPosition m1 (MatrixPosition (x, y)) == 
                    getValueAtMatrixPosition m2 (MatrixPosition (x, y))) 
        [(x, y) | x <- [0..8], y <- [0..8]]

matrixToIntList :: Matrix -> [Int]
matrixToIntList m = concatMap matrixRowToIntList (matrixToRowList m)