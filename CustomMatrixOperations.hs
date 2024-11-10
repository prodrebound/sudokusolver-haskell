module CustomMatrixOperations where

import CustomDataTypes

matrixRowToIntList :: MatrixRow -> [Int]
matrixRowToIntList (MatrixRow(x1, x2, x3, x4, x5, x6, x7, x8, x9)) = [x1, x2, x3, x4, x5, x6, x7, x8, x9]

matrixToRowList :: Matrix -> [MatrixRow]
matrixToRowList (Matrix(r1, r2, r3, r4, r5, r6, r7, r8, r9)) = [r1, r2, r3, r4, r5, r6, r7, r8, r9]

getRowFromRowList :: [MatrixRow] -> Int -> MatrixRow
getRowFromRowList (m:ms) 0 = m
getRowFromRowList (m:ms) x = getRowFromRowList ms (x-1)

getListItem :: [a] -> Int -> a
getListItem [] _ = error "Index out of bounds"
getListItem (m:ms) 0 = m
getListItem (m:ms) x
    | x < 0     = error "Negative index"
    | otherwise = getListItem ms (x-1)

getRowIntListFromMatrix :: Matrix -> Int -> [Int]
getRowIntListFromMatrix m x = matrixRowToIntList (getRowFromRowList (matrixToRowList m) x)

getColumnIntListFromMatrix :: Matrix -> Int -> [Int]
getColumnIntListFromMatrix (Matrix(r1, r2, r3, r4, r5, r6, r7, r8, r9)) x = [getListItem (matrixRowToIntList r1) x, getListItem (matrixRowToIntList r2) x, getListItem (matrixRowToIntList r3) x, getListItem (matrixRowToIntList r4) x, getListItem (matrixRowToIntList r5) x, getListItem (matrixRowToIntList r6) x, getListItem (matrixRowToIntList r7) x, getListItem (matrixRowToIntList r8) x, getListItem (matrixRowToIntList r9) x]

getElementFromRow :: MatrixRow -> Int -> Int
getElementFromRow m x = getListItem (matrixRowToIntList m) x

--list must contain 9 elements
rowListToMatrix :: [MatrixRow] -> Matrix
rowListToMatrix [r0, r1, r2, r3, r4, r5, r6, r7, r8] = Matrix (r0, r1, r2, r3, r4, r5, r6, r7, r8)

listToMatrixRow :: [Int] -> MatrixRow
listToMatrixRow [a, b, c, d, e, f, g, h, i] = MatrixRow (a, b, c, d, e, f, g, h, i)

listToMatrix :: [Int] -> Matrix
listToMatrix list = Matrix (
        listToMatrixRow (take 9 list),
        listToMatrixRow (take 9 (drop 9 list)),
        listToMatrixRow (take 9 (drop 18 list)),
        listToMatrixRow (take 9 (drop 27 list)),
        listToMatrixRow (take 9 (drop 36 list)),
        listToMatrixRow (take 9 (drop 45 list)),
        listToMatrixRow (take 9 (drop 54 list)),
        listToMatrixRow (take 9 (drop 63 list)),
        listToMatrixRow (take 9 (drop 72 list))
    )