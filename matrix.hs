--free field displayed by -1

data MatrixRow = MatrixRow(Int, Int, Int, Int, Int, Int, Int, Int, Int)
data Matrix = Matrix (MatrixRow, MatrixRow, MatrixRow, MatrixRow, MatrixRow, MatrixRow, MatrixRow, MatrixRow, MatrixRow)

matrixRowToIntList :: MatrixRow -> [Int]
matrixRowToIntList (MatrixRow(x1, x2, x3, x4, x5, x6, x7, x8, x9)) = [x1, x2, x3, x4, x5, x6, x7, x8, x9]

matrixToRowList :: Matrix -> [MatrixRow]
matrixToRowList (Matrix(r1, r2, r3, r4, r5, r6, r7, r8, r9)) = [r1, r2, r3, r4, r5, r6, r7, r8, r9]

getRowFromRowList :: [MatrixRow] -> Int -> MatrixRow
getRowFromRowList (m:ms) 0 = m
getRowFromRowList (m:ms) x = getRowFromRowList ms (x-1)

getIntFromList :: [Int] -> Int -> Int
getIntFromList (m:ms) 0 = m
getIntFromList (m:ms) x = getIntFromList ms (x-1)

getRowIntListFromMatrix :: Matrix -> Int -> [Int]
getRowIntListFromMatrix m x = matrixRowToIntList (getRowFromRowList (matrixToRowList m) x)

getColumnIntListFromMatrix :: Matrix -> Int -> [Int]
getColumnIntListFromMatrix (Matrix(r1, r2, r3, r4, r5, r6, r7, r8, r9)) x = [getIntFromList (matrixRowToIntList r1) x, getIntFromList (matrixRowToIntList r2) x, getIntFromList (matrixRowToIntList r3) x, getIntFromList (matrixRowToIntList r4) x, getIntFromList (matrixRowToIntList r5) x, getIntFromList (matrixRowToIntList r6) x, getIntFromList (matrixRowToIntList r7) x, getIntFromList (matrixRowToIntList r8) x, getIntFromList (matrixRowToIntList r9) x]

getElementFromRow :: MatrixRow -> Int -> Int
getElementFromRow m x = getIntFromList (matrixRowToIntList m) x

--list must contain 9 elements
rowListToMatrix :: [MatrixRow] -> Matrix
rowListToMatrix [r0, r1, r2, r3, r4, r5, r6, r7, r8] = (Matrix (r0, r1, r2, r3, r4, r5, r6, r7, r8))

data MatrixPosition = MatrixPosition(Int, Int)

incrementMatrixPosition :: MatrixPosition -> MatrixPosition
incrementMatrixPosition (MatrixPosition(8, y)) = MatrixPosition (0, y+1)

--if true then we should be finished
checkIfMatrixRowOutOfBound :: MatrixPosition -> Bool
checkIfMatrixRowOutOfBound (MatrixPosition(x, y)) = y > 8

checkIfNumberIn3x3 :: Matrix -> MatrixPosition -> Int -> Bool
checkIfNumberIn3x3 m (MatrixPosition(x,y)) num =
    let startX = (x `div` 3) * 3
        startY = (y `div` 3) * 3
        block = [ getIntFromList (getRowIntListFromMatrix m row) col
            | row <- [startY..startY+2]
            , col <- [startX..startX+2]
            ]
    in elem num (filterOutEmptyPositions block)

checkIfPositionLegal :: Matrix -> MatrixPosition -> Int -> Bool
checkIfPositionLegal m (MatrixPosition(x, y)) i =   not (elem i (filterOutEmptyPositions (getColumnIntListFromMatrix m x))) && not (elem i (filterOutEmptyPositions (getRowIntListFromMatrix m y))) && not (checkIfNumberIn3x3 m (MatrixPosition (x,y)) i)

--all the initial given values in this Matrix are 1, all values which are not given are 0
checkIfPositionEmpty :: Matrix -> MatrixPosition -> Bool
checkIfPositionEmpty m (MatrixPosition(x, y)) = (getIntFromList (matrixRowToIntList (getRowFromRowList (matrixToRowList m) y)) x) == -1

filterOutEmptyPositions :: [Int] -> [Int]
filterOutEmptyPositions arr = [x | x <- arr, x /= -1]

getValueAtMatrixPosition :: Matrix -> MatrixPosition -> Int
getValueAtMatrixPosition m (MatrixPosition(x,y)) = getElementFromRow (getRowFromRowList (matrixToRowList m) y) x

getFixValueMatrix :: Matrix -> [Bool]
getFixValueMatrix m = [getValueAtMatrixPosition m (MatrixPosition (x, y)) == -1 | x <- [0..8], y <- [0..8]]

--nÃ¤chste Schritte:
--check if MatrixPosition(x,y) is fix
--iterate over Matrix
--implement backtracking