--freies Feld wird durch -1 dargestellt

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

getColumnListFromMatrix :: Matrix -> Int -> [Int]
getColumnListFromMatrix (Matrix(r1, r2, r3, r4, r5, r6, r7, r8, r9)) x = [getIntFromList(matrixRowToIntList r1) x, getIntFromList (matrixRowToIntList r2) x, getIntFromList (matrixRowToIntList r3) x, getIntFromList (matrixRowToIntList r4) x, getIntFromList (matrixRowToIntList r5) x, getIntFromList (matrixRowToIntList r6) x, getIntFromList (matrixRowToIntList r7) x, getIntFromList (matrixRowToIntList r8) x, getIntFromList (matrixRowToIntList r9) x]

data MatrixPosition = MatrixPosition(Int, Int)

incrementMatrixPosition :: MatrixPosition -> MatrixPosition
incrementMatrixPosition (MatrixPosition(8, y)) = MatrixPosition(0, y+1)

--if true then we should be finished
checkIfMatrixRowOutOfBound :: MatrixPosition -> Bool
checkIfMatrixRowOutOfBound (MatrixPosition(x, y)) = y > 8

checkIfPositionEmpty :: Matrix -> MatrixPosition -> Bool
checkIfPositionEmpty m (MatrixPosition(x, y)) = getIntFromList (matrixRowToIntList (getRowFromRowList (matrixToRowList m) y)) x == -1

--checkIfPositionLegal :: Matrix -> MatrixPosition -> Bool
--checkIfPositionLegal m p = 