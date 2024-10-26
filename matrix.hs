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

getListItem :: [a] -> Int -> a
getListItem (m:ms) 0 = m
getListItem (m:ms) x = getListItem ms (x-1)

getRowIntListFromMatrix :: Matrix -> Int -> [Int]
getRowIntListFromMatrix m x = matrixRowToIntList (getRowFromRowList (matrixToRowList m) x)

getColumnIntListFromMatrix :: Matrix -> Int -> [Int]
getColumnIntListFromMatrix (Matrix(r1, r2, r3, r4, r5, r6, r7, r8, r9)) x = [getListItem (matrixRowToIntList r1) x, getListItem (matrixRowToIntList r2) x, getListItem (matrixRowToIntList r3) x, getListItem (matrixRowToIntList r4) x, getListItem (matrixRowToIntList r5) x, getListItem (matrixRowToIntList r6) x, getListItem (matrixRowToIntList r7) x, getListItem (matrixRowToIntList r8) x, getListItem (matrixRowToIntList r9) x]

getElementFromRow :: MatrixRow -> Int -> Int
getElementFromRow m x = getListItem (matrixRowToIntList m) x

--list must contain 9 elements
rowListToMatrix :: [MatrixRow] -> Matrix
rowListToMatrix [r0, r1, r2, r3, r4, r5, r6, r7, r8] = (Matrix (r0, r1, r2, r3, r4, r5, r6, r7, r8))

data MatrixPosition = MatrixPosition(Int, Int)

incrementMatrixPosition :: MatrixPosition -> MatrixPosition
incrementMatrixPosition (MatrixPosition(8, y)) = MatrixPosition (0, y+1)
incrementMatrixPosition (MatrixPosition(x,y)) = MatrixPosition (x+1, y)

decrementMatrixPosition :: MatrixPosition -> MatrixPosition
decrementMatrixPosition (MatrixPosition(0, y)) = MatrixPosition (8, y-1)
decrementMatrixPosition (MatrixPosition(x, y)) = MatrixPosition (x-1, y)

--if true then we should be finished
checkIfMatrixRowOutOfBound :: MatrixPosition -> Bool
checkIfMatrixRowOutOfBound (MatrixPosition(x, y)) = y > 8 || y < 0

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
checkIfPositionLegal m (MatrixPosition(x, y)) i =   not (elem i (filterOutEmptyPositions (getColumnIntListFromMatrix m x))) && not (elem i (filterOutEmptyPositions (getRowIntListFromMatrix m y))) && not (checkIfNumberIn3x3 m (MatrixPosition (x,y)) i)

--all the initial given values in this Matrix are 1, all values which are not given are 0
checkIfPositionEmpty :: Matrix -> MatrixPosition -> Bool
checkIfPositionEmpty m (MatrixPosition(x, y)) = (getListItem (matrixRowToIntList (getRowFromRowList (matrixToRowList m) y)) x) == -1

filterOutEmptyPositions :: [Int] -> [Int]
filterOutEmptyPositions arr = [x | x <- arr, x /= -1]

getValueAtMatrixPosition :: Matrix -> MatrixPosition -> Int
getValueAtMatrixPosition m (MatrixPosition(x,y)) = getElementFromRow (getRowFromRowList (matrixToRowList m) y) x

getFixValueMatrix :: Matrix -> [Bool]
getFixValueMatrix m = [getValueAtMatrixPosition m (MatrixPosition (x, y)) /= -1 | y <- [0..8], x <- [0..8]]

matrixPosToListPos :: MatrixPosition -> Int
matrixPosToListPos (MatrixPosition(x,y)) = x + 9*y

checkIfMatrixPosFix :: Matrix -> MatrixPosition -> [Bool] -> Bool
checkIfMatrixPosFix m p fixvalues = getListItem fixvalues (matrixPosToListPos p) == True

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

sudokuList = [3,-1,6,5,-1,8,4,-1,-1,5,2,-1,-1,-1,-1,-1,-1,-1,-1,8,7,-1,-1,-1,-1,3,1,-1,-1,3,-1,1,-1,-1,8,-1,9,-1,-1,8,6,3,-1,-1,5,-1,5,-1,-1,9,-1,6,-1,-1,1,3,-1,-1,-1,-1,2,5,-1,-1,-1,-1,-1,-1,-1,-1,7,4,-1,-1,5,2,-1,6,3,-1,-1]
sudokuMatrix = listToMatrix sudokuList

printIteratedMatrix :: Matrix -> MatrixPosition -> IO()
printIteratedMatrix m p = 
    if checkIfMatrixRowOutOfBound p 
    then print "finished" 
    else do
        print (getValueAtMatrixPosition m p)
        printIteratedMatrix m (incrementMatrixPosition p)


-- TODO implement backtracking

setValueAtMatrixPos :: Matrix -> MatrixPosition -> Int -> Matrix
setValueAtMatrixPos m (MatrixPosition(x,y)) value = 
    let rowList = matrixToRowList m
        targetRow = getRowFromRowList rowList y
        oldIntList = matrixRowToIntList targetRow
        newRowList = take x oldIntList ++ [value] ++ drop (x+1) oldIntList
        newRow = listToMatrixRow newRowList
        newMatrixRowList = take y rowList ++ [newRow] ++ drop (y+1) rowList
    in rowListToMatrix newMatrixRowList

backtracking :: Matrix -> MatrixPosition -> Matrix
backtracking m pos = backtrackingIteration m pos (getFixValueMatrix m)

backtrackingIteration :: Matrix -> MatrixPosition -> [Bool] -> Matrix
backtrackingIteration m pos fixvalues 
    | checkIfMatrixRowOutOfBound pos = m
    | getValueAtMatrixPosition m pos == 10 = backtrackingIteration (setValueAtMatrixPos m pos 0) (getPreviousMatrixPosition m pos fixvalues) fixvalues
    | checkIfMatrixPosFix m pos fixvalues || checkIfPositionLegal m pos (getValueAtMatrixPosition m pos) = backtrackingIteration m (getNextMatrixPosition m pos fixvalues) fixvalues
    | otherwise =  backtrackingIteration (increaseBacktrackingValue m pos (getValueAtMatrixPosition m pos)) pos fixvalues

getNextMatrixPosition :: Matrix -> MatrixPosition -> [Bool] -> MatrixPosition
getNextMatrixPosition m pos fixvalues = if checkIfMatrixPosFix m (incrementMatrixPosition pos) fixvalues then  getNextMatrixPosition m (incrementMatrixPosition pos) fixvalues else incrementMatrixPosition pos

getPreviousMatrixPosition :: Matrix -> MatrixPosition -> [Bool] -> MatrixPosition
getPreviousMatrixPosition m pos fixvalues = if checkIfMatrixPosFix m (decrementMatrixPosition pos) fixvalues then  getPreviousMatrixPosition m (decrementMatrixPosition pos) fixvalues else decrementMatrixPosition pos

increaseBacktrackingValue :: Matrix -> MatrixPosition -> Int -> Matrix
increaseBacktrackingValue m pos (-1) = setValueAtMatrixPos m pos 1
increaseBacktrackingValue m pos n = setValueAtMatrixPos m pos (n+1)