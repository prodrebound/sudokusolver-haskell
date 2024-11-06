{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Use notElem" #-}
{-# HLINT ignore "Redundant ==" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

data MatrixRow = MatrixRow(Int, Int, Int, Int, Int, Int, Int, Int, Int)
data Matrix = Matrix (MatrixRow, MatrixRow, MatrixRow, MatrixRow, MatrixRow, MatrixRow, MatrixRow, MatrixRow, MatrixRow)
data MatrixPosition = MatrixPosition(Int, Int)

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
checkIfPositionEmpty m (MatrixPosition(x, y)) = getListItem (matrixRowToIntList (getRowFromRowList (matrixToRowList m) y)) x == -1

filterOutEmptyPositions :: [Int] -> [Int]
filterOutEmptyPositions arr = [x | x <- arr, x /= -1]

getValueAtMatrixPosition :: Matrix -> MatrixPosition -> Int
getValueAtMatrixPosition m (MatrixPosition(x,y)) = getElementFromRow (getRowFromRowList (matrixToRowList m) y) x

getFixValueMatrix :: Matrix -> [Bool]
getFixValueMatrix m = [getValueAtMatrixPosition m (MatrixPosition (x, y)) /= -1 | y <- [0..8], x <- [0..8]]

matrixPosToListPos :: MatrixPosition -> Int
matrixPosToListPos (MatrixPosition(x,y)) = x + 9*y

checkIfMatrixPosFix :: MatrixPosition -> [Bool] -> Bool
checkIfMatrixPosFix p fixvalues = getListItem fixvalues (matrixPosToListPos p) == True

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

sudokuList = [5,1,8,6,-1,-1,4,-1,-1,-1,-1,6,-1,-1,-1,-1,-1,7,4,3,-1,2,-1,-1,-1,-1,6,7,-1,-1,-1,-1,-1,-1,3,-1,-1,6,4,-1,1,8,2,-1,5,-1,5,-1,7,2,6,9,4,8,-1,-1,9,-1,-1,1,8,2,3,-1,2,1,-1,-1,-1,7,-1,-1,3,-1,-1,-1,4,-1,-1,6,9]
sudokuMatrix = listToMatrix sudokuList

printIteratedMatrix :: Matrix -> MatrixPosition -> IO()
printIteratedMatrix m p =
    if checkIfMatrixRowOutOfBound p
    then print "finished"
    else do
        print (getValueAtMatrixPosition m p)
        printIteratedMatrix m (incrementMatrixPosition p)

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

main = do
    putStrLn "Original Sudoku:"
    printSudokuMatrix sudokuMatrix
    putStrLn "\nLösung:"
    let result = backtracking sudokuMatrix (MatrixPosition (0,0))
    printSudokuMatrix result