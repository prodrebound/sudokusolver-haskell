module PositionOperations where

import CustomDataTypes

incrementMatrixPosition :: MatrixPosition -> MatrixPosition
incrementMatrixPosition (MatrixPosition(8, y)) = MatrixPosition (0, y+1)
incrementMatrixPosition (MatrixPosition(x,y)) = MatrixPosition (x+1, y)

decrementMatrixPosition :: MatrixPosition -> MatrixPosition
decrementMatrixPosition (MatrixPosition(0, y)) = MatrixPosition (8, y-1)
decrementMatrixPosition (MatrixPosition(x, y)) = MatrixPosition (x-1, y)

--if true then we should be finished
checkIfMatrixRowOutOfBound :: MatrixPosition -> Bool
checkIfMatrixRowOutOfBound (MatrixPosition(x, y)) = y > 8 || y < 0

matrixPosToListPos :: MatrixPosition -> Int
matrixPosToListPos (MatrixPosition(x,y)) = x + 9*y