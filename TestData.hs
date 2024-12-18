module TestData where

import CustomDataTypes
import CustomMatrixOperations

sudokuList :: [Int]
sudokuList = [6,-1,-1,-1,-1,4,5,3,-1,8,-1,-1,-1,5,-1,-1,-1,9,5,9,4,-1,-1,-1,8,2,-1,-1,5,-1,3,-1,-1,-1,-1,-1,-1,3,-1,-1,-1,-1,-1,8,5,-1,-1,2,-1,-1,1,-1,7,-1,2,-1,8,-1,-1,-1,-1,5,-1,-1,-1,-1,7,-1,-1,-1,1,-1,-1,-1,-1,-1,-1,2,4,-1,6]

sudokuMatrix :: Matrix
sudokuMatrix = listToMatrix sudokuList