module TestData where

import CustomDataTypes
import CustomMatrixOperations

sudokuList :: [Int]
sudokuList = [5,1,8,6,-1,-1,4,-1,-1,-1,-1,6,-1,-1,-1,-1,-1,7,
              4,3,-1,2,-1,-1,-1,-1,6,7,-1,-1,-1,-1,-1,-1,3,-1,
              -1,6,4,-1,1,8,2,-1,5,-1,5,-1,7,2,6,9,4,8,-1,-1,9,
              -1,-1,1,8,2,3,-1,2,1,-1,-1,-1,7,-1,-1,3,-1,-1,-1,4,-1,-1,6,9]

sudokuMatrix :: Matrix
sudokuMatrix = listToMatrix sudokuList