{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module CustomDataTypes where

data MatrixRow = MatrixRow(Int, Int, Int, Int, Int, Int, Int, Int, Int)
data Matrix = Matrix (MatrixRow, MatrixRow, MatrixRow, MatrixRow, MatrixRow, MatrixRow, MatrixRow, MatrixRow, MatrixRow)
data MatrixPosition = MatrixPosition(Int, Int)