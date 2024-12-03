{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module CustomDataTypes where

data MatrixRow = MatrixRow (Int, Int, Int, Int, Int, Int, Int, Int, Int)
    deriving (Eq)


data Matrix = Matrix (MatrixRow, MatrixRow, MatrixRow, MatrixRow, MatrixRow, MatrixRow, MatrixRow, MatrixRow, MatrixRow)
    deriving (Eq)

data MatrixPosition = MatrixPosition(Int, Int)