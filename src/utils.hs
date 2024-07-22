module Utils where

import Data.Array.Unboxed
import Text.Printf (printf)

type Matrix2D = UArray (Int, Int) Double

type Matrix3D = UArray (Int, Int, Int) Double

-- Utility functions for printing matrices --

extract2DSlice :: Matrix3D -> Int -> Matrix2D
extract2DSlice matrix sliceIdx =
    let ((_, _, _), (_, numRows, numCols)) = bounds matrix
        sliceBounds = ((1, 1), (numRows, numCols))
        sliceElems = [((i, j), matrix ! (sliceIdx, i, j)) | i <- [1..numRows], j <- [1..numCols]]
    in array sliceBounds sliceElems

print2DMatrix :: Matrix2D -> IO ()
print2DMatrix matrix =
    let ((_, _), (numRows, numCols)) = bounds matrix
        rows = [[matrix ! (i, j) | j <- [1..numCols]] | i <- [1..numRows]]
        formatElement x = if x == 0 then "  .  " else printf "%5.1f" x
        formatRow row = unwords (map formatElement row)
    in mapM_ (putStrLn . formatRow) rows