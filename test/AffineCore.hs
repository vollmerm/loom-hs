module AffineCore (main) where

import Loom

main :: IO ()
main = do
  let rows = 3
      cols = 5
      shape = sh2 rows cols
  arr <- newArr (rows * cols)
  runProg $
    parallel $
      parForAffine2D (skew2D 1) shape $ \ix ->
        withIx2 ix $ \i j ->
          writeArr arr (index2 shape ix) (i * 10 + j)
  xs <- toList arr
  if xs == [i * 10 + j | i <- [0 .. rows - 1], j <- [0 .. cols - 1]]
    then pure ()
    else error ("unexpected affine result: " <> show xs)
