module TransformCore (main) where

import Loom.Expert

main :: IO ()
main = do
  let rows = 6
      cols = 8
      shape = sh2 rows cols
      transform =
        composeTransform2D
          (skewTransform2D 1)
          (tileTransform2D 2 3)
  arr <- newArr (rows * cols)
  runProg $
    parallel $
      parForTransform2D transform shape $ \ix ->
        withIx2 ix $ \i j ->
          writeArr arr (index2 shape ix) (i * 10 + j)
  xs <- toList arr
  if xs == [i * 10 + j | i <- [0 .. rows - 1], j <- [0 .. cols - 1]]
    then pure ()
    else error ("unexpected transform result: " <> show xs)
