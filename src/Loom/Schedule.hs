module Loom.Schedule
  ( Schedule
  , identity
  , affine
  , tile
  , permute
  , compose
  , render
  , (>>>)
  ) where

import Loom.Internal.Kernel
  ( ScheduleN
  , affineN
  , affineScheduleN
  , composeScheduleN
  , identityScheduleN
  , permuteScheduleN
  , renderScheduleN
  , tileScheduleN
  )

type Schedule = ScheduleN

identity :: Schedule
identity = identityScheduleN

affine :: [[Int]] -> [Int] -> Schedule
affine matrix offset = affineScheduleN (affineN matrix offset)

tile :: [Int] -> Schedule
tile = tileScheduleN

permute :: [Int] -> Schedule
permute = permuteScheduleN

compose :: Schedule -> Schedule -> Schedule
compose = composeScheduleN

render :: Schedule -> String
render = renderScheduleN

infixr 9 >>>

(>>>) :: Schedule -> Schedule -> Schedule
(>>>) = compose
