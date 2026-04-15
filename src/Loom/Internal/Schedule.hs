module Loom.Internal.Schedule
  ( Schedule
  , identity
  , affine
  , tile
  , tile2
  , tile3
  , permute
  , interchange
  , compose
  , render
  , (>>>)
  , lowerScheduleN
  , isIdentitySchedule
  , compileSchedule2D
  , tileSchedule3D
  ) where

import Loom.Internal.Kernel
  ( ScheduleN
  , Transform2D
  , affine2
  , affineN
  , affineScheduleN
  , affineTransform2D
  , composeScheduleN
  , composeTransform2D
  , identityScheduleN
  , identityTransform2D
  , renderScheduleN
  , tileScheduleN
  , tileTransform2D
  )

data ScheduleStage
  = ScheduleAffine ![[Int]] ![Int]
  | ScheduleTile ![Int]

-- | A public schedule description.
--
-- Users normally construct values of this type through 'identity', 'tile',
-- 'tile2', 'tile3', 'permute', 'interchange', and 'affine', then pass them to
-- the public shape-first loop API.
data Schedule = Schedule !ScheduleN ![ScheduleStage]

-- | The default traversal order.
identity :: Schedule
identity = Schedule identityScheduleN []

-- | Build an affine schedule from an integer matrix and offset vector.
affine :: [[Int]] -> [Int] -> Schedule
affine matrix offset =
  Schedule (affineScheduleN (affineN matrix offset)) [ScheduleAffine matrix offset]

-- | Tile a domain by the given tile sizes.
--
-- @
-- tile [32, 32]
-- tile [8, 8, 4]
-- @
tile :: [Int] -> Schedule
tile dims = Schedule (tileScheduleN dims) [ScheduleTile dims]

-- | Two-dimensional tiling.
tile2 :: Int -> Int -> Schedule
tile2 rows cols = tile [rows, cols]

-- | Three-dimensional tiling.
tile3 :: Int -> Int -> Int -> Schedule
tile3 depth rows cols = tile [depth, rows, cols]

-- | Permute loop dimensions.
--
-- The list gives the source dimension for each output dimension.
permute :: [Int] -> Schedule
permute dims = affine (permuteMatrix dims) (replicate (length dims) 0)

-- | Swap the two dimensions of a 2D loop nest.
interchange :: Schedule
interchange = permute [1, 0]

-- | Compose two schedules from left to right.
compose :: Schedule -> Schedule -> Schedule
compose (Schedule lowerLeft stagesLeft) (Schedule lowerRight stagesRight) =
  Schedule (composeScheduleN lowerLeft lowerRight) (stagesLeft ++ stagesRight)

-- | Render a schedule in a compact textual form.
render :: Schedule -> String
render (Schedule lower _) = renderScheduleN lower

infixr 9 >>>

-- | Infix synonym for 'compose'.
(>>>) :: Schedule -> Schedule -> Schedule
(>>>) = compose

lowerScheduleN :: Schedule -> ScheduleN
lowerScheduleN (Schedule lower _) = lower

isIdentitySchedule :: Schedule -> Bool
isIdentitySchedule (Schedule _ stages) = null stages

compileSchedule2D :: Schedule -> Maybe Transform2D
compileSchedule2D (Schedule _ stages) = go stages
  where
    go [] = Just identityTransform2D
    go (stage : rest) = do
      stageTransform <- compileStage2D stage
      restTransform <- go rest
      pure (composeTransform2D stageTransform restTransform)

compileStage2D :: ScheduleStage -> Maybe Transform2D
compileStage2D (ScheduleTile [rows, cols]) =
  Just (tileTransform2D rows cols)
compileStage2D (ScheduleAffine [[a00, a01], [a10, a11]] [b0, b1]) =
  Just (affineTransform2D (affine2 a00 a01 a10 a11 b0 b1))
compileStage2D _ =
  Nothing

tileSchedule3D :: Schedule -> Maybe (Int, Int, Int)
tileSchedule3D (Schedule _ [ScheduleTile [depth, rows, cols]]) =
  Just (depth, rows, cols)
tileSchedule3D _ =
  Nothing

permuteMatrix :: [Int] -> [[Int]]
permuteMatrix dims =
  [ [ if source == target then 1 else 0
    | source <- [0 .. rank - 1]
    ]
  | target <- dims
  ]
  where
    rank = length dims
