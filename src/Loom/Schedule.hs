{-|
Module      : Loom.Schedule
Description : Public schedule constructors for shape-first Loom loops.
Copyright   :
License     : BSD-3-Clause

Import this module when you want to control loop traversal without leaving the
main Loom API.

Typical usage:

@
import Loom
import qualified Loom.Schedule as Schedule

runProg $ parallel $
  for2 rows cols (Schedule.tile2 32 32 >>> Schedule.interchange) $ \\i j -> ...
@

Start with 'identity' for the default traversal, then add tiling, permutation,
or an explicit affine schedule when you need it.
-}
module Loom.Schedule
  (
    -- * Type
    Schedule
    -- * Constructors
  , identity
  , affine
  , tile
  , tile2
  , tile3
  , permute
  , interchange
    -- * Composition and rendering
  , compose
  , render
  , (>>>)
  ) where

import Loom.Internal.Schedule
