module Loom.Polyhedral
  ( AffineVar (..)
  , AffineExpr
  , AccessKind (..)
  , Access2D (..)
  , Dependence2D (..)
  , Schedule2D
  , PhaseSummary2D (..)
  , KernelSummary2D (..)
  , PolyhedralError (..)
  , Phase2D
  , Kernel2D
  , rowVar
  , colVar
  , auxVar
  , constant
  , scaled
  , plus
  , renderAffineExpr
  , readAccess2D
  , writeAccess2D
  , renderAccess2D
  , identitySchedule2D
  , affineSchedule2D
  , tileSchedule2D
  , wavefrontSchedule2D
  , composeSchedule2D
  , renderSchedule2D
  , phase2D
  , kernel2D
  , summarizeKernel2D
  , validateKernel2D
  , lowerKernel2D
  ) where

import Loom.Internal.Polyhedral
