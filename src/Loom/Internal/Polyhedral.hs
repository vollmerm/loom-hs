{-# LANGUAGE BangPatterns #-}

module Loom.Internal.Polyhedral
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

import Data.List (sortOn)
import Loom.Internal.Kernel
  ( Affine2
  , AffineN
  , Prog
  , ScheduleN
  , applyAffine2
  , affineN
  , affineScheduleN
  , Sh2
  , barrier
  , composeScheduleN
  , ix2
  , composeAffine2
  , identityAffine2
  , identityScheduleN
  , invertAffine2
  , parForScheduleN
  , unIx2
  , unIxN
  , unSh2
  , parForSh2
  , parForWavefront2D
  , shN
  , tileScheduleN
  , withIx2
  )

data AffineVar
  = LoopRow
  | LoopCol
  | AuxVar String
  deriving (Eq, Ord, Show)

data AffineExpr = AffineExpr ![(AffineVar, Int)] !Int
  deriving (Eq, Show)

data AccessKind
  = ReadAccess
  | WriteAccess
  deriving (Eq, Show)

data Access2D = Access2D
  { accessKind :: !AccessKind
  , accessArray :: !String
  , accessIndex :: !AffineExpr
  }
  deriving (Eq, Show)

data Dependence2D
  = IndependentDependence2D
  | WavefrontDependence2D
  | OpaqueDependence2D String
  deriving (Eq, Show)

data ScheduleStage2D
  = ScheduleAffineStage2D !Affine2
  | ScheduleTileStage2D !Int !Int
  | ScheduleWavefrontStage2D

newtype Schedule2D = Schedule2D [ScheduleStage2D]

data PhaseSummary2D = PhaseSummary2D
  { phaseSummaryName :: !String
  , phaseSummarySchedule :: !Schedule2D
  , phaseSummaryReads :: ![Access2D]
  , phaseSummaryWrites :: ![Access2D]
  , phaseSummaryDependence :: !Dependence2D
  }

data KernelSummary2D = KernelSummary2D
  { kernelSummaryShape :: !Sh2
  , kernelSummaryPhases :: ![PhaseSummary2D]
  }

data PolyhedralError
  = InvalidSchedule2D String
  | IllegalDependence2D String
  deriving (Eq, Show)

data Phase2D = Phase2D
  { phaseName :: !String
  , phaseSchedule :: !Schedule2D
  , phaseReads :: ![Access2D]
  , phaseWrites :: ![Access2D]
  , phaseDependence :: !Dependence2D
  , phaseLowerBody :: !(Int -> Int -> Prog ())
  }

data Kernel2D = Kernel2D !Sh2 ![Phase2D]

data ValidatedPhase2D = ValidatedPhase2D
  { validatedPhaseSummary :: !PhaseSummary2D
  , validatedPhaseBody :: !(Int -> Int -> Prog ())
  }

rowVar :: AffineExpr
rowVar = AffineExpr [(LoopRow, 1)] 0

colVar :: AffineExpr
colVar = AffineExpr [(LoopCol, 1)] 0

auxVar :: String -> AffineExpr
auxVar name = AffineExpr [(AuxVar name, 1)] 0

constant :: Int -> AffineExpr
constant n = AffineExpr [] n

scaled :: Int -> AffineExpr -> AffineExpr
scaled k (AffineExpr coeffs offset)
  | k == 0 = constant 0
  | otherwise = AffineExpr (normalizeTerms [(var, k * coeff) | (var, coeff) <- coeffs]) (k * offset)

plus :: AffineExpr -> AffineExpr -> AffineExpr
plus (AffineExpr left leftOffset) (AffineExpr right rightOffset) =
  AffineExpr (normalizeTerms (left ++ right)) (leftOffset + rightOffset)

renderAffineExpr :: AffineExpr -> String
renderAffineExpr (AffineExpr coeffs offset) =
  case filter (/= "") (map renderTerm coeffs ++ [renderOffset offset]) of
    [] -> "0"
    firstTerm : restTerms -> foldl' (\acc term -> acc ++ renderJoin term ++ stripSign term) firstTerm restTerms
  where
    renderTerm (_, 0) = ""
    renderTerm (var, 1) = renderVar var
    renderTerm (var, (-1)) = '-' : renderVar var
    renderTerm (var, coeff) = show coeff ++ "*" ++ renderVar var

    renderOffset 0 = ""
    renderOffset n = show n

    renderJoin ('-' : _) = " - "
    renderJoin _ = " + "

    stripSign ('-' : xs) = xs
    stripSign xs = xs

renderVar :: AffineVar -> String
renderVar LoopRow = "row"
renderVar LoopCol = "col"
renderVar (AuxVar name) = name

readAccess2D :: String -> AffineExpr -> Access2D
readAccess2D name expr = Access2D ReadAccess name expr

writeAccess2D :: String -> AffineExpr -> Access2D
writeAccess2D name expr = Access2D WriteAccess name expr

renderAccess2D :: Access2D -> String
renderAccess2D (Access2D kind name expr) =
  renderKind kind ++ " " ++ name ++ "[" ++ renderAffineExpr expr ++ "]"
  where
    renderKind ReadAccess = "read"
    renderKind WriteAccess = "write"

identitySchedule2D :: Schedule2D
identitySchedule2D = Schedule2D []

affineSchedule2D :: Affine2 -> Schedule2D
affineSchedule2D affine = Schedule2D [ScheduleAffineStage2D affine]

tileSchedule2D :: Int -> Int -> Schedule2D
tileSchedule2D tileRows tileCols = Schedule2D [ScheduleTileStage2D tileRows tileCols]

wavefrontSchedule2D :: Schedule2D
wavefrontSchedule2D = Schedule2D [ScheduleWavefrontStage2D]

composeSchedule2D :: Schedule2D -> Schedule2D -> Schedule2D
composeSchedule2D (Schedule2D left) (Schedule2D right) =
  Schedule2D (left ++ right)

renderSchedule2D :: Schedule2D -> String
renderSchedule2D (Schedule2D stages) =
  case normalizeScheduleStages stages of
    [] -> "identity"
    normalized ->
      foldl1 (\acc stageText -> acc ++ " -> " ++ stageText) (map renderStage normalized)
  where
    renderStage (ScheduleAffineStage2D _) = "affine"
    renderStage (ScheduleTileStage2D tileRows tileCols) =
      "tile(" ++ show tileRows ++ "," ++ show tileCols ++ ")"
    renderStage ScheduleWavefrontStage2D = "wavefront"

phase2D ::
  String ->
  Schedule2D ->
  Dependence2D ->
  [Access2D] ->
  [Access2D] ->
  (Int -> Int -> Prog ()) ->
  Phase2D
phase2D name schedule dependence phaseReads' phaseWrites' body =
  Phase2D name schedule phaseReads' phaseWrites' dependence body

kernel2D :: Sh2 -> [Phase2D] -> Kernel2D
kernel2D = Kernel2D

summarizeKernel2D :: Kernel2D -> KernelSummary2D
summarizeKernel2D (Kernel2D shape phases) =
  KernelSummary2D shape (map summarizePhase2D phases)

validateKernel2D :: Kernel2D -> Either PolyhedralError KernelSummary2D
validateKernel2D (Kernel2D shape phases) = do
  validated <- mapM validatePhase2D phases
  pure (KernelSummary2D shape (map validatedPhaseSummary validated))

lowerKernel2D :: Kernel2D -> Either PolyhedralError (Prog ())
lowerKernel2D (Kernel2D shape phases) = do
  validatedPhases <- mapM validatePhase2D phases
  pure (go shape validatedPhases)
  where
    go _ [] = pure ()
    go shape' [validatedPhase] =
      lowerPhase2D shape' (validatedPhaseSummary validatedPhase) (validatedPhaseBody validatedPhase)
    go shape' (validatedPhase : rest) = do
      lowerPhase2D shape' (validatedPhaseSummary validatedPhase) (validatedPhaseBody validatedPhase)
      barrier
      go shape' rest

summarizePhase2D :: Phase2D -> PhaseSummary2D
summarizePhase2D phase =
  PhaseSummary2D
    { phaseSummaryName = phaseName phase
    , phaseSummarySchedule = normalizeSchedule2D (phaseSchedule phase)
    , phaseSummaryReads = phaseReads phase
    , phaseSummaryWrites = phaseWrites phase
    , phaseSummaryDependence = phaseDependence phase
    }

validatePhase2D :: Phase2D -> Either PolyhedralError ValidatedPhase2D
validatePhase2D phase = do
  let summary = summarizePhase2D phase
      stages = unwrapSchedule2D (phaseSummarySchedule summary)
  mapM_ validateStage2D stages
  validateDependence2D (phaseDependence phase) stages
  pure (ValidatedPhase2D summary (phaseLowerBody phase))

validateStage2D :: ScheduleStage2D -> Either PolyhedralError ()
validateStage2D (ScheduleAffineStage2D affine) =
  case invertAffine2 affine of
    Nothing ->
      Left (InvalidSchedule2D "affine schedule stages must be invertible integer transforms")
    Just _ ->
      pure ()
validateStage2D (ScheduleTileStage2D tileRows tileCols)
  | tileRows <= 0 =
      Left (InvalidSchedule2D "tile schedule stages require a positive row tile size")
  | tileCols <= 0 =
      Left (InvalidSchedule2D "tile schedule stages require a positive column tile size")
  | otherwise =
      pure ()
validateStage2D ScheduleWavefrontStage2D =
  pure ()

validateDependence2D :: Dependence2D -> [ScheduleStage2D] -> Either PolyhedralError ()
validateDependence2D IndependentDependence2D _ =
  pure ()
validateDependence2D WavefrontDependence2D [ScheduleWavefrontStage2D] =
  pure ()
validateDependence2D WavefrontDependence2D _ =
  Left (IllegalDependence2D "wavefront dependences require the wavefront schedule in the MVP polyhedral subset")
validateDependence2D (OpaqueDependence2D _) [] =
  pure ()
validateDependence2D (OpaqueDependence2D message) _ =
  Left (IllegalDependence2D ("opaque dependences are only accepted with the identity schedule: " ++ message))

lowerPhase2D :: Sh2 -> PhaseSummary2D -> (Int -> Int -> Prog ()) -> Prog ()
lowerPhase2D shape phaseSummary body =
  case unwrapSchedule2D (phaseSummarySchedule phaseSummary) of
    [] ->
      parForSh2 shape (\ix -> withIx2 ix body)
    [ScheduleWavefrontStage2D] ->
      parForWavefront2D shape (\ix -> withIx2 ix body)
    stages ->
      let (rows, cols) = unSh2 shape
       in parForScheduleN (scheduleStagesToScheduleN stages) (shN [rows, cols]) $ \ix ->
            case unIxN ix of
              [i, j] -> body i j
              _ -> error "scheduleStagesToScheduleN produced a non-2D index"

scheduleStagesToScheduleN :: [ScheduleStage2D] -> ScheduleN
scheduleStagesToScheduleN =
  foldl' (\acc stage -> composeScheduleN acc (stageToScheduleN stage)) identitySchedule
  where
    identitySchedule = identityScheduleN
    stageToScheduleN (ScheduleAffineStage2D affine) = affineScheduleN (affine2ToAffineN affine)
    stageToScheduleN (ScheduleTileStage2D tileRows tileCols) = tileScheduleN [tileRows, tileCols]
    stageToScheduleN ScheduleWavefrontStage2D =
      error "wavefront stages are lowered separately"

affine2ToAffineN :: Affine2 -> AffineN
affine2ToAffineN affine =
  affineN
    [ [rowAt e0 - rowAt origin, rowAt e1 - rowAt origin]
    | rowAt <- [fst . unIx2, snd . unIx2]
    ]
    [fst (unIx2 origin), snd (unIx2 origin)]
  where
    origin = applyAffine2 affine (ix2 0 0)
    e0 = applyAffine2 affine (ix2 1 0)
    e1 = applyAffine2 affine (ix2 0 1)

normalizeScheduleStages :: [ScheduleStage2D] -> [ScheduleStage2D]
normalizeScheduleStages stages =
  reverse (go stages identityAffine2 [])
  where
    go [] !pendingAffine acc =
      flushAffineStage pendingAffine acc
    go (ScheduleAffineStage2D affine : rest) !pendingAffine acc =
      go rest (composeAffine2 affine pendingAffine) acc
    go (stage : rest) !pendingAffine acc =
      go rest identityAffine2 (stage : flushAffineStage pendingAffine acc)

flushAffineStage :: Affine2 -> [ScheduleStage2D] -> [ScheduleStage2D]
flushAffineStage affine acc =
  if isIdentityAffine2 affine
    then acc
    else ScheduleAffineStage2D affine : acc

isIdentityAffine2 :: Affine2 -> Bool
isIdentityAffine2 affine =
  -- Affine2 is abstract outside Kernel, so test a basis of points instead.
  all
    (\point -> unIx2 (applyAffine2 affine point) == unIx2 point)
    [ix2 0 0, ix2 1 0, ix2 0 1]

unwrapSchedule2D :: Schedule2D -> [ScheduleStage2D]
unwrapSchedule2D (Schedule2D stages) = stages

normalizeSchedule2D :: Schedule2D -> Schedule2D
normalizeSchedule2D = Schedule2D . normalizeScheduleStages . unwrapSchedule2D

normalizeTerms :: [(AffineVar, Int)] -> [(AffineVar, Int)]
normalizeTerms =
  foldr mergeTerm [] . sortOn fst
  where
    mergeTerm (_, 0) acc = acc
    mergeTerm (var, coeff) [] = [(var, coeff)]
    mergeTerm (var, coeff) ((var', coeff') : rest)
      | var == var' =
          let !total = coeff + coeff'
           in if total == 0
                then rest
                else (var, total) : rest
      | otherwise =
          (var, coeff) : (var', coeff') : rest
