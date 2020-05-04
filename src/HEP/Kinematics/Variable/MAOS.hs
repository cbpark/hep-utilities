{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module HEP.Kinematics.Variable.MAOS
    (
      SolutionType (..)

    , maosMomenta
    , momentumSolution
    ) where

import HEP.Kinematics
import HEP.Kinematics.Vector.LorentzTVector (setXYT)
import HEP.Kinematics.Vector.LorentzVector  (invariantMassSq, setXYZT)
import HEP.Kinematics.Vector.TwoVector      (setXY)

import Control.Monad.Trans.State.Strict

import Control.Monad                        (join, unless)
import Data.Maybe                           (mapMaybe)

-- import Debug.Trace

data SolutionType = Balanced | Unbalanced | Unknown deriving (Eq, Show)

maosMomenta :: Double                          -- ^ the MT2 value
            -> (FourMomentum, Double, Double)  -- ^ four-momentum of first visible particle and
                                               --   the associated parent and invisible particle masses
            -> (FourMomentum, Double, Double)  -- ^ four-momentum of first visible particle and
                                               --   the associated parent and invisible particle masses
            -> TransverseMomentum              -- ^ missing transverse energy
            -> ([FourMomentum], [FourMomentum], SolutionType)
maosMomenta mT2 (vis1, mY1, mX1) (vis2, mY2, mX2) miss =
    let visM1 = mass vis1
        visM2 = mass vis2
        getScale v = let !ptv = pt v in ptv * ptv
        !s = sqrt $
             (getScale vis1 + getScale vis2 + getScale miss
              + visM1 * visM1 + mX1 * mX1 + visM2 * visM2 + mX2 * mX2) / 8.0

        !vis1' = fmap (/ s) vis1
        !vis2' = fmap (/ s) vis2
        !visM1' = visM1 / s
        !visM2' = visM2 / s
        !miss' = fmap (/ s) miss
        !mT2' = mT2 / s
        !mY1' = mY1 / s
        !mX1' = mX1 / s
        !mY2' = mY2 / s
        !mX2' = mX2 / s

        soltype = if balanced mT2 (vis1', visM1', mX1') (vis2', visM2', mX2')
                  then Balanced
                  else Unbalanced
        input = Input vis1' vis2' visM1' visM2'
                      miss' mY1' mY2' mX1' mX2' mT2' s
        (sol1, sol2) = maosMomenta' soltype input
    in (map (fmap (* s)) sol1, map (fmap (* s)) sol2, soltype)

type Mass = Double

data Input = Input { visible1    :: !FourMomentum
                   , visible2    :: !FourMomentum
                   , mVisible1   :: !Mass
                   , mVisible2   :: !Mass
                   , missing     :: !TransverseMomentum
                   , mParent1    :: !Mass
                   , mParent2    :: !Mass
                   , mInvisible1 :: !Mass
                   , mInvisible2 :: !Mass
                   , mT2value    :: !Mass
                   , scale       :: !Double }

-- | Determines whether it's balanced.
--
-- If the event kinematics is in an unbalanced configuration,
-- the MT2 value is determined by larger unconstrained minimum,
-- which is m_Visible + m_Invisible.
balanced :: Mass
         -> (FourMomentum, Mass, Mass)
         -> (FourMomentum, Mass, Mass)
         -> Bool
balanced mT2 (vis1, mVis1, mX1) (vis2, mVis2, mX2) =
    let mMin1 = mVis1 + mX1
        mMin2 = mVis2 + mX2
        balanced' vis mVis mX = let (a, b, c) = coeffky vis mVis mX mT2
                                in b * b - a * c >= 0
    in if mMin1 < mMin2
       then balanced' vis1 mVis1 mX1
       else balanced' vis2 mVis2 mX2

-- |
-- This gives the coefficients of equations, a ky^2 + 2 b ky + c = 0,
-- which is from the condition that kx has a real value.
--
-- See the 'newkxFrom' function.
coeffky :: FourMomentum -> Mass -> Mass -> Mass -> (Double, Double, Double)
coeffky vis mVis mInv mT2 =
    let visMSq = mVis * mVis
        mInvSq = mInv * mInv
        visEtSq = let eT = transverseEnergy vis in eT `seq` eT * eT
        (px', py') = pxpy vis
        d = mT2 * mT2 - visMSq - mInvSq
        [a, b, c] = (* visEtSq) <$>
                    [ - 4 * visMSq
                    , 2 * d * py'
                    , d * d - 4 * (visEtSq - px' * px') * mInvSq ]
    in a `seq` b `seq` c `seq` (a, b, c)

maosMomenta' :: SolutionType -> Input -> ([FourMomentum], [FourMomentum])
maosMomenta' soltype input@Input {..} =
    case mT2Solution soltype input of
        Nothing         -> ([], [])
        Just (kT1, kT2) ->
            let kSol1 = momentumSolution visible1 kT1 mParent1 mInvisible1
                kSol2 = momentumSolution visible2 kT2 mParent2 mInvisible2
            in if null kSol2 || null kSol1  -- failed to find real solutions!
               then ([], [])
               else (kSol1, kSol2)

data Input' = Input' { userInput :: Input, upperBound :: !Double }

mT2Solution :: SolutionType
            -> Input
            -> Maybe (TransverseMomentum, TransverseMomentum)
mT2Solution soltyp input@Input {..} = do
    (kx0, ky0) <- join mT2Solution'
    let !kT = setXY kx0 ky0
    return (kT, missing - kT)
  where
    mT2Solution' :: Maybe (Maybe (Double, Double))
    mT2Solution' =
        if soltyp == Unbalanced
        then return (mT2UnbalSol input)
        else do let (!kLower, !kUpper) =
                        kLowerUpper visible1 mVisible1 mInvisible1 mT2value
                case startingPoint kLower (input, kUpper) of
                    Nothing                -> Nothing
                    Just (kx1, ky, !deltaM) -> do
                        let (kSol', _, _) =
                                execState (mT2BalSol (Input' input kUpper))
                                (Just (kx1, ky), ky, deltaM)
                        return kSol'

startingPoint :: Double -> (Input, Double) -> Maybe (Double, Double, Mass)
startingPoint kLower inp@(input@Input {..}, kUpper) =
    if kLower > kUpper
        then Nothing
        else case newkxFrom kLower input of
                 Just (kx1a, kx1b) -> do
                     let (kx1, deltaM) = deltaMT kx1a kx1b kLower input
                     return (kx1, kLower, deltaM)
                 Nothing -> do let !kLower' = kLower + scale / 1.0e+7
                               startingPoint kLower' inp

kLowerUpper :: FourMomentum -> Mass -> Mass -> Mass -> (Double, Double)
kLowerUpper vis mVis mInv mT2 =
    let (!a, !b, !c) = coeffky vis mVis mInv mT2
        a' = a + eps
        !term1 = - b / a'
        !term2 = sqrt (b * b - a * c) / a'
        termP = term1 + term2
        termN = term1 - term2
    in if termP > termN then (termN, termP) else (termP, termN)

-- |
-- kx can be obtained analytically for a given ky value
-- using the definition of the transverse mass.
newkxFrom :: Double -> Input -> Maybe (Double, Double)
newkxFrom ky Input {..} = do
    let (px', py') = pxpy visible1
        !visEtSq = let eT = transverseEnergy visible1 in eT * eT
        !mInvSq = mInvisible1 * mInvisible1
        !d = mParent1 * mParent1 - mVisible1 * mVisible1 - mInvSq
        a = 4 * (visEtSq - px' * px')
        b = - 2 * px' * d - 4 * px' * py' * ky
        c = 4 * (visEtSq - py' * py') * ky * ky - 4 * d * py' * ky
            + 4 * visEtSq * mInvSq - d * d
        term2Sq = b * b - a * c
    if term2Sq < 0
        then Nothing
        else do let a' = a + eps
                    !term1 = - b / a'
                    !term2 = sqrt term2Sq / a'
                return (term1 + term2, term1 - term2)

deltaMT :: Double -> Double -> Double -> Input -> (Double, Mass)
deltaMT kx1a kx1b ky1 Input {..} =
    let !mX1Sq = mInvisible1 * mInvisible1
        !mX2Sq = mInvisible2 * mInvisible2
        !ky1Sq = ky1 * ky1
        !inv1a = setXYT kx1a ky1 (sqrt $ kx1a * kx1a + ky1Sq + mX1Sq)
        !inv1b = setXYT kx1b ky1 (sqrt $ kx1b * kx1b + ky1Sq + mX2Sq)
        !(missX, missY) = pxpy missing
        !kx2a = missX - kx1a
        !kx2b = missX - kx1b
        !ky2  = missY - ky1
        !ky2Sq = ky2 * ky2
        !inv2a = setXYT kx2a ky2 (sqrt $ kx2a * kx2a + ky2Sq + mX1Sq)
        !inv2b = setXYT kx2b ky2 (sqrt $ kx2b * kx2b + ky2Sq + mX2Sq)
        [mTrans1a, mTrans1b] = transverseMass1 visible1 <$> [inv1a, inv1b]
        [mTrans2a, mTrans2b] = transverseMass1 visible2 <$> [inv2a, inv2b]
        deltaMTa = abs (mTrans1a - mTrans2a)
        deltaMTb = abs (mTrans1b - mTrans2b)
    in if deltaMTa < deltaMTb then (kx1a, deltaMTa) else (kx1b, deltaMTb)

mT2BalSol :: Input' -> State (Maybe (Double, Double), Double, Mass) ()
mT2BalSol input@Input' {..} = do
    (k0, ky, deltaM) <- get
    unless (ky > upperBound) $ do
        let !ky' = ky + scale userInput / 1.0e+5
        case newkxFrom ky' userInput of
            Nothing           -> put (k0, ky', deltaM)
            Just (kx1a, kx1b) -> do
                let (kx1', !deltaM') = deltaMT kx1a kx1b ky' userInput
                if deltaM' < deltaM
                    then put (Just (kx1', ky'), ky', deltaM')
                    else put (              k0, ky', deltaM )
        mT2BalSol input

-- |
-- See Eq. (14) in <http://arxiv.org/abs/0711.4526 arXiv:0711.4526>.
mT2UnbalSol :: Input -> Maybe (Double, Double)
mT2UnbalSol Input {..}
    | mVisible1 < eps = Nothing
    | otherwise       = do let r = mInvisible1 / mVisible1
                               (px1, py1) = pxpy visible1
                           return (r * px1, r * py1)

-- |
-- calculates the possible momentum solutions for the decay topology of
-- parent --> visible invisible, where the longitudinal momentum of invisible
-- particle is unknown.
--
-- See Eq. (3) in <http://arxiv.org/abs/0810.4853 arXiv:0810.4853>.
momentumSolution :: FourMomentum        -- ^ four-momentum of visible particle
                 -> TransverseMomentum  -- ^ transverse momentum of invisible particle
                 -> Double              -- ^ mass of the parent particle
                 -> Double              -- ^ mass of the invisible particle
                 -> [FourMomentum]
momentumSolution vis invis mY mX = let (kx, ky) = pxpy invis
                                       kz = longitudinalP vis invis mY mX
                                   in mapMaybe (setMomentum kx ky) kz
  where
    setMomentum _ _ Nothing  = Nothing
    setMomentum x y (Just z) = let t = sqrt (x * x + y * y + z * z + mX * mX)
                               in t `seq` Just (setXYZT x y z t)

longitudinalP :: FourMomentum -> TransverseMomentum -> Double -> Double
              -> [Maybe Double]
longitudinalP vis invis mY mX =
    let visMassSq = invariantMassSq vis
        visTrans = transverseVector vis
        mXSq = mX * mX
        !d = 0.5 * (mY * mY - mXSq - visMassSq) + visTrans `dot` invis
        !visEt = transverseEnergy vis
        ptInv = pt invis
        !invisEt = sqrt $ ptInv * ptInv + mXSq
        disc = d * d - visEt * visEt * invisEt * invisEt
        disc' = if abs disc < 1.0e-4 then 0 else disc
    in if disc' < 0
       then [Nothing]
       else let !term1 = pz vis * d
                !term2 = energy vis * sqrt disc'
            in Just . (/ (visEt * visEt)) <$> [term1 + term2, term1 - term2]

eps :: Double
eps = 1.0e-12
