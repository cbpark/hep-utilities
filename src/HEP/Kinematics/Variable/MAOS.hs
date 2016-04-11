{-# LANGUAGE RecordWildCards #-}

module HEP.Kinematics.Variable.MAOS
  (
    SolutionType (..)
  , maosMomenta
  , momentumSolution
  ) where

import           Control.Monad                        (unless)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Maybe                           (mapMaybe)

import           HEP.Kinematics
import           HEP.Kinematics.Vector.LorentzTVector (setXYT)
import           HEP.Kinematics.Vector.LorentzVector  (setXYZT)
import           HEP.Kinematics.Vector.TwoVector      (setXY)

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
        getScale v = pt v ** 2
        scale = sqrt $ (getScale vis1 + getScale vis2 + getScale miss
                        + visM1 ** 2 + mX1 ** 2 + visM2 ** 2 + mX2 ** 2) / 8.0
    in case scale of
        0 -> ([], [], Unknown)
        s -> let vis1' = fmap (/ s) vis1
                 vis2' = fmap (/ s) vis2
                 miss' = fmap (/ s) miss
                 mT2' = mT2 / s
                 mY1' = mY1 / s
                 mX1' = mX1 / s
                 mY2' = mY2 / s
                 mX2' = mX2 / s
                 soltype = if balanced mT2 (vis1', mX1') (vis2', mX2')
                           then Balanced else Unbalanced
                 input = Input vis1' vis2' miss' mY1' mY2' mX1' mX2' mT2' s
                 (sol1, sol2) = runReader (maosMomenta' soltype) input
             in (map (fmap (* s)) sol1, map (fmap (* s)) sol2, soltype)

type Mass = Double

data Input = Input { visible1    :: FourMomentum
                   , visible2    :: FourMomentum
                   , missing     :: TransverseMomentum
                   , mParent1    :: Mass
                   , mParent2    :: Mass
                   , mInvisible1 :: Mass
                   , mInvisible2 :: Mass
                   , mT2value    :: Mass
                   , scale       :: Double }

-- | Determines whether it's balanced.
--
--   If the event kinematics is in an unbalanced configuration,
--   the MT2 value is determined by larger unconstrained minimum,
--   which is m_Visible + m_Invisible.
balanced :: Mass -> (FourMomentum, Mass) -> (FourMomentum, Mass) -> Bool
balanced mT2 (vis1, mX1) (vis2, mX2) =
    let mMin1 = mass vis1 + mX1
        mMin2 = mass vis2 + mX2
        balanced' vis mX = let (a, b, c) = coeffky vis mT2 mX
                           in b ** 2 - a * c >= 0
    in if mMin1 < mMin2 then balanced' vis1 mX1 else balanced' vis2 mX2

-- |
-- This gives the coefficients of equations, a ky^2 + 2 b ky + c = 0,
-- which is from the condition that kx has a real value.
--
-- See the 'newkxFrom' function.
coeffky :: FourMomentum -> Mass -> Mass -> (Double, Double, Double)
coeffky vis mT2 mInv =
    let visMSq = mass vis ** 2
        mInvSq = mInv ** 2
        visEtSq = transverseEnergy vis ** 2
        (px', py') = pxpy vis
        d = mT2 ** 2 - visMSq - mInvSq
        a = - 4 * visMSq * visEtSq
        b = 2 * d * py' * visEtSq
        c = (d ** 2 - 4 * (visEtSq - px' ** 2) * mInvSq) * visEtSq
    in (a, b, c)

maosMomenta' :: SolutionType -> Reader Input ([FourMomentum], [FourMomentum])
maosMomenta' soltype = do
    input@Input {..} <- ask
    case runReader (mT2Solution soltype) input of
        Nothing         -> return ([], [])
        Just (kT1, kT2) -> do
            let kSol1 = momentumSolution visible1 kT1 mParent1 mInvisible1
                kSol2 = momentumSolution visible2 kT2 mParent2 mInvisible2
            return (kSol1, kSol2)

data Input' = Input' { userInput :: Input, upperBound :: Double }

mT2Solution :: SolutionType
            -> Reader Input (Maybe (TransverseMomentum, TransverseMomentum))
mT2Solution soltype = do
    input@Input {..} <- ask
    kSol <- if soltype == Unbalanced
            then return (runReader mT2UnbalSol input)
            else do let (kLower, kUpper) = kLowerUpper visible1 mT2value mInvisible1
                    case runReader (startingPoint kLower) (input, kUpper) of
                        Nothing -> return Nothing
                        Just (kx1, ky, deltaM) -> do
                            let (kSol, _, _) =
                                    runReader (execStateT mT2BalSol
                                              (Just (kx1, ky), ky, deltaM))
                                              (Input' input kUpper)
                            return kSol
    case kSol of Nothing         -> return Nothing
                 Just (kx0, ky0) -> do let kT = setXY kx0 ky0
                                       return $ Just (kT, missing - kT)

startingPoint :: Double -> Reader (Input, Double) (Maybe (Double, Double, Mass))
startingPoint kLower = do
    (input@Input {..}, kUpper) <- ask
    if kLower > kUpper
        then return Nothing
        else case runReader (newkxFrom kLower) input of
            Just (kx1a, kx1b) -> do
                let (kx1, deltaM) = runReader (deltaMT kx1a kx1b kLower) input
                return $ Just (kx1, kLower, deltaM)
            Nothing -> startingPoint $! kLower + scale / 1.0e+7

kLowerUpper :: FourMomentum -> Mass -> Mass -> (Double, Double)
kLowerUpper vis mT2 mInv =
    let (a, b, c) = coeffky vis mT2 mInv
        term1 = - b / a
        term2 = sqrt (b ** 2 - a * c) / a
        termP = term1 + term2
        termN = term1 - term2
    in if termP > termN then (termN, termP) else (termP, termN)

-- |
-- kx can be obtained analytically for a given ky value
-- using the definition of the transverse mass.
newkxFrom :: Double -> Reader Input (Maybe (Double, Double))
newkxFrom ky = do
    Input {..} <- ask
    let visM = mass visible1
        (px', py') = pxpy visible1
        visEtSq = transverseEnergy visible1 ** 2
        mInvSq = mInvisible1 ** 2
        d = mParent1 ** 2 - visM ** 2 - mInvSq
        a = 4 * (visEtSq - px' ** 2)
        b = - 2 * px' * d - 4 * px' * py' * ky
        c = 4 * (visEtSq - py' ** 2) * ky ** 2 - 4 * d * py' * ky
            + 4 * visEtSq * mInvSq - d ** 2
        term2Sq = b ** 2 - a * c
    if term2Sq < 0
        then return Nothing
        else do let term1 = - b / a
                    term2 = sqrt term2Sq / a
                return $ Just (term1 + term2, term1 - term2)

deltaMT :: Double -> Double -> Double -> Reader Input (Double, Mass)
deltaMT kx1a kx1b ky1 = do
    Input {..} <- ask
    let mX1Sq = mInvisible1 ** 2
        mX2Sq = mInvisible2 ** 2
        ky1Sq = ky1 ** 2
        inv1a = setXYT kx1a ky1 (sqrt $! kx1a ** 2 + ky1Sq + mX1Sq)
        inv1b = setXYT kx1b ky1 (sqrt $! kx1b ** 2 + ky1Sq + mX2Sq)
        (missX, missY) = pxpy missing
        kx2a = missX - kx1a
        kx2b = missX - kx1b
        ky2  = missY - ky1
        ky2Sq = ky2 ** 2
        inv2a = setXYT kx2a ky2 (sqrt $! kx2a ** 2 + ky2Sq + mX1Sq)
        inv2b = setXYT kx2b ky2 (sqrt $! kx2b ** 2 + ky2Sq + mX2Sq)
        mTrans1a = transverseMass1 visible1 inv1a
        mTrans1b = transverseMass1 visible1 inv1b
        mTrans2a = transverseMass1 visible2 inv2a
        mTrans2b = transverseMass1 visible2 inv2b
        deltaMTa = abs (mTrans1a - mTrans2a)
        deltaMTb = abs (mTrans1b - mTrans2b)
    return $ if deltaMTa < deltaMTb then (kx1a, deltaMTa) else (kx1b, deltaMTb)

mT2BalSol :: StateT (Maybe (Double, Double), Double, Mass) (Reader Input') ()
mT2BalSol = do
    Input' {..} <- lift ask
    (k0, ky, deltaM) <- get
    unless (ky > upperBound) $ do
        let ky' = ky + scale userInput / 1.0e+5
        case runReader (newkxFrom ky') userInput of
            Nothing -> put (k0, ky', deltaM)
            Just (kx1a, kx1b) -> do
                let (kx1', deltaM') = runReader (deltaMT kx1a kx1b ky') userInput
                if deltaM' < deltaM
                    then put (Just (kx1', ky'), ky', deltaM')
                    else put (              k0, ky', deltaM )
        mT2BalSol

-- |
-- See Eq. (14) in <http://arxiv.org/abs/0711.4526 arXiv:0711.4526>.
mT2UnbalSol :: Reader Input (Maybe (Double, Double))
mT2UnbalSol = do Input {..} <- ask
                 case mass visible1 of
                     0     -> return Nothing
                     visM1 -> do let r = mInvisible1 / visM1
                                     (px1, py1) = pxpy visible1
                                 return $ Just (r * px1, r * py1)

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
  where setMomentum _ _ Nothing  = Nothing
        setMomentum x y (Just z) =
          let t = sqrt $! x ** 2 + y ** 2 + z ** 2 + mX ** 2
          in Just (setXYZT x y z t)

longitudinalP :: FourMomentum -> TransverseMomentum -> Double -> Double
              -> [Maybe Double]
longitudinalP vis invis mY mX =
  let visMass = mass vis
      visTrans = transverseVector vis
      mXSq = mX ** 2
      d = 0.5 * (mY ** 2 - mXSq - visMass ** 2) + visTrans `dot` invis
      visEt = transverseEnergy vis
      invisEt = sqrt $! pt invis ** 2 + mXSq
      disc = d ** 2 - (visEt * invisEt) ** 2
      disc' = if abs disc < 1.0e-4 then 0 else disc
  in if disc' < 0
     then [Nothing]
     else let term1 = pz vis * d
              term2 = energy vis * sqrt disc'
          in map (Just . (/ (visEt ** 2))) [term1 + term2, term1 - term2]
