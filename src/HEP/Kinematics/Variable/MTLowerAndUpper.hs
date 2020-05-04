{-# LANGUAGE RecordWildCards #-}

module HEP.Kinematics.Variable.MTLowerAndUpper (mTLowerBound) where

import HEP.Kinematics
import HEP.Kinematics.Vector.LorentzVector (setXYZT)
import HEP.Kinematics.Vector.TwoVector     (setXY)

import Control.Monad                       (replicateM, when)
import Data.Maybe                          (mapMaybe)

import Control.Monad.IO.Class              (MonadIO (..))
import Control.Monad.ST                    (runST)
import Control.Monad.Trans.Class           (MonadTrans (..))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import System.Random.MWC

type Mass = Double
type Splitting = TransverseMomentum
data Result = Result { recomass :: Mass, splitting :: TransverseMomentum }

type StepSize = Double

data Input = Input { visible1      :: !FourMomentum
                   , visible2      :: !FourMomentum
                   , missing       :: !TransverseMomentum
                   , mIntermediate :: !Mass }

mTLowerBound :: FourMomentum        -- ^ four-momentum of the first visible system
             -> FourMomentum        -- ^ four-momentum of the second visible system
             -> TransverseMomentum  -- ^ missing transverse momentum
             -> Double              -- ^ mass of the intermediate state
             -> IO Double
mTLowerBound vis1 vis2 miss m = do
    let scale' = sqrt $ (getScale vis1 + getScale vis2 + getScale miss) / 8.0
        scale = if scale' == 0 then 2.0 * m else scale'
        vis1' = fmap (/ scale) vis1
        vis2' = fmap (/ scale) vis2
        miss' = fmap (/ scale) miss
        m' = m / scale
    Result {..} <- runReaderT mTLowerBound' (Input vis1' vis2' miss' m')
    return (recomass * scale)
  where
    getScale v = let (px', py', pz') = pxpypz v
                 in px' * px' + py' * py' + pz' * pz'

mTLowerBound' :: MonadIO m => ReaderT Input m Result
mTLowerBound' = do
    input <- ask
    (p, s) <- runReaderT startingPoint input
    (result, _) <- runReaderT (execStateT (findMinimum distFromWall) (p, s)) input
    return result

distFromWall, typicalScale :: StepSize
distFromWall = 2.0 / typicalScale
typicalScale = 26.0

findMinimum :: MonadIO m => StepSize -> StateT (Result, Seed) (ReaderT Input m) ()
findMinimum dist =
    when (dist > tolerance) $ do
        (r0@(Result recomass0 splitting0), s0) <- get
        let (delta, s) = runState (deltaK dist) s0
            split = splitting0 + delta
        input <- lift ask
        case runReader (recoMass split) input of
            Nothing            -> do put (r0, s)
                                     findMinimum (dist * shrinkageFactor)
            Just r@Result {..} -> if recomass < recomass0
                                  then do put (r, s)
                                          findMinimum (dist * growthFactor)
                                  else do put (r0, s)
                                          findMinimum (dist * shrinkageFactor)
          where
            tolerance = 1.0e-8 / typicalScale
            growthFactor = 1.1
            shrinkageFactor = 0.99

startingPoint :: MonadIO m => ReaderT Input m (Result, Seed)
startingPoint = do
    s <- liftIO $ createSystemRandom >>= save
    let (split0, s0) = runState (deltaK distFromWall) s
    fmap (runReader (physicalPoint s0 split0)) ask
  where
    physicalPoint :: Seed -> Splitting -> Reader Input (Result, Seed)
    physicalPoint s split = do
        input <- ask
        case runReader (recoMass split) input of
            Nothing -> do let (split', s') = runState (deltaK distFromWall) s
                          physicalPoint s' split'
            Just r  -> return (r, s)

deltaK :: StepSize -> State Seed Splitting
deltaK dist = do
    s <- get
    let (r1, r2, s') = proceedRd s
        theta = (r1 - 0.5) * pi
        step = dist * tan theta
        angle = 2.0 * pi * r2
        delta = setXY (step * cos angle) (step * sin angle)
    put s'
    return delta
  where
    proceedRd :: Seed -> (Double, Double, Seed)
    proceedRd s = runST $ do gen <- restore s
                             rs <- replicateM 2 (uniform gen)
                             -- to generate variates within [0, 1).
                             let (r1':(r2':_)) = map (\x -> x - 2 ** (-53)) rs
                             s' <- save gen
                             return (r1', r2', s')

recoMass :: Splitting -> Reader Input (Maybe Result)
recoMass split = do
    Input {..} <- ask
    let invis1 = fmap (/ 2.0) (missing + split)
        invis2 = fmap (/ 2.0) (missing - split)
        kneu1 = kNeutrino visible1 invis1 mIntermediate
        kneu2 = kNeutrino visible2 invis2 mIntermediate
    return $ if null kneu1 || null kneu2
             then Nothing
             else Just $ Result (minimum
                                 [invariantMass [visible1, visible2, k1, k2 ] |
                                  k1 <- kneu1 , k2 <- kneu2 ]) split

kNeutrino :: FourMomentum -> TransverseMomentum -> Mass -> [FourMomentum]
kNeutrino vis invis m = let (kx, ky) = pxpy invis
                            kz = kNeutrinoL vis invis m
                        in mapMaybe (setMomentum kx ky) kz
  where setMomentum _ _ Nothing  = Nothing
        setMomentum x y (Just z) = let t = sqrt $ x * x + y * y + z * z
                                   in Just (setXYZT x y z t)

kNeutrinoL :: FourMomentum -> TransverseMomentum -> Mass -> [Maybe Double]
kNeutrinoL vis invis m =
    let visMass = mass vis
        visTrans = transverseVector vis
        disc' = 0.5 * (m * m - visMass * visMass) + invis `dot` visTrans
        visEt = transverseEnergy vis
        invisEt = norm invis
        disc = disc' * disc' - visEt * visEt * invisEt * invisEt
    in if disc < 0
       then [Nothing]
       else let term1 = pz vis * disc'
                term2 = energy vis * sqrt disc
            in map (Just . (/ (visEt * visEt))) [term1 + term2, term1 - term2]
