{-# LANGUAGE RecordWildCards #-}

module HEP.Kinematics.Variable.MTLowerAndUpper where

import           Control.Monad                       (replicateM)
import           Control.Monad.IO.Class
import           Control.Monad.ST                    (runST)
import           Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Maybe
import           System.Random.MWC

import           HEP.Kinematics
import           HEP.Kinematics.Vector.LorentzVector (setXYZT)
import           HEP.Kinematics.Vector.TwoVector     (setXY)

type Mass = Double
type StepSize = Double
type Splitting = TransverseMomentum
type Result = (Mass, Splitting)

mTBound :: Mass
mTBound = undefined

data Input = Input { visible1      :: FourMomentum
                   , visible2      :: FourMomentum
                   , missing       :: TransverseMomentum
                   , mIntermediate :: Mass }

-- mTLowerBound' :: FourMomentum -> FourMomentum -> TransverseMomentum -> Mass
--               -> StateT Result IO ()
-- mTLowerBound' vis1 vis2 miss m = do
--   splitting <- liftIO $ startPoint vis1 vis2 miss m
--   return undefined

-- mTLowerBound'' :: Result -> StepSize
--                -> FourMomentum -> FourMomentum -> TransverseMomentum -> Mass
--                -> StateT StepSize IO Result
-- mTLowerBound'' result size vis1 vis2 miss m =
--   if size < 1.0e-8 / typicalScale
--   then return result
--   else do let (boundM, splitting) = result
--               splitting' = deltaK r1 r2 distFromWall

startPoint :: ReaderT Input IO Result
startPoint = do
  input <- ask
  s <- liftIO $ createSystemRandom >>= save
  let (split0, s0) = getSplit distFromWall s
  return $ runReader (findPoint s0 split0 updateFunc) input
    where updateFunc :: Splitting -> Seed -> (Splitting, Seed)
          updateFunc _ = getSplit distFromWall

findPoint :: Seed -> Splitting -> (Splitting -> Seed -> (Splitting, Seed))
          -> Reader Input Result
findPoint s splitting splitFn = do
  input <- ask
  case runReader (recoMass splitting) input of
    Nothing -> do let (splitting', s') = splitFn splitting s
                  findPoint s' splitting' splitFn
    Just sp -> return sp

getSplit :: StepSize -> Seed -> (Splitting, Seed)
getSplit dist s =
  let (r1, r2, s') = proceedRd s
      theta = (r1 - 0.5) * pi
      distToStep = dist * tan theta
      angToStep = 2.0 * r2 * pi
      deltaK = setXY (distToStep * cos angToStep) (distToStep * sin angToStep)
  in (deltaK, s')

proceedRd :: Seed -> (Double, Double, Seed)
proceedRd s = runST $ do gen <- restore s
                         rs <- replicateM 2 (uniform gen)
                         -- to generate variates within [0, 1).
                         let rs' = fmap (\x -> x - 2 ** (-53)) rs
                         s' <- save gen
                         return (head rs', (head . tail) rs', s')

distFromWall :: StepSize
distFromWall = 2.0 / typicalScale

typicalScale :: Double
typicalScale = 26.0

recoMass :: Splitting -> Reader Input (Maybe Result)
recoMass splitting = do
  Input {..} <- ask
  let invis1 = fmap (*0.5) (missing + splitting)
      invis2 = fmap (*0.5) (missing - splitting)
      kneu1 = kNeutrino visible1 invis1 mIntermediate
      kneu2 = kNeutrino visible2 invis2 mIntermediate
  return $ if null kneu1 || null kneu2
           then Nothing
           else Just ( minimum [invariantMass [visible1, visible2, k1, k2] |
                                k1 <- kneu1, k2 <- kneu2 ]
                     , splitting)

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
      disc' = 0.5 * (m ** 2 - visMass ** 2) + invis `dot` visTrans
      visEt = transverseEnergy vis
      invisEt = norm invis
      disc = disc' ** 2 - (visEt * invisEt) ** 2
  in if disc < 0
     then [Nothing]
     else let term1 = pz vis * disc'
              term2 = energy vis * sqrt disc
          in map (Just . (/ (visEt * visEt))) [term1 + term2, term1 - term2]
