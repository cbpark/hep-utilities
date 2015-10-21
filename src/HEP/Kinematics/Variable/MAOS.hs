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
import Control.Monad.IO.Class
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
            -> IO ([FourMomentum], [FourMomentum], SolutionType)
maosMomenta mT2 (vis1, mY1, mX1) (vis2, mY2, mX2) miss = do
  let visM1 = mass vis1
      visM2 = mass vis2
      getScale v = pt v ** 2
      scale = sqrt $ (getScale vis1 + getScale vis2 + getScale miss
                     + visM1 ** 2 + mX1 ** 2 + visM2 ** 2 + mX2 ** 2) / 8.0
  -- putStrLn $ "scale = " ++ show scale
  case scale of
       0 -> return ([], [], Unknown)
       s -> do let soltype = if balanced mT2 (vis1, mX1) (vis2, mX2)
                             then Balanced
                             else Unbalanced
               --     mMin1 = visM1 + mX1
               --     mMin2 = visM2 + mX2
               -- if mMin1 < mMin2
               -- then do let input = Input vis1 vis2 miss mY1 mY2 mX1 mX2 mT2 s
               --         (sol1, sol2) <- runReaderT maosMomenta' input
               --         putStrLn "here1"
               --         return  (sol1, sol2, soltype)
               -- else do let input = Input vis2 vis1 miss mY2 mY1 mX2 mX1 mT2 s
               --         (sol1, sol2) <- runReaderT maosMomenta' input
               --         putStrLn "here2"
               --         return (sol2, sol1, soltype)
               let input = Input vis2 vis1 miss mY2 mY1 mX2 mX1 mT2 s
               (sol1, sol2) <- runReaderT maosMomenta' input
               return  (sol2, sol1, soltype)

data Input = Input { visible1    :: FourMomentum
                   , visible2    :: FourMomentum
                   , missing     :: TransverseMomentum
                   , mParent1    :: Mass
                   , mParent2    :: Mass
                   , mInvisible1 :: Mass
                   , mInvisible2 :: Mass
                   , mT2value    :: Mass
                   , scale       :: Double }

balanced :: Mass -> (FourMomentum, Mass) -> (FourMomentum, Mass) -> Bool
balanced mT2 (vis1, mX1) (vis2, mX2) =
  let mMin1 = mass vis1 + mX1
      mMin2 = mass vis2 + mX2
      balanced' vis mX = let (a, b, c) = coeffky vis mT2 mX
                         in b ** 2 - a * c >= 0
  in if mMin1 < mMin2 then balanced' vis1 mX1 else balanced' vis2 mX2

type Mass = Double

coeffky :: FourMomentum -> Mass -> Mass -> (Double, Double, Double)
coeffky vis mT2 mInv =
  let -- visMSq = mass vis ** 2
      mInvSq = mInv ** 2
      visEtSq = transverseEnergy vis ** 2
      (px', py') = pxpy vis
      -- d = mT2 ** 2 - visMSq - mInvSq
      -- a = - 4 * visMSq * visEtSq
      a = - 4 * visEtSq ** 2 + 4 * visEtSq * px' ** 2 + 4 * visEtSq * py' ** 2
      -- b = 2 * d * py' * visEtSq
      b = - 2 * visEtSq ** 2 * py' + 2 * visEtSq * mT2 ** 2 * py'
          - 2 * visEtSq * mInvSq * py' + 2 * visEtSq * px' ** 2 * py'
          + 2 * visEtSq * py' ** 3
      -- c = (d ** 2 - 4 * (visEtSq - px' ** 2) * mInvSq) * visEtSq
      c = (visEtSq ** 2)*visEtSq- 2 * (visEtSq ** 2)*(mT2 ** 2)- 2 * (visEtSq ** 2)*(mInv ** 2)- 2 * (visEtSq ** 2)*(px' ** 2)- 2 * (visEtSq ** 2)*(py' ** 2)+visEtSq*(mT2 ** 4)- 2 * visEtSq*(mT2 ** 2)*(mInv ** 2)+ 2 * visEtSq*(mT2 ** 2)*(px' ** 2)+ 2 * visEtSq*(mT2 ** 2)*(py' ** 2)+visEtSq*(mInv ** 4)+ 2 * visEtSq*(mInv ** 2)*(px' ** 2)- 2 * visEtSq*(mInv ** 2)*(py' ** 2)+visEtSq*(px' ** 4)+ 2 * visEtSq*(px' ** 2)*(py' ** 2)+visEtSq*(py' ** 4)
  in (a, b, c)

maosMomenta' :: MonadIO m => ReaderT Input m ([FourMomentum], [FourMomentum])
maosMomenta' = do
  input@Input {..} <- ask
  kT <- runReaderT mT2Solution input
  case kT of
    Nothing         -> return ([], [])
    Just (kT1, kT2) -> do
      let kSol1 = momentumSolution visible1 kT1 mParent1 mInvisible1
          kSol2 = momentumSolution visible2 kT2 mParent2 mInvisible2
      return (kSol1, kSol2)

data Input' = Input' { visible    :: FourMomentum
                     , mParent    :: Mass
                     , mInvisible :: Mass
                     , upperBound :: Double
                     , inc        :: Double }

mT2Solution :: MonadIO m => ReaderT Input m (Maybe (TransverseMomentum, TransverseMomentum))
mT2Solution = do
  Input {..} <- ask
  let input' = Input' visible2 mT2value mInvisible2 0 0
  (kLower, kUpper) <- kLowerUpper visible2 mT2value mInvisible2
  liftIO $ putStrLn $ "kLower = " ++ show kLower ++ ", kUpper = " ++ show kUpper
  kx <- runReaderT (newkxFrom kLower) input'
  case kx of
    Nothing         -> return Nothing
    Just (kx1, kx2) -> do
      let deltaM = runReader (deltaMT kx1 kx2 kLower) input'
          input'' = input' { upperBound = kUpper
                           , inc = (kUpper ** 2 - kLower ** 2) / scale }
      (kSol, _, _) <- runReaderT (execStateT mT2Solution'
                                  (Just (setXY kx1 kLower) , kLower, deltaM))
                      input''
      liftIO $ putStrLn $ "deltaM = " ++ show deltaM
      return $ case kSol of Nothing -> Nothing
                            Just kT -> Just (missing - kT, kT)

kLowerUpper :: MonadIO m => FourMomentum -> Mass -> Mass -> m (Double, Double)
kLowerUpper vis mT2 mInv = do let (a, b, c) = coeffky vis mT2 mInv
                                  term1 = - b / a
                                  term2 = sqrt (b ** 2 - a * c) / a
                                  termP = term1 + term2
                                  termN = term1 - term2
                              liftIO $ putStrLn $ "visEt = " ++ show (transverseEnergy vis)
                              liftIO $ putStrLn $ "a = " ++ show a ++ ", b = " ++ show b ++ ", c = " ++ show c
                              return $ case termP `compare` termN of GT -> (termN, termP)
                                                                     _  -> (termP, termN)

newkxFrom :: MonadIO m => Double -> ReaderT Input' m (Maybe (Double, Double))
newkxFrom ky = do
  Input' {..} <- ask
  let visM = mass visible
      (px', py') = pxpy visible
      visEtSq = transverseEnergy visible ** 2
      d = mParent ** 2 - visM ** 2 - mInvisible ** 2
      a = 4 * (visEtSq - px' ** 2)
      b = - 2 * px' * d - 4 * px' * py' * ky
      c = 4 * (visEtSq - py' ** 2) * ky ** 2 - 4 * d * py' * ky
          + 4 * visEtSq * mInvisible ** 2 - d ** 2
      term2Sq = b ** 2 - a * c
  liftIO $ putStrLn $ "a = " ++ show a ++ ", b = " ++ show b ++ ", c = " ++ show c
  liftIO $ putStrLn $ "term2Sq = " ++ show term2Sq
  liftIO $ putStrLn $ "b **2 - 4*a*c = " ++ show (4 * b ** 2 - 4 * a * c)
  if term2Sq < 0
  then return Nothing
  else do let term1 = - b / a
              term2 = sqrt term2Sq / a
          return $ Just (term1 + term2, term1 - term2)

deltaMT :: Double -> Double -> Double -> Reader Input' Double
deltaMT kx1 kx2 ky = do
  Input' {..} <- ask
  let inv1 = setXYT kx1 ky (sqrt (kx1 ** 2 + ky ** 2 + mInvisible ** 2))
      inv2 = setXYT kx2 ky (sqrt (kx2 ** 2 + ky ** 2 + mInvisible ** 2))
      mTrans1 = transverseMass1 visible inv1
      mTrans2 = transverseMass1 visible inv2
  return $ abs (mTrans1 - mTrans2)

mT2Solution' :: MonadIO m => StateT (Maybe TransverseMomentum, Double, Mass) (ReaderT Input' m) ()
mT2Solution' = do
  input@Input' {..} <- lift ask
  (betterK, ky, deltaM) <- get
  unless (ky > upperBound) $ do
    let ky' = ky + inc
    kx <- runReaderT (newkxFrom ky') input
    case kx of
      Nothing -> do put (Nothing, ky', deltaM)
                    return ()
      Just (kx1, kx2) -> do
        let deltaM' = runReader (deltaMT kx1 kx2 ky') input
        if deltaM' < deltaM
        then put (Just (setXY kx1 ky'), ky', deltaM')
        else put (             betterK, ky', deltaM )
        mT2Solution'

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
        setMomentum x y (Just z) = let t = sqrt $! x ** 2 + y ** 2 + z ** 2 + mX ** 2
                                   in Just (setXYZT x y z t)

longitudinalP :: FourMomentum -> TransverseMomentum -> Double -> Double
              -> [Maybe Double]
longitudinalP vis invis mY mX =
  let visMass = mass vis
      visTrans = transverseVector vis
      disc' = 0.5 * (mY ** 2 - mX ** 2 - visMass ** 2) + visTrans `dot` invis
      visEt = transverseEnergy vis
      invisEt = sqrt $! pt invis ** 2 + mX ** 2
      disc = disc' ** 2 - (visEt * invisEt) ** 2
  in if disc < 0
     then [Nothing]
     else let term1 = pz vis * disc'
              term2 = energy vis * sqrt disc
          in map (Just . (/ (visEt ** 2))) [term1 + term2, term1 - term2]
