{-# LANGUAGE RecordWildCards #-}

module HEP.Kinematics.Variable.MT2 (mT2) where

import HEP.Kinematics

import Control.Lens               ((^.))
import Control.Monad.Trans.Class  (MonadTrans (..))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Linear.Matrix              (M33, det33)
import Linear.V3

type Mass = Double

data Input = Input { visible1        :: FourMomentum
                   , visible2        :: FourMomentum
                   , missing         :: TransverseMomentum
                   , mInvisible1     :: Mass
                   , mInvisible2     :: Mass
                   , precision       :: Double
                   , useDeciSections :: Bool }

-- | calculates MT2.
--
-- This uses the algorithm in <http://arxiv.org/abs/1411.4312 arXiv:1411.4312>
-- by C.G. Lester and B. Nachman.
mT2 :: FourMomentum -> FourMomentum -> TransverseMomentum -> Mass -> Mass
    -> Double -> Bool -> Mass
mT2 vis1 vis2 miss mInv1 mInv2 pre sec =
    let mVis1 = mass vis1
        mVis2 = mass vis2
        getScale v = pt v ** 2
        scale = sqrt $ (getScale vis1 + getScale vis2 + getScale miss
                       + mVis1 ** 2 + mInv1 ** 2 + mVis2 ** 2 + mInv2 ** 2) / 8.0
    in case scale of 0 -> 0
                     s -> let m1Min = mVis1 + mInv1
                              m2Min = mVis2 + mInv2
                          in if m2Min > m1Min
                             then mT2' m2Min s
                                  (Input vis1 vis2 miss mInv1 mInv2 pre sec)
                             else mT2' m1Min s
                                  (Input vis2 vis1 miss mInv2 mInv1 pre sec)

mT2' :: Mass -> Double -> Input -> Mass
mT2' mMin scale input@Input {..} =
    let mLower = mMin
        mUpper = runReader (growUpper (mMin + scale)) input
    in case mUpper of
        Nothing -> -1
        Just m  -> runReader (evalStateT (bisect useDeciSections) (mLower, m))
                             input

growUpper :: Mass -> Reader Input (Maybe Mass)
growUpper mUpper = do
    Input {..} <- ask
    let side1 = mkEllipse mUpper mInvisible1 (-visible1) zero
        side2 = mkEllipse mUpper mInvisible2 visible2 missing
    case ellipsesAreDisjoint side1 side2 of
        Nothing    -> return Nothing
        Just False -> return (Just mUpper)
        Just _     -> growUpper $ mUpper * 2

bisect :: Bool -> StateT (Mass, Mass) (Reader Input) Mass
bisect sec = do
    (mLower, mUpper) <- get
    Input {..} <- lift ask
    if mUpper - mLower <= precision
        then return $ (mLower + mUpper) / 2.0
        else do let trialM = if sec
                             then (mLower * 15 + mUpper) / 16
                             else (mLower + mUpper) / 2
                if trialM <= mLower || trialM >= mUpper
                    then return trialM
                    else do
                        let side1 = mkEllipse trialM mInvisible1 (-visible1) zero
                            side2 = mkEllipse trialM mInvisible2 visible2 missing
                        case ellipsesAreDisjoint side1 side2 of
                            Nothing    -> return mLower
                            Just False -> put (mLower, trialM) >> bisect sec
                            Just _     -> put (trialM, mUpper) >> bisect False

type CoeffMatrix = M33 Double

mkEllipse :: Mass -- ^ The test parent mass
          -> Mass -- ^ The mass of the inivisible particle
          -> FourMomentum -> TransverseMomentum -> Maybe CoeffMatrix
mkEllipse m mInv vis inv =
    let mVis = mass vis
        mVisSq = mVis ** 2
        mInvSq = mInv ** 2
        mSq = m ** 2
        (px', py') = pxpy vis
        (kx', ky') = pxpy inv
        axx = 4 * mVis ** 2 + 4 * py' ** 2
        ayy = 4 * mVis ** 2 + 4 * px' ** 2
        axy = - 4 * px' * py'
        ax  = - 4 * mVisSq * kx' - 2 * mInvSq * px' + 2 * mSq * px'
              - 2 * mVisSq * px' + 4 * ky' * px' * py' - 4 * kx' * py' ** 2
        ay  = - 4 * mVisSq * ky' - 4 * ky' * px' ** 2 - 2 * mInvSq * py'
              + 2 * mSq * py' - 2 * mVisSq * py' + 4 * kx' * px' * py'
        az  = - mInvSq ** 2 + 2 * mInvSq * mSq - mSq ** 2 + 2 * mInvSq * mVisSq
              + 2 * mSq * mVisSq - mVisSq ** 2 + 4 * mVisSq * kx' ** 2
              + 4 * mVisSq * ky' ** 2 + 4 * mInvSq * kx' * px'
              - 4 * mSq * kx' * px' + 4 * mVisSq * kx' * px'
              + 4 * mInvSq * px' ** 2 + 4 * (ky' ** 2) * (px' ** 2)
              + 4 * mInvSq * ky' * py' - 4 * mSq * ky' * py'
              + 4 * mVisSq * ky' * py' - 8 * kx' * ky' * px' * py'
              + 4 * mInvSq * py' ** 2 + 4 * (kx' ** 2) * (py' ** 2)
    in mkCoeffMatrix axx ayy axy ax ay az

mkCoeffMatrix :: Double -> Double -> Double -> Double -> Double -> Double
              -> Maybe CoeffMatrix
mkCoeffMatrix axx ayy axy ax ay az =
    let row1 = V3 axx axy ax
        row2 = V3 axy ayy ay
        row3 = V3 ax  ay  az
    in if axx < 0 || ayy < 0 then Nothing else Just (V3 row1 row2 row3)

cxx, cxy, cx, cyy, cy, cz :: CoeffMatrix -> Double
cxx = (^._x._x)
cxy = (^._x._y)
cx  = (^._x._z)
cyy = (^._y._y)
cy  = (^._y._z)
cz  = (^._z._z)

coeffLamPow :: CoeffMatrix -> CoeffMatrix -> [Double]
coeffLamPow m1 m2 =
    let coeffLamPow3 = det33 m1
        coeffLamPow2 = coeff12 m1 m2
        coeffLamPow1 = coeff12 m2 m1
        coeffLamPow0 = det33 m2
        coeffLamPow' = [coeffLamPow3, coeffLamPow2, coeffLamPow1, coeffLamPow0]
    in if abs coeffLamPow3 >= abs coeffLamPow0
       then coeffLamPow'
       else reverse coeffLamPow'
  where
    coeff12 m1' m2' = let axx = cxx m1'; axy = cxy m1'; ayy = cyy m1'
                          ax = cx m1'; ay = cy m1'; a = cz m1'
                          bxx = cxx m2'; bxy = cxy m2'; byy = cyy m2'
                          bx = cx m2'; by = cy m2'; b = cz m2'
                      in axx * ayy * b + 2.0 * axy * ay * bx - 2 * ax * ayy * bx
                         + a * ayy * bxx - 2 * a * axy * bxy + 2 * ax * ay * bxy
                         + 2 * ax * axy * by - 2 * axx * ay * by + a * axx * byy
                         - byy * ax ** 2 - b * axy ** 2 - bxx * ay ** 2

ellipsesAreDisjoint :: Maybe CoeffMatrix -> Maybe CoeffMatrix -> Maybe Bool
ellipsesAreDisjoint Nothing   _         = Nothing
ellipsesAreDisjoint _         Nothing   = Nothing
ellipsesAreDisjoint (Just m1) (Just m2)
    | m1 == m2 = Just False
    | otherwise =
      let [c3, c2, c1, c0] = coeffLamPow m1 m2
      in case c3 of
          0 -> Nothing
          _ -> let [a, b, c] = map (/ c3) [c2, c1, c0]
                   s2 = - 3 * b + a ** 2
                   s4 = - 27 * c ** 2 + 18 * c * a * b + (a ** 2) * (b ** 2)
                        - 4 * c * a ** 3 - 4 * b ** 3
               in if s2 <= 0 || s4 <= 0
                  then Just False
                  else Just (a < 0 || 3 * a * c + b * a ** 2 - 4 * b ** 2 < 0)
