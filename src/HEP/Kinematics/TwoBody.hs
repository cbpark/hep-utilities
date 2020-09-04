--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Kinematics.TwoBody
-- Copyright   :  (c) 2019 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
--------------------------------------------------------------------------------
module HEP.Kinematics.TwoBody
    (
      TwoBodyEvent (..)

    , mkTwoBodyEvent
    , mandelstamS
    , mandelstamT
    , mandelstamU
    ) where

import HEP.Kinematics                      (FourMomentum)
import HEP.Kinematics.Vector.LorentzVector (setXYZT)

import Control.Monad                       (guard)
import Linear.Metric                       (Metric (..))

data TwoBodyEvent = TwoBodyEvent { initial      :: !TwoBodyStates
                                 , final        :: !TwoBodyStates
                                 , scale        :: !Double
                                 , cosAngle     :: !Double
                                 , azimuthAngle :: !Double
                                 } deriving Show

type TwoBodyStates = (FourMomentum, FourMomentum)

mkTwoBodyEvent :: Double                            -- ^ scale of event: sqrt(s)
               -> (Double, Double, Double, Double)  -- ^ particle masses
               -> (Double, Double)                  -- ^ random numbers in [0, 1)
               -> Maybe TwoBodyEvent
mkTwoBodyEvent s (m1, m2, m3, m4) (r1, r2) = do
    guard $ s > m1 + m2
         && s > m3 + m4
         && r1 >= 0 && r1 < 1 && r2 >= 0 && r2 < 1

    let scaleSq = s * s
        [m1Sq, m2Sq, m3Sq, m4Sq] = (\m -> m * m) <$> [m1, m2, m3, m4]
        costh = -1 + 2 * r1
        sinth = sqrt (1 - costh * costh)
        phi = 2 * pi * r2
        cosphi = cos phi
        sinphi = sin phi

        denom = 2 * s
        pin = lambda12 scaleSq m1Sq m2Sq / denom
        pout = lambda12 scaleSq m3Sq m4Sq / denom

        e1 = (scaleSq + m1Sq - m2Sq) / denom
        p1 = setXYZT 0 0 pin e1

        e2 = (scaleSq - m1Sq + m2Sq) / denom
        p2 = setXYZT 0 0 (-pin) e2

        e3 = (scaleSq + m3Sq - m4Sq) / denom
        p3 = setXYZT (pout * sinth * cosphi)
                     (pout * sinth * sinphi)
                     (pout * costh)
                     e3

        e4 = (scaleSq - m3Sq + m4Sq) / denom
        p4 = setXYZT (- pout * sinth * cosphi)
                     (- pout * sinth * sinphi)
                     (- pout * costh)
                     e4

    return $ TwoBodyEvent { initial      = (p1, p2)
                          , final        = (p3, p4)
                          , scale        = s
                          , cosAngle     = costh
                          , azimuthAngle = phi
                          }

lambda12 :: Double -> Double -> Double -> Double
lambda12 x y z =
    let lambda = x * x + y * y + z * z - 2 * x * y - 2 * y * z - 2 * z * x
    in if lambda >= 0 then sqrt lambda else sqrt (- lambda)

mandelstamS :: TwoBodyEvent -> Double
mandelstamS TwoBodyEvent { scale = s } = s * s

mandelstamT :: TwoBodyEvent -> Double
mandelstamT TwoBodyEvent { initial = (p1, _), final = (p3, _) } =
    let v = p1 - p3
    in v `dot` v

mandelstamU :: TwoBodyEvent -> Double
mandelstamU TwoBodyEvent { initial = (p1, _), final = (_, p4) } =
    let v = p1 - p4
    in v `dot` v
