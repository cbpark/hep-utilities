--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Kinematics.Variable
-- Copyright   :  (c) 2015 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- Various kinematic variables for collider studies.
--
--------------------------------------------------------------------------------

module HEP.Kinematics.Variable
       (
         mT2Symm
       , mT2Asymm
       , mTLowerBound
       ) where

import           HEP.Kinematics                          (FourMomentum,
                                                          TransverseMomentum)
import           HEP.Kinematics.Variable.MT2             (mT2)
import           HEP.Kinematics.Variable.MTLowerAndUpper (mTLowerBound)

mT2Symm :: FourMomentum       -- ^ four-momentum of the first visible system
        -> FourMomentum       -- ^ four-momentum of the second visible system
        -> TransverseMomentum -- ^ missing transverse momentum
        -> Double             -- ^ invariant mass of each invisible system
        -> Double
mT2Symm vis1 vis2 miss mInv = mT2 vis1 vis2 miss mInv mInv 0 True

mT2Asymm :: FourMomentum       -- ^ four-momentum of the first visible system
         -> FourMomentum       -- ^ four-momentum of the second visible system
         -> TransverseMomentum -- ^ missing transverse momentum
         -> Double             -- ^ invariant mass of the first invisible system
         -> Double             -- ^ invariant mass of the second invisible system
         -> Double
mT2Asymm vis1 vis2 miss mInv1 mInv2 = mT2 vis1 vis2 miss mInv1 mInv2 0 True
