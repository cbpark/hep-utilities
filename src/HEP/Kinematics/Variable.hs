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
       , maosMomenta
       , maosMomentaSymmetric
       , maosMomentaSymmetric2
       , SolutionType (..)
       ) where

import           HEP.Kinematics                          (FourMomentum,
                                                          TransverseMomentum)
import           HEP.Kinematics.Variable.MAOS            (SolutionType (..),
                                                          maosMomenta)
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

-- | MAOS momenta for symmetric decay chains.
--
-- Each decay topology is like parent --> visible + invisible.
maosMomentaSymmetric :: Double              -- ^ the MT2 value
                     -> FourMomentum        -- ^ four-momentum of the first visible system
                     -> FourMomentum        -- ^ four-momentum of the second visible system
                     -> TransverseMomentum  -- ^ missing transverse momentum
                     -> Double              -- ^ parent particle mass
                     -> Double              -- ^ invisible particle mass
                     -> ([FourMomentum], [FourMomentum], SolutionType)
maosMomentaSymmetric mT2value vis1 vis2 miss mY mX =
    maosMomenta mT2value (vis1, mY, mX) (vis2, mY, mX) miss

-- | This calculates the modified MAOS momenta defined in
-- <http://arxiv.org/abs/1106.6087 arXiv:1106.6087>.
maosMomentaSymmetric2 :: Double              -- ^ the MT2 value
                      -> FourMomentum        -- ^ four-momentum of the first visible system
                      -> FourMomentum        -- ^ four-momentum of the second visible system
                      -> TransverseMomentum  -- ^ missing transverse momentum
                      -> Double              -- ^ invisible particle mass
                      -> ([FourMomentum], [FourMomentum], SolutionType)
maosMomentaSymmetric2 mT2value vis1 vis2 miss mX =
    maosMomenta mT2value (vis1, mT2value, mX) (vis2, mT2value, mX) miss
