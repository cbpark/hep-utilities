--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Kinematics.Variable
-- Copyright   :  (c) 2015-2020 Chan Beom Park
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

    , mAT
    , mATMAOS
    ) where

import           HEP.Kinematics
import           HEP.Kinematics.Variable.Antler          (Visibles (..))
import qualified HEP.Kinematics.Variable.Antler          as AT
import qualified HEP.Kinematics.Variable.Internal        as Internal
import           HEP.Kinematics.Variable.MAOS
import           HEP.Kinematics.Variable.MT2             (mT2)
import           HEP.Kinematics.Variable.MTLowerAndUpper (mTLowerBound)

mT2Symm :: FourMomentum       -- ^ four-momentum of the first visible system
        -> FourMomentum       -- ^ four-momentum of the second visible system
        -> TransverseMomentum -- ^ missing transverse momentum
        -> Double             -- ^ invariant mass of each invisible system
        -> Double
mT2Symm = Internal.mT2Symm

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
maosMomentaSymmetric = Internal.maosMomentaSymmetric

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

-- | returns the list of M_{AT}.
mAT :: FourMomentum  -- ^ the four-momentum of visible particles (1)
    -> FourMomentum  -- ^ the four-momentum of visible particles (2)
    -> Double        -- ^ Q_{x}
    -> Double        -- ^ Q_{y}
    -> Double        -- ^ a guess of the longitudinal momentum of the resonance
    -> Double        -- ^ the mass of intermediate particle
    -> Double        -- ^ the mass of invisible particle
    -> Maybe [Double]
mAT p1 p2 qx qy qz mA mB = do
    at <- AT.mkAntler mB mA (Visibles p1 p2)
    AT.mAT at qx qy qz

-- | returns (M_{AT}, M_{MAOS}, M_{T2}).
mATMAOS :: FourMomentum        -- ^ the four-momentum of visible particles (1)
        -> FourMomentum        -- ^ the four-momentum of visible particles (2)
        -> TransverseMomentum  -- ^ the missing transverse momentum
        -> Double              -- ^ Q_{x}
        -> Double              -- ^ Q_{y}
        -> Double              -- ^ the mass of intermediate particle
        -> Double              -- ^ the mass of invisible particle
        -> Maybe [Double]
mATMAOS p1 p2 ptmiss qx qy mA mB = do
    at <- AT.mkAntler mB mA (Visibles p1 p2)
    (mATs, _, _) <- AT.mATMAOS at qx qy ptmiss
    return mATs
