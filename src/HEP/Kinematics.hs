{-# LANGUAGE FlexibleInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Kinematics
-- Copyright   :  (c) 2014 - 2015 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- Types and type classes for physics objects.
--
--------------------------------------------------------------------------------
module HEP.Kinematics
       ( HasFourMomentum (..)

       , invariantMass
       , transverseMass
       , ptCompare
       , ptScalarSum
       , ptVectorSum
       , momentumSum
       , deltaEta
       , deltaPhi
       , deltaR
       , cosTheta
       ) where

import           Data.Foldable                        as Foldable
import           Data.Function                        (on)
import           Data.Traversable                     (fmapDefault)
import           Linear.V3                            (V3 (..))
import           Linear.V4                            (V4 (..))

import           HEP.Kinematics.Vector.LorentzTVector (LorentzTVector (..))
import qualified HEP.Kinematics.Vector.LorentzTVector as TV
import           HEP.Kinematics.Vector.LorentzVector  (LorentzVector (..))
import qualified HEP.Kinematics.Vector.LorentzVector  as LV

-- | Type class for four-momentum objects in high-energy processes.
--
-- Minimal complete definition: 'fourMomentum'.
class HasFourMomentum a where
  fourMomentum :: a -> LorentzVector Double

  -- | Transverse momentum.
  pt :: a -> Double
  pt = LV.pt . fourMomentum

  -- | Pseudorapidity.
  eta :: a -> Double
  eta = LV.eta . fourMomentum

  -- | Azimuthal angle.
  phi :: a -> Double
  phi = LV.phi . fourMomentum

  -- | Invariant mass.
  mass :: a -> Double
  mass = LV.invariantMass . fourMomentum

instance HasFourMomentum (LorentzVector Double) where
  fourMomentum = id
  {-# INLINE fourMomentum #-}

-- | Invariant mass.
invariantMass :: (Traversable f, HasFourMomentum a) => f a -> Double
invariantMass = LV.invariantMass . momentumSum

-- | Transverse mass of the visible + invisible particle system.
transverseMass :: HasFourMomentum a => a -> LorentzTVector Double -> Double
transverseMass p = TV.invariantMass ((makeTV . fourMomentum) p)
  where makeTV (LorentzVector (V4 t x y z)) =
          LorentzTVector $ V3 (sqrt $ t ** 2 - z ** 2) x y

-- | Comparison of objects by the magnitude of transverse momentum
-- in descending order.
ptCompare :: HasFourMomentum a => a -> a -> Ordering
ptCompare = flip compare `on` pt

-- | Scalar sum of transverse momenta.
ptScalarSum :: HasFourMomentum a => Foldable f => f a -> Double
ptScalarSum = Foldable.foldl' (\acc p -> acc + pt p) 0

-- | Vector sum of transverse momenta.
ptVectorSum :: (Traversable f, HasFourMomentum a) => f a -> Double
ptVectorSum = LV.pt . momentumSum

-- | Total four-momentum.
momentumSum :: (Traversable f, HasFourMomentum a) => f a -> LorentzVector Double
momentumSum = LV.vectorSum . fmapDefault fourMomentum

-- | Pseudorapidity difference.
deltaEta :: HasFourMomentum a => a -> a -> Double
deltaEta = LV.deltaEta `on` fourMomentum

-- | Azimuthal angle difference.
deltaPhi :: HasFourMomentum a => a -> a -> Double
deltaPhi = LV.deltaPhi `on` fourMomentum

-- | Cone size.
deltaR :: HasFourMomentum a => a -> a -> Double
deltaR = LV.deltaR `on` fourMomentum

-- | Cosine of angle between four-momenta.
cosTheta :: HasFourMomentum a => a -> a -> Double
cosTheta = LV.cosTheta `on` fourMomentum
