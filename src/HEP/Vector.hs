{-# LANGUAGE FlexibleInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Vector
-- Copyright   :  (c) 2014 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- Types and type classes for vector objects.
--
--------------------------------------------------------------------------------
module HEP.Vector (HasFourMomentum (..)) where

import           Data.Foldable             as Foldable
import           Data.Function             (on)
import           Data.Traversable          (Traversable, fmapDefault)
import           Linear.V3                 (V3 (..))
import           Linear.V4                 (V4 (..))

import           HEP.Vector.LorentzTVector (LorentzTVector (..))
import qualified HEP.Vector.LorentzTVector as TV
import           HEP.Vector.LorentzVector  (LorentzVector (..))
import qualified HEP.Vector.LorentzVector  as LV

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
  invariantMass :: Traversable f => f a -> Double
  invariantMass = LV.invariantMass . momentumSum

  -- | Transverse mass of the visible + invisible particle system.
  transverseMass :: a -> LorentzTVector Double -> Double
  transverseMass p = TV.invariantMass ((makeTV . fourMomentum) p)
    where makeTV (LorentzVector (V4 t x y z)) =
            LorentzTVector $ V3 (sqrt $ t ** 2 - z ** 2) x y

  -- | Comparison of objects by the magnitude of transverse momentum
  -- in descending order.
  ptCompare :: a -> a -> Ordering
  ptCompare = flip compare `on` pt

  -- | Scalar sum of transverse momenta.
  ptScalarSum :: Foldable f => f a -> Double
  ptScalarSum = Foldable.foldl' (\acc p -> acc + pt p) 0

  -- | Vector sum of transverse momenta.
  ptVectorSum :: Traversable f => f a -> Double
  ptVectorSum = LV.pt . momentumSum

  -- | Total four-momentum.
  momentumSum :: Traversable f => f a -> LorentzVector Double
  momentumSum = LV.vectorSum . fmapDefault fourMomentum

  -- | Pseudorapidity difference.
  deltaEta :: a -> a -> Double
  deltaEta = LV.deltaEta `on` fourMomentum

  -- | Azimuthal angle difference.
  deltaPhi :: a -> a -> Double
  deltaPhi = LV.deltaPhi `on` fourMomentum

  -- | Cone size.
  deltaR :: a -> a -> Double
  deltaR = LV.deltaR `on` fourMomentum

  -- | Cosine of angle between four-momenta.
  cosTheta :: a -> a -> Double
  cosTheta = LV.cosTheta `on` fourMomentum

instance HasFourMomentum (LorentzVector Double) where
  fourMomentum = id
  {-# INLINE fourMomentum #-}
