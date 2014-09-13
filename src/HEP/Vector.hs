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

import           Data.Foldable            as Foldable
import           Data.Function            (on)

import           HEP.Vector.LorentzVector (LorentzVector)
import qualified HEP.Vector.LorentzVector as LV
import           HEP.Vector.TwoVector     (phi2MPiPi)

-- | Type class for four-momentum objects in high-energy processes.
--
-- Minimal complete definition: 'fourMomentum'.
class HasFourMomentum a where
  fourMomentum :: a -> LorentzVector Double

  -- | Transverse momentum.
  pt :: a -> Double
  pt = LV.pT . fourMomentum

  -- | Pseudorapidity.
  eta :: a -> Double
  eta = LV.eta . fourMomentum

  -- | Azimuthal angle.
  phi :: a -> Double
  phi = LV.phi . fourMomentum

  -- | Invariant mass.
  invariantMass :: (Foldable f, Functor f) => f a -> Double
  invariantMass = LV.invariantMass . momentumSum

  -- | Comparison of objects by the magnitude of transverse momentum.
  ptCompare :: a -> a -> Ordering
  ptCompare = flip compare `on` pt

  -- | Scalar sum of transverse momenta.
  ptScalarSum :: Foldable f => f a -> Double
  ptScalarSum = Foldable.foldr (\p acc -> pt p + acc) 0

  -- | Vector sum of transverse momenta.
  ptVectorSum :: (Foldable f, Functor f) => f a -> Double
  ptVectorSum = LV.pT . momentumSum

  -- | Total four-momentum.
  momentumSum :: (Foldable f, Functor f) => f a -> LorentzVector Double
  momentumSum = LV.vectorSum . fmap fourMomentum

  -- | Pseudorapidity difference.
  deltaEta :: a -> a -> Double
  deltaEta = (-) `on` eta

  -- | Azimuthal angle difference.
  deltaPhi :: a -> a -> Double
  deltaPhi p p' = phi2MPiPi $ phi p - phi p'

  -- | Cone size.
  deltaR :: a -> a -> Double
  deltaR p p' = sqrt $ deta * deta + dphi * dphi
    where deta = deltaEta p p'
          dphi = deltaPhi p p'

  -- | Cosine of angle between four-momenta.
  cosTheta :: a -> a -> Double
  cosTheta p p' = cos $ (LV.deltaTheta `on` fourMomentum) p p'
