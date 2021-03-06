{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Kinematics
-- Copyright   :  (c) 2014-2017 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- Types and type classes for physics objects.
--
--------------------------------------------------------------------------------
module HEP.Kinematics
    (
      HasFourMomentum (..)
    , FourMomentum
    , Mass
    , SpatialMomentum
    , TransverseMomentum
    , module LM
    , module LV

    , invariantMass
    , invariantMassSq
    , transverseMass
    , transverseMass1
    , transverseMassCluster
    , transverseVector
    , transverseEnergy
    , promote2TV
    , promote2V3
    , promote2V4
    , ptCompare
    , ptScalarSum
    , ptVectorSum
    , momentumSum
    , deltaEta
    , deltaPhi
    , deltaR
    , cosTheta
    , cosThetaBeam
    , beta
    , gamma
    ) where

import           HEP.Kinematics.Vector.LorentzTVector (LorentzTVector)
import qualified HEP.Kinematics.Vector.LorentzTVector as TV
import           HEP.Kinematics.Vector.LorentzVector  (LorentzVector)
import qualified HEP.Kinematics.Vector.LorentzVector  as LV
import           HEP.Kinematics.Vector.ThreeVector    (ThreeVector)
import qualified HEP.Kinematics.Vector.ThreeVector    as ThreeVector
import           HEP.Kinematics.Vector.TwoVector      (TwoVector)

import           Data.Foldable                        (foldl')
import           Data.Function                        (on)
import           Data.Functor.Identity                (Identity (..))
import           Data.Traversable                     (fmapDefault)

import           Control.Lens                         ((^.))
import           Linear.Metric                        as LM
import           Linear.V2
import           Linear.V3                            (R3 (..), V3 (..))
import           Linear.V4                            (R4 (..), V4 (..))
import           Linear.Vector                        as LV

type FourMomentum = LorentzVector Double

type Mass = Double

-- | Type class for four-momentum objects in high-energy processes.
--
-- Minimal complete definition: 'fourMomentum'.
class HasFourMomentum a where
    fourMomentum :: a -> FourMomentum

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

    epxpypz :: a -> (Double, Double, Double, Double)
    epxpypz p = let (V4 e' px' py' pz') = fourMomentum p ^._xyzw
                in (e', px', py', pz')

    pxpypz :: a -> (Double, Double, Double)
    pxpypz p = let (_, px', py', pz') = epxpypz p in (px', py', pz')

    pxpy :: a -> (Double, Double)
    pxpy p = let (px', py', _) = pxpypz p in (px', py')

    px :: a -> Double
    px p = let (px', _) = pxpy p in px'

    py :: a -> Double
    py p = let (_, py') = pxpy p in py'

    pz :: a -> Double
    pz p = let (_, _, pz') = pxpypz p in pz'

    energy :: a -> Double
    energy p = let (e', _, _, _) = epxpypz p in e'

instance HasFourMomentum FourMomentum where
    fourMomentum = id
    {-# INLINE fourMomentum #-}

type TransverseMomentum = TwoVector Double

instance HasFourMomentum TransverseMomentum where
    fourMomentum v2 = let (x, y) = pxpy v2
                          eT = sqrt (x * x + y * y)
                      in eT `seq` LV.setXYZT x y 0 eT
    {-# INLINE fourMomentum #-}
    pxpy v2 = let (V2 x y) = v2 ^._xy in (x, y)
    {-# INLINE pxpy #-}
    px = (^._x)
    {-# INLINE px #-}
    py = (^._y)
    {-# INLINE py #-}

type SpatialMomentum = ThreeVector Double

instance HasFourMomentum SpatialMomentum where
    fourMomentum v3 = let (x, y, z) = pxpypz v3
                          e = sqrt (x * x + y * y + z * z)
                      in e `seq` LV.setXYZT x y z e
    {-# INLINE fourMomentum #-}
    pxpypz v3 = let (V3 x y z) = v3 ^._xyz in (x, y, z)
    {-# INLINE pxpypz #-}
    px = (^._x)
    {-# INLINE px #-}
    py = (^._y)
    {-# INLINE py #-}
    pz = (^._z)
    {-# INLINE pz #-}

-- | Invariant mass.
invariantMass :: (Traversable f, HasFourMomentum a) => f a -> Double
invariantMass = LV.invariantMass . momentumSum
{-# INLINE invariantMass #-}

-- | Invariant mass squared.
invariantMassSq :: (Traversable f, HasFourMomentum a) => f a -> Double
invariantMassSq = quadrance . momentumSum
{-# INLINE invariantMassSq #-}

-- | Transverse mass of the visible + invisible particle system.
transverseMass :: (Traversable f, HasFourMomentum a)
               => f a -> LorentzTVector Double -> Double
transverseMass p = TV.invariantMass ((makeTV . fourMomentum . momentumSum) p)
  where makeTV v4 = let (e', px', py', pz') = epxpypz v4
                        eT = sqrt (e' * e' - pz' * pz')
                    in eT `seq` TV.setXYT px' py' eT
{-# INLINE transverseMass #-}

transverseMass1 :: HasFourMomentum a => a -> LorentzTVector Double -> Double
transverseMass1 = transverseMass . Identity
{-# INLINE transverseMass1 #-}

-- | Cluster transverse mass.
-- This is the same as mTtrue in
-- <http://arxiv.org/abs/0902.4864 arXiv:0902.4864>.
transverseMassCluster :: (Traversable f, HasFourMomentum a) =>
                         f a -> TransverseMomentum -> Double
transverseMassCluster p v2 = let (V2 x y) = v2 ^._xy
                             in transverseMass p (TV.setXYM x y 0)

transverseVector :: HasFourMomentum a => a -> TransverseMomentum
transverseVector = LV.transV . fourMomentum
{-# INLINE transverseVector #-}

transverseEnergy :: HasFourMomentum a => a -> Double
transverseEnergy v = let !m = mass v
                         !pt' = pt v
                     in sqrt $! m * m + pt' * pt'
{-# INLINE transverseEnergy #-}

promote2TV :: TransverseMomentum
           -> Mass  -- ^ mass
           -> LorentzTVector Double
promote2TV v2 m = let eT = sqrt $ quadrance v2 + m * m
                  in eT `seq` TV.setXYT (px v2) (py v2) eT
{-# INLINE promote2TV #-}

promote2V3 :: TransverseMomentum
           -> Double  -- ^ the longitudinal component
           -> SpatialMomentum
promote2V3 v2 = ThreeVector.setXYZ (px v2) (py v2)
{-# INLINE promote2V3 #-}

promote2V4 :: SpatialMomentum
           -> Mass  -- ^ mass
           -> FourMomentum
promote2V4 v3 m = let e = sqrt $ quadrance v3 + m * m
                  in e `seq` LV.setXYZT (px v3) (py v3) (pz v3) e
{-# INLINE promote2V4 #-}

-- | Comparison of objects by the magnitude of transverse momentum
-- in descending order.
ptCompare :: HasFourMomentum a => a -> a -> Ordering
ptCompare = flip compare `on` pt

-- | Scalar sum of transverse momenta.
ptScalarSum :: (Foldable f, HasFourMomentum a) => f a -> Double
ptScalarSum = foldl' (\acc p -> acc + pt p) 0

-- | Vector sum of transverse momenta.
ptVectorSum :: (Traversable f, HasFourMomentum a) => f a -> Double
ptVectorSum = LV.pt . momentumSum

-- | Total four-momentum.
momentumSum :: (Traversable f, HasFourMomentum a) => f a -> FourMomentum
momentumSum = LV.vectorSum . fmapDefault fourMomentum
{-# INLINE momentumSum #-}

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

-- | Cosine of angle between the beam direction (+z).
cosThetaBeam :: HasFourMomentum a => a -> Double
cosThetaBeam v = let (x, y, z) = pxpypz v
                 in ThreeVector.cosTheta (ThreeVector.setXYZ x y z)

beta :: HasFourMomentum a => a -> Double
beta = LV.beta . fourMomentum

gamma :: HasFourMomentum a => a -> Double
gamma = LV.gamma . fourMomentum
