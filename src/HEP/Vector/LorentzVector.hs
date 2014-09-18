--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Vector.LorentzVector
-- Copyright   :  (c) 2014 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- A (3+1)-dimensional Lorentz vector type and utility functions for
-- four-momentum objects in high-energy processes.
--
--------------------------------------------------------------------------------
module HEP.Vector.LorentzVector
       ( -- * Type
         LorentzVector (..)

         -- * Function
       , setXYZT
       , setEtaPhiPtM

       , vectorSum
       , invariantMass

       , pt
       , eta
       , phi

       , deltaEta
       , deltaPhi
       , deltaR
       , deltaTheta
       , cosTheta

       , boostVector
       ) where

import           Control.Applicative    (Applicative (..))
import           Data.Function          (on)
import           Data.Traversable       (Traversable, fmapDefault)
import           Linear.Metric          (Metric (..))
import           Linear.V2              (V2 (..))
import           Linear.V3              (V3 (..))
import           Linear.V4              (V4 (..))
import           Linear.Vector          (Additive (..), sumV, (^/))

import           HEP.Vector.ThreeVector (ThreeVector)
import qualified HEP.Vector.ThreeVector as V3
import           HEP.Vector.TwoVector   (TwoVector)
import qualified HEP.Vector.TwoVector   as V2

-- | The Lorentz vector type. Its metric is defined as diag [1, -1, -1, -1].
newtype LorentzVector a = LorentzVector { getVector :: V4 a }
                        deriving (Eq, Ord, Show)

instance Functor LorentzVector where
  fmap f (LorentzVector v4) = LorentzVector (fmap f v4)

instance Applicative LorentzVector where
  pure a = LorentzVector (V4 a a a a)
  LorentzVector v4 <*> LorentzVector v4' = LorentzVector (v4 <*> v4')

instance Additive LorentzVector where
  zero = pure 0

instance Metric LorentzVector where
  (LorentzVector (V4 t x y z)) `dot` (LorentzVector (V4 t' x' y' z')) =
    t * t' - x * x' - y * y' - z * z'

-- | Makes 'LorentzVector' out of components based on x, y, z, t coordinates.
setXYZT :: a -> a -> a -> a -> LorentzVector a
setXYZT px' py' pz' e' = LorentzVector $ V4 e' px' py' pz'

-- | Makes 'LorentzVector' out of components based on pseudorapidity,
-- azimuthal angle, transverse momentum, and mass coordinates.
setEtaPhiPtM :: Floating a => a -> a -> a -> a -> LorentzVector a
setEtaPhiPtM eta' phi' pt' m' = setXYZT px py pz e
  where e = sqrt $ px ** 2 + py ** 2 + pz ** 2 + m' ** 2
        px = pt' * cos phi'
        py = pt' * sin phi'
        pz = pt' * sinh eta'

-- | Vector sum of Lorentz vectors.
--
-- >>> vectorSum [LorentzVector (V4 4 1 2 3), LorentzVector (V4 12 5 6 7)]
-- LorentzVector {getVector = V4 16 6 8 10}
vectorSum :: (Traversable f, Num a) => f (LorentzVector a) -> LorentzVector a
vectorSum = LorentzVector . sumV . fmapDefault getVector

-- | Invariant mass.
invariantMass :: Floating a => LorentzVector a -> a
invariantMass = norm

transV :: LorentzVector a -> TwoVector a
transV (LorentzVector (V4 _ x y _)) = V2.TwoVector (V2 x y)

-- | Magnitude of transverse momentum.
pt :: Floating a => LorentzVector a -> a
pt = norm . V2.getVector . transV

spatialV :: Num a => LorentzVector a -> ThreeVector a
spatialV (LorentzVector (V4 _ x y z)) = V3.ThreeVector (V3 x y z)

-- | Pseudorapidity.
eta :: (Floating a, Ord a) => LorentzVector a -> a
eta = V3.pseudoRapidity . spatialV

-- | Azimuthal angle.
phi :: RealFloat a => LorentzVector a -> a
phi = V3.phi . spatialV

-- | Pseudorapidity difference between two Lorentz vectors.
deltaEta :: (Floating a, Ord a) => LorentzVector a -> LorentzVector a -> a
deltaEta = (-) `on` eta

-- | Azimuthal angle difference between two Lorentz vectors.
deltaPhi :: RealFloat a => LorentzVector a -> LorentzVector a -> a
deltaPhi v v' = V2.phi2MPiPi $! phi v - phi v'

-- | Size of the cone spanned by two Lorentz vectors.
deltaR :: RealFloat a => LorentzVector a -> LorentzVector a -> a
deltaR v v' = sqrt $! deltaEta v v' ** 2 + deltaPhi v v' ** 2

-- | Separation angle between two Lorentz vectors.
deltaTheta :: (Floating a, Ord a) => LorentzVector a -> LorentzVector a -> a
deltaTheta = V3.angle `on` spatialV

-- | Cosine of angle between two Lorentz vectors.
cosTheta :: (Floating a , Ord a) => LorentzVector a -> LorentzVector a -> a
cosTheta v v' = cos $! deltaTheta v v'

-- | Boost vector. It returns 'ThreeVector'.
boostVector :: Fractional a => LorentzVector a -> ThreeVector a
boostVector v@(LorentzVector (V4 t _ _ _)) =
  V3.ThreeVector $ (V3.getVector . spatialV) v ^/ t
