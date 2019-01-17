{-# LANGUAGE CPP #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Kinematics.Vector.LorentzVector
-- Copyright   :  (c) 2014-2017 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- A (3+1)-dimensional Lorentz vector type and utility functions for
-- four-momentum objects in high-energy processes.
--
--------------------------------------------------------------------------------
module HEP.Kinematics.Vector.LorentzVector
    ( -- * Type
      LorentzVector

      -- * Function
    , setXYZT
    , setEtaPhiPtM

    , vectorSum
    , invariantMass
    , transV

    , pt
    , eta
    , phi

    , deltaEta
    , deltaPhi
    , deltaR
    , deltaTheta
    , cosTheta

    , boostVector
    , beta
    , gamma
    , boost
    ) where

import           HEP.Kinematics.Vector.ThreeVector (ThreeVector)
import qualified HEP.Kinematics.Vector.ThreeVector as V3
import           HEP.Kinematics.Vector.TwoVector   (TwoVector)
import qualified HEP.Kinematics.Vector.TwoVector   as V2

import           Control.Applicative
import           Control.Lens                      ((^.))
import           Data.Function                     (on)
import           Data.Semigroup                    (Semigroup (..))
import           Data.Traversable                  (fmapDefault)
import           Linear.Metric                     (Metric (..))
import           Linear.V2                         (V2 (..))
import           Linear.V3                         (V3 (..))
import           Linear.V4
import           Linear.Vector                     (Additive (..), sumV, (^/))

-- | The Lorentz vector type. Its metric is defined as diag [1, -1, -1, -1].
newtype LorentzVector a = LorentzVector (V4 a) deriving (Eq, Show)

instance Num a => Num (LorentzVector a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

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

instance Num a => Semigroup (LorentzVector a) where
    LorentzVector v4 <> LorentzVector v4' = LorentzVector (v4 ^+^ v4')

instance Num a => Monoid (LorentzVector a) where
    mempty = zero

#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

instance R1 LorentzVector where
    _x f (LorentzVector (V4 t x y z)) = (\x' -> LorentzVector (V4 t x' y z)) <$> f x

instance R2 LorentzVector where
    _y f (LorentzVector (V4 t x y z)) = (\y' -> LorentzVector (V4 t x y' z)) <$> f y
    _xy f (LorentzVector (V4 t x y z)) =
        (\(V2 x' y') -> LorentzVector (V4 t x' y' z)) <$> f (V2 x y)

instance R3 LorentzVector where
    _z f (LorentzVector (V4 t x y z)) = LorentzVector . V4 t x y <$> f z
    _xyz f (LorentzVector (V4 t x y z)) =
        (\(V3 x' y' z') -> LorentzVector (V4 t x' y' z')) <$> f (V3 x y z)

instance R4 LorentzVector where
    _w f (LorentzVector (V4 t x y z)) = (\t' -> LorentzVector (V4 t' x y z)) <$> f t
    _xyzw f (LorentzVector v4) = LorentzVector <$> f v4

-- | Makes 'LorentzVector' out of components based on x, y, z, t coordinates.
setXYZT :: a -> a -> a -> a -> LorentzVector a
setXYZT px' py' pz' e' = LorentzVector (V4 e' px' py' pz')

-- | Makes 'LorentzVector' out of components based on pseudorapidity,
-- azimuthal angle, transverse momentum, and mass coordinates.
setEtaPhiPtM :: Floating a => a -> a -> a -> a -> LorentzVector a
setEtaPhiPtM eta' phi' pt' m' = setXYZT px py pz e
  where
    e = sqrt $ px ** 2 + py ** 2 + pz ** 2 + m' ** 2
    px = pt' * cos phi'
    py = pt' * sin phi'
    pz = pt' * sinh eta'

-- | Vector sum of Lorentz vectors.
--
-- >>> vectorSum [LorentzVector (V4 4 1 2 3), LorentzVector (V4 12 5 6 7)]
-- LorentzVector (V4 16 6 8 10)
vectorSum :: (Traversable f, Num a) => f (LorentzVector a) -> LorentzVector a
vectorSum = LorentzVector . sumV . fmapDefault (^._xyzw)

-- | Invariant mass.
invariantMass :: Floating a => LorentzVector a -> a
invariantMass = norm

transV :: LorentzVector a -> TwoVector a
transV (LorentzVector (V4 _ x y _)) = V2.setXY x y

-- | Magnitude of transverse momentum.
pt :: Floating a => LorentzVector a -> a
pt = norm . transV

spatialV :: LorentzVector a -> ThreeVector a
spatialV (LorentzVector (V4 _ x y z)) = V3.setXYZ x y z

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
boostVector v@(LorentzVector (V4 t _ _ _)) = spatialV v ^/ t

beta :: LorentzVector Double -> Double
beta v@(LorentzVector (V4 t _ _ _)) = (norm . spatialV) v / t

gamma :: LorentzVector Double -> Double
gamma v = let b = beta v in 1.0 / sqrt (1.0 - b * b)

boost :: LorentzVector Double -> Double -> Double -> Double -> LorentzVector Double
boost (LorentzVector (V4 t x y z)) bx by bz =
    let b2 = bx * bx + by * by + bz * bz
        ga = 1.0 / sqrt (1 - b2)
        bp = bx * x + by * y + bz * z
        ga2 = if b2 > 0 then (ga - 1.0) / b2 else 0

        x' = x + ga2 * bp * bx + ga * bx * t
        y' = y + ga2 * bp * by + ga * by * t
        z' = z + ga2 * bp * bz + ga * bz * t
        t' = ga * (t + bp)
        v' = setXYZT x' y' z' t'
    in v'
