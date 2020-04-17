{-# LANGUAGE CPP        #-}
{-# LANGUAGE MultiWayIf #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Kinematics.Vector.ThreeVector
-- Copyright   :  (c) 2014-2017 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- Three-dimensional vector.
--
--------------------------------------------------------------------------------
module HEP.Kinematics.Vector.ThreeVector
    (
      -- * Type
      ThreeVector

      -- * Function
    , setXYZ
    , angle
    , cosTheta
    , pseudoRapidity
    , phi
    ) where

import Control.Applicative
import Data.Function       (on)

import Control.Lens        ((^.))
import Linear.Metric       (Metric (..))
import Linear.V2           (V2 (..))
import Linear.V3           (R1 (..), R2 (..), R3 (..), V3 (..))
import Linear.Vector       (Additive (..))

-- | Three-dimensional vector type.
newtype ThreeVector a = ThreeVector (V3 a) deriving (Eq, Show)

instance Num a => Num (ThreeVector a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Functor ThreeVector where
    fmap f (ThreeVector v3) = ThreeVector (fmap f v3)

instance Applicative ThreeVector where
    pure a = ThreeVector (V3 a a a)
    ThreeVector v3 <*> ThreeVector v3' = ThreeVector (v3 <*> v3')

instance Additive ThreeVector where
    zero = pure 0

instance Metric ThreeVector where
    (ThreeVector (V3 x y z)) `dot` (ThreeVector (V3 x' y' z')) =
        x * x' + y * y' + z * z'

instance Num a => Semigroup (ThreeVector a) where
    ThreeVector v3 <> ThreeVector v3' = ThreeVector (v3 ^+^ v3')

instance Num a => Monoid (ThreeVector a) where
    mempty = zero

#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

instance R1 ThreeVector where
    _x f (ThreeVector (V3 x y z)) = (\x' -> ThreeVector (V3 x' y z)) <$> f x
    {-# INLINE _x #-}

instance R2 ThreeVector where
    _y f (ThreeVector (V3 x y z)) = (\y' -> ThreeVector (V3 x y' z)) <$> f y
    {-# INLINE _y #-}
    _xy f (ThreeVector (V3 x y z)) =
        (\(V2 x' y') -> ThreeVector (V3 x' y' z)) <$> f (V2 x y)
    {-# INLINE _xy #-}

instance R3 ThreeVector where
    _z f (ThreeVector (V3 x y z)) = ThreeVector . V3 x y <$> f z
    {-# INLINE _z #-}
    _xyz f (ThreeVector v3) = ThreeVector <$> f v3
    {-# INLINE _xyz #-}

setXYZ :: a -> a -> a -> ThreeVector a
setXYZ x y z = ThreeVector (V3 x y z)

-- | Angle of the 3-vector with respect to another 3-vector.
angle :: ThreeVector Double -> ThreeVector Double -> Double
angle v v' = let ptot2 = ((*) `on` (quadrance . (^._xyz))) v v'
             in if ptot2 <= 0
                then 0
                else let arg = (dot `on` (^._xyz)) v v' / sqrt ptot2
                     in if | arg >  1  -> 0
                           | arg < -1  -> pi
                           | otherwise -> acos arg

-- | Cosine of the angle with respect to the z-direction.
cosTheta :: ThreeVector Double -> Double
cosTheta (ThreeVector v@(V3 _ _ z)) = let ptot = norm v
                                      in if ptot > 0 then z / ptot else 1

-- | Pseudorapidity.
pseudoRapidity :: ThreeVector Double -> Double
pseudoRapidity v@(ThreeVector (V3 _ _ z))
    | ct * ct < 1 = -0.5 * log ((1.0 - ct) / (1.0 + ct))
    | z == 0      =  0
    | z >  0      =  10e10
    | otherwise   = -10e10
  where ct = cosTheta v

-- | Azimuthal angle from -pi to pi.
phi :: ThreeVector Double -> Double
phi (ThreeVector (V3 x y _)) | x == 0 && y == 0 = 0
                             | otherwise        = atan2 y x
