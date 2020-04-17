{-# LANGUAGE CPP #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Kinematics.Vector.TwoVector
-- Copyright   :  (c) 2014-2017 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- Two-dimensional vector.
--
--------------------------------------------------------------------------------
module HEP.Kinematics.Vector.TwoVector
    ( -- * Type
      TwoVector

      -- * Function
    , setXY
    , setPtPhi
    , phi2MPiPi
    ) where

import Control.Applicative

import Linear.Metric       (Metric (..))
import Linear.V2           (R1 (..), R2 (..), V2 (..))
import Linear.Vector       (Additive (..))

-- | Two-dimensional vector type.
newtype TwoVector a = TwoVector (V2 a) deriving (Eq, Show)

instance Num a => Num (TwoVector a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Functor TwoVector where
    fmap f (TwoVector v2) = TwoVector (fmap f v2)

instance Applicative TwoVector where
    pure a = TwoVector (V2 a a)
    TwoVector v2 <*> TwoVector v2' = TwoVector (v2 <*> v2')

instance Additive TwoVector where
    zero = pure 0

instance Metric TwoVector where
    TwoVector (V2 x y) `dot` TwoVector (V2 x' y') = x * x' + y * y'

instance Num a => Semigroup (TwoVector a) where
    TwoVector v2 <> TwoVector v2' = TwoVector (v2 ^+^ v2')

instance Num a => Monoid (TwoVector a) where
    mempty = zero

#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

instance R1 TwoVector where
    _x f (TwoVector (V2 x y)) = (\x' -> TwoVector (V2 x' y)) <$> f x
    {-# INLINE _x #-}

instance R2 TwoVector where
    _y f (TwoVector (V2 x y)) = TwoVector . V2 x <$> f y
    {-# INLINE _y #-}
    _xy f (TwoVector v2) = TwoVector <$> f v2
    {-# INLINE _xy #-}

setXY :: a -> a -> TwoVector a
setXY x y = TwoVector (V2 x y)

setPtPhi :: Floating a => a -> a -> TwoVector a
setPtPhi pt phi = let px = pt * cos phi
                      py = pt * sin phi
                  in TwoVector (V2 px py)

-- | Angle in the interval [-pi, pi).
--
-- >>> phi2MPiPi (2 * pi)
-- 0.0
--
-- >>> phi2MPiPi (-pi)
-- -3.141592653589793
phi2MPiPi :: (Floating a, Ord a) => a -> a
phi2MPiPi x | x >= pi   = phi2MPiPi $! x - 2*pi
            | x < -pi   = phi2MPiPi $! x + 2*pi
            | otherwise = x
