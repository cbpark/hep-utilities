--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Kinematics.Vector.TwoVector
-- Copyright   :  (c) 2014 - 2015 Chan Beom Park
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
         TwoVector (..)

         -- * Function
       , setXY
       , phi2MPiPi
       , zeroTW
       ) where

import           Linear.Metric (Metric (..))
import           Linear.V2     (V2 (..))
import           Linear.Vector (Additive (..))

-- | Two-dimensional vector type.
newtype TwoVector a = TwoVector { getVector :: V2 a } deriving (Eq, Ord, Show)

instance Functor TwoVector where
  fmap f (TwoVector v2) = TwoVector (fmap f v2)

instance Applicative TwoVector where
  pure a = TwoVector (V2 a a)
  TwoVector v2 <*> TwoVector v2' = TwoVector (v2 <*> v2')

instance Additive TwoVector where
  zero = pure 0

instance Metric TwoVector where
  TwoVector (V2 x y) `dot` TwoVector (V2 x' y') = x * x' + y * y'

instance Num a => Monoid (TwoVector a) where
  mempty = zero
  TwoVector v2 `mappend` TwoVector v2' = TwoVector (v2 ^+^ v2')

setXY :: a -> a -> TwoVector a
setXY x y = TwoVector (V2 x y)

-- | Angle in the interval [-pi, pi).
--
-- >>> phi2MPiPi (2 * pi)
-- 0.0
phi2MPiPi :: (Floating a, Ord a) => a -> a
phi2MPiPi x | x >= pi   = phi2MPiPi $! x - 2*pi
            | x < -pi   = phi2MPiPi $! x + 2*pi
            | otherwise = x

zeroTW :: Num a => TwoVector a
zeroTW = zero
