{-# LANGUAGE DeriveFunctor #-}

module HEP.Vector.TwoVector
    (
      TwoVector(..)

    , phi2MPiPi
    ) where

import           HEP.Vector          (Metric (..), Vector (..))

import           Control.Applicative (Applicative (..))

data TwoVector a = TwoVector !a !a
                   deriving (Eq, Show, Functor)

instance Applicative TwoVector where
    pure a = TwoVector a a
    TwoVector x y <*> TwoVector x' y' = TwoVector (x x') (y y')

instance Vector TwoVector where
    zero = pure 0

instance Metric TwoVector where
    (TwoVector x y) `dot` (TwoVector x' y') = x * x' + y * y'

-- | returns phi angle in the interval [-PI,PI).
phi2MPiPi :: (Floating a, Ord a) => a -> a
phi2MPiPi x | x >= pi   = phi2MPiPi (x - 2*pi)
            | x < -pi   = phi2MPiPi (x + 2*pi)
            | otherwise = x
