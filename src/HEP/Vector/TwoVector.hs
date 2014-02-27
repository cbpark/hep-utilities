module HEP.Vector.TwoVector
    ( TwoVector(..)
    , phi2MPiPi
    ) where

import HEP.Vector

import Control.Applicative

data TwoVector a = TwoVector !a !a deriving (Eq, Show)

instance Functor TwoVector where
    fmap f (TwoVector x y) = TwoVector (f x) (f y)

instance Applicative TwoVector where
    pure a = TwoVector a a
    TwoVector x y <*> TwoVector x' y' = TwoVector (x x') (y y')

instance Num a => Num (TwoVector a) where
    (+)         = liftA2 (+)
    (-)         = liftA2 (-)
    (*)         = liftA2 (*)
    negate      = fmap negate
    abs         = fmap abs
    signum      = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (TwoVector a) where
    recip        = fmap recip
    (/)          = liftA2 (/)
    fromRational = pure . fromRational

instance Vector TwoVector where
    zero = pure 0

instance Metric TwoVector where
    (TwoVector x y) `dot` (TwoVector x' y') = x * x' + y * y'

-- | returns phi angle in the interval [-PI,PI).
phi2MPiPi :: (Floating a, Ord a) => a -> a
phi2MPiPi x | x >= pi   = phi2MPiPi (x - 2*pi)
            | x < -pi   = phi2MPiPi (x + 2*pi)
            | otherwise = x
