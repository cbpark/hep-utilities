-- | These modules are intended to be imported qualified, to avoid name clashes,
-- e.g.
--
-- > import qualified HEP.Vector.TwoVector as V2

module HEP.Vector.TwoVector
    ( TwoVector(..)
    , phi2MPiPi
    ) where

import Control.Applicative
import Linear.Vector
import Linear.Metric

data TwoVector a = TwoVector !a !a
                   deriving (Eq, Show, Ord, Read)

instance Functor TwoVector where
    fmap f (TwoVector x y) = TwoVector (f x) (f y)

instance Applicative TwoVector where
    pure a = TwoVector a a
    TwoVector x y <*> TwoVector x' y' = TwoVector (x x') (y y')

instance Num a => Num (TwoVector a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (TwoVector a) where
    recip = fmap recip
    (/) = liftA2 (/)
    fromRational = pure . fromRational

instance Metric TwoVector where
    (TwoVector x y) `dot` (TwoVector x' y') = x * x' + y * y'

instance Additive TwoVector where
    zero = pure 0
    liftU2 = liftA2
    liftI2 = liftA2

-- | returns phi angle in the interval [-PI,PI).
phi2MPiPi :: (Floating a, Ord a) => a -> a
phi2MPiPi x | x >= pi   = phi2MPiPi (x - 2*pi)
            | x < -pi   = phi2MPiPi (x + 2*pi)
            | otherwise = x
