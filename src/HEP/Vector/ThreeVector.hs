-- | These modules are intended to be imported qualified, to avoid name clashes,
-- e.g.
--
-- > import qualified HEP.Vector.ThreeVector as TV

module HEP.Vector.ThreeVector
    ( ThreeVector(..)
    , cosTheta
    , pseudoRapidity
    , phi
    )  where

import Control.Applicative
import Linear.Vector
import Linear.Metric

data ThreeVector a = ThreeVector !a !a !a deriving (Eq, Show, Ord, Read)

instance Functor ThreeVector where
    fmap f (ThreeVector a b c) = ThreeVector (f a) (f b) (f c)

instance Applicative ThreeVector where
    pure a = ThreeVector a a a
    ThreeVector a b c <*> ThreeVector d e f = ThreeVector (a d) (b e) (c f)

instance Num a => Num (ThreeVector a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (ThreeVector a) where
    recip = fmap recip
    (/) = liftA2 (/)
    fromRational = pure . fromRational

instance Metric ThreeVector where
    (ThreeVector a b c) `dot` (ThreeVector d e f) = a * d + b * e + c * f

instance Additive ThreeVector where
    zero = pure 0
    liftU2 = liftA2
    liftI2 = liftA2

cosTheta :: ThreeVector Double -> Double
cosTheta v3@(ThreeVector _ _ z) = case ptot of
                                    0 -> 1
                                    _   -> z / ptot
    where ptot = norm v3

pseudoRapidity :: ThreeVector Double -> Double
pseudoRapidity v3@(ThreeVector _ _ z)
    | ct * ct < 1 = -0.5 * log ((1.0 - ct) / (1.0 + ct))
    | z > 0       = 1.0e10
    | otherwise   = -1.0e10
    where ct = cosTheta v3

-- | returns the azimuthal angle from -pi to pi.
phi :: ThreeVector Double -> Double
phi (ThreeVector x y _) | x == 0 && y == 0 = 0
                        | otherwise        = atan2 x y
