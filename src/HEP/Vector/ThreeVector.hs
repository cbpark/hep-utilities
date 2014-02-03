-- | These modules are intended to be imported qualified, to avoid name clashes,
-- e.g.
--
-- > import qualified HEP.Vector.ThreeVector as V3

module HEP.Vector.ThreeVector
    ( ThreeVector(..)
    , cosTheta
    , pseudoRapidity
    , phi
    )  where

import Control.Applicative
import Linear.Vector
import Linear.Metric

data ThreeVector a = ThreeVector !a !a !a
                     deriving (Eq, Show, Ord, Read)

instance Functor ThreeVector where
    fmap f (ThreeVector x y z) = ThreeVector (f x) (f y) (f z)

instance Applicative ThreeVector where
    pure a = ThreeVector a a a
    ThreeVector x y z <*> ThreeVector x' y' z' =
        ThreeVector (x x') (y y') (z z')

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
    (ThreeVector x y z) `dot` (ThreeVector x' y' z') = x * x' + y * y' + z * z'

instance Additive ThreeVector where
    zero = pure 0
    liftU2 = liftA2
    liftI2 = liftA2

cosTheta :: (Floating a, Eq a) => ThreeVector a -> a
cosTheta v3@(ThreeVector _ _ z) = case ptot of
                                    0 -> 1
                                    _ -> z / ptot
    where ptot = norm v3

pseudoRapidity :: (Floating a, Ord a) => ThreeVector a -> a
pseudoRapidity v3@(ThreeVector _ _ z)
    | ct * ct < 1 = -0.5 * log ((1.0 - ct) / (1.0 + ct))
    | z > 0       = 1.0e10
    | otherwise   = -1.0e10
    where ct = cosTheta v3

-- | returns the azimuthal angle from -pi to pi.
phi :: (RealFloat a) => ThreeVector a -> a
phi (ThreeVector x y _) | x == 0 && y == 0 = 0
                        | otherwise        = atan2 x y
