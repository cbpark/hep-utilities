-- | These modules are intended to be imported qualified, to avoid name clashes,
-- e.g.
--
-- > import qualified HEP.Vector.FourVector as V4

module HEP.Vector.FourVector
    ( FourVector(..)
    , invariantMass
    , pseudoRapidity
    , eta
    , phi
    , deltaPhi
    , deltaR
    ) where

import Control.Applicative
import Linear.Vector
import Linear.Metric

import qualified HEP.Vector.TwoVector as V2
import qualified HEP.Vector.ThreeVector as V3

data FourVector a = FourVector !a !a !a !a
                    deriving (Eq, Show, Ord, Read)

instance Functor FourVector where
    fmap f (FourVector t x y z) = FourVector (f t) (f x) (f y) (f z)

instance Applicative FourVector where
    pure a = FourVector a a a a
    FourVector t x y z <*> FourVector t' x' y' z' =
        FourVector (t t') (x x') (y y') (z z')

instance Num a => Num (FourVector a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (FourVector a) where
    recip = fmap recip
    (/) = liftA2 (/)
    fromRational = pure . fromRational

instance Metric FourVector where
    (FourVector t x y z) `dot` (FourVector t' x' y' z') =
        t * t' - x * x' - y * y' - z * z'

instance Additive FourVector where
    zero = pure 0
    liftU2 = liftA2
    liftI2 = liftA2

invariantMass :: FourVector Double -> Double
invariantMass = norm

spatialVector :: Num a => FourVector a -> V3.ThreeVector a
spatialVector (FourVector _ x y z) = V3.ThreeVector x y z

pseudoRapidity :: FourVector Double -> Double
pseudoRapidity = V3.pseudoRapidity . spatialVector

eta :: FourVector Double -> Double
eta = pseudoRapidity

phi :: FourVector Double -> Double
phi = V3.phi . spatialVector

deltaPhi :: FourVector Double -> FourVector Double -> Double
deltaPhi v v' = V2.phi2MPiPi $ phi v - phi v'

deltaR :: FourVector Double -> FourVector Double -> Double
deltaR v v' = sqrt $ deta * deta + dphi * dphi
    where deta = eta v - eta v'
          dphi = deltaPhi v v'
