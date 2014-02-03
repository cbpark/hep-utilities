-- | These modules are intended to be imported qualified, to avoid name clashes,
-- e.g.
--
-- > import qualified HEP.Vector.LorentzVector as V4

module HEP.Vector.LorentzVector
    ( LorentzVector(..)
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

data LorentzVector a = LorentzVector !a !a !a !a
                    deriving (Eq, Show, Ord, Read)

instance Functor LorentzVector where
    fmap f (LorentzVector t x y z) = LorentzVector (f t) (f x) (f y) (f z)

instance Applicative LorentzVector where
    pure a = LorentzVector a a a a
    LorentzVector t x y z <*> LorentzVector t' x' y' z' =
        LorentzVector (t t') (x x') (y y') (z z')

instance Num a => Num (LorentzVector a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (LorentzVector a) where
    recip = fmap recip
    (/) = liftA2 (/)
    fromRational = pure . fromRational

instance Metric LorentzVector where
    (LorentzVector t x y z) `dot` (LorentzVector t' x' y' z') =
        t * t' - x * x' - y * y' - z * z'

instance Additive LorentzVector where
    zero = pure 0
    liftU2 = liftA2
    liftI2 = liftA2

invariantMass :: LorentzVector Double -> Double
invariantMass = norm

spatialVector :: Num a => LorentzVector a -> V3.ThreeVector a
spatialVector (LorentzVector _ x y z) = V3.ThreeVector x y z

pseudoRapidity :: LorentzVector Double -> Double
pseudoRapidity = V3.pseudoRapidity . spatialVector

eta :: LorentzVector Double -> Double
eta = pseudoRapidity

phi :: LorentzVector Double -> Double
phi = V3.phi . spatialVector

deltaPhi :: LorentzVector Double -> LorentzVector Double -> Double
deltaPhi v v' = V2.phi2MPiPi $ phi v - phi v'

deltaR :: LorentzVector Double -> LorentzVector Double -> Double
deltaR v v' = sqrt $ deta * deta + dphi * dphi
    where deta = eta v - eta v'
          dphi = deltaPhi v v'
