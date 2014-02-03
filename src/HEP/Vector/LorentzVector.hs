-- | These modules are intended to be imported qualified, to avoid name clashes,
-- e.g.
--
-- > import qualified HEP.Vector.LorentzVector as V4

module HEP.Vector.LorentzVector
    ( LorentzVector(..)
    , invariantMass
    , transverseMass
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
import qualified HEP.Vector.LorentzTransverseVector as TV

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

invariantMass :: Floating a => LorentzVector a -> a
invariantMass = norm

lorentzTVector :: Floating a => LorentzVector a -> TV.LorentzTransverseVector a
lorentzTVector (LorentzVector t x y z) =
    TV.LorentzTransverseVector (sqrt $ t*t - z*z) x y

transverseMass :: Floating a => LorentzVector a -> LorentzVector a -> a
transverseMass v v' = TV.transverseMass (lorentzTVector v) (lorentzTVector v')

spatialVector :: Num a => LorentzVector a -> V3.ThreeVector a
spatialVector (LorentzVector _ x y z) = V3.ThreeVector x y z

pseudoRapidity :: (Floating a, Ord a) => LorentzVector a -> a
pseudoRapidity = V3.pseudoRapidity . spatialVector

eta :: (Floating a, Ord a) => LorentzVector a -> a
eta = pseudoRapidity

phi :: RealFloat a => LorentzVector a -> a
phi = V3.phi . spatialVector

deltaPhi :: RealFloat a => LorentzVector a -> LorentzVector a -> a
deltaPhi v v' = V2.phi2MPiPi $ phi v - phi v'

deltaR :: RealFloat a => LorentzVector a -> LorentzVector a -> a
deltaR v v' = sqrt $ deta * deta + dphi * dphi
    where deta = eta v - eta v'
          dphi = deltaPhi v v'
