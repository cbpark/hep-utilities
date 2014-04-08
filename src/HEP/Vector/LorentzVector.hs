{-# LANGUAGE DeriveFunctor  #-}

module HEP.Vector.LorentzVector
    ( LorentzVector(..)
    , invariantMass
    , transverseMass
    , eta
    , phi
    , deltaPhi
    , deltaR
    , boostVector
    ) where

import           HEP.Vector                (Metric (..), Vector (..))
import qualified HEP.Vector.LorentzTVector as TV
import qualified HEP.Vector.ThreeVector    as V3
import qualified HEP.Vector.TwoVector      as V2

import           Control.Applicative       (Applicative (..))

data LorentzVector a = LorentzVector !a !a !a !a
                       deriving (Eq, Show, Functor)

instance Applicative LorentzVector where
    pure a = LorentzVector a a a a
    LorentzVector t x y z <*> LorentzVector t' x' y' z' =
        LorentzVector (t t') (x x') (y y') (z z')

instance Metric LorentzVector where
    (LorentzVector t x y z) `dot` (LorentzVector t' x' y' z') =
        t * t' - x * x' - y * y' - z * z'

instance Vector LorentzVector where
    zero = pure 0

invariantMass :: Floating a => LorentzVector a -> a
invariantMass = norm

transverseMass :: Floating a => LorentzVector a -> LorentzVector a -> a
transverseMass v v' = TV.invariantMass (transverseV v) (transverseV v')
    where transverseV (LorentzVector t x y z) =
              TV.LorentzTVector (sqrt $ t*t - z*z) x y

spatialV :: Num a => LorentzVector a -> V3.ThreeVector a
spatialV (LorentzVector _ x y z) = V3.ThreeVector x y z

eta :: (Floating a, Ord a) => LorentzVector a -> a
eta = V3.pseudoRapidity . spatialV

phi :: RealFloat a => LorentzVector a -> a
phi = V3.phi . spatialV

deltaPhi :: RealFloat a => LorentzVector a -> LorentzVector a -> a
deltaPhi v v' = V2.phi2MPiPi $ phi v - phi v'

deltaR :: RealFloat a => LorentzVector a -> LorentzVector a -> a
deltaR v v' = sqrt $ deta * deta + dphi * dphi
    where deta = eta v - eta v'
          dphi = deltaPhi v v'

boostVector :: Fractional a => LorentzVector a -> V3.ThreeVector a
boostVector v@(LorentzVector t _ _ _) = spatialV v ./ t
