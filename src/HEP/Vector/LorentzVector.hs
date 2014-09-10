module HEP.Vector.LorentzVector
    (
      LorentzVector (..)

    , invariantMass
    , transverseMass
    , eta
    , phi
    , pT
    , deltaPhi
    , deltaR
    , deltaTheta
    , boostVector
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.Function             (on)
import           Linear.Metric
import           Linear.V2
import           Linear.V3
import           Linear.V4
import           Linear.Vector

import qualified HEP.Vector.LorentzTVector as TV
import qualified HEP.Vector.ThreeVector    as V3
import qualified HEP.Vector.TwoVector      as V2

newtype LorentzVector a = LorentzVector { getVector :: V4 a }
                        deriving (Eq, Ord, Show)

components :: LorentzVector a -> (a, a, a, a)
components v = (t v, x v, y v, z v)
  where t = view _x . getVector
        x = view _y . getVector
        y = view _z . getVector
        z = view _w . getVector

instance Functor LorentzVector where
  fmap f v = let (t, x, y, z) = components v
             in LorentzVector (V4 (f t) (f x) (f y) (f z))

instance Applicative LorentzVector where
  pure a = LorentzVector (V4 a a a a)
  v <*> v' = let (t, x, y, z)     = components v
                 (t', x', y', z') = components v'
             in LorentzVector (V4 (t t') (x x') (y y') (z z'))

instance Additive LorentzVector where
  zero = pure 0

instance Metric LorentzVector where
  v `dot` v' = let (t, x, y, z)     = components v
                   (t', x', y', z') = components v'
               in t * t' - x * x' - y * y' - z * z'

invariantMass :: Floating a => LorentzVector a -> a
invariantMass = norm

transverseMass :: Floating a => LorentzVector a -> LorentzVector a -> a
transverseMass = TV.invariantMass `on` transverseV
    where transverseV v = let (t, x, y, z) = components v
                          in TV.LorentzTVector (V3 (sqrt $ t * t - z * z) x y)

transV :: LorentzVector a -> V2.TwoVector a
transV v = let (_, x, y, _) = components v
           in V2.TwoVector (V2 x y)

pT :: Floating a => LorentzVector a -> a
pT = norm . V2.getVector . transV

spatialV :: Num a => LorentzVector a -> V3.ThreeVector a
spatialV v = let (_, x, y, z) = components v
             in V3.ThreeVector (V3 x y z)

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

deltaTheta :: (Floating a, Ord a) => LorentzVector a -> LorentzVector a -> a
deltaTheta = V3.angle `on` spatialV

boostVector :: Fractional a => LorentzVector a -> V3.ThreeVector a
boostVector v = let e = view _x (getVector v)
                in V3.ThreeVector ((V3.getVector . spatialV) v ^/ e)
