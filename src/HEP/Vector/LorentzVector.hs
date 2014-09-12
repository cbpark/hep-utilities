module HEP.Vector.LorentzVector
    (
      LorentzVector (..)

    , setXYZT
    , setEtaPhiPtM
    , vectorSum
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

import           Control.Applicative       (Applicative (..))
import           Data.Foldable             (Foldable (..))
import           Data.Function             (on)
import           Linear.Metric             (Metric (..))
import           Linear.V2                 (V2 (..))
import           Linear.V3                 (V3 (..))
import           Linear.V4                 (V4 (..))
import           Linear.Vector             (Additive (..), sumV, (^/))

import qualified HEP.Vector.LorentzTVector as TV
import           HEP.Vector.ThreeVector    (ThreeVector)
import qualified HEP.Vector.ThreeVector    as V3
import           HEP.Vector.TwoVector      (TwoVector)
import qualified HEP.Vector.TwoVector      as V2

newtype LorentzVector a = LorentzVector { getVector :: V4 a }
                        deriving (Eq, Ord, Show)

setXYZT :: a -> a -> a -> a -> LorentzVector a
setXYZT px' py' pz' e' = LorentzVector (V4 e' px' py' pz')

setEtaPhiPtM :: Floating a => a -> a -> a -> a -> LorentzVector a
setEtaPhiPtM eta' phi' pt' m' = LorentzVector (V4 e px py pz)
  where e = sqrt $ px * px + py * py + pz * pz + m' * m'
        px = pt' * cos phi'
        py = pt' * sin phi'
        pz = pt' * sinh eta'

instance Functor LorentzVector where
  fmap f (LorentzVector (V4 t x y z)) = LorentzVector (V4 (f t) (f x) (f y) (f z))

instance Applicative LorentzVector where
  pure a = LorentzVector (V4 a a a a)
  (LorentzVector (V4 t x y z)) <*> (LorentzVector (V4 t' x' y' z')) =
    LorentzVector (V4 (t t') (x x') (y y') (z z'))

instance Additive LorentzVector where
  zero = pure 0

instance Metric LorentzVector where
  (LorentzVector (V4 t x y z)) `dot` (LorentzVector (V4 t' x' y' z')) =
    t * t' - x * x' - y * y' - z * z'

vectorSum :: (Foldable f, Functor f, Num a)
             => f (LorentzVector a) -> LorentzVector a
vectorSum = LorentzVector . sumV . fmap getVector

invariantMass :: Floating a => LorentzVector a -> a
invariantMass = norm

transverseMass :: Floating a => LorentzVector a -> LorentzVector a -> a
transverseMass = TV.invariantMass `on` transverseV
    where transverseV (LorentzVector (V4 t x y z)) =
            TV.LorentzTVector (V3 (sqrt $ t * t - z * z) x y)

transV :: LorentzVector a -> TwoVector a
transV (LorentzVector (V4 _ x y _)) = V2.TwoVector (V2 x y)

pT :: Floating a => LorentzVector a -> a
pT = norm . V2.getVector . transV

spatialV :: Num a => LorentzVector a -> ThreeVector a
spatialV (LorentzVector (V4 _ x y z)) = V3.ThreeVector (V3 x y z)

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

boostVector :: Fractional a => LorentzVector a -> ThreeVector a
boostVector v@(LorentzVector (V4 t _ _ _)) =
  V3.ThreeVector ((V3.getVector . spatialV) v ^/ t)
