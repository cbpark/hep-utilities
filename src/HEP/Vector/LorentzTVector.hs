module HEP.Vector.LorentzTVector
    (
      LorentzTVector (..)

    , invariantMass
    ) where

import           Control.Applicative (Applicative (..))
import           Linear.Metric       (Metric (..))
import           Linear.V3           (V3 (..))
import           Linear.Vector       (Additive (..), (^+^))

newtype LorentzTVector a = LorentzTVector { getVector :: V3 a }
                         deriving (Eq, Ord, Show)

instance Functor LorentzTVector where
  fmap f (LorentzTVector (V3 t x y)) = LorentzTVector (V3 (f t) (f x) (f y))

instance Applicative LorentzTVector where
  pure a = LorentzTVector (V3 a a a)
  (LorentzTVector (V3 t x y)) <*> (LorentzTVector (V3 t' x' y')) =
    LorentzTVector (V3 (t t') (x x') (y y'))

instance Additive LorentzTVector where
  zero = pure 0

instance Metric LorentzTVector where
  (LorentzTVector (V3 t x y)) `dot` (LorentzTVector (V3 t' x' y')) =
    t * t' - x * x' - y * y'

invariantMass :: Floating a => LorentzTVector a -> LorentzTVector a -> a
invariantMass v v' = norm (v ^+^ v')
