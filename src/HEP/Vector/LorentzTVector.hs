module HEP.Vector.LorentzTVector
    (
      LorentzTVector (..)

    , invariantMass
    ) where

import           Control.Applicative (Applicative (..))
import           Control.Lens        (view)
import           Linear.Metric       (Metric (..))
import           Linear.V3           (V3 (..), _x, _y, _z)
import           Linear.Vector       (Additive (..), (^+^))

newtype LorentzTVector a = LorentzTVector { getVector :: V3 a }
                         deriving (Eq, Ord, Show)

components :: LorentzTVector a -> (a, a, a)
components v = (t v, x v, y v)
  where t = view _x . getVector
        x = view _y . getVector
        y = view _z . getVector

instance Functor LorentzTVector where
  fmap f v = let (t, x, y) = components v
             in LorentzTVector (V3 (f t) (f x) (f y))

instance Applicative LorentzTVector where
  pure a = LorentzTVector (V3 a a a)
  v <*> v' = let (t , x , y ) = components v
                 (t', x', y') = components v'
             in LorentzTVector (V3 (t t') (x x') (y y'))

instance Additive LorentzTVector where
  zero = pure 0

instance Metric LorentzTVector where
  v `dot` v' = let (t , x , y ) = components v
                   (t', x', y') = components v'
               in t * t' - x * x' - y * y'

invariantMass :: Floating a => LorentzTVector a -> LorentzTVector a -> a
invariantMass v v' = norm (v ^+^ v')
