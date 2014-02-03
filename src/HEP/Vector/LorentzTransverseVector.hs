-- | These modules are intended to be imported qualified, to avoid name clashes,
-- e.g.
--
-- > import qualified HEP.Vector.LorentzTransverseVector as TV

module HEP.Vector.LorentzTransverseVector
    ( LorentzTransverseVector(..)
    , invariantMass
    ) where

import Control.Applicative
import Linear.Vector
import Linear.Metric

data LorentzTransverseVector a = LorentzTransverseVector !a !a !a
                                 deriving (Eq, Show, Ord, Read)

instance Functor LorentzTransverseVector where
    fmap f (LorentzTransverseVector t x y) =
        LorentzTransverseVector (f t) (f x) (f y)

instance Applicative LorentzTransverseVector where
    pure a = LorentzTransverseVector a a a
    LorentzTransverseVector t x y <*> LorentzTransverseVector t' x' y' =
        LorentzTransverseVector (t t') (x x') (y y')

instance Num a => Num (LorentzTransverseVector a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (LorentzTransverseVector a) where
    recip = fmap recip
    (/) = liftA2 (/)
    fromRational = pure . fromRational

instance Metric LorentzTransverseVector where
    (LorentzTransverseVector t x y) `dot` (LorentzTransverseVector t' x' y') =
        t * t' - x * x' - y * y'

instance Additive LorentzTransverseVector where
    zero = pure 0
    liftU2 = liftA2
    liftI2 = liftA2

invariantMass :: Floating a =>
                  LorentzTransverseVector a
               -> LorentzTransverseVector a
               -> a
invariantMass v v' = norm $ v ^+^ v'
