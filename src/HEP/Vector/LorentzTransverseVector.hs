module HEP.Vector.LorentzTransverseVector
    ( LorentzTransverseVector(..)
    , invariantMass
    ) where

import HEP.Vector

import Control.Applicative

data LorentzTransverseVector a = LorentzTransverseVector !a !a !a
                                 deriving (Eq, Show)

instance Functor LorentzTransverseVector where
    fmap f (LorentzTransverseVector t x y) =
        LorentzTransverseVector (f t) (f x) (f y)

instance Applicative LorentzTransverseVector where
    pure a = LorentzTransverseVector a a a
    LorentzTransverseVector t x y <*> LorentzTransverseVector t' x' y' =
        LorentzTransverseVector (t t') (x x') (y y')

instance Monad LorentzTransverseVector where
    return a = LorentzTransverseVector a a a
    LorentzTransverseVector t x y >>= f = LorentzTransverseVector t' x' y'
        where LorentzTransverseVector t' _  _  = f t
              LorentzTransverseVector _  x' _  = f x
              LorentzTransverseVector _  _  y' = f y

instance Num a => Num (LorentzTransverseVector a) where
    (+)         = liftA2 (+)
    (*)         = liftA2 (*)
    (-)         = liftA2 (-)
    negate      = fmap negate
    abs         = fmap abs
    signum      = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (LorentzTransverseVector a) where
    recip        = fmap recip
    (/)          = liftA2 (/)
    fromRational = pure . fromRational

instance Metric LorentzTransverseVector where
    (LorentzTransverseVector t x y) `dot` (LorentzTransverseVector t' x' y') =
        t * t' - x * x' - y * y'

instance Vector LorentzTransverseVector where
    zero = pure 0

invariantMass :: Floating a =>
                 LorentzTransverseVector a ->
                 LorentzTransverseVector a -> a
invariantMass v v' = norm (v .+. v')
