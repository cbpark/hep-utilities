{-# LANGUAGE DeriveFunctor #-}

module HEP.Vector.LorentzTVector
    (
      LorentzTVector(..)

    , invariantMass
    ) where

import           HEP.Vector          (Metric (..), Vector (..))

import           Control.Applicative (Applicative (..))

data LorentzTVector a = LorentzTVector !a !a !a
                        deriving (Eq, Show, Functor)

instance Applicative LorentzTVector where
    pure a = LorentzTVector a a a
    LorentzTVector t x y <*> LorentzTVector t' x' y' =
        LorentzTVector (t t') (x x') (y y')

instance Metric LorentzTVector where
    (LorentzTVector t x y) `dot` (LorentzTVector t' x' y') =
        t * t' - x * x' - y * y'

instance Vector LorentzTVector where
    zero = pure 0

invariantMass :: Floating a => LorentzTVector a -> LorentzTVector a -> a
invariantMass v v' = norm (v .+. v')
