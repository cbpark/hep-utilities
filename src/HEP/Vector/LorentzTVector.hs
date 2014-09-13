--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Vector.LorentzTVector
-- Copyright   :  (c) 2014 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- (2+1)-dimensional vector.
--
--------------------------------------------------------------------------------
module HEP.Vector.LorentzTVector
       ( -- * Type
         LorentzTVector (..)

         -- * Function
       , invariantMass
       ) where

import           Control.Applicative (Applicative (..))
import           Linear.Metric       (Metric (..))
import           Linear.V3           (V3 (..))
import           Linear.Vector       (Additive (..), (^+^))

-- | Type for (2+1)-dimensional vector.
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

-- | Invariant mass. It would be a transverse mass in (3+1)-dimensional
-- space.
invariantMass :: Floating a => LorentzTVector a -> LorentzTVector a -> a
invariantMass v v' = norm (v ^+^ v')
