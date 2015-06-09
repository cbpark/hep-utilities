--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Kinematics.Vector.LorentzTVector
-- Copyright   :  (c) 2014 - 2015 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- (2+1)-dimensional vector.
--
--------------------------------------------------------------------------------
module HEP.Kinematics.Vector.LorentzTVector
       ( -- * Type
         LorentzTVector (..)

         -- * Function
       , setXYM
       , invariantMass
       ) where

import           Linear.Metric       (Metric (..))
import           Linear.V3           (V3 (..))
import           Linear.Vector       (Additive (..))

-- | Type for (2+1)-dimensional vector.
newtype LorentzTVector a = LorentzTVector { getVector :: V3 a }
                         deriving (Eq, Ord, Show)

instance Functor LorentzTVector where
  fmap f (LorentzTVector v3) = LorentzTVector (fmap f v3)

instance Applicative LorentzTVector where
  pure a = LorentzTVector (V3 a a a)
  LorentzTVector v3 <*> LorentzTVector v3' = LorentzTVector (v3 <*> v3')

instance Additive LorentzTVector where
  zero = pure 0

instance Metric LorentzTVector where
  (LorentzTVector (V3 t x y)) `dot` (LorentzTVector (V3 t' x' y')) =
    t * t' - x * x' - y * y'

-- | Makes 'LorentzTVector' out of components based on x, y, m coordinates.
setXYM :: Floating a => a -> a -> a -> LorentzTVector a
setXYM px py m = LorentzTVector $ V3 (sqrt $ px ** 2 + py ** 2 + m ** 2) px py

-- | Invariant mass. It would be a transverse mass in (3+1)-dimensional space.
invariantMass :: Floating a => LorentzTVector a -> LorentzTVector a -> a
invariantMass v v' = norm (v ^+^ v')
