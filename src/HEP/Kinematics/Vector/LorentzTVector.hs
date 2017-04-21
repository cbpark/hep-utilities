--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Kinematics.Vector.LorentzTVector
-- Copyright   :  (c) 2014-2017 Chan Beom Park
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
      LorentzTVector

      -- * Function
    , setXYM
    , setXYT
    , invariantMass
    ) where

import           Control.Applicative

import           Linear.Metric       (Metric (..))
import           Linear.V2           (V2 (..))
import           Linear.V3           (R1 (..), R2 (..), V3 (..))
import           Linear.Vector       (Additive (..))

-- | Type for (2+1)-dimensional vector.
newtype LorentzTVector a = LorentzTVector (V3 a) deriving (Eq, Show)

instance Num a => Num (LorentzTVector a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

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

instance R1 LorentzTVector where
    _x f (LorentzTVector (V3 t x y)) = (\x' -> LorentzTVector (V3 t x' y)) <$> f x

instance R2 LorentzTVector where
    _y f (LorentzTVector (V3 t x y)) = (LorentzTVector . V3 t x) <$> f y
    _xy f (LorentzTVector (V3 t x y)) =
        (\(V2 x' y') -> LorentzTVector (V3 t x' y')) <$> f (V2 x y)

-- | Makes 'LorentzTVector' out of components based on x, y, t coordinates.
setXYT :: a -> a -> a -> LorentzTVector a
setXYT px py et = LorentzTVector (V3 et px py)

-- | Makes 'LorentzTVector' out of components based on x, y, m coordinates.
setXYM :: Floating a => a -> a -> a -> LorentzTVector a
setXYM px py m = let et = sqrt $ px ** 2 + py ** 2 + m ** 2
                 in setXYT px py et

-- | Invariant mass. It would be a transverse mass in (3+1)-dimensional space.
invariantMass :: Floating a => LorentzTVector a -> LorentzTVector a -> a
invariantMass v v' = norm (v ^+^ v')
