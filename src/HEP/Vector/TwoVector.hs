--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Vector.TwoVector
-- Copyright   :  (c) 2014 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- Two-dimensional vector.
--
--------------------------------------------------------------------------------
module HEP.Vector.TwoVector
       ( -- * Type
         TwoVector (..)

         -- * Function
       , phi2MPiPi
       ) where

import           Linear.V2 (V2)

-- | Two-dimensional vector type.
newtype TwoVector a = TwoVector { getVector :: V2 a }
                    deriving (Eq, Ord, Show)

-- | returns phi angle in the interval [-PI,PI).
phi2MPiPi :: (Floating a, Ord a) => a -> a
phi2MPiPi x | x >= pi   = phi2MPiPi $! x - 2*pi
            | x < -pi   = phi2MPiPi $! x + 2*pi
            | otherwise = x
