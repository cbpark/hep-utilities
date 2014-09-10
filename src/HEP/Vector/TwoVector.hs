module HEP.Vector.TwoVector
    (
      TwoVector (..)

    , phi2MPiPi
    ) where

import           Linear.V2 (V2 (..))

newtype TwoVector a = TwoVector { getVector :: V2 a }
                    deriving (Eq, Ord, Show)

-- | returns phi angle in the interval [-PI,PI).
phi2MPiPi :: (Floating a, Ord a) => a -> a
phi2MPiPi x | x >= pi   = phi2MPiPi (x - 2*pi)
            | x < -pi   = phi2MPiPi (x + 2*pi)
            | otherwise = x
