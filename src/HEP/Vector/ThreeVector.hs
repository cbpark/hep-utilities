{-# LANGUAGE MultiWayIf #-}

module HEP.Vector.ThreeVector
    (
      ThreeVector (..)

    , angle
    , cosTheta
    , pseudoRapidity
    , phi
    )  where

import           Data.Function (on)
import           Linear.Metric (Metric (..))
import           Linear.V3     (V3 (..))

newtype ThreeVector a = ThreeVector { getVector :: V3 a }
                      deriving (Eq, Ord, Show)

cosTheta :: (Floating a, Ord a) => ThreeVector a -> a
cosTheta v@(ThreeVector (V3 _ _ z)) = let ptot = (norm . getVector) v
                                      in if ptot > 0 then z / ptot else 1

-- | returns the angle of the 3-vector with respect to another 3-vector.
angle :: (Floating a, Ord a) => ThreeVector a -> ThreeVector a -> a
angle v v' = let ptot2 = ((*) `on` (quadrance . getVector)) v v'
             in if ptot2 <= 0
                then 0
                else let arg = (dot `on` getVector) v v' / sqrt ptot2
                     in if | arg >  1  -> 0
                           | arg < -1  -> pi
                           | otherwise -> acos arg

pseudoRapidity :: (Floating a, Ord a) => ThreeVector a -> a
pseudoRapidity v@(ThreeVector (V3 _ _ z))
  | ct * ct < 1 = -0.5 * log ((1.0 - ct) / (1.0 + ct))
  | z == 0      =  0
  | z >  0      =  1.0e10
  | otherwise   = -1.0e10
  where ct = cosTheta v

-- | returns the azimuthal angle from -pi to pi.
phi :: RealFloat a => ThreeVector a -> a
phi (ThreeVector (V3 x y _)) | x == 0 && y == 0 = 0
                             | otherwise        = atan2 x y
