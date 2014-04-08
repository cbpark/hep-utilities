{-# LANGUAGE DeriveFunctor #-}

module HEP.Vector.ThreeVector
    ( ThreeVector(..)
    , cosTheta
    , pseudoRapidity
    , phi
    )  where

import           HEP.Vector          (Metric (..), Vector (..))

import           Control.Applicative (Applicative (..))

data ThreeVector a = ThreeVector !a !a !a
                     deriving (Eq, Show, Functor)

instance Applicative ThreeVector where
    pure a = ThreeVector a a a
    ThreeVector x y z <*> ThreeVector x' y' z' = ThreeVector (x x') (y y') (z z')

instance Vector ThreeVector where
    zero = pure 0

instance Metric ThreeVector where
    (ThreeVector x y z) `dot` (ThreeVector x' y' z') = x * x' + y * y' + z * z'

cosTheta :: (Floating a, Ord a) => ThreeVector a -> a
cosTheta v3@(ThreeVector _ _ z) = if ptot > 0 then z / ptot else 1
    where ptot = norm v3

pseudoRapidity :: (Floating a, Ord a) => ThreeVector a -> a
pseudoRapidity v3@(ThreeVector _ _ z)
    | ct * ct < 1 = -0.5 * log ((1.0 - ct) / (1.0 + ct))
    | z > 0       =  1.0e10
    | otherwise   = -1.0e10
    where ct = cosTheta v3

-- | returns the azimuthal angle from -pi to pi.
phi :: (RealFloat a) => ThreeVector a -> a
phi (ThreeVector x y _) | x == 0 && y == 0 = 0
                        | otherwise        = atan2 x y
