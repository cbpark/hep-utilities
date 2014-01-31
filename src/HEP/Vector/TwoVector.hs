-- | These modules are intended to be imported qualified, to avoid name clashes,
-- e.g.
--
-- > import qualified HEP.Vector.TwoVector as V2

module HEP.Vector.TwoVector
    ( phi2MPiPi
    ) where

-- | returns phi angle in the interval [-PI,PI).
phi2MPiPi :: Double -> Double
phi2MPiPi x | x >= pi   = phi2MPiPi (x - 2*pi)
            | x < -pi   = phi2MPiPi (x + 2*pi)
            | otherwise = x
