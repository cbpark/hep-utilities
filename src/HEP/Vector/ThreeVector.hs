-- | These modules are intended to be imported qualified, to avoid name clashes,
-- e.g.
--
-- > import qualified HEP.Vector.ThreeVector as TV

module HEP.Vector.ThreeVector
    ( ThreeVector(..)
    , add
    , multiply
    , dot
    , cosTheta
    , pseudoRapidity
    , phi
    ) where

data ThreeVector = ThreeVector
    { vx :: Double
    , vy :: Double
    , vz :: Double
    } deriving (Show, Eq)

add :: ThreeVector -> ThreeVector -> ThreeVector
v `add` v' = ThreeVector { vx = vx v + vx v'
                         , vy = vy v + vy v'
                         , vz = vz v + vz v'
                         }

infixl 6 `add`

multiply :: Double -> ThreeVector -> ThreeVector
d `multiply` (ThreeVector {vx=vx', vy=vy', vz=vz'}) =
    ThreeVector { vx = d * vx', vy = d * vy', vz = d * vz' }

infixl 7 `multiply`

dot :: ThreeVector -> ThreeVector -> Double
v `dot` v' = vx v * vx v' + vy v * vy v' + vz v * vz v'

cosTheta :: ThreeVector -> Double
cosTheta v = case ptot of
               0.0 -> 1.0
               _   -> vz v / ptot
    where ptot = sqrt $ v `dot` v

pseudoRapidity :: ThreeVector -> Double
pseudoRapidity v
    | ct * ct < 1 = -0.5 * log ((1.0 - ct) / (1.0 + ct))
    | vz v > 0    =  1.0e10
    | otherwise   = -1.0e10
    where ct = cosTheta v

-- | returns the azimuthal angle from -pi to pi.
phi :: ThreeVector -> Double
phi v | fx == 0.0 && fy == 0.0 = 0.0
      | otherwise              = atan2 fx fy
      where fx = vx v
            fy = vy v
