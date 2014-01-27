-- | These modules are intended to be imported qualified, to avoid name clashes,
-- e.g.
--
-- > import qualified HEP.Vector.FourVector as FV

module HEP.Vector.FourVector
    ( FourVector(..)
    , add
    , multiply
    , subtract
    , dot
    , invariantMass
    , boostVector
    , pseudoRapidity
    , eta
    , phi
    ) where

import Prelude hiding (negate, subtract)
import qualified HEP.Vector.ThreeVector as TV

data FourVector = FourVector
    { vt :: Double
    , vx :: Double
    , vy :: Double
    , vz :: Double } deriving (Show, Eq)

add :: FourVector -> FourVector -> FourVector
v `add` v' = FourVector { vt = vt v + vt v'
                        , vx = vx v + vx v'
                        , vy = vy v + vy v'
                        , vz = vz v + vz v'
                        }

infixl 6 `add`

multiply :: Double -> FourVector -> FourVector
d `multiply` (FourVector {vt=vt', vx=vx', vy=vy', vz=vz'}) =
    FourVector { vt = d * vt', vx = d * vx', vy = d * vy', vz = d * vz' }

infixl 7 `multiply`

negate :: FourVector -> FourVector
negate = ((-1) `multiply`)

subtract :: FourVector -> FourVector -> FourVector
v `subtract` v' = v `add` negate v'

dot :: FourVector -> FourVector -> Double
v `dot` v' = vt v * vt v' -
             vx v * vx v' -
             vy v * vy v' -
             vz v * vz v'

normSq :: FourVector -> Double
normSq v =  v `dot` v

invariantMass :: FourVector -> FourVector -> Double
invariantMass v v' = (sqrt . normSq) (v `add` v')

spatialVector :: FourVector -> TV.ThreeVector
spatialVector v = TV.ThreeVector { TV.vx = vx v
                                 , TV.vy = vy v
                                 , TV.vz = vz v}

boostVector :: FourVector -> TV.ThreeVector
boostVector v = (1 / vt v) `TV.multiply` spatialVector v

pseudoRapidity :: FourVector -> Double
pseudoRapidity = TV.pseudoRapidity . spatialVector

eta :: FourVector -> Double
eta = pseudoRapidity

phi :: FourVector -> Double
phi = TV.phi . spatialVector
