module HEP.Kinematics.Variable.Internal where

import HEP.Kinematics
import HEP.Kinematics.Variable.MAOS (SolutionType (..), maosMomenta)
import HEP.Kinematics.Variable.MT2  (mT2)

mT2Symm :: FourMomentum
        -> FourMomentum
        -> TransverseMomentum
        -> Double
        -> Double
mT2Symm vis1 vis2 miss mInv = mT2 vis1 vis2 miss mInv mInv 0 True

maosMomentaSymmetric :: Double
                     -> FourMomentum
                     -> FourMomentum
                     -> TransverseMomentum
                     -> Double
                     -> Double
                     -> ([FourMomentum], [FourMomentum], SolutionType)
maosMomentaSymmetric mT2value vis1 vis2 miss mY mX =
    maosMomenta mT2value (vis1, mY, mX) (vis2, mY, mX) miss
