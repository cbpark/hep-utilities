module HEP.Kinematics.Variable.MAOS where

import           Data.Maybe                          (mapMaybe)

import           HEP.Kinematics
import           HEP.Kinematics.Vector.LorentzVector (setXYZT)

-- |
-- calculates the possible momentum solutions for the decay topology of
-- parent --> visible invisible, where the longitudinal momentum of invisible
-- particle is unknown.
--
-- See Eq. (3) in <http://arxiv.org/abs/0810.4853 arXiv:0810.4853>.
momentumSolution :: FourMomentum        -- ^ four-momentum of visible particle
                 -> TransverseMomentum  -- ^ transverse momentum of invisible particle
                 -> Double              -- ^ mass of the parent particle
                 -> Double              -- ^ mass of the invisible particle
                 -> [FourMomentum]
momentumSolution vis invis mY mX = let (kx, ky) = pxpy invis
                                       kz = longitudinalP vis invis mY mX
                                   in mapMaybe (setMomentum kx ky) kz
  where setMomentum _ _ Nothing  = Nothing
        setMomentum x y (Just z) = let t = sqrt $! x ** 2 + y ** 2 + z ** 2 + mX ** 2
                                   in Just (setXYZT x y z t)

longitudinalP :: FourMomentum -> TransverseMomentum -> Double -> Double
              -> [Maybe Double]
longitudinalP vis invis mY mX =
  let visMass = mass vis
      visTrans = transverseVector vis
      disc' = 0.5 * (mY ** 2 - mX ** 2 - visMass ** 2) + visTrans `dot` invis
      visEt = transverseEnergy vis
      invisEt = sqrt $! pt invis ** 2 + mX ** 2
      disc = disc' ** 2 - (visEt * invisEt) ** 2
  in if disc < 0
     then [Nothing]
     else let term1 = pz vis * disc'
              term2 = energy vis * sqrt disc
          in map (Just . (/ (visEt ** 2))) [term1 + term2, term1 - term2]
