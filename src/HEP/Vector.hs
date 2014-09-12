module HEP.Vector (HasFourMomentum (..)) where

import           Data.Function            (on)

import           HEP.Vector.LorentzVector (LorentzVector)
import qualified HEP.Vector.LorentzVector as LV
import           HEP.Vector.TwoVector     (phi2MPiPi)

class HasFourMomentum a where
  fourMomentum :: a -> LorentzVector Double
  pt :: a -> Double
  eta :: a -> Double
  phi :: a -> Double

  invarintMass :: [a] -> Double
  invarintMass = LV.invariantMass . momentumSum

  ptCompare :: a -> a -> Ordering
  ptCompare = flip compare `on` pt

  ptSum :: [a] -> Double
  ptSum = foldr (\p acc -> pt p + acc) 0

  momentumSum :: [a] -> LorentzVector Double
  momentumSum = LV.vectorSum . map fourMomentum

  deltaEta :: a -> a -> Double
  deltaEta = (-) `on` eta

  deltaPhi :: a -> a -> Double
  deltaPhi p p' = phi2MPiPi $ phi p - phi p'

  deltaR :: a -> a -> Double
  deltaR p p' = sqrt $ deta * deta + dphi * dphi
    where deta = deltaEta p p'
          dphi = deltaPhi p p'

  cosTheta :: a -> a -> Double
  cosTheta p p' = cos $ (LV.deltaTheta `on` fourMomentum) p p'
