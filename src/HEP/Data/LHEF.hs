{-# LANGUAGE RecordWildCards #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Data.LHEF
-- Copyright   :  (c) 2017 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- Helper functions to use in analyses of LHEF (Les Houches Event File) data files.
--
--------------------------------------------------------------------------------

module HEP.Data.LHEF
    (
      module LT
    , module LP
    , module LPU
    , module HK
    , module LV
    , module TV
    , module PI

    , energyOf
    , idOf
    , is
    , finalStates
    , initialStates
    , getDaughters
    , particlesFrom
    ) where

import           Control.Monad.Trans.Reader
import qualified Data.IntMap                          as M
import           HEP.Kinematics                       as HK
import           HEP.Kinematics.Vector.LorentzTVector as TV (setXYM)
import           HEP.Kinematics.Vector.LorentzVector  as LV (setEtaPhiPtM,
                                                             setXYZT)
import           HEP.Particle.ID                      as PI

import           HEP.Data.LHEF.Parser                 as LP
import           HEP.Data.LHEF.PipesUtil              as LPU (getLHEFEvent)
import           HEP.Data.LHEF.Type                   as LT

energyOf :: Particle -> Double
energyOf Particle { pup = (_, _, _, e, _) } = e

idOf :: Particle -> Int
idOf Particle { .. } = idup

is :: Particle -> ParticleType -> Bool
p `is` pid = ((`elem` getType pid) . abs . idup) p

initialStates :: Reader EventEntry [Particle]
initialStates = M.elems <$> asks (M.filter (\Particle { .. } -> fst mothup == 1))

finalStates :: Reader EventEntry [Particle]
finalStates = M.elems <$> asks (M.filter (\Particle { .. } -> istup == 1))

particlesFrom :: ParticleType -> Reader EventEntry [[Particle]]
particlesFrom pid = asks (M.keys . M.filter (`is` pid)) >>= mapM getDaughters

getDaughters :: Int -> Reader EventEntry [Particle]
getDaughters i = do
    pm <- ask
    daughters <- asks $ M.filter (\Particle { .. } -> fst mothup == i)
    return $ M.foldrWithKey
        (\k p acc -> case istup p of
                1 -> p : acc
                _ -> runReader (getDaughters k) pm ++ acc) []
        daughters
