--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Data.LHEF.Type
-- Copyright   :  (c) 2019 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- Types for LHEF event data.
--
-- See <http://arxiv.org/abs/hep-ph/0609017 A standard format for Les Houches Event Files>.
--
--------------------------------------------------------------------------------

module HEP.Data.LHEF.Type where

import Data.IntMap                         (IntMap)

import HEP.Kinematics                      (HasFourMomentum (..))
import HEP.Kinematics.Vector.LorentzVector (setXYZT)

data EventInfo = EventInfo
                 { -- | Number of particle entries in the event.
                   nup    :: {-# UNPACK #-} !Int
                   -- | ID of the process for the event.
                 , idprup :: Int
                   -- | Event weight.
                 , xwgtup :: Double
                   -- | Scale of the event in GeV.
                 , scalup :: Double
                   -- | The QED coupling \alpha_{QED} used for the event.
                 , aqedup :: Double
                   -- | The QCD coupling \alpha_{QCD} used for the event.
                 , aqcdup :: Double
                 } deriving Show

data Particle = Particle
                { -- | Particle ID according to Particle Data Group convention.
                  idup   :: {-# UNPACK #-} !Int
                  -- | Status code.
                , istup  :: {-# UNPACK #-} !Int
                  -- | Index of first and last mother.
                , mothup :: {-# UNPACK #-} !(Int, Int)
                  -- | Integer tag for the color flow line passing through the
                  -- (anti-)color of the particle.
                , icolup :: (Int, Int)
                  -- | Lab frame momentum (P_x, P_y, P_z, E, M) of particle in GeV.
                , pup    :: {-# UNPACK #-} !(Double, Double, Double, Double, Double)
                  -- | Invariant lifetime (distance from production to decay) in mm.
                , vtimup :: Double
                  -- | Consine of the angle between the spin-vector of particle and
                  -- the three-momentum of the decaying particle, specified in the
                  -- lab frame.
                , spinup :: Double
                } deriving Show

instance HasFourMomentum Particle where
    -- fourMomentum :: Particle -> FourMomentum
    fourMomentum Particle { pup = (x, y, z, e, _) } = setXYZT x y z e
    {-# INLINE fourMomentum #-}

    pt Particle { pup = (x, y, _, _, _) } = sqrt (x ** 2 + y ** 2)
    {-# INLINE pt #-}

    mass Particle { pup = (_, _, _, _, m) } = m
    {-# INLINE mass #-}

    epxpypz Particle { pup = (x, y, z, e, _) } = (e, x, y, z)
    {-# INLINE epxpypz #-}

    pxpypz Particle { pup = (x, y, z, _, _) } = (x, y, z)
    {-# INLINE pxpypz #-}

    pxpy Particle { pup = (x, y, _, _, _) } = (x, y)
    {-# INLINE pxpy #-}

    px Particle { pup = (x, _, _, _, _) } = x
    {-# INLINE px #-}

    py Particle { pup = (_, y, _, _, _) } = y
    {-# INLINE py #-}

    pz Particle { pup = (_, _, z, _, _) } = z
    {-# INLINE pz #-}

    energy Particle { pup = (_, _, _, e, _) } = e
    {-# INLINE energy #-}

type EventEntry = IntMap Particle

type Event = (EventInfo, EventEntry)

newtype ParticleType = ParticleType { getType :: [Int] } deriving (Show, Eq)
