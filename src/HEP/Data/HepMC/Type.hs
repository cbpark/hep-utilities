{-# LANGUAGE StrictData #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Data.HepMC.Type
-- Copyright   :  (c) 2017 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- Types for HepMC event data.
--
-- See <http://hepmc.web.cern.ch/hepmc/releases/HepMC2_user_manual.pdf HepMC 2 user manual>.
--
--------------------------------------------------------------------------------

module HEP.Data.HepMC.Type where

import Data.ByteString.Char8               (ByteString)
import HEP.Kinematics                      (HasFourMomentum (..))
import HEP.Kinematics.Vector.LorentzVector (setXYZT)

type Version = ByteString

data GenEvent = GenEvent { -- | event number
                           eventNumber         :: Int
                           -- | number of multi paricle interactions
                         , numMultiParticleInt :: Int
                           -- | event scale
                         , eventScale          :: Double
                           -- | \alpha_{QCD}
                         , alphaQCD            :: Double
                           -- | \alpha_{QED}
                         , alphaQED            :: Double
                           -- | signal process id
                         , signalProcId        :: Int
                           -- | barcode for signal process vertex
                         , signalProcVertex    :: Int
                           -- | number of vertices in this event
                         , numVertex           :: Int
                           -- | barcode for beam particles (1, 2)
                         , beamParticles       :: (Int, Int)
                           -- | random state list (may be zero)
                         , randomStateList     :: Maybe (Int, [Int])
                           -- | weight list (may be zero)
                         , weightList          :: Maybe (Int, [Double])
                         , eventHeader         :: EventHeader
                         , vertices            :: [GenVertex]
                         } deriving Show

data EventHeader = EventHeader { weightInfo       :: Maybe WeightNames
                               , unitInfo         :: Maybe MomentumPositionUnit
                               , crossSectionInfo :: Maybe GenCrossSection
                               , heavyIonInfo     :: Maybe HeavyIon
                               , pdfInfo          :: Maybe PdfInfo
                               } deriving Show

data WeightNames = WeightNames { -- | number of entries in weight name list
                                 numEntries  :: Int
                                 -- | list of weight names enclosed in quotes
                               , weightNames :: [ByteString]
                               } deriving Show

data MomentumUnit = GeV | MeV deriving (Eq, Show)
data LengthUnit = MM | CM deriving (Eq, Show)

data MomentumPositionUnit = MomentumPositionUnit
                            { -- | momentum units (MEV or GEV)
                              momentumUnit :: MomentumUnit
                              -- | length units (MM or CM)
                            , lengthUnit   :: LengthUnit
                            } deriving (Eq, Show)

data GenCrossSection = GenCrossSection
                       { -- | cross section in pb
                         xsec      :: Double
                         -- | error associated with this cross section in pb
                       , errorXsec :: Double
                       } deriving Show

data HeavyIon = HeavyIon { -- | Number of hard scatterings
                           numHardScattering             :: Int
                           -- | Number of projectile participants
                         , numProjectileParticipants     :: Int
                           -- | Number of target participants
                         , numTargetParticipants         :: Int
                           -- | Number of NN (nucleon-nucleon) collisions
                         , numNNCollisions               :: Int
                           -- | Number of spectator neutrons
                         , numSpectatorNeutrons          :: Int
                           -- | Number of spectator protons
                         , numSpectatorProtons           :: Int
                           -- | Number of N-Nwounded collisions
                         , numNNwoundedCollisions        :: Int
                           -- | Number of Nwounded-N collisons
                         , numNwoundedNCollisions        :: Int
                           -- | Number of Nwounded-Nwounded collisions
                         , numNwoundedNwoundedCollisions :: Int
                           -- | Impact Parameter(fm) of collision
                         , impactParamCollision          :: Double
                           -- | Azimuthal angle of event plane
                         , eventPlaneAngle               :: Double
                           -- | eccentricity of participating nucleons
                           -- in the transverse plane
                         , eccentricity                  :: Double
                           -- | nucleon-nucleon inelastic cross-section
                         , inelasticXsecNN               :: Double
                         } deriving Show

data PdfInfo = PdfInfo { -- | flavour code of first and second partons
                         flavor           :: (Int, Int)
                         -- | fraction of beam momentum carried by first and second partons
                       , beamMomentumFrac :: (Double, Double)
                         -- | Q-scale used in evaluation of PDF’s (in GeV)
                       , scaleQPDF        :: Double
                         -- | x * f(x)
                       , xfx              :: (Double, Double)
                         -- | LHAPDF set id of first and second partons (zero by default)
                       , idLHAPDF         :: (Int, Int)
                       } deriving Show

data GenVertex = GenVertex { -- | barcode
                             vbarcode    :: Int
                             -- | id
                           , vid         :: Int
                             -- | (x, y, z, c\tau)
                           , vposition   :: (Double, Double, Double, Double)
                             -- | number of ”orphan” incoming particles
                           , numOrphan   :: Int
                             -- | number of outgoing particles
                           , numOutgoing :: Int
                             -- | list of weights (may be zero)
                           , vWeightList :: Maybe (Int, [Double])
                           , particles   :: [GenParticle]
                           } deriving Show

data GenParticle = GenParticle { -- | barcode
                                 pbarcode             :: Int
                                 -- | PDG id
                               , pdgID                :: Int
                                 -- | (px, py, pz, energy)
                               , pMomentum            :: (Double, Double, Double, Double)
                                 -- | generated mass
                               , pMass                :: Double
                                 -- | status code
                               , statusCode           :: Int
                                 -- | (\theta, \phi) of polarization
                               , polarization         :: (Double, Double)
                                 -- | barcode for vertex that has this particle
                                 -- as an incoming particle
                               , vbarcodeThisIncoming :: Int
                                 -- | flow list (may be zero)
                               , flows                :: Maybe (Int, [(Int, Int)])
                               } deriving Show

instance HasFourMomentum GenParticle where
    -- fourMomentum :: GenParticle -> FourMomentum
    fourMomentum GenParticle { pMomentum = (x, y, z, e) } = setXYZT x y z e
    {-# INLINE fourMomentum #-}

    pt GenParticle { pMomentum = (x, y, _, _) } = sqrt (x * x + y * y)
    {-# INLINE pt #-}

    mass GenParticle { pMass = m } = m
    {-# INLINE mass #-}

    epxpypz GenParticle { pMomentum = (x, y, z, e) } = (e, x, y, z)
    {-# INLINE epxpypz #-}

    pxpypz GenParticle { pMomentum = (x, y, z, _) } = (x, y, z)
    {-# INLINE pxpypz #-}

    pxpy GenParticle { pMomentum = (x, y, _, _) } = (x, y)
    {-# INLINE pxpy #-}

    px GenParticle { pMomentum = (x, _, _, _) } = x
    {-# INLINE px #-}

    py GenParticle { pMomentum = (_, y, _, _) } = y
    {-# INLINE py #-}

    pz GenParticle { pMomentum = (_, _, z, _) } = z
    {-# INLINE pz #-}

    energy GenParticle { pMomentum = (_, _, _, e) } = e
    {-# INLINE energy #-}
