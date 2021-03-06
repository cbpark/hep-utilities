{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE StrictData        #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Data.LHCO.Type
-- Copyright   :  (c) 2017 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- Types for LHCO event data.
--
-- See <http://www.jthaler.net/olympicswiki/doku.php LHC Olympics Wiki>.
--
--------------------------------------------------------------------------------

module HEP.Data.LHCO.Type where

import HEP.Kinematics                      (HasFourMomentum (..), Mass)
import HEP.Kinematics.Vector.LorentzVector (setEtaPhiPtM)

data Header = Header { lhcoNumEve      :: Int -- ^ event number.
                     , lhcoTriggerWord :: Int -- ^ triggering information.
                     } deriving Show

data RawObject = RawObject { -- | type of object.
                             --
                             --     * 0 = photon
                             --
                             --     * 1 = electron
                             --
                             --     * 2 = muon
                             --
                             --     * 3 = hadronically-decaying tau
                             --
                             --     * 4 = jet
                             --
                             --     * 6 = missing transverse energy
                             lhcoTyp   :: Int
                           , lhcoEta   :: Double  -- ^ pseudorapidity.
                           , lhcoPhi   :: Double  -- ^ azimuthal angle.
                           , lhcoPt    :: Double  -- ^ transverse momentum.
                             -- | invariant mass of the object.
                             --
                             -- For a jet, it is constructed from all energy and
                             -- momentum that are contained within it.
                           , lhcoJmass :: Double
                             -- | number of tracks associated with the object.
                             --
                             -- In the case of a lepton, the number is multiplied
                             -- by the charge of the lepton.
                           , lhcoNtrk  :: Double
                             -- | either 1 or 2 for a jet that has been tagged as
                             -- containing a b-quark.
                           , lhcoBtag  :: Double
                             -- | ratio of the hadronic /vs/ electromagnetic
                             -- energy deposited in the calorimeter cells.
                           , lhcoHadem :: Double
                           } deriving Show

data Event = Event { nev      :: Int
                   , photon   :: [PhyObj Photon]
                   , electron :: [PhyObj Electron]
                   , muon     :: [PhyObj Muon]
                   , tau      :: [PhyObj Tau]
                   , jet      :: [PhyObj Jet]
                   , bjet     :: [PhyObj Bjet]
                   , met      :: PhyObj Met
                   } deriving Show

data Photon
data Electron
data Muon
data Tau
data Jet
data Bjet
data Met
data Unknown

type Eta = Double
type Phi = Double
type Pt = Double
type Ntrk = Int

newtype Track = Track (Eta, Phi, Pt) deriving Eq
instance Show Track where
    show (Track (e, ph, p)) = "eta = " ++ show e ++ ", phi = " ++ show ph ++
                              ", pt = " ++ show p

data Charge = CPlus | CMinus deriving Eq
instance Show Charge where show q = case q of CPlus -> "1"
                                              _     -> "-1"

data TauProng = OneProng | ThreeProng deriving Eq
instance Show TauProng where show t = case t of OneProng -> "1"
                                                _        -> "3"

data BTag = Loose | Tight deriving Eq
instance Show BTag where show b = case b of Loose -> "1"
                                            _     -> "2"

data EachObj where
    EachObj :: PhyObj t -> EachObj

data PhyObj t where
    ObjPhoton   :: Track -> PhyObj Photon
    ObjElectron :: Track -> Charge -> PhyObj Electron
    ObjMuon     :: Track -> Mass -> Charge -> PhyObj Muon
    ObjTau      :: Track -> Mass -> Charge -> TauProng -> PhyObj Tau
    ObjJet      :: Track -> Mass -> Ntrk -> PhyObj Jet
    ObjBjet     :: Track -> Mass -> Ntrk -> BTag -> PhyObj Bjet
    ObjMet      :: (Phi, Pt) -> PhyObj Met
    ObjUnknown  :: PhyObj Unknown

instance HasFourMomentum (PhyObj Photon) where
    fourMomentum (ObjPhoton (Track (e, ph, p))) = setEtaPhiPtM e ph p 0
    {-# INLINE fourMomentum #-}

    pt (ObjPhoton (Track (_, _, p))) = p
    {-# INLINE pt #-}

    eta (ObjPhoton (Track (e, _, _))) = e
    {-# INLINE eta #-}

    phi (ObjPhoton (Track (_, ph, _))) = ph
    {-# INLINE phi #-}

    mass _ = 0
    {-# INLINE mass #-}

instance HasFourMomentum (PhyObj Electron) where
    fourMomentum (ObjElectron (Track (e, ph, p)) _) = setEtaPhiPtM e ph p 0
    {-# INLINE fourMomentum #-}

    pt (ObjElectron (Track (_, _, p)) _) = p
    {-# INLINE pt #-}

    eta (ObjElectron (Track (e, _, _)) _) = e
    {-# INLINE eta #-}

    phi (ObjElectron (Track (_, ph, _)) _) = ph
    {-# INLINE phi #-}

    mass _ = 0
    {-# INLINE mass #-}

instance HasFourMomentum (PhyObj Muon) where
    fourMomentum (ObjMuon (Track (e, ph, p)) m _) = setEtaPhiPtM e ph p m
    {-# INLINE fourMomentum #-}

    pt (ObjMuon (Track (_, _, p)) _ _) = p
    {-# INLINE pt #-}

    eta (ObjMuon (Track (e, _, _)) _ _) = e
    {-# INLINE eta #-}

    phi (ObjMuon (Track (_, ph, _)) _ _) = ph
    {-# INLINE phi #-}

    mass (ObjMuon (Track (_, _, _)) m _) = m
    {-# INLINE mass #-}

instance HasFourMomentum (PhyObj Tau) where
    fourMomentum (ObjTau (Track (e, ph, p)) m _ _) = setEtaPhiPtM e ph p m
    {-# INLINE fourMomentum #-}

    pt (ObjTau (Track (_, _, p)) _ _ _) = p
    {-# INLINE pt #-}

    eta (ObjTau (Track (e, _, _)) _ _ _) = e
    {-# INLINE eta #-}

    phi (ObjTau (Track (_, ph, _)) _ _ _) = ph
    {-# INLINE phi #-}

    mass (ObjTau (Track (_, _, _)) m _ _) = m
    {-# INLINE mass #-}

instance HasFourMomentum (PhyObj Jet) where
    fourMomentum (ObjJet (Track (e, ph, p)) m _) = setEtaPhiPtM e ph p m
    {-# INLINE fourMomentum #-}

    pt (ObjJet (Track (_, _, p)) _ _) = p
    {-# INLINE pt #-}

    eta (ObjJet (Track (e, _, _)) _ _) = e
    {-# INLINE eta #-}

    phi (ObjJet (Track (_, ph, _)) _ _) = ph
    {-# INLINE phi #-}

    mass (ObjJet (Track (_, _, _)) m _) = m
    {-# INLINE mass #-}

instance HasFourMomentum (PhyObj Bjet) where
    fourMomentum (ObjBjet (Track (e, ph, p)) m _ _) = setEtaPhiPtM e ph p m
    {-# INLINE fourMomentum #-}

    pt (ObjBjet (Track (_, _, p)) _ _ _) = p
    {-# INLINE pt #-}

    eta (ObjBjet (Track (e, _, _)) _ _ _) = e
    {-# INLINE eta #-}

    phi (ObjBjet (Track (_, ph, _)) _ _ _) = ph
    {-# INLINE phi #-}

    mass (ObjBjet (Track (_, _, _)) m _ _) = m
    {-# INLINE mass #-}

instance Show (PhyObj Photon) where
    show (ObjPhoton t) = "(" ++ show t ++ ")"

instance Show (PhyObj Electron) where
    show (ObjElectron t c) = "(" ++ show t ++ ", charge = " ++ show c ++ ")"

instance Show (PhyObj Muon) where
    show (ObjMuon t m c) = "(" ++ show t ++ ", jmass = " ++ show m ++
                           ", charge = " ++ show c ++ ")"

instance Show (PhyObj Tau) where
    show (ObjTau t m c p) = "(" ++ show t ++ ", jmass = " ++ show m ++
                            ", charge = " ++ show c ++
                            ", prong = " ++ show p ++ ")"

instance Show (PhyObj Jet) where
    show (ObjJet t m n) = "(" ++ show t ++ ", " ++ showJetMassNtrk m n ++ ")"

instance Show (PhyObj Bjet) where
    show (ObjBjet t m n b) = "(" ++ show t ++ ", " ++ showJetMassNtrk m n ++
                           ", btag = " ++ show b ++ ")"

instance Show (PhyObj Met) where
    show (ObjMet (ph, p)) = "(phi = " ++ show ph ++ ", pt = " ++ show p ++ ")"

showJetMassNtrk :: Double -> Int -> String
showJetMassNtrk m n = "jmass = " ++ show m ++ ", ntrk = " ++ show n
