{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Data.HepMC.Type
-- Copyright   :  (c) 2017 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- Parsers for HepMC event data. See "HEP.Data.HepMC.PipesUtil" for functions
-- using pipes.
--
--------------------------------------------------------------------------------

module HEP.Data.HepMC.Parser (hepmcHeader, hepmcEvent) where

import Control.Applicative              ((<|>))
import Control.Monad                    (replicateM, void)
import Data.Attoparsec.ByteString.Char8
import Data.Maybe                       (isJust)
import Data.Monoid                      (All (..))
import Prelude                          hiding (takeWhile)

import HEP.Data.HepMC.Type
import HEP.Data.ParserUtil              (skipTillEnd)

-- | Parsing the HepMC Header.
-- This returns the 'Version'.
hepmcHeader :: Parser Version
hepmcHeader = do
    skipSpace
    v <- hepmcVersion <* skipSpace
    _ <- beginBlock   <* skipSpace
    return v
  where
    hepmcVersion =
        string "HepMC::Version" >> skipSpace >> takeWhile (not . isSpace)
    beginBlock = void (string "HepMC::IO_GenEvent-START_EVENT_LISTING")

-- | Parsing the 'GenEvent'.
-- It has 'GenVertex', which contains 'GenParticle'.
hepmcEvent :: Parser GenEvent
hepmcEvent = lineE
             <*> header (EventHeader Nothing Nothing Nothing Nothing Nothing)
             <*> many1' vertexParticles
  where
    vertexParticles :: Parser GenVertex
    vertexParticles = lineV <*> many1' lineP

header :: EventHeader -> Parser EventHeader
header h@EventHeader {..} =
    if isFilled
    then return h
    else do s <- satisfy (inClass "NUCHF")
            case s of
                'N' -> lineN >>= \r -> header (h { weightInfo       = Just r })
                'U' -> lineU >>= \r -> header (h { unitInfo         = Just r })
                'C' -> lineC >>= \r -> header (h { crossSectionInfo = Just r })
                'H' -> lineH >>= \r -> header (h { heavyIonInfo     = Just r })
                'F' -> lineF >>= \r -> header (h { pdfInfo          = Just r })
                _   -> return h
         <|> return h
  where
    isFilled = (getAll . mconcat . map All) [ isJust weightInfo
                                            , isJust unitInfo
                                            , isJust crossSectionInfo
                                            , isJust heavyIonInfo
                                            , isJust pdfInfo
                                            ]

lineE :: Parser (EventHeader -> [GenVertex] -> GenEvent)
lineE = do
    char 'E' >> skipSpace
    evnum  <- decimal        <* skipSpace
    nmint  <- signed decimal <* skipSpace
    esc    <- double         <* skipSpace
    aqcd   <- double         <* skipSpace
    aqed   <- double         <* skipSpace
    sid    <- decimal        <* skipSpace
    bcd    <- signed decimal <* skipSpace
    nvtx   <- decimal        <* skipSpace
    bcdbm1 <- decimal        <* skipSpace
    bcdbm2 <- decimal        <* skipSpace
    rnum   <- decimal        <* skipSpace
    rList' <- replicateM rnum (skipSpace *> decimal)
    wgtnum <- decimal
    wList' <- replicateM wgtnum (skipSpace *> double)
    skipTillEnd
    return $ \h vs -> GenEvent { eventNumber         = evnum
                               , numMultiParticleInt = nmint
                               , eventScale          = esc
                               , alphaQCD            = aqcd
                               , alphaQED            = aqed
                               , signalProcId        = sid
                               , signalProcVertex    = bcd
                               , numVertex           = nvtx
                               , beamParticles       = (bcdbm1, bcdbm2)
                               , randomStateList     = mkList rnum rList'
                               , weightList          = mkList wgtnum wList'
                               , eventHeader         = h
                               , vertices            = vs }

lineN :: Parser WeightNames
lineN = do
    skipSpace
    n     <- decimal <* skipSpace
    wstrs <- replicateM n (skipSpace *> weightStrings)
    skipTillEnd
    return (WeightNames n wstrs)
  where
    weightStrings = char '"' *> takeWhile (/= '"') <* char '"'

lineU :: Parser MomentumPositionUnit
lineU = do
    skipSpace
    mUnit <- (string "GEV" >> return GeV) <|> (string "MEV" >> return MeV)
    skipSpace
    lUnit <- (string "MM" >> return MM) <|> (string "CM" >> return CM)
    skipTillEnd
    return (MomentumPositionUnit mUnit lUnit)

lineC :: Parser GenCrossSection
lineC = do
    skipSpace
    xs  <- double <* skipSpace
    err <- double
    skipTillEnd
    return (GenCrossSection xs err)

lineH :: Parser HeavyIon
lineH = do
    skipSpace
    nhsc        <- decimal <* skipSpace
    npp         <- decimal <* skipSpace
    ntp         <- decimal <* skipSpace
    nnn         <- decimal <* skipSpace
    nsn         <- decimal <* skipSpace
    nsp         <- decimal <* skipSpace
    nnnw        <- decimal <* skipSpace
    nnwn        <- decimal <* skipSpace
    nnwnw       <- decimal <* skipSpace
    impct       <- double  <* skipSpace
    epangle     <- double  <* skipSpace
    ecc         <- double  <* skipSpace
    inelstcxsec <- double
    skipTillEnd
    return HeavyIon { numHardScattering             = nhsc
                    , numProjectileParticipants     = npp
                    , numTargetParticipants         = ntp
                    , numNNCollisions               = nnn
                    , numSpectatorNeutrons          = nsn
                    , numSpectatorProtons           = nsp
                    , numNNwoundedCollisions        = nnnw
                    , numNwoundedNCollisions        = nnwn
                    , numNwoundedNwoundedCollisions = nnwnw
                    , impactParamCollision          = impct
                    , eventPlaneAngle               = epangle
                    , eccentricity                  = ecc
                    , inelasticXsecNN               = inelstcxsec }

lineF :: Parser PdfInfo
lineF = do
    skipSpace
    f1    <- signed decimal <* skipSpace
    f2    <- signed decimal <* skipSpace
    bx1   <- double         <* skipSpace
    bx2   <- double         <* skipSpace
    sqpdf <- double         <* skipSpace
    xfx1  <- double         <* skipSpace
    xfx2  <- double         <* skipSpace
    id1   <- signed decimal <* skipSpace
    id2   <- signed decimal
    skipTillEnd
    return PdfInfo { flavor           = (f1, f2)
                   , beamMomentumFrac = (bx1, bx2)
                   , scaleQPDF        = sqpdf
                   , xfx              = (xfx1, xfx2)
                   , idLHAPDF         = (id1, id2) }

lineV :: Parser ([GenParticle] -> GenVertex)
lineV = do
    char 'V' >> skipSpace
    vbcd     <- signed decimal <* skipSpace
    vid'     <- signed decimal <* skipSpace
    vx       <- double         <* skipSpace
    vy       <- double         <* skipSpace
    vz       <- double         <* skipSpace
    vctau    <- double         <* skipSpace
    norphans <- decimal        <* skipSpace
    nouts    <- decimal        <* skipSpace
    nwgts    <- decimal
    wgts     <- replicateM nwgts (skipSpace *> double)
    skipTillEnd
    return $ \ps -> GenVertex { vbarcode    = vbcd
                              , vid         = vid'
                              , vposition   = (vx, vy, vz, vctau)
                              , numOrphan   = norphans
                              , numOutgoing = nouts
                              , vWeightList = mkList nwgts wgts
                              , particles   = ps }

lineP :: Parser GenParticle
lineP = do
    char 'P' >> skipSpace
    pbcd   <- decimal        <* skipSpace
    pid'   <- signed decimal <* skipSpace
    px     <- double         <* skipSpace
    py     <- double         <* skipSpace
    pz     <- double         <* skipSpace
    pe     <- double         <* skipSpace
    gmass  <- double         <* skipSpace
    scode  <- decimal        <* skipSpace
    polTh  <- double         <* skipSpace
    polPh  <- double         <* skipSpace
    vbcd   <- signed decimal <* skipSpace
    nflows <- decimal
    fList' <- replicateM nflows
              ((,) <$> (skipSpace *> signed decimal <* skipSpace)
                   <*> signed decimal)
    finalLine >> skipTillEnd
    return GenParticle { pbarcode             = pbcd
                       , pdgID                = pid'
                       , pMomentum            = (px, py, pz, pe)
                       , pMass                = gmass
                       , statusCode           = scode
                       , polarization         = (polTh, polPh)
                       , vbarcodeThisIncoming = vbcd
                       , flows                = mkList nflows fList' }
  where finalLine = many' $ string "HepMC::IO_GenEvent-END_EVENT_LISTING"

mkList :: Int -> [a] -> Maybe (Int, [a])
mkList n ls = if n > 0 then Just (n, ls) else Nothing
