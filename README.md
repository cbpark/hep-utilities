hep-utilities
=============

[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![Build Status](https://travis-ci.com/cbpark/hep-utilities.svg?branch=master)](https://travis-ci.com/cbpark/hep-utilities)

Types and functions for various vectors, kinematics variables, and utilities for collider studies. It also contains tools for analyzing data of Monte Carlo event generators in high energy physics such as [Les Houches Event File](http://home.thep.lu.se/~leif/LHEF/) (LHEF), LHC Olympics (LHCO), and [HepMC](http://hepmc.web.cern.ch/hepmc/)
using Haskell.

## Usage

See codes in [examples](examples). If you're going to use [pipes](http://hackage.haskell.org/package/pipes), see [`HEP.Data.LHEF.PipesUtil`](src/HEP/Data/LHEF/PipesUtil.hs), [`HEP.Data.LHCO.PipesUtil`](src/HEP/Data/LHCO/PipesUtil.hs), and [`HEP.Data.HepMC.PipesUtil`](src/HEP/Data/HepMC/PipesUtil.hs). For example,

``` haskell
module Main where

import           HEP.Data.LHEF      (getLHEFEvent)

import           Pipes
import           Pipes.ByteString   (fromHandle)
import qualified Pipes.Prelude      as P

import           System.Environment (getArgs)
import           System.IO          (IOMode (..), withFile)

main :: IO ()
main = do
    infile <- head <$> getArgs

    putStrLn $ "-- Parsing " ++ show infile ++ "."
    withFile infile ReadMode $ \hin ->
        runEffect $ getLHEFEvent fromHandle hin >-> P.take 3 >-> P.print
    putStrLn "-- Done parsing."
```

## References

- [A standard format for Les Houches Event Files](http://arxiv.org/abs/hep-ph/0609017).
- [Les Houches Event File](http://home.thep.lu.se/~leif/LHEF/).
- [How to Read LHC Olympics Data Files](http://madgraph.phys.ucl.ac.be/Manual/lhco.html).
- [LHC Olympics Wiki](http://www.jthaler.net/olympicswiki/doku.php).
- [PGS 4 and the LHC Olympics](http://online.kitp.ucsb.edu/online/lhco_c06/conway/).
- [HepMC 2 user manual](http://hepmc.web.cern.ch/hepmc/releases/HepMC2_user_manual.pdf).
