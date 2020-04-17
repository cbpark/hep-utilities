--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Data.LHCO.PipesUtil
-- Copyright   :  (c) 2017 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- Helper functions for analyses of LHCO data files using pipes.
--
--------------------------------------------------------------------------------

module HEP.Data.LHCO.PipesUtil (getLHCOEvent) where

import HEP.Data.LHCO.Parser (lhcoEvent)
import HEP.Data.LHCO.Type   (Event)
import HEP.Data.ParserUtil  (parseEvent)

import Data.ByteString      (ByteString)
import Pipes                (Producer)

-- | Parsing LHCO event, 'Event'
--
-- Example usage:
--
-- > import           HEP.Data.LHCO      (getLHCOEvent)
-- > import           Pipes              (runEffect, (>->))
-- > import           Pipes.ByteString   (fromHandle)
-- > import qualified Pipes.Prelude      as P
-- > import           System.Environment (getArgs)
-- > import           System.IO          (IOMode (..), withFile)
-- >
-- > main :: IO ()
-- > main = do
-- >    infile <- head <$> getArgs
-- >    withFile infile ReadMode $ \hin ->
-- >        runEffect $ getLHCOEvent fromHandle hin >-> P.take 3 >-> P.print
-- >    putStrLn "-- Done parsing."
getLHCOEvent :: Monad m
             => (a -> Producer ByteString m ())
             -> a
             -> Producer Event m ()
getLHCOEvent produce = parseEvent lhcoEvent . produce
