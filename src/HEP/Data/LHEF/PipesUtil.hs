--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Data.LHEF.PipesUtil
-- Copyright   :  (c) 2017 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- Helper functions for analyses of LHEF data files using pipes.
--
--------------------------------------------------------------------------------

module HEP.Data.LHEF.PipesUtil
       (
         getLHEFEvent
       , initialStates
       , finalStates
       , groupByMother
       ) where

import           HEP.Data.LHEF.Parser (lhefEvent)
import           HEP.Data.LHEF.Type
import           HEP.Data.ParserUtil  (parseEvent)

import           Data.ByteString      (ByteString)
import qualified Data.IntMap          as M
import           Pipes
import qualified Pipes.Prelude        as P

import           Control.Monad        (forever)
import           Data.Function        (on)
import           Data.List            (groupBy)

-- | Parsing LHEF event, 'Event'
--
-- Example usage:
--
-- > import           HEP.Data.LHEF      (getLHEFEvent)
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
-- >        runEffect $ getLHEFEvent fromHandle hin >-> P.take 3 >-> P.print
-- >    putStrLn "-- Done parsing."
getLHEFEvent :: Monad m
             => (a -> Producer ByteString m ())
             -> a
             -> Producer Event m ()
getLHEFEvent produce = parseEvent lhefEvent . produce

getParticles :: Monad m => (Particle -> Bool) -> Pipe EventEntry [Particle] m ()
getParticles f = forever $ particles >-> getSome
  where particles = P.map M.elems
        getSome = void $ await >>= yield . filter f

initialStates :: Monad m => Pipe EventEntry [Particle] m ()
initialStates = getParticles ((==1) . fst . mothup)

finalStates :: Monad m => Pipe EventEntry [Particle] m ()
finalStates = getParticles ((==1) . istup)

groupByMother :: Monad m => Pipe [Particle] [[Particle]] m ()
groupByMother = P.map (groupBy ((==) `on` mothup))
