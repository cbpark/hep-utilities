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

import           Control.Monad        (forever)
import           Data.Function        (on)
import qualified Data.IntMap          as M
import           Data.List            (groupBy)
import           Pipes
import           Pipes.ByteString     (fromHandle)
import qualified Pipes.Prelude        as P
import           System.IO            (Handle)

import           HEP.Data.LHEF.Parser (lhefEvent)
import           HEP.Data.LHEF.Type
import           HEP.Data.ParserUtil  (parseEvent)

-- | Parsing LHEF event, 'Event'
--
-- Example usage:
--
-- > import           Pipes
-- > import qualified Pipes.Prelude      as P
-- > import           System.Environment
-- > import           System.IO
-- > import           HEP.Data.LHEF      (getLHEFEvent)
-- >
-- > main = do
-- >     infile <- head <$> getArgs
-- >     withFile infile ReadMode $ \hin ->
-- >         runEffect $ getLHEFEvent hin >-> P.print
getLHEFEvent :: MonadIO m => Handle -> Producer Event m ()
getLHEFEvent = parseEvent lhefEvent . fromHandle

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
