module Main where

import           HEP.Data.LHCO      (getLHCOEvent)

import           Pipes              (runEffect, (>->))
import           Pipes.ByteString   (fromHandle)
import qualified Pipes.Prelude      as P

import           System.Environment (getArgs)
import           System.IO          (IOMode (..), withFile)

main :: IO ()
main = do
    infile <- head <$> getArgs

    putStrLn $ "-- Parsing " ++ show infile ++ "."
    withFile infile ReadMode $ \hin ->
        runEffect $ getLHCOEvent fromHandle hin >-> P.take 3 >-> P.print
    putStrLn "-- Done parsing."
