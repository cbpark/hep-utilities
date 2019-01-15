module Main where

import           Control.Monad      (when)
import           Pipes              (runEffect, (>->))
import qualified Pipes.Prelude      as P
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import           System.IO          (IOMode (..), withFile)

import           HEP.Data.LHEF      (getLHEFEvent)

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) $ do
           putStrLn "Usage: lhef_pipesparse filename"
           exitFailure

    let infile = head args
    putStrLn $ "-- Parsing " ++ show infile ++ "."
    withFile infile ReadMode $ \hin ->
        runEffect $ getLHEFEvent hin >-> P.take 3 >-> P.print
    putStrLn "-- Done parsing."
