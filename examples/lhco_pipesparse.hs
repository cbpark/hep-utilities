module Main where

import           Control.Monad      (when)
import           Pipes              (runEffect, (>->))
import qualified Pipes.Prelude      as P
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import           System.IO          (IOMode (..), withFile)

import           HEP.Data.LHCO      (getLHCOEvent)

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) $ do
           putStrLn "Usage: lhco_pipesparse_test filename"
           exitFailure

    let infile = head args
    putStrLn $ "-- Parsing " ++ show infile ++ "."
    withFile infile ReadMode $ \hin ->
        runEffect $ getLHCOEvent hin >-> P.take 3 >-> P.print
    putStrLn "-- Done parsing."
