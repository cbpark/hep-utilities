module Main where

import           Control.Monad                   (when)
import           Data.Attoparsec.ByteString.Lazy (Result (..), parse)
import           Data.ByteString.Lazy.Char8      (ByteString)
import qualified Data.ByteString.Lazy.Char8      as C
import           System.Environment              (getArgs)
import           System.Exit                     (exitFailure)
import           System.IO                       (IOMode (..), withFile)

import           HEP.Data.HepMC

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) $ do
           putStrLn "Usage: hepmc_test_parse filename"
           exitFailure

    let infile = head args
    putStrLn $ "-- Parsing " ++ show infile ++ "."
    withFile infile ReadMode $ \inh -> do str <- C.hGetContents inh
                                          evstr <- parseHeader str
                                          parseAndPrint evstr

parseHeader :: ByteString -> IO ByteString
parseHeader str = case parse hepmcHeader str of
                      Fail r _ _        -> do C.putStrLn r
                                              return r
                      Done evstr ver -> do print ver
                                           return evstr

parseAndPrint :: ByteString -> IO ()
parseAndPrint str = case parse hepmcEvent str of
                        Fail {}              -> return ()
                        Done unused evParsed -> do print evParsed
                                                   parseAndPrint unused
