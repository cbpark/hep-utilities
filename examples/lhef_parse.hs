module Main where

import           Control.Monad                   (when)
import           Data.Attoparsec.ByteString.Lazy (Result (..), parse)
import           Data.ByteString.Lazy.Char8      (ByteString)
import qualified Data.ByteString.Lazy.Char8      as C
import           System.Environment              (getArgs)
import           System.Exit                     (exitFailure)
import           System.IO                       (IOMode (..), withFile)

import           HEP.Data.LHEF

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) $ do
           putStrLn "Usage: lhef_parse filename"
           exitFailure

    let infile = head args
    putStrLn $ "-- Parsing " ++ show infile ++ "."
    withFile infile ReadMode $ \inh -> do evstr <- C.hGetContents inh
                                          parseAndPrint evstr

parseAndPrint :: ByteString -> IO ()
parseAndPrint str = case parse lhefEvent str of
                        Fail r _ _           -> C.putStrLn r
                        Done unused evParsed -> do print evParsed
                                                   parseAndPrint unused
