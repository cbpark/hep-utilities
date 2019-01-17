module Main where

import HEP.Kinematics.TwoBody

import Control.Applicative    (liftA2)
import Control.Monad          (replicateM)
import System.Random.MWC

main :: IO ()
main = do
    rs <- createSystemRandom >>= genRandoms 10
    let twobodies = map (mkTwoBodyEvent 250 (0, 0, 125, 91)) rs
    mapM_ print (twobodies :: [Maybe TwoBodyEvent])
  where
    genRandoms :: Int -> GenIO -> IO [(Double, Double)]
    genRandoms nev gen =
        replicateM nev (liftA2 (,) (uniform gen) (uniform gen))
