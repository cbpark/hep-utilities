{-# LANGUAGE RecordWildCards #-}

module Main where

import           HEP.Kinematics                      (FourMomentum,
                                                      TransverseMomentum)
import           HEP.Kinematics.Variable             (mTLowerBound)
import           HEP.Kinematics.Vector.LorentzVector (setXYZT)
import           HEP.Kinematics.Vector.TwoVector     (setXY)

main :: IO ()
main = do
  ms <- mapM calc [input1, input2, input3, input4, input5]
  putStr $ unlines (map show ms)

data Input = Input { visible1      :: FourMomentum
                   , visible2      :: FourMomentum
                   , missing       :: TransverseMomentum
                   , mIntermediate :: Double }

calc :: Input -> IO Double
calc Input {..} = mTLowerBound visible1 visible2 missing mtau

mtau :: Double
mtau = 1.77682

input1 :: Input
input1 = Input visA visB ptmiss mtau
  where visA = setXYZT 24.4994998687 (-36.586361435) 55.682416967 70.991623018
        visB = setXYZT (-5.01150522757) 6.56305506273 27.3938843585 28.6165908531
        ptmiss = setXY (-22.378734312890003) 30.114831829400003

input2 :: Input
input2 = Input visA visB ptmiss mtau
  where visA = setXYZT (-4.25420053069) (-18.0298062078) (-45.3358742472) 48.9748211593
        visB = setXYZT 5.99984472641 29.4269106812 (-10.7147131691) 31.8867647632
        ptmiss = setXY (-1.4755369473799993) (-10.912496205699995)

input3 :: Input
input3 = Input visA visB ptmiss mtau
  where visA = setXYZT 22.4219337654 (-6.78381619743) (-1.12349445812) 23.4528608023
        visB = setXYZT (-12.217856512680001) 4.31480614675 (-19.12852757138) 23.11902353941
        ptmiss = setXY (-10.62280146074) 3.894942135880001

input4 :: Input
input4 = Input visA visB ptmiss mtau
  where visA = setXYZT (-7.60863717246) 22.06059903779 (-38.08630179223) 44.6734172231
        visB = setXYZT 4.916778144609999 (-15.65711629521) 56.0084303585 58.3709791011
        ptmiss = setXY 2.418267922405 (-6.786272931236)

input5 :: Input
input5 = Input visA visB ptmiss mtau
  where visA = setXYZT 15.026008527450001 (-46.1157046709) (-1.7967212585999999) 48.5409522686
        visB = setXYZT (-17.81567839581) 54.942445595799995 (-4.92558441941) 57.980358045900005
        ptmiss = setXY 2.0606581585700003 (-7.0937926384)
