--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Kinematics.Variable
-- Copyright   :  (c) 2015 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- Various kinematic variables for collider studies.
--
--------------------------------------------------------------------------------

module HEP.Kinematics.Variable
       (
         mT2
       , mTLowerBound
       ) where

import           HEP.Kinematics.Variable.MT2             (mT2)
import           HEP.Kinematics.Variable.MTLowerAndUpper (mTLowerBound)
