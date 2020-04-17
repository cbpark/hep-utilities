{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Analysis.Histogram1D
-- Copyright   :  (c) 2015-2017 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- Types and functions for 1D histogram.
--
--------------------------------------------------------------------------------
module HEP.Analysis.Histogram1D
    (
      Hist1D (..)

    , emptyHist
    , histogram
    , histogram1
    , scaleHist
    , integrate
    , unitNormalize
    , add
    , sub
    , showHist1D
    , bins
    , contents
    , consHist
    , printHist
    ) where

import           Control.Arrow                    (second)
import           System.IO

import           Control.Monad.Trans.State.Strict (StateT (..))
import           Data.Attoparsec.ByteString       (Parser)
import           Data.ByteString.Char8            (ByteString)
import           Data.Vector.Unboxed              (Unbox, Vector)
import qualified Data.Vector.Unboxed              as V
import           Pipes
import qualified Pipes.Attoparsec                 as PA
import           Pipes.ByteString                 (fromHandle)
import qualified Pipes.Prelude                    as P

newtype Hist1D a = Hist1D (Maybe (Vector (a, Double))) deriving Show

emptyHist :: Hist1D a
emptyHist = Hist1D Nothing

instance (Eq a, Unbox a) => Semigroup (Hist1D a) where
    h1 <> h2 = h1 `seq` h2 `seq` add h1 h2

instance (Eq a, Unbox a) => Monoid (Hist1D a) where
    mempty = emptyHist

#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

add :: (Eq a, Unbox a) => Hist1D a -> Hist1D a -> Hist1D a
add = combine (+)

sub :: (Eq a, Unbox a) => Hist1D a -> Hist1D a -> Hist1D a
sub = combine (-)

combine :: (Eq a, Unbox a) =>
           (Double -> Double -> Double) -> Hist1D a -> Hist1D a -> Hist1D a
combine _ (Hist1D Nothing)   hist               = hist
combine _ hist               (Hist1D Nothing)   = hist
combine f (Hist1D (Just h1)) (Hist1D (Just h2)) =
    let (bin1, x1) = V.unzip h1
        (bin2, x2) = V.unzip h2
    in Hist1D $ if bin1 /= bin2
                then Nothing
                else Just $ V.zip bin1 (V.zipWith f x1 x2)

histogram :: (Fractional a, Ord a, Unbox a) =>
             Int  -- ^ Number of bins
          -> a    -- ^ Lower bound
          -> a    -- ^ Upper bound
          -> [a]  -- ^ Data
          -> Hist1D a
histogram nbin lo hi xs
    | hi < lo   = Hist1D Nothing
    | otherwise = let !binVector = binList nbin lo hi
                      !lowhigh = V.zip binVector (V.tail binVector)
                      !hist = V.map (flip (uncurry count) (V.fromList xs)) lowhigh
                  in Hist1D $ Just (V.zip binVector hist)

-- | Histogram for single element.
histogram1 :: (Fractional a, Ord a, Unbox a) => Int -> a -> a -> a -> Hist1D a
histogram1 nbin lo hi x = histogram nbin lo hi [x]

binList :: (Fractional a, Unbox a) => Int -> a -> a -> Vector a
binList nbin lo hi = V.iterateN (nbin + 1) (+ binsize) lo
  where binsize = (hi - lo) / fromIntegral nbin

count :: (Ord a, Unbox a) => a -> a -> Vector a -> Double
count lo hi = fromIntegral . V.length . V.filter ((&&) <$> (>= lo) <*> (< hi))

scaleHist :: Unbox a => Double -> Hist1D a -> Hist1D a
scaleHist _ (Hist1D Nothing)  = Hist1D Nothing
scaleHist s (Hist1D (Just h)) = Hist1D $ (Just . V.map (second (s *))) h

integrate :: Unbox a => Hist1D a -> Double
integrate (Hist1D Nothing)  = 0
integrate (Hist1D (Just h)) = V.foldr (\(_, x) i -> x + i) 0 h

unitNormalize :: Unbox a => Hist1D a -> Hist1D a
unitNormalize (Hist1D Nothing) = Hist1D Nothing
unitNormalize hist             = scaleHist (1.0 / integrate hist) hist

showHist1D :: (Show a, Unbox a) => Hist1D a -> String
showHist1D (Hist1D Nothing)  = ""
showHist1D (Hist1D (Just h)) = unlines . map toStr $ V.toList h
  where toStr (b, x) = show b ++ ", " ++ show x

bins :: Unbox a => Hist1D a -> [a]
bins (Hist1D Nothing)  = []
bins (Hist1D (Just h)) = map fst (V.toList h)

contents :: Unbox a => Hist1D a -> [Double]
contents (Hist1D Nothing)  = []
contents (Hist1D (Just h)) = map snd (V.toList h)

consHist :: (MonadIO m, Fractional a, Ord a, Unbox a) =>
            Parser a -> Int -> a -> a -> Handle -> m (Hist1D a)
consHist parser nbin lo hi hin = P.fold mappend mempty id hist
  where
    hist = (getValue parser . fromHandle) hin >-> P.map (histogram1 nbin lo hi)

getValue :: Monad m => Parser a -> Producer ByteString m () -> Producer a m ()
getValue parser s = do
    (r, s') <- lift $ runStateT (PA.parse parser) s
    case r of Just (Right v) -> yield v >> getValue parser s'
              _              -> return ()

printHist :: (Unbox a, Show a) =>
             FilePath                   -- ^ Input file
          -> (Handle -> IO (Hist1D a))
          -> (Hist1D a -> Hist1D a)     -- ^ Function for transforming histogram
                                        --   (ex: 'unitNormalize')
          -> IO ()
printHist infile histFunc f =
    withFile infile ReadMode histFunc >>= putStr . showHist1D . f
