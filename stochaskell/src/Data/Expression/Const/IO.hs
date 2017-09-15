{-# LANGUAGE OverloadedStrings #-}
module Data.Expression.Const.IO where

import Control.Monad
import Data.Array.Abstract
import Data.Array.IO
import Data.Array.Unboxed
import Data.Binary
import Data.Binary.IEEE754
import Data.Binary.Put
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Expression.Const
import System.ProgressBar
import Text.CSV.Lazy.ByteString
import Util

readRealMatrix :: FilePath -> Interval Integer -> Interval Integer -> IO ConstVal
readRealMatrix fname (rlo,rhi) (clo,chi) = do
  table <- fromCSVTable . csvTable . parseCSV <$> LC.readFile fname
  a <- newArray ([rlo,clo],[rhi,chi]) 0 :: IO (IOUArray [Integer] Double)
  let go _ [] = return ()
      go i (row:rows) = do
        forM_ (zip [clo..] row) $ \(j,x) ->
          writeArray a [i,j] $ read (LC.unpack x)
        when (i `mod` 1000 == 0) $
          progressBar (msg $ "Reading "++ fname) percentage 80 (i-rlo) (rhi-rlo)
        go (i+1) rows
  go rlo $ take (fromInteger (rhi-rlo+1)) table
  a' <- freeze a
  return $ Approx a'

writeNPy :: FilePath -> ConstVal -> IO ()
writeNPy fname (Approx a) = LC.writeFile fname dat
  where sh = [hi-lo+1 | (lo,hi) <- shape a]
        dic = "{'descr': '<f8', 'fortran_order': False,"++
              " 'shape': ("++ show `commas` sh ++
                              (if length sh == 1 then "," else "") ++"), }"
        pad = (10 + length dic + 1) `mod` 16
        header | pad == 0 = dic ++"\n"
               | otherwise = dic ++ replicate (16-pad) ' ' ++"\n"
        dat = runPut $ do
          putWord8 0x93
          putByteString "NUMPY"
          putWord8 1
          putWord8 0
          putWord16le . fromIntegral $ length header
          putLazyByteString $ LC.pack header
          mapM_ putFloat64le (elems a)
writeNPy fname c = writeNPy fname (approx c)
