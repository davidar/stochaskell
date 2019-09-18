{-# LANGUAGE OverloadedStrings #-}
module Data.Expression.Const.IO
  ( readRealMatrix
  , writeNPy
  ) where

import Control.Monad
import Data.Array.Abstract hiding (elems)
import Data.Array.IO
import Data.Array.Unboxed
import Data.Binary
import Data.Binary.IEEE754
import Data.Binary.Put
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Csv as CSV
import Data.Expression.Const
import qualified Data.Vector as V
import GHC.Exts
import Util

-- | read CSV file into a matrix
readRealMatrix :: FilePath -> IO ConstVal
readRealMatrix fname = do
  contents <- LC.readFile fname
  let table = fromRight' $ CSV.decode CSV.NoHeader contents :: V.Vector (V.Vector Double)
  return . list $ fromList . map real . toList <$> table

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
