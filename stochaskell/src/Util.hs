{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Util where

import Data.Boolean
import qualified Data.Bimap as Bimap
import qualified Data.ByteString as B
import Data.List
import qualified Data.Number.LogFloat as LF
import GHC.Exts
import Numeric.SpecFunctions
import System.Process
import Text.Printf (printf)

type Label = String

indent :: String -> String
indent = intercalate "\n" . map ("  "++) . lines

commas :: (a -> String) -> [a] -> String
commas f xs = intercalate ", " $ map f xs

-- left-to-right composition
compose :: [a -> a] -> a -> a
compose [] x = x
compose (f:fs) x = compose fs $ f x

selectItems :: [a] -> [Bool] -> [a]
selectItems xs ps = map fst . filter snd $ zip xs ps

firstDiff :: (Eq a) => [a] -> [a] -> Int
firstDiff (x:xs) (y:ys) | x == y = 1 + firstDiff xs ys
firstDiff _ _ = 0

toHex :: B.ByteString -> String
toHex bytes = do
  c <- B.unpack bytes
  printf "%02x" c

lfact :: Integer -> LF.LogFloat
lfact = LF.logToLogFloat . logFactorial

delete :: (Eq k) => k -> [(k,v)] -> [(k,v)]
delete k = filter p
  where p (k',_) = k' /= k

linspace :: (IsList l, e ~ Item l, Fractional e, Integral i) => e -> e -> i -> l
linspace lo hi n = fromList [lo + step * fromIntegral i | i <- [0..n-1]]
  where step = (hi - lo)/(fromIntegral n - 1)

system_ :: String -> IO ()
system_ cmd = do
    putStrLn cmd
    _ <- system cmd
    return ()

instance (Num t) => Num [t] where
    (+) = zipWith (+)
    (-) = zipWith (-)
    (*) = zipWith (*)
    negate = map negate
    abs    = map abs
    signum = map signum
    fromInteger x = repeat $ fromInteger x

instance (Fractional t) => Fractional [t] where
  (/) = zipWith (/)

type instance BooleanOf (a,b,c,d,e,f,g) = BooleanOf a

instance ( bool ~ BooleanOf a, IfB a
         , bool ~ BooleanOf b, IfB b
         , bool ~ BooleanOf c, IfB c
         , bool ~ BooleanOf d, IfB d
         , bool ~ BooleanOf e, IfB e
         , bool ~ BooleanOf f, IfB f
         , bool ~ BooleanOf g, IfB g
         ) => IfB (a,b,c,d,e,f,g) where
  ifB cond (a,b,c,d,e,f,g) (a',b',c',d',e',f',g') =
    ( ifB cond a a'
    , ifB cond b b'
    , ifB cond c c'
    , ifB cond d d'
    , ifB cond e e'
    , ifB cond f f'
    , ifB cond g g'
    )
