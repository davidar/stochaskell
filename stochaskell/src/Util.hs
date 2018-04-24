{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Util where

import Data.Boolean
import qualified Data.ByteString as B
import Data.List
import qualified Data.Number.LogFloat as LF
import Debug.Trace
import GHC.Exts
import Numeric.SpecFunctions
import System.IO
import System.Process
import Text.Printf (printf)

type Label = String

fixpt :: (Eq a) => (a -> a) -> a -> a
fixpt f x | f x == x  = x
          | otherwise = fixpt f (f x)

indent :: String -> String
indent = indent' 2 2
indent' :: Int -> Int -> String -> String
indent' _ _ "" = ""
indent' a b s = replicate a ' ' ++ intercalate "\n" (hd : [replicate b ' ' ++ l | l <- tl])
  where (hd:tl) = lines s

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
    putStrLn' cmd
    _ <- system cmd
    return ()

putStrLn' :: String -> IO ()
putStrLn' = hPutStrLn stderr

mean :: (Foldable t, Fractional a) => t a -> a
mean xs = sum xs / fromIntegral (length xs)

square :: (Num a) => a -> a
square x = x*x

clip :: (Ord a) => (a,a) -> a -> a
clip (lo,hi) x | x < lo = lo
               | x > hi = hi
               | otherwise = x

pairWith :: (a -> a -> b) -> [a] -> [b]
pairWith f (x:y:zs) = f x y : pairWith f (y:zs)
pairWith _ _ = []

unreplicate :: (Eq a, Show a) => [a] -> a
unreplicate (x:xs) | (x ==) `all` xs = x
unreplicate xs = error $ show xs ++" is not replicated"

traceShow' :: (Show a) => String -> a -> a
traceShow' s x = trace ("["++ s ++"] "++ show x) x

notNull :: (Foldable t) => t a -> Bool
notNull = not . null

instance (Num t) => Num [t] where
    (+) = zipWith (+)
    (-) = zipWith (-)
    (*) = zipWith (*)
    negate = map negate
    abs    = map abs
    signum = map signum
    fromInteger = repeat . fromInteger

instance (Fractional t) => Fractional [t] where
  (/) = zipWith (/)
  fromRational = repeat . fromRational

instance (Floating t) => Floating [t] where
  pi = repeat pi
  (**) = zipWith (**)
  exp = map exp
  log = map log
  sqrt = map sqrt
  sin = map sin
  cos = map cos
  tan = map tan
  asin = map asin
  acos = map acos
  atan = map atan
  sinh = map sinh
  cosh = map cosh
  tanh = map tanh
  asinh = map asinh
  acosh = map acosh
  atanh = map atanh

type instance BooleanOf (IO a) = Bool
instance IfB (IO a) where ifB a b c = if a then b else c

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
