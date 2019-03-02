{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Util where

import Control.DeepSeq
import qualified Control.Exception
import Control.Monad
import Data.Boolean
import qualified Data.ByteString as B
import Data.List
import qualified Data.Number.LogFloat as LF
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Debug.Trace
import GHC.Exts
import Numeric.SpecFunctions
import System.IO
import qualified System.IO.Unsafe
import System.Process
import System.Random
import Text.Printf (printf)

type Label = String

fixpt :: (Eq a) => (a -> a) -> a -> a
fixpt = fixpt' 0
fixpt' :: (Eq a) => Int -> (a -> a) -> a -> a
fixpt' n f x | n > 10 = error "infinite loop"
             | f x == x  = x
             | otherwise = fixpt' (n+1) f (f x)

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

-- | extract elements of first list where second list contains 'True'
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
putStrLn' = hPutStrLn stdout --stderr

-- | calculate the mean of a set
mean :: (Foldable t, Fractional a) => t a -> a
mean xs = sum xs / fromIntegral (length xs)

-- | convenience function for squaring a value
square :: (Num a) => a -> a
square x = x*x

clip :: (Ord a) => (a,a) -> a -> a
clip (lo,hi) x | x < lo = lo
               | x > hi = hi
               | otherwise = x

pairWith :: (a -> a -> b) -> [a] -> [b]
pairWith f (x:y:zs) = f x y : pairWith f (y:zs)
pairWith _ _ = []

replicated :: (Eq a) => [a] -> Bool
replicated (x:xs) = (x ==) `all` xs
replicated [] = True
unreplicate :: (Eq a, Show a) => [a] -> a
unreplicate [] = error "unreplicate []"
unreplicate xs = if replicated xs then head xs else error (show xs ++" is not replicated")

traceShow' :: (Show a) => String -> a -> a
traceShow' s x = trace ("["++ s ++"] "++ show x) x

liftMaybe :: MonadPlus m => Maybe a -> m a
liftMaybe = maybe mzero return

-- | extract result from an 'Either' value when possible, throw an exception
-- with the error string otherwise
fromRight' :: Either String b -> b
fromRight' (Left e) = error $ "fromRight: "++ e
fromRight' (Right x) = x

unsafeCatch :: a -> Either String a
unsafeCatch x = unsafeCatch' (seq x) x
unsafeCatchDeep :: (NFData a) => a -> Either String a
unsafeCatchDeep x = unsafeCatch' (deepseq x) x
unsafeCatch' :: (IO (Either String a) -> IO (Either String a)) -> a -> Either String a
unsafeCatch' f x = System.IO.Unsafe.unsafePerformIO $
  Control.Exception.catch (f $ return (Right x)) handler
  where handler :: Control.Exception.ErrorCall -> IO (Either String a)
        handler = return . Left . show

-- | piecewise linear interpolation
interpolate :: (Ord a, Fractional a) => [(a,a)] -> a -> a
interpolate ((x0,y0):(x1,y1):xys) x
  | x < x0 = y0
  | x < x1 = y0 + (y1 - y0) * (x - x0) / (x1 - x0)
  | otherwise = interpolate ((x1,y1):xys) x
interpolate [(_,y)] _ = y

-- | sets the global random seed
--
-- Warning: does not yet cover all sources of randomness
setRandomSeed :: Int -> IO ()
setRandomSeed = setStdGen . mkStdGen

-- | start a stopwatch
tic :: IO NominalDiffTime
tic = getPOSIXTime

-- | read a stopwatch
toc :: NominalDiffTime -> IO NominalDiffTime
toc t = do
  t' <- getPOSIXTime
  return (t' - t)

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

type instance BooleanOf (a,b,c,d,e) = BooleanOf a
type instance BooleanOf (a,b,c,d,e,f) = BooleanOf a
type instance BooleanOf (a,b,c,d,e,f,g) = BooleanOf a
type instance BooleanOf (a,b,c,d,e,f,g,h) = BooleanOf a
type instance BooleanOf (a,b,c,d,e,f,g,h,i) = BooleanOf a

instance ( bool ~ BooleanOf a, IfB a
         , bool ~ BooleanOf b, IfB b
         , bool ~ BooleanOf c, IfB c
         , bool ~ BooleanOf d, IfB d
         , bool ~ BooleanOf e, IfB e
         ) => IfB (a,b,c,d,e) where
  ifB cond (a,b,c,d,e) (a',b',c',d',e') =
    ( ifB cond a a'
    , ifB cond b b'
    , ifB cond c c'
    , ifB cond d d'
    , ifB cond e e'
    )
instance ( bool ~ BooleanOf a, IfB a
         , bool ~ BooleanOf b, IfB b
         , bool ~ BooleanOf c, IfB c
         , bool ~ BooleanOf d, IfB d
         , bool ~ BooleanOf e, IfB e
         , bool ~ BooleanOf f, IfB f
         ) => IfB (a,b,c,d,e,f) where
  ifB cond (a,b,c,d,e,f) (a',b',c',d',e',f') =
    ( ifB cond a a'
    , ifB cond b b'
    , ifB cond c c'
    , ifB cond d d'
    , ifB cond e e'
    , ifB cond f f'
    )
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
instance ( bool ~ BooleanOf a, IfB a
         , bool ~ BooleanOf b, IfB b
         , bool ~ BooleanOf c, IfB c
         , bool ~ BooleanOf d, IfB d
         , bool ~ BooleanOf e, IfB e
         , bool ~ BooleanOf f, IfB f
         , bool ~ BooleanOf g, IfB g
         , bool ~ BooleanOf h, IfB h
         ) => IfB (a,b,c,d,e,f,g,h) where
  ifB cond (a,b,c,d,e,f,g,h) (a',b',c',d',e',f',g',h') =
    ( ifB cond a a'
    , ifB cond b b'
    , ifB cond c c'
    , ifB cond d d'
    , ifB cond e e'
    , ifB cond f f'
    , ifB cond g g'
    , ifB cond h h'
    )
instance ( bool ~ BooleanOf a, IfB a
         , bool ~ BooleanOf b, IfB b
         , bool ~ BooleanOf c, IfB c
         , bool ~ BooleanOf d, IfB d
         , bool ~ BooleanOf e, IfB e
         , bool ~ BooleanOf f, IfB f
         , bool ~ BooleanOf g, IfB g
         , bool ~ BooleanOf h, IfB h
         , bool ~ BooleanOf i, IfB i
         ) => IfB (a,b,c,d,e,f,g,h,i) where
  ifB cond (a,b,c,d,e,f,g,h,i) (a',b',c',d',e',f',g',h',i') =
    ( ifB cond a a'
    , ifB cond b b'
    , ifB cond c c'
    , ifB cond d d'
    , ifB cond e e'
    , ifB cond f f'
    , ifB cond g g'
    , ifB cond h h'
    , ifB cond i i'
    )
