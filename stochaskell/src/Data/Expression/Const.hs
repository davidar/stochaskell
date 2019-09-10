{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, RankNTypes,
             TypeFamilies, MonadComprehensions, DeriveGeneric, DeriveAnyClass #-}
module Data.Expression.Const where

import Prelude hiding ((<*),(*>),isInfinite)

import Control.DeepSeq
import Data.Array.Abstract hiding (elems)
import Data.Array.Unboxed hiding ((!),bounds)
import Data.Boolean
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Char
import Data.List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Number.Transfinite hiding (log)
import qualified Data.Random as Rand
import Data.Random.Distribution (logPdf)
import Data.Random.Distribution.Abstract
import Data.Ratio
import Debug.Trace
import GHC.Exts
import GHC.Generics
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Data as LAD
import Numeric.SpecFunctions
import Util

constFuns :: Map String ([ConstVal] -> ConstVal)
constFuns = Map.fromList
  [("+", \[a,b] -> a + b)
  ,("+s", sum)
  ,("&&s", foldl1 (&&*))
  ,("-", \[a,b] -> a - b)
  ,("*", \[a,b] -> a * b)
  ,("/", \[a,b] -> a / b)
  ,("**", \[a,b] -> a ** b)
  ,("negate", negate . head)
  ,("abs", abs . head)
  ,("exp", exp . head)
  ,("log", log . head)
  ,("cos", cos . head)
  ,("sin", sin . head)
  ,("tan", tan . head)
  ,("sqrt", sqrt . head)
  ,("true", const true)
  ,("false", const false)
  ,("pi", const pi)
  ,("logFactorial", real . logFactorial . integer . head)
  ,("getExternal", head)
  ,("<>", \[a,b] -> a <> b)
  ,("<.>", \[u,v] -> u <.> v)
  ,("*>", \[a,v] -> a *> v)
  ,("#>", \[m,v] -> m #> v)
  ,("<\\>", \[m,v] -> m <\> v)
  ,("diag", diag . head)
  ,("asColumn", asColumn . head)
  ,("asRow", asRow . head)
  ,("eye", eye . integer . head)
  ,("zeros", \[m,n] -> zeros (integer m) (integer n))
  ,("chol", chol . head)
  ,("inv", inv . head)
  ,("det", det . head)
  ,("log_det", logDet . head)
  ,("tr", tr . head)
  ,("tr'", tr' . head)
  ,("vectorSize", integer . vectorSize . head)
  ,("matrixRows", integer . matrixRows . head)
  ,("matrixCols", integer . matrixCols . head)
  ,("ifThenElse", \[a,b,c] -> ifB a b c)
  ,("==", \[a,b] -> a ==* b)
  ,("/=", \[a,b] -> a /=* b)
  ,("<=", \[a,b] -> a <=* b)
  ,(">=", \[a,b] -> a >=* b)
  ,("<",  \[a,b] -> a <*  b)
  ,(">",  \[a,b] -> a >*  b)
  ,("&&", \[a,b] -> a &&* b)
  ,("||", \[a,b] -> a ||* b)
  ,("not", notB . head)
  ,("min", \[a,b] -> if a < b then a else b)
  ,("max", \[a,b] -> if a > b then a else b)
  ,("floor", floor . head)
  ,("id", head)
  ,("deleteIndex", \[a,i]   -> deleteIndex a [integer i])
  ,("insertIndex", \[a,i,x] -> insertIndex a [integer i] x)
  ,("replaceIndex", \[a,i,x] -> replaceIndex a [integer i] x)
  ,("findSortedInsertIndex", \[x,a] -> findSortedInsertIndexC x a)
  ,("quad_form_diag", \[m,v] -> diag v <> m <> diag v)
  ,("bernoulli_pdf",     \[b,p]   -> if toBool b then p else 1-p)
  ,("bernoulli_lpdf",    \[b,p]   -> log $ if toBool b then p else 1-p)
  ,("bernoulliLogit_lpdf", \[b,l] -> let p = 1/(1 + exp (-l)) in
                                     log $ if toBool b then p else 1-p)
  ,("pmf_lpdf",          \[k,ps]  -> log $ toList ps !! (integer k - 1))
  ,("gamma_lpdf",        \[g,a,b] -> real $ lpdfGamma (real g) (real a) (real b))
  ,("neg_binomial_lpdf", \[k,a,b] -> real $ lpdfNegBinomial (integer k) (real a) (real b))
  ,("poisson_lpdf",      \[k,l]   -> real $ lpdfPoisson (integer k) (real l))
  ,("normal_lpdf",       \[x,m,s] -> real $ logPdf (Rand.Normal (toDouble m) (toDouble s)) (real x))
  ,("normals_lpdf",      \[x,m,s] -> real . sum $ zipWith3 lpdfNormal (list x) (list m) (list s))
  ,("uniform_lpdf",      \[x,a,b] -> real $ lpdfUniform (toDouble x) (toDouble a) (toDouble b))
  ,("uniform_cdf",       \[x,a,b] -> real $ cdfUniform (toDouble x) (toDouble a) (toDouble b))
  ,("discreteUniform_lpdf", \[x,a,b] -> real $
     lpdfDiscreteUniform (toInteger x) (toInteger a) (toInteger b))
  ]

toBool :: (Boolean b, Eq b) => b -> Bool
toBool b = if notB b == false then True else False
fromBool :: (Boolean b) => Bool -> b
fromBool True  = true
fromBool False = false

-- | convert between boolean types (eg. 'Bool' to 'Language.Stochaskell.B')
boolean :: (Eq a, Boolean a, Boolean b) => a -> b
boolean = fromBool . toBool
-- | convert between integer types (eg. 'Int' to 'Language.Stochaskell.Z')
integer :: (Integral i, Num n) => i -> n
integer = fromInteger . toInteger
-- | convert between real types (eg. 'Double' to 'Language.Stochaskell.R')
real :: (Real r, Fractional f) => r -> f
real = fromRational . toRational

type Tag = Int

-- | constant numeric array (of arbitrary dimension) or tagged union value
data ConstVal = Exact  (UArray [Integer] Int)
              | Approx (UArray [Integer] Double)
              | Tagged Tag [ConstVal]
              deriving (Generic, NFData)

instance Show ConstVal where
    show (Tagged c cs) = "C"++ show c ++ show cs
    show c | dimension c >= 1 = show (toList c)
    show (Exact  a) | isScalar a = show (toScalar a)
    show (Approx a) | isScalar a = show (toScalar a)

instance Read ConstVal where
  readsPrec d (c:s) | isSpace c = readsPrec d s
  readsPrec d s@('[':_) = [(fromList   x, s') | (x, s') <- readsPrec d s]
  readsPrec d s         = [(fromDouble x, s') | (x, s') <- readsPrec d s] -- TODO: Int when possible

class ToConstVal t where
  toConstVal :: t -> ConstVal

approx :: ConstVal -> ConstVal
approx (Exact a) = Approx (amap fromIntegral a)
approx a = a

isApprox :: ConstVal -> Bool
isApprox (Approx _) = True
isApprox (Exact  _) = False

broadcast :: (ConstVal,ConstVal) -> (ConstVal,ConstVal)
broadcast (a',b')
  | shape a == shape b = (a,b)
  | shape a == [] =
    let n = length . range $ bounds b
    in (approx' $ listArray' (shape b) (replicate n a), b)
  | shape b == [] =
    let n = length . range $ bounds a
    in (a, approx' $ listArray' (shape a) (replicate n b))
  | otherwise =
    (a `slice` [ zipWith clip (shape a) i | i <- fromShape sh ]
    ,b `slice` [ zipWith clip (shape b) i | i <- fromShape sh ])
  where approx' = if isApprox a' || isApprox b' then approx else id
        (a,b) = (approx' a', approx' b')
        Just sh = coerceShape (shape a) (shape b)

constUnOp :: (forall a. Num a => a -> a) -> ConstVal -> ConstVal
constUnOp f (Exact  a) = Exact  (amap f a)
constUnOp f (Approx a) = Approx (amap f a)

constUnOp' :: (forall a. Floating a => a -> a) -> ConstVal -> ConstVal
constUnOp' f (Exact  a) = Approx (amap (f . fromIntegral) a)
constUnOp' f (Approx a) = Approx (amap f a)

constBinOp :: (forall a. Num a => a -> a -> a) -> ConstVal -> ConstVal -> ConstVal
constBinOp f a b = case broadcast (a,b) of
  (Exact  a, Exact  b) -> Exact  (zipWithA f a b)
  (Approx a, Approx b) -> Approx (zipWithA f a b)

constBinOp' :: (forall a. Floating a => a -> a -> a) -> ConstVal -> ConstVal -> ConstVal
constBinOp' f a b = case broadcast (a,b) of
  (Approx a, Approx b) -> Approx (zipWithA f a b)
  (Exact  a, Exact  b) -> Approx (zipWithA f (amap fromIntegral a) (amap fromIntegral b))

-- | map boolean function over elements of array
binarize :: (forall a. (Num a, EqB a, OrdB a) => a -> BooleanOf a) -> ConstVal -> ConstVal
binarize f (Exact  a) = Exact $ amap (\x -> if f x then 1 else 0) a
binarize f (Approx a) = Exact $ amap (\x -> if f x then 1 else 0) a

isScalar :: (Indexable a [i] e) => a -> Bool
isScalar a | ([],[]) <- bounds a = True
           | otherwise = False

toScalar :: (Indexable a [i] e, Show a) => a -> e
toScalar a | isScalar a = a![]
           | otherwise  = error $ "can't convert non-scalar "++ show a ++" to real"

fromScalar x = array ([],[]) [([], x)]

toDouble :: ConstVal -> Double
toDouble (Exact  a) = fromIntegral (toScalar a)
toDouble (Approx a) = toScalar a

fromDouble :: Double -> ConstVal
fromDouble = Approx . fromScalar

fromRationalArray :: Array [Integer] Rational -> ConstVal
fromRationalArray a = if all ((1 ==) . denominator) xs
  then Exact  $ listArray (bounds a) (fromInteger . numerator <$> xs)
  else Approx $ listArray (bounds a) (real <$> xs)
  where xs = elems a

dimension :: ConstVal -> Int
dimension = length . fst . bounds

entries :: (Fractional f) => ConstVal -> [f]
entries (Exact  a) = real <$> elems a
entries (Approx a) = real <$> elems a

toVector :: ConstVal -> ShapedVector Double
toVector c | dimension c /= 1 = error $ "not one-dimensional: "++ show c
toVector (Approx a) = ShVec (lo,hi) $ fromList (elems a)
  where ([lo],[hi]) = bounds a
toVector a = toVector (approx a)
fromVector :: ShapedVector Double -> ConstVal
fromVector v = Approx $ listArray ([lo],[hi]) (toList v)
  where (lo,hi) = bounds v

isMatrix :: ConstVal -> Bool
isMatrix c = dimension c == 2
toMatrix :: ConstVal -> ShapedMatrix Double
toMatrix c | not (isMatrix c) = error $ "not two-dimensional: "++ show c
toMatrix (Approx a) = ShMat r c $ LAD.matrix ncol xs
  where ncol = fromInteger . cardinality $ shape a !! 1
        xs = elems a
        r:c:_ = shape a
toMatrix a = toMatrix (approx a)
fromMatrix :: ShapedMatrix Double -> ConstVal
fromMatrix (ShMat (r,r') (c,c') m) = Approx $ listArray ([r,c],[r',c']) (toList $ LAD.flatten m)

isSquareMatrix :: ConstVal -> Bool
isSquareMatrix m | [(1,r),(1,c)] <- shape m, r == c = True
isSquareMatrix _ = False

foldrConst :: (ConstVal -> ConstVal -> ConstVal) -> ConstVal -> ConstVal -> ConstVal
foldrConst f r = foldr f r . toList

foldrConst' :: (ConstVal -> ConstVal -> Either String ConstVal) -> ConstVal -> ConstVal -> Either String ConstVal
foldrConst' f r = foldr f' (Right r) . toList
  where f' _ (Left e) = Left $ "foldrConst': "++ e
        f' x (Right y) = f x y

foldlConst :: (ConstVal -> ConstVal -> ConstVal) -> ConstVal -> ConstVal -> ConstVal
foldlConst f r = foldl f r . toList

foldlConst' :: (ConstVal -> ConstVal -> Either String ConstVal) -> ConstVal -> ConstVal -> Either String ConstVal
foldlConst' f r = foldl f' (Right r) . toList
  where f' (Left e) _ = Left $ "foldlConst': "++ e
        f' (Right y) x = f y x

scanrConst :: (ConstVal -> ConstVal -> ConstVal) -> ConstVal -> ConstVal -> ConstVal
scanrConst f r = fromList . scanr f r . toList

scanrConst' :: (ConstVal -> ConstVal -> Either String ConstVal) -> ConstVal -> ConstVal -> Either String ConstVal
scanrConst' f r = fmap fromList . sequence . scanr f' (Right r) . toList
  where f' _ (Left e) = Left $ "scanrConst': "++ e
        f' x (Right y) = f x y

scanlConst :: (ConstVal -> ConstVal -> ConstVal) -> ConstVal -> ConstVal -> ConstVal
scanlConst f r = fromList . scanl f r . toList

scanlConst' :: (ConstVal -> ConstVal -> Either String ConstVal) -> ConstVal -> ConstVal -> Either String ConstVal
scanlConst' f r = fmap fromList . sequence . scanl f' (Right r) . toList
  where f' (Left e) _ = Left $ "scanlConst': "++ e
        f' (Right y) x = f y x

isZeros :: ConstVal -> Bool
isZeros (Exact  a) = (0 ==) `all` elems a
isZeros (Approx a) = (0 ==) `all` elems a

listArray' :: [Interval Integer] -> [ConstVal] -> ConstVal
listArray' sh xs | isScalar `all` xs = if any isApprox xs
  then Approx $ listArray (unzip sh) (real    <$> xs)
  else Exact  $ listArray (unzip sh) (integer <$> xs)
listArray' sh xs | ((lo',hi') ==) `all` bs' = if any isApprox xs
  then Approx $ array (lo ++ lo', hi ++ hi')
    [(is ++ js, real    (x!js)) | (is,x) <- range (lo,hi) `zip` xs
                                , js <- range (lo',hi')]
  else Exact  $ array (lo ++ lo', hi ++ hi')
    [(is ++ js, integer (x!js)) | (is,x) <- range (lo,hi) `zip` xs
                                , js <- range (lo',hi')]
  where (lo,hi) = unzip sh
        (lo',hi'):bs' = map bounds xs

-- TODO: Exact when possible
arrayStrings :: [([Integer], LC.ByteString)] -> ConstVal
arrayStrings l = Approx $ array (minimum ks, maximum ks) [(k, read $ LC.unpack v) | (k,v) <- l]
  where ks = map fst l

elems' :: ConstVal -> [ConstVal]
elems' (Exact  a) = map fromIntegral (elems a)
elems' (Approx a) = map fromDouble   (elems a)

-- | extract the given indices from an array, eg.
--
-- > diagonal = table `slice` [[i,i] | i <- 1...n]
slice :: ConstVal -> AbstractArray Integer [Integer] -> ConstVal
slice (Exact  a) i = Exact  $ ixmap (bounds i) (i!) a
slice (Approx a) i = Approx $ ixmap (bounds i) (i!) a

reshape :: [Interval Integer] -> ConstVal -> ConstVal
reshape sh (Exact  a) = Exact  $ listArray (unzip sh) (elems a)
reshape sh (Approx a) = Approx $ listArray (unzip sh) (elems a)

findSortedInsertIndexC :: ConstVal -> ConstVal -> ConstVal
findSortedInsertIndexC x a = case find (\j -> x < (a![j])) [1..n] of
  Just i  -> integer i
  Nothing -> integer $ n + 1
  where a' = list a :: [ConstVal]
        n = fromIntegral $ length a'

instance Num ConstVal where
    (+) = constBinOp (+)
    (-) = constBinOp (-)
    (*) = constBinOp (*)
    abs = constUnOp abs
    signum = constUnOp signum
    fromInteger = Exact . fromScalar . fromInteger

instance Fractional ConstVal where
    fromRational = Approx . fromScalar . fromRational
    (/) = constBinOp' (/)

instance Floating ConstVal where
    exp = constUnOp' exp
    log = constUnOp' log
    cos = constUnOp' cos
    sin = constUnOp' sin
    tan = constUnOp' tan
    pi = Approx (fromScalar pi)
    (**) = constBinOp' (**)

instance Enum ConstVal where
    toEnum = Exact . fromScalar . fromIntegral
    fromEnum = integer

instance Real ConstVal where
    toRational (Exact  a) = toRational (toScalar a)
    toRational (Approx a) = toRational (toScalar a)

instance Integral ConstVal where
    toInteger (Exact a) | isScalar a = toInteger (a![])
    toInteger c = error $ show c ++" is not an integer"
    quotRem c k = let (q,r) = quotRem (toInteger c) (toInteger k)
                  in (fromInteger q, fromInteger r)

instance RealFrac ConstVal where
    floor (Exact a) = integer (toScalar a)
    floor (Approx a) = floor (toScalar a)

instance IsList ConstVal where
    type Item ConstVal = ConstVal
    fromList [] = Exact $ array ([1],[0]) []
    fromList xs = if ok then val else error "cannot concat irregular arrays"
      where n = integer (length xs)
            (lo,hi):bs = map bounds xs
            ok = all (== (lo,hi)) bs
            g :: (IArray UArray a) => (ConstVal -> a) -> UArray [Integer] a
            g f = array (1:lo,n:hi)
              [(i:js, f (x!js)) | (i,x) <- zip [1..n] xs , js <- range (lo,hi)]
            val = if any isApprox xs then Approx (g real) else Exact (g integer)
    toList val = (if isApprox val then Approx . g real
                                  else Exact . g integer) <$> [a..b]
      where (a:lo,b:hi) = bounds val
            g :: (IArray UArray a) => (ConstVal -> a) -> Integer -> UArray [Integer] a
            g f i = array (lo,hi) [(js, f (val!(i:js))) | js <- range (lo,hi)]

instance Indexable ConstVal [Integer] ConstVal where
    (Exact  a) ! i | length i == length (shape a) = Exact  $ fromScalar (a!i)
    (Approx a) ! i | length i == length (shape a) = Approx $ fromScalar (a!i)
    c ! i = slice c [ i ++ j | j <- fromShape (drop (length i) (shape c)) ]
    bounds (Exact  a) = bounds a
    bounds (Approx a) = bounds a
    deleteIndex  c [i]   = fromList $ deleteIndex  (toList c) (integer $ i-1)
    insertIndex  c [i] x = fromList $ insertIndex  (toList c) (integer $ i-1) x
    replaceIndex c [i] x = fromList $ replaceIndex (toList c) (integer $ i-1) x

instance Ix ConstVal where
    range (Exact a, Exact b) | bounds a == bounds b =
        [ Exact $ listArray (bounds a) c | c <- range (elems a, elems b) ]
    range r = error $ "range "++ show r
    inRange (Exact a, Exact b) (Exact c)
      | bounds a == bounds b && bounds b == bounds c =
        and $ zipWith3 f (elems a) (elems b) (elems c)
      where f x y z = x <= z && z <= y
    index (Exact a, Exact b) (Exact c)
      | bounds a == bounds b && bounds b == bounds c =
        index (elems a, elems b) (elems c)

instance Scalable ConstVal ConstVal where
    (Exact  a) *> (Exact  v) = Exact  $ toScalar a *> v
    (Approx a) *> (Approx v) = Approx $ toScalar a *> v
    a *> v = approx a *> approx v

instance InnerProduct ConstVal ConstVal where
    u <.> v = fromDouble $ toVector u <.> toVector v

instance Vector ConstVal Integer ConstVal where
    blockVector = fromVector . blockVector . map toVector'
      where toVector' x | isScalar x = toVector (list [x])
                        | otherwise  = toVector x
    vectorSize v = case shape v of
      (1,n):_ -> n -- TODO: conflated with matrixRows
      sh -> error $ "vectorSize "++ show sh

instance Semigroup ConstVal where
    a <> b = fromMatrix $ toMatrix a <> toMatrix b

instance LinearOperator ConstVal ConstVal where
    m  #> v = fromVector $ toMatrix m  #> toVector v
    diag     = fromMatrix . diag     . toVector
    asColumn a | isZeros a = 0
    asColumn a = fromMatrix . asColumn $ toVector a
    asRow    a | isZeros a = 0
    asRow    a = fromMatrix . asRow    $ toVector a

instance MLDivide ConstVal ConstVal where
    m <\> v = case dimension v of
      1 -> fromVector $ toMatrix m <\> toVector v
      2 -> fromMatrix $ toMatrix m <\> toMatrix v

instance Matrix ConstVal Integer ConstVal where
    blockMatrix = fromMatrix . blockMatrix . map (map toMatrix)
    eye n = Exact $ array ([1,1],[n,n])
      [ ([i,j], if i == j then 1 else 0) | i <- [1..n], j <- [1..n] ]
    zeros n n' = Exact $ array ([1,1],[n,n'])
      [ ([i,j], 0) | i <- [1..n], j <- [1..n'] ]
    matrixRows m = case shape m of
      [(1,r),_] -> r
    matrixCols m = case shape m of
      [_,(1,c)] -> c

instance SquareMatrix ConstVal ConstVal where
    chol   = fromMatrix . chol   . toMatrix
    inv    = fromMatrix . inv    . toMatrix
    det a | isZeros a = 0
    det a  = fromDouble . det    $ toMatrix a
    logDet a | isZeros a = log 0
    logDet a = fromDouble . logDet $ toMatrix a

instance LA.Transposable ConstVal ConstVal where
    tr  = fromMatrix . tr  . toMatrix
    tr' = fromMatrix . tr' . toMatrix

instance Boolean ConstVal where
    true  = Exact $ fromScalar 1
    false = Exact $ fromScalar 0
    notB (Exact a) = if toScalar a == 0 then true else false
    a &&* b = fromBool $ toBool a && toBool b
    a ||* b = fromBool $ toBool a || toBool b

type instance BooleanOf ConstVal = ConstVal

instance IfB ConstVal where
    ifB a b c = if toBool a then b else c

instance Eq ConstVal where
    (Exact  a) == (Exact  b) = a == b
    (Approx a) == (Approx b) = a == b
    a == b = approx a == approx b

instance Ord ConstVal where
    (Exact  a) `compare` (Exact  b) = a `compare` b
    (Approx a) `compare` (Approx b) = a `compare` b
    a `compare` b = approx a `compare` approx b

instance EqB ConstVal where
    a ==* b = fromBool $ a == b

instance OrdB ConstVal where
    a <* b = fromBool $ a < b

instance Transfinite ConstVal where
    infinity = Approx $ fromScalar infinity
    negativeInfinity = Approx $ fromScalar negativeInfinity
    isInfinite (Approx a) | isScalar a = isInfinite $ toScalar a
    isInfinite _ = False
