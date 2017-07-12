{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, RankNTypes,
             TypeFamilies #-}
module Data.Expression.Const where

import Prelude hiding ((<*),(*>))

import Data.Array hiding ((!),bounds)
import Data.Array.Abstract
import Data.Boolean
import Data.Number.Transfinite hiding (log)
import Data.Ratio
import Debug.Trace
import GHC.Exts
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Data as LAD

toBool :: (Boolean b, Eq b) => b -> Bool
toBool b = if notB b == false then True else False
fromBool :: (Boolean b) => Bool -> b
fromBool True  = true
fromBool False = false

boolean :: (Eq a, Boolean a, Boolean b) => a -> b
boolean = fromBool . toBool
integer :: (Integral i, Num n) => i -> n
integer = fromInteger . toInteger
real :: (Real r, Fractional f) => r -> f
real = fromRational . toRational

data ConstVal = Exact  (Array [Integer] Rational)
              | Approx (Array [Integer] Double)

instance Show ConstVal where
    show c | dimension c >= 1 = show (toList c)
    show (Approx a) | isScalar a = show (toScalar a)
    show (Approx a) = show a
    show c = show $ approx c

approx :: ConstVal -> ConstVal
approx (Exact a) = Approx (fromRational <$> a)
approx a = a

isApprox :: ConstVal -> Bool
isApprox (Approx _) = True
isApprox (Exact  _) = False

constUnOp :: (forall a. Fractional a => a -> a) -> ConstVal -> ConstVal
constUnOp f (Exact  a) = Exact  (f <$> a)
constUnOp f (Approx a) = Approx (f <$> a)

constUnOp' :: (forall a. Floating a => a -> a) -> ConstVal -> ConstVal
constUnOp' f (Exact  a) = Approx (f . fromRational <$> a)
constUnOp' f (Approx a) = Approx (f <$> a)

constBinOp :: (forall a. Fractional a => a -> a -> a) -> ConstVal -> ConstVal -> ConstVal
constBinOp f (Exact  a) (Exact  b) = Exact  (zipWithA f a b)
constBinOp f (Exact  a) (Approx b) = Approx (zipWithA f (fromRational <$> a) b)
constBinOp f (Approx a) (Exact  b) = Approx (zipWithA f a (fromRational <$> b))
constBinOp f (Approx a) (Approx b) = Approx (zipWithA f a b)

constBinOp' :: (forall a. Floating a => a -> a -> a) -> ConstVal -> ConstVal -> ConstVal
constBinOp' f (Approx a) (Approx b) = Approx (zipWithA f a b)
constBinOp' f a b = constBinOp' f (approx a) (approx b)

isScalar :: (Ix t, Show t) => Array [t] r -> Bool
isScalar a = bounds a == ([],[])

toScalar :: (Ix t, Show t, Show r) => Array [t] r -> r
toScalar a | isScalar a = a![]
           | otherwise  = error $ "can't convert non-scalar "++ show a ++" to real"

fromScalar :: (Ix t, Show t) => e -> Array [t] e
fromScalar x = array ([],[]) [([], x)]

toDouble :: ConstVal -> Double
toDouble (Exact  a) = fromRational (toScalar a)
toDouble (Approx a) = toScalar a

fromDouble :: Double -> ConstVal
fromDouble = Approx . fromScalar

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

toMatrix :: ConstVal -> ShapedMatrix Double
toMatrix c | dimension c /= 2 = error $ "not two-dimensional: "++ show c
toMatrix (Approx a) = ShMat r c $ LAD.matrix ncol xs
  where ncol = fromInteger . cardinality $ shape a !! 1
        xs = elems a
        r:c:_ = shape a
toMatrix a = toMatrix (approx a)
fromMatrix :: ShapedMatrix Double -> ConstVal
fromMatrix (ShMat (r,r') (c,c') m) = Approx $ listArray ([r,c],[r',c']) (toList $ LAD.flatten m)

foldrConst :: (ConstVal -> ConstVal -> ConstVal) -> ConstVal -> ConstVal -> ConstVal
foldrConst f r = foldr f r . toList

foldrConst' :: (ConstVal -> ConstVal -> Maybe ConstVal) -> ConstVal -> ConstVal -> Maybe ConstVal
foldrConst' f r = foldr f' (Just r) . toList
  where f' _ Nothing = Nothing
        f' x (Just y) = f x y

-- column-major
vectorise :: ConstVal -> ConstVal
vectorise c | dimension c == 2 = fromList . concat $ toList <$> toList (tr' c)

eye :: Interval Integer -> ConstVal
eye (lo,hi) = Exact $ array ([lo,lo],[hi,hi])
  [ ([i,j], if i == j then 1 else 0) | i <- [lo..hi], j <- [lo..hi] ]

zeros :: Interval Integer -> Interval Integer -> ConstVal
zeros (lo,hi) (lo',hi') = Exact $ array ([lo,lo'],[hi,hi'])
  [ ([i,j], 0) | i <- [lo..hi], j <- [lo'..hi'] ]

isZeros :: ConstVal -> Bool
isZeros (Exact  a) = (0 ==) `all` elems a
isZeros (Approx a) = (0 ==) `all` elems a

listArray' :: [Interval Integer] -> [ConstVal] -> ConstVal
listArray' sh xs = if any isApprox xs then approx a else a
  where a = Exact $ listArray (unzip sh) (toRational <$> xs)

elems' :: ConstVal -> [ConstVal]
elems' (Exact  a) = map fromRational (elems a)
elems' (Approx a) = map fromDouble   (elems a)

instance Num ConstVal where
    (+) = constBinOp (+)
    (-) = constBinOp (-)
    (*) = constBinOp (*)
    abs = constUnOp abs
    signum = constUnOp signum
    fromInteger = Exact . fromScalar . fromInteger

instance Fractional ConstVal where
    fromRational = Exact . fromScalar
    (/) = constBinOp (/)

instance Floating ConstVal where
    exp = constUnOp' exp
    log = constUnOp' log
    pi = Approx (fromScalar pi)
    (**) = constBinOp' (**)

instance Enum ConstVal where
    toEnum = Exact . fromScalar . fromIntegral
    fromEnum = integer

instance Real ConstVal where
    toRational (Exact  a) = toScalar a
    toRational (Approx a) = toRational (toScalar a)

instance Integral ConstVal where
    toInteger (Exact a) | isScalar a && denominator (a![]) == 1 = numerator (a![])
    toInteger c = error $ show c ++" is not an integer"
    quotRem c k = let (q,r) = quotRem (toInteger c) (toInteger k)
                  in (fromInteger q, fromInteger r)

instance IsList ConstVal where
    type Item ConstVal = ConstVal
    fromList [] = error "fromList [] -> ConstVal"
    fromList xs = if ok then val else error "cannot concat irregular arrays"
      where n = integer (length xs)
            (lo,hi):bs = map bounds xs
            ok = all (== (lo,hi)) bs
            a = Exact $ array (1:lo,n:hi)
              [ (i:js, toRational $ x!js) | (i,x) <- zip [1..n] xs
                                          , js <- range (lo,hi) ]
            val = if any isApprox xs then approx a else a
    toList val = if isApprox val then map approx xs else xs
      where (a:lo,b:hi) = bounds val
            xs = [ Exact $ array (lo,hi)
                   [ (js, toRational $ val!(i:js)) | js <- range (lo,hi) ]
                 | i <- [a..b] ]

instance Indexable ConstVal [Integer] ConstVal where
    (Exact  a) ! i = Exact  $ fromScalar (a!i)
    (Approx a) ! i = Approx $ fromScalar (a!i)
    bounds (Exact  a) = bounds a
    bounds (Approx a) = bounds a
    deleteIndex (Exact a) [i] | dimension (Exact a) == 1 =
        Exact $ listArray ([lo],[hi-1]) xs
      where ([lo],[hi]) = bounds a
            xs = deleteIndex (elems a) (integer $ i - lo)
    insertIndex (Exact a) [i] x | dimension (Exact a) == 1 =
        Exact $ listArray ([lo],[hi+1]) xs
      where ([lo],[hi]) = bounds a
            xs = insertIndex (elems a) (integer $ i - lo) (toRational x)

instance Ix ConstVal where
    range (Exact a, Exact b) | bounds a == bounds b =
        [ Exact $ listArray (bounds a) (fromInteger <$> c) | c <- range (lo,hi) ]
      where lo = ceiling <$> elems a
            hi = floor   <$> elems b
    inRange (Exact a, Exact b) (Exact c)
      | bounds a == bounds b && bounds b == bounds c =
        and $ zipWith3 f (elems a) (elems b) (elems c)
      where f x y z = x <= z && z <= y
    index (Exact a, Exact b) (Exact c)
      | bounds a == bounds b && bounds b == bounds c =
        index (lo,hi) ix
      where lo = ceiling <$> elems a
            hi = floor   <$> elems b
            ix = round   <$> elems c

instance Scalable ConstVal ConstVal where
    (Exact  a) *> (Exact  v) = Exact  $ toScalar a *> v
    (Approx a) *> (Approx v) = Approx $ toScalar a *> v
    a *> v = approx a *> approx v

instance InnerProduct ConstVal ConstVal where
    u <.> v = fromDouble $ toVector u <.> toVector v

instance Matrix ConstVal ConstVal ConstVal where
    a <> b = fromMatrix $ toMatrix a <> toMatrix b

instance LinearOperator ConstVal ConstVal ConstVal where
    m  #> v = fromVector $ toMatrix m  #> toVector v
    m <\> v = fromVector $ toMatrix m <\> toVector v

instance SquareMatrix ConstVal ConstVal where
    chol   = fromMatrix . chol   . toMatrix
    inv    = fromMatrix . inv    . toMatrix
    det    = fromDouble . det    . toMatrix
    logDet = fromDouble . logDet . toMatrix

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
