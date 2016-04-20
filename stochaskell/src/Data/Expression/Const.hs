{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, RankNTypes,
             TypeFamilies #-}
module Data.Expression.Const where

import Data.Array hiding ((!),bounds)
import Data.Array.Abstract
import Data.Boolean
import Data.Ratio
import GHC.Exts
import qualified Numeric.LinearAlgebra.Data as LAD

integer :: (Integral i, Num n) => i -> n
integer = fromInteger . toInteger

data ConstVal = Exact  (Array [Integer] Rational)
              | Approx (Array [Integer] Double)
              deriving (Eq, Ord)

instance Show ConstVal where
    show (Approx a) = show a
    show c = show $ approx c

approx :: ConstVal -> ConstVal
approx (Exact a) = Approx (fromRational <$> a)
approx a = a

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

toScalar :: (Ix t, Show t, Show r) => Array [t] r -> r
toScalar a | bounds a == ([],[]) = a![]
           | otherwise = error $ "can't convert non-scalar "++ show a ++" to real"

fromScalar :: (Ix t) => e -> Array [t] e
fromScalar x = array ([],[]) [([], x)]

toDouble :: ConstVal -> Double
toDouble (Exact  a) = fromRational (toScalar a)
toDouble (Approx a) = toScalar a

fromDouble :: Double -> ConstVal
fromDouble = Approx . fromScalar

flatten :: [ConstVal] -> ConstVal
flatten = fromList . map toRational

toVector :: ConstVal -> ShapedVector Double
toVector a = ShVec (lo,hi) $ fromList (fromRational <$> toList a)
  where ([lo],[hi]) = bounds a
fromVector :: ShapedVector Double -> ConstVal
fromVector v = Approx $ listArray ([lo],[hi]) (toList v)
  where (lo,hi) = bounds v

toMatrix :: ConstVal -> ShapedMatrix Double
toMatrix a = ShMat r c $ LAD.matrix ncol xs
  where ncol = fromInteger . cardinality $ shape a !! 1
        xs = fromRational <$> toList a
        r:c:_ = shape a
fromMatrix :: ShapedMatrix Double -> ConstVal
fromMatrix (ShMat (r,r') (c,c') m) = Approx $ listArray ([r,c],[r',c']) (toList $ LAD.flatten m)

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

instance Enum ConstVal where
    toEnum = Exact . fromScalar . fromIntegral
    fromEnum = integer

instance Real ConstVal where
    toRational (Exact  a) = toScalar a
    toRational (Approx a) = toRational (toScalar a)

instance Integral ConstVal where
    toInteger (Exact a) | denominator (a![]) == 1 = numerator (a![])
    toInteger _ = error "not an integer"
    quotRem c k = let (q,r) = quotRem (toInteger c) (toInteger k)
                  in (fromInteger q, fromInteger r)

instance IsList ConstVal where
    type Item ConstVal = Rational
    fromList xs = Exact $ listArray ([1], [fromIntegral $ length xs]) (map toRational xs)
    toList (Exact  a) = elems a
    toList (Approx a) = toRational <$> elems a

instance Indexable ConstVal [Integer] ConstVal where
    (Exact  a) ! i = Exact  $ fromScalar (a!i)
    (Approx a) ! i = Approx $ fromScalar (a!i)
    bounds (Exact  a) = bounds a
    bounds (Approx a) = bounds a

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

instance LinearOperator ConstVal ConstVal ConstVal where
    m #> v = fromVector $ toMatrix m #> toVector v

instance SquareMatrix ConstVal where
    chol = fromMatrix . chol . toMatrix

toBool :: (Boolean b, Eq b) => b -> Bool
toBool b = if notB b == false then True else False

fromBool :: (Boolean b) => Bool -> b
fromBool True  = true
fromBool False = false

instance Boolean ConstVal where
    true  = Exact $ fromScalar 1
    false = Exact $ fromScalar 0
    notB (Exact a) = if toScalar a == 0 then true else false
    a &&* b = fromBool $ toBool a && toBool b
    a ||* b = fromBool $ toBool a && toBool b

type instance BooleanOf ConstVal = ConstVal

instance IfB ConstVal where
    ifB a b c = if toBool a then b else c

instance EqB ConstVal where
    a ==* b = fromBool $ a == b

instance OrdB ConstVal where
    (Exact a) <* (Exact b) = fromBool $ a < b
