{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances,
             TypeOperators, GADTs, FunctionalDependencies #-}
module Data.Array.Abstract where

import Control.Applicative
import Data.List

data Interval i = Interval { lowerBound  :: i
                           , cardinality :: i
                           } deriving (Eq, Ord)

data AbstractArray i e = AArr { shape :: [Interval i]
                              , apply :: [i] -> e
                              }
instance (Show i) => Show (Interval i) where
  show (Interval a s) = "[" ++ show a ++ "," ++ show a ++ "+" ++ show s ++ ")"

instance Functor (AbstractArray i) where
    fmap f (AArr sh g) = AArr sh (f . g)

instance Applicative (AbstractArray i) where
    pure x = AArr [] (const x)
    (AArr sa f) <*> (AArr sb g) = AArr (sa ++ sb) h
      where h x = let (is,js) = splitAt (length sa) x
                  in f is $ g js

-- emulate <https://ghc.haskell.org/trac/ghc/ticket/10976>
instance Monad (AbstractArray i) where
    return = pure
    (AArr sa f) >>= k = AArr (sa ++ sb) h
      where sb = shape (k $ error "non-rectangular array")
            h x = let (is,js) = splitAt (length sa) x
                  in k (f is) `apply` js

infix 5 ...
(...) :: (Num a) => a -> a -> AbstractArray a a
a...b = AArr [Interval a size] f
  where size = b - a + 1
        f [x] = x
        f _ = error "shape mismatch"

infixl 9 !
class Vector v i e | v -> i e, i e -> v where
    (!) :: v -> i -> e
    vector :: AbstractArray i e -> v

class Matrix m i e | m -> i e, i e -> m where
    matrix :: AbstractArray i e -> m

class LinearOperator m u v | m -> u v where
    (#>) :: m -> u -> v

class SquareMatrix m where
    chol :: m -> m
