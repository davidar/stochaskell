{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances,
             TypeOperators, GADTs, FunctionalDependencies #-}
module Data.Array.Abstract where

import Control.Applicative
import Data.List

data AbstractArray i e = AArr { shape :: [i]
                              , apply :: [i] -> e
                              }

instance (Integral i, Show e) => Show (AbstractArray i e) where
    show (AArr [] c) = show (c [])
    show (AArr (d:ds) f) = "[" ++ es ++ "]"
      where es = intercalate "," [ show $ AArr ds (\is -> f (i:is))
                                 | i <- [0..d-1] ]

instance Functor (AbstractArray i) where
    fmap f (AArr sh g) = AArr sh (f . g)

instance Applicative (AbstractArray i) where
    pure x = AArr [] (const x)
    (AArr sa f) <*> (AArr sb g) = AArr (sa ++ sb) h
      where h x = let (is,js) = splitAt (length sa) x
                  in f is $ g js

infix 5 ...
(...) :: (Num a) => a -> a -> AbstractArray a a
a...b = AArr [size] f
  where size = b - a + 1
        f [x] = x + a
        f _ = error "shape mismatch"

infixl 9 !
class Indexable a i e | a -> i e where
    (!) :: a -> i -> e

class LinearOperator m u v | m -> u v where
    (#>) :: m -> u -> v

class SquareMatrix m where
    chol :: m -> m
