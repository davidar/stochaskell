{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveTraversable, DerivingVia,
             FlexibleInstances #-}
module Main where

import Language.Stochaskell
import Language.Stochaskell.Expression

data Kernel k
  = SumK k k
  | ProductK k k
  | LinearK R
  | SEK R R
  deriving (Show, Generic, Generic1, Functor, Foldable, Traversable)
  deriving Show1 via (Generically1 Kernel)
instance                 ExprType    (Kernel (FixE Kernel))
instance                 Constructor (Kernel (FixE Kernel))
instance (ExprType t) => ExprType    (Kernel (Expression t))
instance (ExprType t) => Constructor (Kernel (Expression t))

prior :: P (FixE' Kernel)
prior = flip unfoldP (0 :: Z) $ \_ -> mixture'
  [(0.25, return . fromConcrete $ SumK 0 0)
  ,(0.25, return . fromConcrete $ ProductK 0 0)
  ,(0.25, do
    x <- uniform 0 1
    return . fromConcrete $ LinearK x)
  ,(0.25, do
    x <- uniform 0 1
    y <- uniform 0 1
    return . fromConcrete $ SEK x y)
  ]

main = do
  k <- simulate prior
  print k
