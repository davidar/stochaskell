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
type K = FixE' Kernel

prior :: P K
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

data KernelProposal a
  = Update K
  | SumLeft a K
  | SumRight K a
  | ProductLeft a K
  | ProductRight K a
  deriving (Show, Generic, Generic1, Functor, Foldable, Traversable)
  deriving Show1 via (Generically1 KernelProposal)
instance                 ExprType    (KernelProposal (FixE KernelProposal))
instance                 Constructor (KernelProposal (FixE KernelProposal))
instance (ExprType t) => ExprType    (KernelProposal (Expression t))
instance (ExprType t) => Constructor (KernelProposal (Expression t))
type KP = FixE' KernelProposal

proposalRaise :: K -> P KP
proposalRaise = unfoldP (fromCaseP f) where
  f :: Kernel (FixE Kernel) -> P (Expression (KernelProposal K))
  f k@(SumK a b) = mixture'
    [(0.50, return . fromConcrete $ Update (fromConcrete k))
    ,(0.25, return . fromConcrete $ SumLeft  (unfixE a) (unfixE b))
    ,(0.25, return . fromConcrete $ SumRight (unfixE a) (unfixE b))
    ]
  f k@(ProductK a b) = mixture'
    [(0.50, return . fromConcrete $ Update (fromConcrete k))
    ,(0.25, return . fromConcrete $ ProductLeft  (unfixE a) (unfixE b))
    ,(0.25, return . fromConcrete $ ProductRight (unfixE a) (unfixE b))
    ]
  f k = return . fromConcrete $ Update (fromConcrete k)

proposalUpdate :: KP -> P KP
proposalUpdate = foldP (fromCaseP f) where
  f :: KernelProposal KP -> P KP
  f Update{} = do
    k <- prior
    return . fromConcrete $ Update k
  f (SumLeft  a b) = return . fromConcrete $ SumLeft (FixE a) b
  f (SumRight a b) = return . fromConcrete $ SumRight a (FixE b)
  f (ProductLeft  a b) = return . fromConcrete $ ProductLeft (FixE a) b
  f (ProductRight a b) = return . fromConcrete $ ProductRight a (FixE b)

proposalLower :: KP -> K
proposalLower = foldE (fromCase f) where
  f :: KernelProposal K -> K
  f (Update k) = k
  f (SumLeft  a b) = fromConcrete $ SumK (FixE a) (FixE b)
  f (SumRight a b) = fromConcrete $ SumK (FixE a) (FixE b)
  f (ProductLeft  a b) = fromConcrete $ ProductK (FixE a) (FixE b)
  f (ProductRight a b) = fromConcrete $ ProductK (FixE a) (FixE b)

main = do
  k0 <- simulate prior
  print k0
  kp0 <- simulate (proposalRaise k0)
  print kp0
  kp1 <- simulate (proposalUpdate kp0)
  print kp1
  let k1 = proposalLower kp1
  print k1
