{-# LANGUAGE FlexibleContexts, MonadComprehensions, NoMonomorphismRestriction, RebindableSyntax #-}

module Main where
import Language.Stochaskell
import Language.Church

stickBreak :: R -> P RVec
stickBreak alpha = do
  sticks <- joint vector [ beta 1 alpha | i <- 1...infinity ] :: P RVec
  let sticks' = vector [ 1 - (sticks!i) | i <- 1...infinity ]
      rems = scanl (*) 1 sticks'
      probs = vector [ (sticks!i) * (rems!i) | i <- 1...infinity ]
  return probs

dirichletProcess :: R -> P R -> P (P R)
dirichletProcess alpha base = do
  probs <- stickBreak alpha
  atoms <- joint vector [ base | i <- 1...infinity ]
  let randomProcess = do
        stick <- pmf probs
        return (atoms!stick)
  return randomProcess

imm :: Z -> P RVec
imm n = do
  let base = uniform 0 100
  paramDist <- dirichletProcess 5 base
  params <- joint vector [ paramDist | j <- 1...n ]
  values <- joint vector [ normal (params!j) 1 | j <- 1...n ]
  return values

main :: IO ()
main = do
  values <- simChurchVec $ imm 10000
  print values
