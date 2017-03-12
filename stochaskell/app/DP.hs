{-# LANGUAGE FlexibleContexts, MonadComprehensions, NoMonomorphismRestriction, RebindableSyntax #-}

module Main where
import Prelude hiding ((==),(/=),(<),(>),(<=),(>=),foldl,foldr,scanl,scanr)

import Control.Applicative ()
import Data.Array.Abstract
import Data.Expression
import Data.Number.Transfinite
import Data.Program
import Data.Random.Distribution.Abstract
import Language.Church

stickBreak :: R -> P RVec
stickBreak alpha = do
  sticks <- joint vector [ beta 1 alpha | i <- 1...infinity ]
  let f r a = r * (1 - a)
      rems = scanl f 1 sticks
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

imm :: Z -> P (RVec,RVec)
imm n = do
  let base = uniform 0 100
  paramDist <- dirichletProcess 1 base
  params <- joint vector [ paramDist | j <- 1...n ]
  values <- joint vector [ normal (params!j) 1 | j <- 1...n ]
  return (params,values)

main :: IO ()
main = putStrLn $ churchProgram (imm 1000)
