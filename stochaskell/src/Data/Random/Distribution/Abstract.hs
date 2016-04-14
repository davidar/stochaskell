{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances #-}
module Data.Random.Distribution.Abstract where

import qualified Data.Random as Rand
import Data.Random.Distribution.Normal (Normal(..))
import System.Random

class Distribution d s m t | d m -> s t where
    sample :: d s -> m t

data Bernoulli a = Bernoulli a
                 | BernoulliLogit a
instance Distribution Bernoulli Double IO Bool where
    sample (Bernoulli 0) = return False
    sample (Bernoulli 1) = return True
    sample (Bernoulli p) = do
      u <- getStdRandom random
      return (u < p)
    sample (BernoulliLogit l) =
      bernoulli $ 1 / (1 + exp (-l))
bernoulli :: Distribution Bernoulli s m t => s -> m t
bernoulli p = sample $ Bernoulli p
bernoulliLogit :: Distribution Bernoulli s m t => s -> m t
bernoulliLogit l = sample $ BernoulliLogit l

instance Distribution Normal Double IO Double where
    sample = Rand.sample
normal :: Distribution Normal s m t => s -> s -> m t
normal m s = sample $ Normal m s
