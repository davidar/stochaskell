{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances #-}
module Data.Random.Distribution.Abstract where

import Data.Array.Abstract (AbstractArray)
import qualified Data.Random as Rand
import Data.Random.Distribution.Normal (Normal(..))
import Data.Random.Distribution.Bernoulli (Bernoulli(..))

class Distribution d s m t | d m -> s t where
    sample :: d s -> m t

data BernoulliLogit a = BernoulliLogit a
bernoulliLogit l = sample $ BernoulliLogit l

instance Distribution BernoulliLogit Double IO Bool where
    sample (BernoulliLogit l) = Rand.sample (Bernoulli p)
      where p = 1 / (1 + exp (-l))

normal m s = sample $ Normal m s

instance Distribution Normal Double IO Double where
    sample = Rand.sample
