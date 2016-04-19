{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances #-}
module Data.Random.Distribution.Abstract where

import qualified Data.Random as Rand
import Data.Random.Distribution.Categorical (Categorical)
import qualified Data.Random.Distribution.Categorical as Categorical
import System.Random

class Distribution d s m t | d m t -> s where
    sample :: d s t -> m t

data Bernoulli p t = Bernoulli p
                   | BernoulliLogit p
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

instance Distribution Categorical Double IO t where
    sample = Rand.sample
categorical :: (Num p, Distribution Categorical p m t) => [(p,t)] -> m t
categorical l = sample $ Categorical.fromList l

data Geometric a t = Geometric a
instance Distribution Geometric Double IO Integer where
    sample (Geometric p) = do
      coin <- bernoulli p
      if coin then return 0 else do
        x <- geometric p
        return (x + 1)
geometric :: Distribution Geometric s m t => s -> m t
geometric p = sample $ Geometric p

data Normal a r = Normal a a
instance Distribution Normal Double IO Double where
    sample (Normal m s) = Rand.sample (Rand.Normal m s)
normal :: Distribution Normal s m t => s -> s -> m t
normal m s = sample $ Normal m s
