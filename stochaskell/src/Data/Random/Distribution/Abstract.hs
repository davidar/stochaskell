{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances #-}
module Data.Random.Distribution.Abstract where

import qualified Data.Random as Rand
import Data.Random.Distribution.Categorical (Categorical)
import qualified Data.Random.Distribution.Categorical as Categorical
import Data.Random.Distribution.Poisson (Poisson(..))
import System.Random

class (Monad m) => Distribution d s m t | d m t -> s where
    sample :: d s t -> m t

data Bernoulli p t = Bernoulli p
                   | BernoulliLogit p
instance Distribution Bernoulli Double IO Bool where
    sample (Bernoulli 0) = return False
    sample (Bernoulli 1) = return True
    sample (Bernoulli p) = do
      u <- uniform 0 1
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

data Gamma a t = Gamma a a
instance Distribution Gamma Double IO Double where
    sample (Gamma a b) = Rand.sample (Rand.Gamma a b)
gamma :: Distribution Gamma s m t => s -> s -> m t
gamma a b = sample $ Gamma a b
exponential :: (Num s, Distribution Gamma s m t) => s -> m t
exponential = gamma 1

data Geometric a t = Geometric a
instance Distribution Geometric Double IO Integer where
    sample (Geometric p) = do
      coin <- bernoulli p
      if coin then return 0 else do
        x <- geometric 0 p
        return (x + 1)
geometric :: (Distribution Geometric s m t, Num t) => Integer -> s -> m t
geometric 0 p = sample $ Geometric p
geometric a p = do
  g <- geometric 0 p
  return (fromInteger a + g)

data Normal a r = Normal a a
instance Distribution Normal Double IO Double where
    sample (Normal m s) = Rand.sample (Rand.Normal m s)
normal :: Distribution Normal s m t => s -> s -> m t
normal m s = sample $ Normal m s

instance Distribution Poisson Double IO Integer where
    sample = Rand.sample
poisson :: Distribution Poisson s m t => s -> m t
poisson a = sample $ Poisson a

data Uniform a r = Uniform a a
instance Distribution Uniform Double IO Double where
    sample (Uniform 0 1) = getStdRandom random
    sample (Uniform lo hi) = do
      u <- uniform 0 1
      return $ lo + (hi - lo) * u
uniform :: Distribution Uniform s m t => s -> s -> m t
uniform a b = sample $ Uniform a b
