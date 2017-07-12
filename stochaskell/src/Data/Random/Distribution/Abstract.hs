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

data Bernoullis p t = Bernoullis p
                    | BernoulliLogits p
bernoullis :: Distribution Bernoullis s m t => s -> m t
bernoullis p = sample $ Bernoullis p
bernoulliLogits :: Distribution Bernoullis s m t => s -> m t
bernoulliLogits l = sample $ BernoulliLogits l

data Beta a t = Beta a a
beta :: Distribution Beta s m t => s -> s -> m t
beta a b = sample $ Beta a b

instance Distribution Categorical Double IO t where
    sample = Rand.sample
categorical :: (Num p, Distribution Categorical p m t) => [(p,t)] -> m t
categorical l = sample $ Categorical.fromList l

data Gamma a t = Gamma a a
instance Distribution Gamma Double IO Double where
    sample (Gamma a b) = Rand.sample (Rand.Gamma a (1/b))
gamma :: Distribution Gamma s m t => s -> s -> m t
gamma a b = sample $ Gamma a b
exponential :: (Num s, Distribution Gamma s m t) => s -> m t
exponential = gamma 1

data InvGamma a t = InvGamma a a
instance Distribution InvGamma Double IO Double where
    sample (InvGamma a b) = do
      x <- gamma a b
      return (1 / x)
invGamma :: Distribution InvGamma s m t => s -> s -> m t
invGamma a b = sample $ InvGamma a b

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

data Normal a r = Normal a
instance Distribution Normal (Double,Double) IO Double where
    sample (Normal (m,s)) = Rand.sample (Rand.Normal m s)
normal :: Distribution Normal (u,v) m t => u -> v -> m t
normal m s = sample $ Normal (m,s)

data Normals a r = Normals a
normals :: Distribution Normals (u,v) m t => u -> v -> m t
normals m s = sample $ Normals (m,s)

data OrderedSample a r = OrderedSample a
orderedSample :: Distribution OrderedSample (n,d) m t => n -> d -> m t
orderedSample n d = sample $ OrderedSample (n,d)

data PMF a r = PMF a
pmf :: Distribution PMF s m t => s -> m t
pmf a = sample $ PMF a

instance Distribution Poisson Double IO Integer where
    sample = Rand.sample
poisson :: Distribution Poisson s m t => s -> m t
poisson a = sample $ Poisson a

data Uniform a r = Uniform a a
instance (Random t) => Distribution Uniform t IO t where
    sample (Uniform a b) = getStdRandom $ randomR (a,b)
uniform :: Distribution Uniform s m t => s -> s -> m t
uniform a b = sample $ Uniform a b

data Uniforms a r = Uniforms a a
uniforms :: Distribution Uniforms s m t => s -> s -> m t
uniforms a b = sample $ Uniforms a b
