{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances, MonadComprehensions #-}
module Data.Random.Distribution.Abstract where

import Data.Array.Abstract
import Data.Monoid
import qualified Data.Random as Rand
import qualified Data.Random.Distribution.Beta as Beta
import qualified Data.Random.Distribution.Categorical as Categorical
import qualified Data.Random.Distribution.ChiSquare as ChiSquare
import qualified Data.Random.Distribution.Poisson as Poisson
import Debug.Trace
import qualified Numeric.LinearAlgebra.Data as LAD
import Numeric.SpecFunctions
import System.Random

-- | generalised probability distribution interface
class (Monad m) => Distribution d s m t | d m t -> s where
    sample :: d s -> m t

data Bernoulli p = Bernoulli p
                 | BernoulliLogit p
instance Distribution Bernoulli Double IO Bool where
    sample (Bernoulli 0) = return False
    sample (Bernoulli 1) = return True
    sample (Bernoulli p) = do
      u <- uniform 0 1
      return (u < p)
    sample (BernoulliLogit l) =
      bernoulli $ 1 / (1 + exp (-l))
bernoulli p = sample $ Bernoulli p
bernoulliLogit l = sample $ BernoulliLogit l

data Bernoullis p = Bernoullis p
                  | BernoulliLogits p
bernoullis p = sample $ Bernoullis p
bernoulliLogits l = sample $ BernoulliLogits l

newtype Beta a = Beta a
instance Distribution Beta (Double,Double) IO Double where
    sample (Beta (a,b)) = Rand.sample (Beta.Beta a b)
beta a b = sample $ Beta (a,b)

newtype Categorical a = Categorical a
instance Distribution Categorical [Double] IO Integer where
    sample (Categorical q) = Rand.sample . Categorical.fromList $ zip q [1..]
categorical q = sample $ Categorical q

newtype Cauchy a = Cauchy a
instance Distribution Cauchy (Double,Double) IO Double where
  sample (Cauchy (a,b)) = do
    x <- normal 0 1
    y <- normal 0 1
    let z = x/y
    return $ a + b*z
cauchy a b = sample $ Cauchy (a,b)

newtype Cauchys a = Cauchys a
instance Distribution Cauchys (ShapedVector Double, ShapedVector Double) IO (ShapedVector Double) where
  sample (Cauchys (m,s)) = joint vector [ cauchy (m!i) (s!i) | i <- a...b ]
    where (a,b) = bounds m
cauchys m s = sample $ Cauchys (m,s)

newtype ChiSquare a = ChiSquare a
instance Distribution ChiSquare Integer IO Double where
  sample (ChiSquare a) = Rand.sample (ChiSquare.ChiSquare a)
chiSquare a = sample $ ChiSquare a

newtype Gamma a = Gamma a
instance Distribution Gamma (Double,Double) IO Double where
    sample (Gamma (a,b)) = Rand.sample (Rand.Gamma a (1/b))
gamma a b = sample $ Gamma (a,b)
lpdfGamma x a b = a * log b + (a - 1) * log x - b * x - logGamma a
exponential x = gamma 1 x

newtype InvGamma a = InvGamma a
instance Distribution InvGamma (Double,Double) IO Double where
    sample (InvGamma (a,b)) = do
      x <- gamma a b
      return (1 / x)
invGamma a b = sample $ InvGamma (a,b)

newtype Geometric a = Geometric a
instance Distribution Geometric Double IO Integer where
    sample (Geometric p) = do
      coin <- bernoulli p
      if coin then return 0 else do
        x <- geometric 0 p
        return (x + 1)
geometric 0 p = sample $ Geometric p
geometric a p = do
  g <- geometric 0 p
  return (fromInteger a + g)

newtype LKJ a = LKJ a
instance Distribution LKJ (Double, Interval Integer) IO (ShapedMatrix Double) where
  sample (LKJ (v,(1,2))) = do
    b <- beta v v
    let r = 2*b - 1
    return . ShMat (1,2) (1,2) $ LAD.matrix 2 [1,r,r,1]
corrLKJ v sh = sample $ LKJ (v,sh)

newtype NegBinomial a = NegBinomial a
instance Distribution NegBinomial (Double, Double) IO Integer where
  sample (NegBinomial (a,b)) = do
    l <- gamma a b
    poisson l
negBinomial a b = sample $ NegBinomial (a,b)
lpdfNegBinomial k a b =
  logGamma (a + fromInteger k) - logGamma a - logFactorial k +
  fromInteger k * log (1 / (b + 1)) + a * log (b / (b + 1))

newtype Normal a = Normal a
instance Distribution Normal (Double,Double) IO Double where
    sample (Normal (m,s)) = Rand.sample (Rand.Normal m s)
normal m s = sample $ Normal (m,s)
lpdfNormal x m s = - log (2*pi) / 2 - log s - (((x - m) / s) ** 2) / 2

-- | log-normal distribution
lognormal m s = do
  x <- normal m s
  return (exp x)

newtype Normals a = Normals a
instance Distribution Normals (ShapedVector Double, ShapedVector Double) IO (ShapedVector Double) where
  sample (Normals (m,s)) = joint vector [ normal (m!i) (s!i) | i <- a...b ]
    where (a,b) = bounds m
instance Distribution Normals (ShapedMatrix Double, ShapedMatrix Double) IO (ShapedMatrix Double) where
  sample (Normals (m,s)) = joint matrix [ normal (m!i!j) (s!i!j) | i <- a...b, j <- c...d ]
    where (ShMat (a,b) (c,d) _) = m
normals m s = sample $ Normals (m,s)

newtype OrderedSample a = OrderedSample a
-- | @orderedSample n d@ gives an ordered vector of @n@ samples from a base
-- distribution @d@
orderedSample n d = sample $ OrderedSample (n,d)

newtype PMF a = PMF a
-- | distribution over indices given by vector of probabilities
pmf a = sample $ PMF a

newtype Poisson a = Poisson a
instance Distribution Poisson Double IO Integer where
    sample (Poisson a) = Rand.sample (Poisson.Poisson a)
poisson a = sample $ Poisson a
lpdfPoisson k l = fromInteger k * log l - logFactorial k - l

newtype PoissonProcess a = PoissonProcess a
poissonProcess t rate mean = sample $ PoissonProcess (t, rate, mean)

newtype Uniform a = Uniform a
instance (Random t) => Distribution Uniform (t,t) IO t where
    sample (Uniform (a,b)) = getStdRandom $ randomR (a,b)
uniform a b = sample $ Uniform (a,b)
lpdfUniform x a b | x < a || b < x = log 0
                  | otherwise = log $ 1 / (b - a)
cdfUniform x a b | x < a = 0
                 | x > b = 1
                 | otherwise = (x - a) / (b - a)
lpdfDiscreteUniform x a b | x < a || b < x = log 0
                          | otherwise = log $ 1 / (fromIntegral $ b - a + 1)

newtype Uniforms a = Uniforms a
uniforms a b = sample $ Uniforms (a,b)

newtype Wishart a = Wishart a
instance Distribution Wishart (Integer, ShapedMatrix Double) IO (ShapedMatrix Double) where
  sample (Wishart (n,v)) = do
    c <- joint vector [ chiSquare (n-i+1) | i <- 1...p ] :: IO (ShapedVector Double)
    z <- joint matrix [ normal 0 1 | i <- 1...p, j <- 1...p ] :: IO (ShapedMatrix Double)
    let a = matrix [ if i == j then (c!i)
                else if i >  j then (z!i!j)
                else 0 | i <- 1...p, j <- 1...p ] :: ShapedMatrix Double
        l = chol v :: ShapedMatrix Double
    return $ l <> a <> tr' a <> tr' l
    where (1,p) = bounds v
wishart n v = sample $ Wishart (n,v)
newtype InvWishart a = InvWishart a
instance Distribution InvWishart (Integer, ShapedMatrix Double) IO (ShapedMatrix Double) where
  sample (InvWishart (n,w)) = do
    x <- wishart n (inv w)
    return (inv x)
invWishart n w = sample $ InvWishart (n,w)
