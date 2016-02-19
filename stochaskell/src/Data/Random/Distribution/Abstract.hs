{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies #-}
module Data.Random.Distribution.Abstract where

class Distribution d s m t | d m -> s t where
    sample :: d s -> m t

data BernoulliLogit a = BernoulliLogit a
bernoulliLogit l = sample $ BernoulliLogit l

data Normal a = Normal a a
normal m s = sample $ Normal m s
