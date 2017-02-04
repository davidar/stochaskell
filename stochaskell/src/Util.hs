module Util where

import qualified Data.Bimap as Bimap

compose :: [a -> a] -> a -> a
compose = foldr (.) id

selectItems :: [a] -> [Bool] -> [a]
selectItems xs ps = map fst . filter snd $ zip xs ps

firstDiff :: (Eq a) => [a] -> [a] -> Int
firstDiff (x:xs) (y:ys) | x == y = 1 + firstDiff xs ys
firstDiff _ _ = 0

instance (Ord k, Ord v) => Ord (Bimap.Bimap k v) where
    m `compare` n = Bimap.toAscList m `compare` Bimap.toAscList n

instance (Num t) => Num [t] where
    (+) = zipWith (+)
    (-) = zipWith (-)
    (*) = zipWith (*)
    negate = map negate
    abs    = map abs
    signum = map signum
    fromInteger x = [fromInteger x]
