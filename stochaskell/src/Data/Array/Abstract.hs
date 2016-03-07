{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances,
             TypeOperators, GADTs, FunctionalDependencies, FlexibleContexts,
             UndecidableInstances #-}
module Data.Array.Abstract where

import qualified Data.Array as A
import Data.Ix
import Foreign.Storable (Storable)
import GHC.Exts
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Data as LAD

asList :: (IsList l, IsList l', Item l ~ Item l') => l -> l'
asList = fromList . toList

instance (Ix i) => Ix [i] where
  range (x:xs,y:ys) = [ z:zs | z <- range (x,y), zs <- range (xs,ys) ]
  range ([],[]) = [[]]
  range _ = []
  inRange (x:xs,y:ys) (z:zs) = x <= z && z <= y && inRange (xs,ys) zs
  inRange ([],[]) [] = True
  inRange _ _ = False
  index b i | inRange b i = unsafeIndex b i
            | otherwise = error "index out of range"
    where unsafeIndex (x:xs,y:ys) (z:zs) =
            index (x,y) z * rangeSize (xs,ys) + unsafeIndex (xs,ys) zs
          unsafeIndex ([],[]) [] = 0
          unsafeIndex _ _ = undefined

type Interval i = (i,i)
cardinality :: (Num a) => Interval a -> a
cardinality (a,b) = b - a + 1

data AbstractArray i e = AArr { shape :: [Interval i]
                              , apply :: [i] -> e
                              }

toArray :: (Ix i) => AbstractArray i e -> A.Array [i] e
toArray a = A.array bounds assocs
  where bounds = unzip (shape a)
        assocs = [ (i, apply a i) | i <- range bounds ]

fromArray :: (Ix i) => A.Array [i] e -> AbstractArray i e
fromArray a = AArr sh $ (A.!) a
  where sh = zip (fst $ A.bounds a) (snd $ A.bounds a)

instance Functor (AbstractArray i) where
    fmap f (AArr sh g) = AArr sh (f . g)

instance Applicative (AbstractArray i) where
    pure x = AArr [] (const x)
    (AArr sa f) <*> (AArr sb g) = AArr (sa ++ sb) h
      where h x = let (is,js) = splitAt (length sa) x
                  in f is $ g js

-- emulate <https://ghc.haskell.org/trac/ghc/ticket/10976>
instance Monad (AbstractArray i) where
    return = pure
    (AArr sa f) >>= k = AArr (sa ++ sb) h
      where sb = shape (k $ error "non-rectangular array")
            h x = let (is,js) = splitAt (length sa) x
                  in k (f is) `apply` js

fromShape :: [Interval a] -> AbstractArray a [a]
fromShape = flip AArr id

infix 5 ...
(...) :: a -> a -> AbstractArray a a
a...b = AArr [(a,b)] f
  where f [x] = x
        f _ = error "shape mismatch"

infixl 9 !
class Vector v i e | v -> i e, i e -> v where
    (!) :: v -> i -> e
    vector :: AbstractArray i e -> v

data ShapedVector t = ShVec (Interval Integer) (LAD.Vector t)
instance (Show t, Storable t) => Show (ShapedVector t) where
    show (ShVec _ v) = show v
instance (Storable t) => IsList (ShapedVector t) where
    type Item (ShapedVector t) = t
    fromList xs = ShVec (1, fromIntegral $ length xs) (LAD.fromList xs)
    toList (ShVec _ v) = LAD.toList v
instance (Num (LAD.Vector t)) => Num (ShapedVector t) where
    (ShVec s u) + (ShVec z v) | s == z = ShVec s (u + v)
    -- TODO ...

instance (Storable t) => Vector (ShapedVector t) Integer t where
    vector a = ShVec (head $ shape a) . LAD.fromList . A.elems $ toArray a
    (ShVec sh v) ! i = LAD.toList v !! index sh i

class Matrix m i e | m -> i e, i e -> m where
    matrix :: AbstractArray i e -> m

instance Matrix (LAD.Matrix Double) Integer Double where
    matrix a = LAD.matrix ncol xs
      where ncol = fromInteger . cardinality $ shape a !! 1
            xs = A.elems $ toArray a

class LinearOperator m u v | m -> u v where
    (#>) :: m -> u -> v

instance LinearOperator (LAD.Matrix Double) (ShapedVector Double) (ShapedVector Double) where
    m #> (ShVec sh v) = ShVec sh . head . LAD.toColumns $ (LA.<>) m (LAD.asColumn v)

class SquareMatrix m where
    chol :: m -> m

instance SquareMatrix (LAD.Matrix Double) where
    chol = LA.tr . LA.chol . LA.trustSym

class Joint m i r f | m -> i where
    joint :: (AbstractArray i r -> f) -> AbstractArray i (m r) -> m f

instance Joint IO Integer e f where
    joint f a = do a' <- sequence $ toArray a
                   return $ f (fromArray a')
