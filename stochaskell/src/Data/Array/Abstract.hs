{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances,
             TypeOperators, GADTs, FunctionalDependencies, FlexibleContexts,
             UndecidableInstances #-}
module Data.Array.Abstract ( module Data.Array.Abstract, LA.tr, LA.tr' ) where

import qualified Data.Array.Unboxed as A
import Data.Ix
import Foreign.Storable (Storable)
import GHC.Exts
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Data as LAD

list :: (IsList l, IsList l', Item l ~ Item l') => l -> l'
list = fromList . toList

zipWithA :: (Ix i, Show i, A.IArray x a, A.IArray y b, A.IArray z c)
  => (a -> b -> c) -> x i a -> y i b -> z i c
zipWithA f a b | A.bounds a == A.bounds b =
                 A.listArray (A.bounds a) $ zipWith f (A.elems a) (A.elems b)
               | otherwise = error $ "arrays not same shape: "++ show (A.bounds a)
                                                      ++" /= "++ show (A.bounds b)

instance (Ix i, Show i) => Ix [i] where
  range (x:xs,y:ys) = [ z:zs | z <- range (x,y), zs <- range (xs,ys) ]
  range ([],[]) = [[]]
  range _ = []
  inRange (x:xs,y:ys) (z:zs) = x <= z && z <= y && inRange (xs,ys) zs
  inRange ([],[]) [] = True
  inRange _ _ = False
  index b i | inRange b i = unsafeIndex b i
            | otherwise = error $ "index "++ show i ++" out of range "++ show b
    where unsafeIndex (x:xs,y:ys) (z:zs) =
            index (x,y) z * rangeSize (xs,ys) + unsafeIndex (xs,ys) zs
          unsafeIndex ([],[]) [] = 0

rrange :: (Ix i) => ([i],[i]) -> [[i]]
rrange (x:xs,y:ys) = [ z:zs | zs <- rrange (xs,ys), z <- range (x,y) ]
rrange ([],[]) = [[]]
rrange _ = []

type Interval i = (i,i)
cardinality :: (Num a) => Interval a -> a
cardinality (a,b) = b - a + 1

data AbstractArray i e = AArr [Interval i] ([i] -> e)
apply :: AbstractArray i e -> [i] -> e
apply = (!) -- TODO

toArray :: (Ix i, Show i) => AbstractArray i e -> A.Array [i] e
toArray a = A.array (bounds a) assocs
  where assocs = [ (i, apply a i) | i <- range (bounds a) ]

fromArray :: (Ix i, Show i) => A.Array [i] e -> AbstractArray i e
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

coerceShape :: (Eq a) => [Interval a] -> [Interval a] -> [Interval a]
coerceShape sh sh' = zipWith g sh sh'
  where g (lo,hi) (lo',hi') | lo /= lo' = error "dimension lower bound mismatch"
                            | hi  == hi' = (lo,hi)
                            | lo' == hi' = (lo,hi)
                            | lo  == hi  = (lo,hi')
                            | otherwise = error "dimensions incompatible"

infix 5 ...
(...) :: a -> a -> AbstractArray a a
a...b = AArr [(a,b)] f
  where f [x] = x
        f _ = error "shape mismatch"

infixl 9 !
class Indexable a i e | a -> i e where
    (!) :: a -> i -> e
    bounds :: a -> Interval i
    deleteIndex :: a -> i -> a
    insertIndex :: a -> i -> e -> a
shape :: (Indexable a [i] e) => a -> [Interval i]
shape x = uncurry zip . bounds $ x

instance Indexable [e] Int e where
    (!) = (!!)
    bounds xs = (0, length xs - 1)
    deleteIndex xs i   = take i xs ++ drop (i+1) xs
    insertIndex xs i x = take i xs ++ [x] ++ drop i xs

instance Indexable (AbstractArray i e) [i] e where
    (AArr _ f) ! i = f i
    bounds (AArr s _) = unzip s

instance (Ix i) => Indexable (A.Array i e) i e where
    (!) = (A.!)
    bounds = A.bounds

instance (A.IArray A.UArray e, Ix i) => Indexable (A.UArray i e) i e where
    (!) = (A.!)
    bounds = A.bounds

class (Indexable v i e) => Vector v i e | v -> i e, i e -> v where
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

instance (Storable t) => Indexable (ShapedVector t) Integer t where
    (ShVec sh v) ! i = LAD.toList v !! index sh i
    bounds (ShVec sh _) = sh
instance (Storable t) => Vector (ShapedVector t) Integer t where
    vector a = ShVec (head $ shape a) . LAD.fromList . A.elems $ toArray a

infixr 7 *>
class Scalable a v | v -> a where
    (*>) :: a -> v -> v

instance (Num e, Ix i) => Scalable e (A.Array i e) where
    a *> v = (a *) <$> v
instance (A.IArray A.UArray e, Num e, Ix i) => Scalable e (A.UArray i e) where
    a *> v = A.amap (a *) v
instance Scalable Double (ShapedVector Double) where
    a *> (ShVec n   v) = ShVec n   $ (LA.scale) a v
instance Scalable Double (ShapedMatrix Double) where
    a *> (ShMat r c m) = ShMat r c $ (LA.scale) a m

infixr 8 <.>
class InnerProduct v e | v -> e where
    (<.>) :: v -> v -> e

instance (LA.Numeric t) => InnerProduct (ShapedVector t) t where
    (ShVec _ u) <.> (ShVec _ v) = (LA.<.>) u v

class Matrix m i e | m -> i e where
    matrix :: AbstractArray i e -> m

data ShapedMatrix t = ShMat (Interval Integer) (Interval Integer) (LAD.Matrix t)
instance (Show t, LA.Element t) => Show (ShapedMatrix t) where
    show (ShMat _ _ m) = show m
instance (LA.Element t) => Indexable (ShapedMatrix t) Integer (ShapedVector t) where
    (ShMat shr shc m) ! i = ShVec shc $ (LAD.!) m (fromInteger $ i - fst shr)
    bounds (ShMat shr _ _) = shr
instance Matrix (ShapedMatrix Double) Integer Double where
    matrix a = ShMat r c $ LAD.matrix ncol xs
      where ncol = fromInteger . cardinality $ shape a !! 1
            xs = A.elems $ toArray a
            r:c:_ = shape a
instance Monoid (ShapedMatrix Double) where
    mappend (ShMat r c m) (ShMat r' c' m') | c == r' = ShMat r c' $ (LA.<>) m m'

instance LA.Transposable (ShapedMatrix Double) (ShapedMatrix Double) where
    tr  (ShMat r c m) = ShMat c r $ LA.tr  m
    tr' (ShMat r c m) = ShMat c r $ LA.tr' m

infixr 8 #>
infixl 7 <\>
class LinearOperator m v | m -> v, v -> m where
    (#>)  :: m -> v -> v
    (<\>) :: m -> v -> v
    diag  :: v -> m
    asColumn :: v -> m
    asRow :: v -> m

outer :: (LinearOperator m v, Num m) => v -> v -> m
outer u v = asColumn u * asRow v

instance LinearOperator (ShapedMatrix Double) (ShapedVector Double) where
    (ShMat r _ m)  #> (ShVec _ v) = ShVec r . head . LAD.toColumns $ (LA.<>)  m (LAD.asColumn v)
    (ShMat _ c m) <\> (ShVec _ v) = ShVec c . head . LAD.toColumns $ (LA.<\>) m (LAD.asColumn v)
    diag     (ShVec n v) = ShMat n n $ LAD.diag v
    asColumn (ShVec (l,h) v) = ShMat (l,h) (l,l) $ LAD.asColumn v
    asRow    (ShVec (l,h) v) = ShMat (l,l) (l,h) $ LAD.asRow v

class SquareMatrix m e | m -> e where
    chol   :: m -> m -- lower-triangular
    inv    :: m -> m
    det    :: m -> e
    logDet :: m -> e -- log abs det

instance SquareMatrix (ShapedMatrix Double) Double where
    chol   (ShMat r c m) = ShMat r c $ (LA.tr . LA.chol . LA.sym) m
    inv    (ShMat r c m) = ShMat r c $ LA.inv m
    det    (ShMat _ _ m) = LA.det m
    logDet (ShMat _ _ m) = log_det
      where (inv_m,(log_det,sign_det)) = LA.invlndet m

class Broadcastable a b c | a b -> c where
    bsxfun :: (c -> c -> c) -> a -> b -> c

class Joint m i r f | m -> i where
    joint :: (AbstractArray i r -> f) -> AbstractArray i (m r) -> m f

instance Joint IO Integer e f where
    joint f a = do a' <- sequence $ toArray a
                   return $ f (fromArray a')
