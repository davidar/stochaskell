{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances,
             TypeOperators, GADTs, FunctionalDependencies, FlexibleContexts,
             UndecidableInstances #-}
module Data.Array.Abstract
  ( LA.Transposable(..)
  , (...)
  , AbstractArray
  , Indexable(..)
  , InnerProduct(..)
  , Interval
  , Joint(..)
  , LinearOperator(..)
  , MLDivide(..)
  , Matrix(..)
  , Scalable(..)
  , ShapedMatrix(..)
  , ShapedVector(..)
  , SquareMatrix(..)
  , Vector(..)
  , cardinality
  , coerceShape
  , deleteAt
  , fromShape
  , insertAt
  , list
  , outer
  , replaceAt
  , rrange
  , shape
  , toArray
  , zipWithA
  ) where

import Control.DeepSeq
import qualified Data.Array.Unboxed as A
import Data.Ix
import Debug.Trace
import Foreign.Storable (Storable)
import GHC.Exts
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Data as LAD

instance (Ix i, A.IArray A.UArray e, NFData i, NFData e) => NFData (A.UArray i e) where
  rnf x = rnf (A.bounds x, A.elems x)

-- | convert between list types (eg. @['Double']@ to 'Language.Stochaskell.RVec')
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

-- | closed interval of integers
type Interval i = (i,i)
cardinality :: (Num a) => Interval a -> a
cardinality (a,b) = b - a + 1

-- | create abstract arrays with an expression like
--
-- > [ i + j | i <- 1...rows, j <- 1...cols ]
data AbstractArray i e = AArr [Interval i] ([i] -> e)

toArray :: (Ix i, Show i) => AbstractArray i e -> A.Array [i] e
toArray a = A.array (bounds a) assocs
  where assocs = [ (i, a!i) | i <- range (bounds a) ]

fromArray :: (Ix i, Show i) => A.Array [i] e -> AbstractArray i e
fromArray a = AArr sh $ (A.!) a
  where sh = zip (fst $ A.bounds a) (snd $ A.bounds a)

instance (Ix i, Show i, Eq e) => Eq (AbstractArray i e) where
  a == b = bounds a == bounds b && elems a == elems b

instance (Ix i, Show i, Ord e) => Ord (AbstractArray i e) where
  compare a b = case bounds a `compare` bounds b of
    EQ -> elems a `compare` elems b
    o -> o

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
                  in k (f is) ! js

fromShape :: [Interval a] -> AbstractArray a [a]
fromShape = flip AArr id

coerceShape :: (Eq a, Show a) => [Interval a] -> [Interval a] -> Maybe [Interval a]
coerceShape sh sh' = sequence $ zipWith g sh sh'
  where g (lo,hi) (lo',hi') | lo /= lo' = trace "dimension lower bound mismatch" Nothing
                            | hi  == hi' = Just (lo,hi)
                            | lo' == hi' = Just (lo,hi)
                            | lo  == hi  = Just (lo,hi')
                            | otherwise = Nothing

-- | create abstract array containing given integer range
infix 5 ...
(...) :: a -> a -> AbstractArray a a
a...b = AArr [(a,b)] f
  where f [x] = x
        f _ = error "shape mismatch"

infixl 9 !
-- | generalised array interface
class Indexable a i e | a -> i e where
    -- | see @('Data.Array.!')@
    (!) :: a -> i -> e
    -- | see 'Data.Array.bounds'
    bounds :: a -> Interval i
    -- | @a ``deleteAt`` i@ -- remove @i@'th element of array
    deleteAt :: (Indexable a i e) => a -> i -> a
    deleteAt = deleteIndex
    -- | @a ``insertAt`` (i,e)@ --
    -- move @a!i@ and following elements up one, and set @a!i = e@
    insertAt :: (Indexable a i e) => a -> (i,e) -> a
    insertAt a (i,e) = insertIndex a i e
    -- | @a ``replaceAt`` (i,e)@ -- set @a!i = e@
    replaceAt :: (Indexable a i e) => a -> (i,e) -> a
    replaceAt a (i,e) = replaceIndex a i e
    deleteIndex :: a -> i -> a
    deleteIndex = deleteAt
    insertIndex :: a -> i -> e -> a
    insertIndex a i e = insertAt a (i,e)
    replaceIndex :: a -> i -> e -> a
    replaceIndex a i e = replaceAt a (i,e)

-- | bounds on each dimension of array
shape :: (Indexable a [i] e) => a -> [Interval i]
shape x = uncurry zip . bounds $ x

instance Indexable [e] Int e where
    (!) = (!!)
    bounds xs = (0, length xs - 1)
    deleteIndex xs i   = take i xs ++ drop (i+1) xs
    insertIndex xs i x = take i xs ++ [x] ++ drop i xs
    replaceIndex xs i x = take i xs ++ [x] ++ drop (i+1) xs

instance Indexable (AbstractArray i e) [i] e where
    (AArr _ f) ! i = f i
    bounds (AArr s _) = unzip s

elems :: (Ix i, Show i) => AbstractArray i e -> [e]
elems a = [a!i | i <- range (bounds a)]

instance (Ix i) => Indexable (A.Array i e) i e where
    (!) = (A.!)
    bounds = A.bounds

instance (A.IArray A.UArray e, Ix i) => Indexable (A.UArray i e) i e where
    (!) = (A.!)
    bounds = A.bounds

-- | generalised vector interface
class Vector v i e | v -> i e, i e -> v where
    -- | convert from abstract array
    vector :: AbstractArray i e -> v
    -- | see 'blockMatrix'
    blockVector :: [v] -> v
    -- | number of elements in vector
    vectorSize :: v -> i

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
instance Vector (ShapedVector Double) Integer Double where
    vector a = ShVec (head $ shape a) . LAD.fromList . A.elems $ toArray a
    blockVector vs = ShVec (1, fromIntegral n) v'
      where v' = LAD.vjoin [v | ShVec _ v <- vs]
            n = LAD.size v'

infixr 7 *>
class Scalable a v | v -> a where
    -- | scalar by array multiplication (see 'Algebra.Vector.*>')
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
    -- | generalisation of 'Numeric.LinearAlgebra.<.>'
    (<.>) :: v -> v -> e

instance (LA.Numeric t) => InnerProduct (ShapedVector t) t where
    (ShVec _ u) <.> (ShVec _ v) = (LA.<.>) u v

-- | generalised matrix interface
class Matrix m i e | m -> i e where
    -- | convert from abstract array
    matrix :: AbstractArray i e -> m
    -- | create a block matrix
    blockMatrix :: [[m]] -> m
    -- | identity matrix
    eye :: i -> m
    -- | zero matrix
    zeros :: i -> i -> m
    -- | number of rows
    matrixRows :: m -> i
    -- | number of columns
    matrixCols :: m -> i
    -- | pack vectors as rows of a matrix
    designMatrix :: (Indexable v i e, Num i) => i -> AbstractArray i v -> m
    designMatrix n a = matrix $ do
        v <- a
        i <- 1...n
        return (v!i)

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
    blockMatrix rows = ShMat (1, fromIntegral r) (1, fromIntegral c) m'
      where m' = LAD.fromBlocks [[m | ShMat (1,r) (1,c) m <- row, r > 0, c > 0] | row <- rows]
            (r,c) = LAD.size m'
instance Semigroup (ShapedMatrix Double) where
    a@(ShMat r c m) <> b@(ShMat r' c' m')
      | c == r' = ShMat r c' $ (LA.<>) m m'
      | otherwise = error $ "cannot multiply "++ show a ++" with "++ show b

instance LA.Transposable (ShapedMatrix Double) (ShapedMatrix Double) where
    tr  (ShMat r c m) = ShMat c r $ LA.tr  m
    tr' (ShMat r c m) = ShMat c r $ LA.tr' m

infixr 8 #>
-- | generalised linear operator interface
class LinearOperator m v | m -> v, v -> m where
    -- | see 'Numeric.LinearAlgebra.#>'
    (#>)  :: m -> v -> v
    -- | see 'Numeric.LinearAlgebra.<#'
    (<#)  :: v -> m -> v
    -- | convert vector to diagonal matrix
    diag  :: v -> m
    -- | convert vector to column
    asColumn :: v -> m
    -- | convert vector to row
    asRow :: v -> m
    -- | outer product
    outer :: (LinearOperator m v, Semigroup m) => v -> v -> m
    outer u v = asColumn u <> asRow v

instance LinearOperator (ShapedMatrix Double) (ShapedVector Double) where
    (ShMat r _ m)  #> (ShVec _ v) = ShVec r . head . LAD.toColumns $ (LA.<>)  m (LAD.asColumn v)
    diag     (ShVec n v) = ShMat n n $ LAD.diag v
    asColumn (ShVec (l,h) v) = ShMat (l,h) (l,l) $ LAD.asColumn v
    asRow    (ShVec (l,h) v) = ShMat (l,l) (l,h) $ LAD.asRow v

infixl 7 <\>
class MLDivide m v | v -> m where
    -- | see 'Numeric.LinearAlgebra.<\>'
    (<\>) :: m -> v -> v

instance MLDivide (ShapedMatrix Double) (ShapedVector Double) where
    (ShMat _ c m) <\> (ShVec _ v) = ShVec c . head . LAD.toColumns $ (LA.<\>) m (LAD.asColumn v)
instance MLDivide (ShapedMatrix Double) (ShapedMatrix Double) where
    (ShMat _ c m) <\> (ShMat _ c' v) = ShMat c c' $ (LA.<\>) m v

class SquareMatrix m e | m -> e where
    -- | lower-triangular Cholesky decomposition, see 'Numeric.LinearAlgebra.chol'
    chol   :: m -> m
    -- | see 'Numeric.LinearAlgebra.inv'
    inv    :: m -> m
    -- | see 'Numeric.LinearAlgebra.det'
    det    :: m -> e
    -- | logarithm of the absolute value of the determinant
    logDet :: m -> e

instance SquareMatrix (ShapedMatrix Double) Double where
    chol   (ShMat r c m) = ShMat r c $ (LA.tr . LA.chol . LA.sym) m
    inv    (ShMat r c m) = ShMat r c $ LA.inv m
    det    (ShMat _ _ m) = LA.det m
    logDet (ShMat _ _ m) = log_det
      where (inv_m,(log_det,sign_det)) = LA.invlndet m

class Broadcastable a b c | a b -> c where
    bsxfun :: (c -> c -> c) -> a -> b -> c

class Joint m i r f | m -> i where
    -- | create distribution over arrays as the product of an array of
    -- independent distributions, eg.
    --
    -- > joint matrix [ normal i j | i <- 1...m, j <- 1...n ]
    joint :: (AbstractArray i r -> f) -> AbstractArray i (m r) -> m f

instance Joint IO Integer e f where
    joint f a = do a' <- sequence $ toArray a
                   return $ f (fromArray a')
