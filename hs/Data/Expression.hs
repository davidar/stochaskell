{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables,
             TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module Data.Expression where

import Data.Array.Abstract (Indexable,LinearOperator,SquareMatrix)
import qualified Data.Array.Abstract as AA
import qualified Data.Bimap as Bimap
import Data.List
import Data.Maybe
import Data.Ratio
import Data.String
import Control.Applicative
import Control.Monad.State

instance (Ord k, Ord v) => Ord (Bimap.Bimap k v) where
    m `compare` n = Bimap.toAscList m `compare` Bimap.toAscList n

type Nat = Int
type R = Double


------------------------------------------------------------------------------
-- EXPRESSIONS                                                              --
------------------------------------------------------------------------------

type Pointer = Nat
type Level = Nat

data Scope = Builtin
           | Dummy Level
           | Volatile Level
           deriving (Eq, Ord, Show)
data Id = Id Scope String
        deriving (Eq, Ord, Show)

data NodeRef = Internal Level Pointer
             | External Id Type
             | Constant Rational
             | Index NodeRef [NodeRef]
             deriving (Eq, Ord, Show)
data Node = Apply { fName :: Id
                  , fArgs :: [NodeRef]
                  , typeNode :: Type
                  }
          | Array { aLevel :: Level
                  , aShape :: [AA.Interval NodeRef]
                  , aDefs  :: DAG
                  , aHead  :: NodeRef
                  , typeNode :: Type
                  }
          deriving (Eq, Ord)

data DAG = DAG { dagLevel :: Level
               , inputs :: [Id]
               , bimap  :: Bimap.Bimap Node Pointer
               } deriving (Eq, Ord)
emptyDAG = DAG 0 [] Bimap.empty

type Block = [DAG]
emptyBlock = [emptyDAG]

data DExpr = DExpr { typeDExpr  :: Type
                   , fromDExpr :: State Block NodeRef }
runDExpr e = runState (fromDExpr e) emptyBlock
instance Eq DExpr where e == f = runDExpr e == runDExpr f
instance Ord DExpr where
    e `compare` f = runDExpr e `compare` runDExpr f

newtype Expr t = Expr { erase :: DExpr }
expr :: forall t. (ExprType t) => State Block NodeRef -> Expr t
expr = Expr . DExpr t     where (TypeIs t) = typeOf :: TypeOf t
typeExpr = typeDExpr . erase
fromExpr = fromDExpr . erase
runExpr  =  runDExpr . erase


------------------------------------------------------------------------------
-- TYPES                                                                    --
------------------------------------------------------------------------------

data Type
    = IntT
    | RealT
    | SubrangeT Type (Maybe NodeRef) (Maybe NodeRef)
    | ArrayT (Maybe String) [AA.Interval NodeRef] Type
    deriving (Eq, Ord, Show)

newtype TypeOf t = TypeIs Type
class ExprType t where
    typeOf :: TypeOf t

instance ExprType Integer where
    typeOf = TypeIs IntT
instance ExprType Bool where
    typeOf = TypeIs $
        SubrangeT IntT (Just $ Constant 0) (Just $ Constant 1)
instance ExprType R where
    typeOf = TypeIs RealT
instance forall a b. (ExprType b) => ExprType (a -> b) where
    typeOf = TypeIs $
        case t of
            (ArrayT _ n r) -> ArrayT Nothing (error "undefined array bounds") r
            r              -> ArrayT Nothing (error "undefined array bounds") r
      where (TypeIs t) = typeOf :: TypeOf b

typeRef ref@(Internal level i) = do
    dag:_ <- get
    if level == dagLevel dag
        then return . typeNode . fromJust . Bimap.lookupR i $ bimap dag
        else liftBlock $ typeRef ref
typeRef (External _ t) = return t


------------------------------------------------------------------------------
-- DAG BUILDING                                                             --
------------------------------------------------------------------------------

-- http://okmij.org/ftp/tagless-final/sharing/sharing.pdf
hashcons :: Node -> State Block NodeRef
hashcons e = do
    (DAG level inp vdag):parent <- get
    case Bimap.lookup e vdag of
        Just k ->
            return $ Internal level k
        Nothing -> do
            let k = Bimap.size vdag
            put $ (DAG level inp $ Bimap.insert e k vdag) : parent
            return $ Internal level k

-- perform an action in the enclosing block
liftBlock state = do
    dag:parent <- get
    let (ref, parent') = runState state parent
    put $ dag:parent'
    return ref

-- does a list of expressions depend on the inputs to this block?
varies (DAG level inputs _) xs = level == 0 || any p xs
  where p (Internal l _) = l == level
        p (External s _) = s `elem` inputs
        p (Constant _) = False
        p (Index a is) = p a || any p is

-- collect External references
externRefs (DAG _ _ defs) = go defs
  where go defs = concat . map (f . snd) $ Bimap.toAscListR defs
        f (Apply _ args _) = mapMaybe extern args
        f (Array _ sh (DAG _ _ defs') r _) =
            mapMaybe extern [r] ++ mapMaybe (extern . AA.lowerBound) sh
                                ++ mapMaybe (extern . AA.cardinality) sh ++ go defs'
        extern (External id _) = Just id
        extern _ = Nothing
        -- TODO Index


------------------------------------------------------------------------------
-- FUNCTION APPPLICATION                                                    --
------------------------------------------------------------------------------

-- simplify and float constants
simplify :: Node -> State Block NodeRef
simplify (Apply "+" [Constant x, Constant y] _) = return . Constant $ x + y
simplify (Apply "-" [Constant x, Constant y] _) = return . Constant $ x - y
simplify (Apply "*" [Constant x, Constant y] _) = return . Constant $ x * y
simplify (Apply "/" [Constant x, Constant y] _) = return . Constant $ x / y
simplify e = do
    dag:parent <- get
    if varies dag $ fArgs e
      then hashcons e
      else liftBlock $ simplify e

apply :: forall a r.    (ExprType r) => Id ->    [ Expr a ]    -> Expr r
apply f xs = expr $ do
    js <- sequence $ map fromExpr xs
    simplify $ Apply f js t
  where (TypeIs t) = typeOf :: TypeOf r

apply1 :: forall a r.   (ExprType r) => Id -> Expr a           -> Expr r
apply1 f x = apply f [x]

apply2 :: forall a b r. (ExprType r) => Id -> Expr a -> Expr b -> Expr r
apply2 f x y = expr $ do
    i <- fromExpr x
    j <- fromExpr y
    simplify $ Apply f [i,j] t
  where (TypeIs t) = typeOf :: TypeOf r


------------------------------------------------------------------------------
-- ARRAY COMPREHENSIONS                                                     --
------------------------------------------------------------------------------

type AAI r = AA.AbstractArray (Expr Integer) (Expr r)
type EVector n   e = Expr (n -> e)
type EMatrix r c e = Expr (r -> c -> e)

type FI1 r = Integer -> r
type FI2 r = Integer -> FI1 r
type FI3 r = Integer -> FI2 r
type FI4 r = Integer -> FI3 r
type FI5 r = Integer -> FI4 r
type FI6 r = Integer -> FI5 r
type FI7 r = Integer -> FI6 r
type FI8 r = Integer -> FI7 r
type FI9 r = Integer -> FI8 r

-- External references captured by this closure
capture ar = externRefs adag
  where dim = length $ AA.shape ar
        dummy = replicate dim 0
        (_,adag:_) = runExpr $ AA.apply ar dummy

makeArray ar sh = do
    block <- get
    let dag = head block
        depth = dagLevel dag
        ids = [ Id (Dummy $ depth + 1) (show i) | i <- [1..length sh] ]
        e = erase $ AA.apply ar [ expr . return $ External i IntT | i <- ids ]
        (ret, vdag:newBlock) = runState (fromDExpr e) $
            DAG (depth + 1) ids Bimap.empty : block
    put newBlock
    hashcons $ Array depth sh vdag ret (ArrayT Nothing sh $ typeDExpr e)

-- constant floating
floatArray ar = do
    dag:_ <- get
    sh <- sequence . flip map (AA.shape ar) $ \interval -> do
        i <- fromExpr $ AA.lowerBound  interval
        z <- fromExpr $ AA.cardinality interval
        return $ AA.Interval i z
    if varies dag (map AA.lowerBound  sh) ||
       varies dag (map AA.cardinality sh) ||
       length (inputs dag `intersect` capture ar) > 0
      then makeArray ar sh
      else liftBlock $ floatArray ar

array :: (Num i, ExprType i, ExprType t) =>
    Int -> AA.AbstractArray (Expr i) (Expr e) -> Expr t
array n a = if length sh == n
                then expr $ floatArray a
                else error "dimension mismatch"
  where sh = AA.shape a

array1 :: (ExprType r) => AAI r -> Expr (FI1 r)
array1 = array 1
array2 :: (ExprType r) => AAI r -> Expr (FI2 r)
array2 = array 2
array3 :: (ExprType r) => AAI r -> Expr (FI3 r)
array3 = array 3
array4 :: (ExprType r) => AAI r -> Expr (FI4 r)
array4 = array 4
array5 :: (ExprType r) => AAI r -> Expr (FI5 r)
array5 = array 5
array6 :: (ExprType r) => AAI r -> Expr (FI6 r)
array6 = array 6
array7 :: (ExprType r) => AAI r -> Expr (FI7 r)
array7 = array 7
array8 :: (ExprType r) => AAI r -> Expr (FI8 r)
array8 = array 8
array9 :: (ExprType r) => AAI r -> Expr (FI9 r)
array9 = array 9

vector v = array1 v
matrix m = array2 m

index :: (ExprType r) => Expr a -> [Expr i] -> Expr r
index a es = expr $ do
    f <- fromExpr a
    js <- sequence $ map fromExpr es
    return $ Index f js


------------------------------------------------------------------------------
-- INSTANCES                                                                --
------------------------------------------------------------------------------

fromRational' :: (ExprType t) => Rational -> Expr t
fromRational' = expr . return . Constant

instance (Num t, ExprType t) => Num (Expr t) where
    (+) = apply2 "+"
    (-) = apply2 "-"
    (*) = apply2 "*"

    negate = apply1 "negate"
    abs    = apply1 "abs"
    signum = apply1 "signum"

    fromInteger x = fromRational' (x % 1)

instance (Fractional t, ExprType t) => Fractional (Expr t) where
    fromRational = fromRational'
    (/) = apply2 "/"

instance (Floating t, ExprType t) => Floating (Expr t) where
    pi = apply "pi" []
    (**) = apply2 "**"

    exp   = apply1 "exp"
    log   = apply1 "log"
    sqrt  = apply1 "sqrt"
    sin   = apply1 "sin"
    cos   = apply1 "cos"
    tan   = apply1 "tan"
    asin  = apply1 "asin"
    acos  = apply1 "acos"
    atan  = apply1 "atan"
    sinh  = apply1 "sinh"
    cosh  = apply1 "cosh"
    tanh  = apply1 "tanh"
    asinh = apply1 "asinh"
    acosh = apply1 "acosh"
    atanh = apply1 "atanh"

instance IsString Id where
    fromString = Id Builtin

instance forall t. (ExprType t) => IsString (Expr t) where
    fromString s = expr . return $ External (Id Builtin s) t
      where (TypeIs t) = typeOf :: TypeOf t

instance (ExprType b) => Indexable (EVector a b) (Expr a) (Expr b) where
    a ! e = expr $ do
        f <- fromExpr a
        j <- fromExpr e
        return $ case f of
            (Index g js) -> Index g (j:js)
            otherwise    -> Index f [j]

asVector :: (ExprType e) => EVector n e -> EVector n e
asVector v = expr $ do
    i <- fromExpr v
    (ArrayT _ [n] t) <- typeRef i
    simplify $ Apply "asVector" [i] (ArrayT (Just "vector") [n] t)

asMatrix :: (ExprType e) => EMatrix r c e -> EMatrix r c e
asMatrix m = expr $ do
    i <- fromExpr m
    (ArrayT _ [r,c] t) <- typeRef i
    simplify $ Apply "asMatrix" [i] (ArrayT (Just "matrix") [r,c] t)

instance (ExprType e) => LinearOperator (EMatrix r c e)
                                        (EVector c e) (EVector r e) where
    m #> v = expr $ do
        i <- fromExpr $ asMatrix m
        j <- fromExpr $ asVector v
        (ArrayT _ [r,c] t) <- typeRef i
        simplify $ Apply "#>" [i,j] (ArrayT (Just "vector") [r] t)

instance (ExprType e) => SquareMatrix (EMatrix n n e) where
    chol m = expr $ do
        i <- fromExpr $ asMatrix m
        t <- typeRef i
        simplify $ Apply "chol" [i] t
