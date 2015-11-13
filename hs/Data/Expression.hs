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

type Id = String
type Nat = Int
type R = Double


------------------------------------------------------------------------------
-- EXPRESSIONS                                                              --
------------------------------------------------------------------------------

type Pointer = Nat
type Level = Nat

data NodeRef = Internal Level Pointer
             | External Id
             | Constant Rational
             | Index NodeRef [NodeRef]
             deriving (Eq, Ord)
data Node = Apply { fName :: Id
                  , fArgs :: [NodeRef]
                  , fRetT :: Type
                  }
          | Array { aLevel :: Level
                  , aShape :: [NodeRef]
                  , aDefs  :: DAG
                  , aHead  :: NodeRef
                  , aHeadT :: Type
                  }
          deriving (Eq, Ord)

data DAG = DAG { dagLevel :: Level
               , inputs :: [Id]
               , bimap  :: Bimap.Bimap Node Pointer
               } deriving (Eq, Ord)
emptyDAG = DAG 0 [] Bimap.empty

type Block = [DAG]
emptyBlock = [emptyDAG]

data DExpr = DExpr { exprType  :: Type
                   , fromDExpr :: State Block NodeRef }
runDExpr e = runState (fromDExpr e) emptyBlock
instance Eq DExpr where e == f = runDExpr e == runDExpr f

newtype Expr t = Expr { erase :: DExpr }
expr :: forall t. (ExprType t) => State Block NodeRef -> Expr t
expr = Expr . DExpr t     where (TypeIs t) = typeOf :: TypeOf t
fromExpr = fromDExpr . erase
runExpr  =  runDExpr . erase


------------------------------------------------------------------------------
-- TYPES                                                                    --
------------------------------------------------------------------------------

data Type
    = IntT
    | RealT
    | ConstrainedT Type (Maybe NodeRef) (Maybe NodeRef)
    | ArrayT Int Type
    deriving (Eq, Ord)

newtype TypeOf t = TypeIs Type
class ExprType t where
    typeOf :: TypeOf t

instance ExprType Integer where
    typeOf = TypeIs IntT
instance ExprType Bool where
    typeOf = TypeIs $
        ConstrainedT IntT (Just $ Constant 0) (Just $ Constant 1)
instance ExprType R where
    typeOf = TypeIs RealT
instance forall a b. (ExprType b) => ExprType (a -> b) where
    typeOf = TypeIs $ case t of (ArrayT n r) -> ArrayT (n+1) r
                                r            -> ArrayT 1 r
      where (TypeIs t) = typeOf :: TypeOf b


------------------------------------------------------------------------------
-- DEBUGGING OUTPUT                                                         --
------------------------------------------------------------------------------

instance Show Node where
    show (Apply f js t) =
        f ++ " " ++ intercalate " " (map show js) ++ " :: " ++ show t
    show (Array _ sh dag@(DAG level is _) ret t) =
        "(" ++ args ++ ") [ " ++ body ++ " ]"
      where f i b = i ++ " < " ++ show b
            args = intercalate ", " $ zipWith f is sh
            body = show ret ++ " :: " ++ show t ++
                if show dag == "" then "" else " |\n" ++ show dag

instance Show NodeRef where
    show (Internal level i) = "#" ++ show level ++ "." ++ show i
    show (External s) = s
    show (Constant c) = if d == 1 then show n
                                  else "(" ++ show n ++ "/" ++ show d ++ ")"
      where n = numerator c
            d = denominator c
    show (Index f js) = show f ++ show (reverse js)

instance Show DAG where
    show (DAG level _ bmap) = intercalate "\n" . map ("    " ++) $ lines defs
      where defs = unlines . map f $ Bimap.toAscListR bmap
            f (i,n) = show (Internal level i) ++ " = " ++ show n

instance Show Type where
    show IntT = "Z"
    show RealT = "R"
    show (ConstrainedT t Nothing  Nothing ) = show t
    show (ConstrainedT t (Just l) Nothing ) = show t ++ "[" ++ show l ++ "..]"
    show (ConstrainedT t Nothing  (Just h)) = show t ++ "[.." ++ show h ++ "]"
    show (ConstrainedT t (Just l) (Just h)) = show t ++ "[" ++ show l ++ ".."
                                                              ++ show h ++ "]"
    show (ArrayT 1 e) = "[ " ++ show e ++ " ]"
    show (ArrayT n e) = "[" ++ show (ArrayT (n-1) e) ++ "]"


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
        p (External s) = s `elem` inputs
        p (Constant _) = False
        p (Index a is) = p a || any p is

-- collect External references
externRefs (DAG _ _ defs) = go defs
  where go defs = concat . map (f . snd) $ Bimap.toAscListR defs
        f (Apply _ args _) = mapMaybe extern args
        f (Array _ sh (DAG _ _ defs') r _) = mapMaybe extern (r:sh) ++ go defs'
        extern (External id) = Just id
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
        ids = [ "$" ++ show (depth+1) ++ "." ++ show i | i <- [1..length sh] ]
        e = erase $ AA.apply ar (map fromString ids)
        (ret, vdag:newBlock) = runState (fromDExpr e) $
            DAG (depth + 1) ids Bimap.empty : block
    put newBlock
    hashcons $ Array depth sh vdag ret (exprType e)

-- constant floating
floatArray ar = do
    dag:_ <- get
    sh <- sequence . map fromExpr $ AA.shape ar
    if varies dag sh || length (inputs dag `intersect` capture ar) > 0
      then makeArray ar sh
      else liftBlock $ floatArray ar

array n a = if length (AA.shape a) == n
                then expr $ floatArray a
                else error "dimension mismatch"

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
    pi = "pi"
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

instance (ExprType t) => IsString (Expr t) where
    fromString = expr . return . External

instance (ExprType b) => Indexable (EVector a b) (Expr a) (Expr b) where
    a ! e = expr $ do
        f <- fromExpr a
        j <- fromExpr e
        return $ case f of
            (Index g js) -> Index g (j:js)
            otherwise    -> Index f [j]

instance (ExprType e) => LinearOperator (EMatrix r c e)
                                        (EVector c e) (EVector r e) where
    (#>) = apply2 "#>"

instance (ExprType e) => SquareMatrix (EMatrix n n e) where
    chol = apply1 "chol"
