{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables, TypeFamilies,
             TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
             FlexibleContexts #-}
module Data.Expression where

import Data.Array.Abstract (LinearOperator,SquareMatrix)
import qualified Data.Array.Abstract as AA
import qualified Data.Bimap as Bimap
import Data.Boolean
import Data.List
import Data.Maybe
import Data.Ratio
import Data.String
import Control.Applicative ()
import Control.Monad.State
import GHC.Exts

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

data ConstVal = Scalar Rational
              | Vector [Rational]
              deriving (Eq, Ord, Show)

data NodeRef = Internal Level Pointer
             | External Id Type
             | Const ConstVal
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
          deriving (Eq, Ord, Show)

data DAG = DAG { dagLevel :: Level
               , inputs :: [Id]
               , bimap  :: Bimap.Bimap Node Pointer
               } deriving (Eq, Ord, Show)
emptyDAG = DAG 0 [] Bimap.empty
nodes dag = Bimap.toAscListR $ bimap dag

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
        SubrangeT IntT (Just . Const $ Scalar 0) (Just . Const $ Scalar 1)
instance ExprType R where
    typeOf = TypeIs RealT
instance forall b. (ExprType b) => ExprType [b] where
    typeOf = TypeIs $
        case t of
            (ArrayT _ _ r) -> ArrayT Nothing (error "undefined array bounds") r
            r              -> ArrayT Nothing (error "undefined array bounds") r
      where (TypeIs t) = typeOf :: TypeOf b

typeRef ref@(Internal level i) = do
    dag:_ <- get
    if level == dagLevel dag
        then return . typeNode . fromJust . Bimap.lookupR i $ bimap dag
        else liftBlock $ typeRef ref
typeRef (External _ t) = return t
typeRef (Const (Scalar _)) = return RealT
typeRef (Const (Vector v)) = return $
    ArrayT Nothing [AA.Interval (Const $ Scalar 1)
                                (Const . Scalar . fromIntegral $ length v)] RealT
typeRef (Index a i) = do
    (ArrayT _ _ t) <- typeRef a
    return t


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
        p (Const _) = False
        p (Index a is) = p a || any p is

-- collect External references
externRefs (DAG _ _ defs) = go defs
  where go defs = concatMap (f . snd) $ Bimap.toAscListR defs
        f (Apply _ args _) = mapMaybe extern args
        f (Array _ sh (DAG _ _ defs') r _) =
            mapMaybe extern [r] ++ mapMaybe (extern . AA.lowerBound) sh
                                ++ mapMaybe (extern . AA.upperBound) sh ++ go defs'
        extern (External id _) = Just id
        extern _ = Nothing
        -- TODO Index


------------------------------------------------------------------------------
-- FUNCTION APPLICATION                                                     --
------------------------------------------------------------------------------

-- simplify and float constants
simplify :: Node -> State Block NodeRef
simplify (Apply op [Const (Scalar x), Const (Scalar y)] _) | isJust f =
    return . Const . Scalar $ fromJust f x y
  where f = lookup op [("+",(+)), ("-",(-)), ("*",(*)), ("/",(/))]
simplify e = do
    dag:parent <- get
    if varies dag $ fArgs e
      then hashcons e
      else liftBlock $ simplify e

apply :: forall a r.    (ExprType r) => Id ->    [ Expr a ]    -> Expr r
apply f xs = expr $ do
    js <- mapM fromExpr xs
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

applyClosed2 :: (ExprType a) => Id -> Expr a -> Expr a -> Expr a
applyClosed2 f x y = expr $ do
    i <- fromExpr x
    j <- fromExpr y
    s <- typeRef i
    t <- typeRef j
    if s /= t then error $ "type mismatch: "++ show s ++" /= "++ show t
              else simplify $ Apply f [i,j] t

apply3 :: forall a b c r. (ExprType r) => Id -> Expr a -> Expr b
                                                     -> Expr c -> Expr r
apply3 f x y z = expr $ do
    i <- fromExpr x
    j <- fromExpr y
    k <- fromExpr z
    simplify $ Apply f [i,j,k] t
  where (TypeIs t) = typeOf :: TypeOf r


------------------------------------------------------------------------------
-- ARRAY COMPREHENSIONS                                                     --
------------------------------------------------------------------------------

type AAI r = AA.AbstractArray (Expr Integer) (Expr r)

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
        i <- fromExpr $ AA.lowerBound interval
        j <- fromExpr $ AA.upperBound interval
        return $ AA.Interval i j
    if varies dag (map AA.lowerBound sh) ||
       varies dag (map AA.upperBound sh) ||
       (not . null $ inputs dag `intersect` capture ar)
      then makeArray ar sh
      else liftBlock $ floatArray ar

array :: (Num i, ExprType i, ExprType t) =>
    Int -> AA.AbstractArray (Expr i) (Expr e) -> Expr t
array n a = if length sh == n
                then expr $ floatArray a
                else error "dimension mismatch"
  where sh = AA.shape a

index :: (ExprType r) => Expr a -> [Expr i] -> Expr r
index a es = expr $ do
    f <- fromExpr a
    js <- mapM fromExpr es
    return $ Index f js


------------------------------------------------------------------------------
-- INSTANCES                                                                --
------------------------------------------------------------------------------

fromRational' :: (ExprType t) => Rational -> Expr t
fromRational' = expr . return . Const . Scalar

instance (Num t) => Num [t] where
    (+) = zipWith (+)
    (-) = zipWith (-)
    (*) = zipWith (*)
    negate = map negate
    abs    = map abs
    signum = map signum
    fromInteger x = [fromInteger x]

instance (Num t, ExprType t) => Num (Expr t) where
    (+) = applyClosed2 "+"
    (-) = applyClosed2 "-"
    (*) = applyClosed2 "*"

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

instance (ExprType e) => AA.Vector (Expr [e]) (Expr Integer) (Expr e) where
    a ! e = expr $ do
        f <- fromExpr a
        j <- fromExpr e
        return $ case f of
            (Index g js) -> Index g (j:js)
            _            -> Index f [j]
    vector = asVector . array 1
instance (ExprType e) => AA.Matrix (Expr [[e]]) (Expr Integer) (Expr e) where
    matrix = asMatrix . array 2

isConstVector :: NodeRef -> Bool
isConstVector (Const (Vector _)) = True
isConstVector _ = False

asVector :: (ExprType e) => Expr [e] -> Expr [e]
asVector v = expr $ do
    i <- fromExpr v
    if isConstVector i then return i else do
      (ArrayT _ [n] t) <- typeRef i
      simplify $ Apply "asVector" [i] (ArrayT (Just "vector") [n] t)

asMatrix :: (ExprType e) => Expr [[e]] -> Expr [[e]]
asMatrix m = expr $ do
    i <- fromExpr m
    (ArrayT _ [r,c] t) <- typeRef i
    simplify $ Apply "asMatrix" [i] (ArrayT (Just "matrix") [r,c] t)

instance (ExprType e) => LinearOperator (Expr [[e]])
                                        (Expr [e]) (Expr [e]) where
    m #> v = expr $ do
        i <- fromExpr $ asMatrix m
        j <- fromExpr $ asVector v
        (ArrayT _ [r,c] t) <- typeRef i
        simplify $ Apply "#>" [i,j] (ArrayT (Just "vector") [r] t)

instance (ExprType e) => SquareMatrix (Expr [[e]]) where
    chol m = expr $ do
        i <- fromExpr $ asMatrix m
        t <- typeRef i
        simplify $ Apply "chol" [i] t

instance Boolean (Expr Bool) where
    true  = "true"
    false = "false"
    notB  = apply1 "not"
    (&&*) = apply2 "&&"
    (||*) = apply2 "||"

type instance BooleanOf (Expr t) = Expr Bool

instance (ExprType t) => IfB (Expr t) where
    ifB = apply3 "ifThenElse"

instance (ExprType t) => EqB (Expr t) where
    (==*) = apply2 "=="
    (/=*) = apply2 "/="

instance (ExprType t) => OrdB (Expr t) where
    (<*)  = apply2 "<"
    (<=*) = apply2 "<="
    (>=*) = apply2 ">="
    (>*)  = apply2 ">"

instance (Real e, ExprType e) => IsList (Expr [e]) where
    type Item (Expr [e]) = e
    fromList = expr . return . Const . Vector . map toRational
    toList = error "not implemented"
