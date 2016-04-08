{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables, TypeFamilies,
             TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
             FlexibleContexts #-}
module Data.Expression where

import qualified Data.Array as A
import qualified Data.Array.Abstract as AA
import Data.Array.Abstract ((!))
import qualified Data.Bimap as Bimap
import Data.Boolean
import Data.Expression.Const
import Data.List
import Data.Maybe
import Data.Ratio
import Data.String
import Control.Applicative ()
import Control.Monad.State
import GHC.Exts


------------------------------------------------------------------------------
-- ORPHANS                                                                  --
------------------------------------------------------------------------------

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


------------------------------------------------------------------------------
-- EXPRESSIONS                                                              --
------------------------------------------------------------------------------

type Pointer = Int
type Level = Int

data Id = Dummy Level Pointer
        | Volatile Level Pointer
        | Internal Level Pointer
        deriving (Eq, Ord, Show)

data NodeRef = Var Id Type
             | Const ConstVal
             | Index NodeRef [NodeRef]
             deriving (Eq, Ord, Show)
data Node = Apply { fName :: String
                  , fArgs :: [NodeRef]
                  , typeNode :: Type
                  }
          | Array { aShape :: [AA.Interval NodeRef]
                  , aDefs  :: DAG
                  , aHead  :: NodeRef
                  , typeNode :: Type
                  }
          deriving (Eq, Ord)

data DAG = DAG { dagLevel :: Level
               , inputs :: [Id]
               , bimap  :: Bimap.Bimap Node Pointer
               } deriving (Eq, Ord)
emptyDAG :: DAG
emptyDAG = DAG 0 [] Bimap.empty
nodes :: DAG -> [(Pointer, Node)]
nodes dag = Bimap.toAscListR $ bimap dag

type Block = [DAG]
emptyBlock :: [DAG]
emptyBlock = [emptyDAG]

data DExpr = DExpr { typeDExpr  :: Type
                   , fromDExpr :: State Block NodeRef }
runDExpr :: DExpr -> (NodeRef, Block)
runDExpr e = runState (fromDExpr e) emptyBlock
instance Eq DExpr where e == f = runDExpr e == runDExpr f
instance Ord DExpr where
    e `compare` f = runDExpr e `compare` runDExpr f

newtype Expr t = Expr { erase :: DExpr }
expr :: forall t. (ExprType t) => State Block NodeRef -> Expr t
expr = Expr . DExpr t     where (TypeIs t) = typeOf :: TypeOf t
typeExpr :: Expr t -> Type
typeExpr = typeDExpr . erase
fromExpr :: Expr t -> State Block NodeRef
fromExpr = fromDExpr . erase
runExpr :: Expr t -> (NodeRef, Block)
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
    typeOf = TypeIs $ SubrangeT IntT (Just $ Const 0) (Just $ Const 1)
instance ExprType Double where
    typeOf = TypeIs RealT
instance forall b. (ExprType b) => ExprType [b] where
    typeOf = TypeIs $
        case t of
            (ArrayT _ _ r) -> ArrayT Nothing (error "undefined array bounds") r
            r              -> ArrayT Nothing (error "undefined array bounds") r
      where (TypeIs t) = typeOf :: TypeOf b

internal :: Level -> Pointer -> State Block NodeRef
internal level i = do
    t <- getType
    return $ Var (Internal level i) t
  where getType = do
          dag:_ <- get
          if level == dagLevel dag
            then return . typeNode . fromJust . Bimap.lookupR i $ bimap dag
            else liftBlock getType

typeRef :: NodeRef -> Type
typeRef (Var _ t) = t
typeRef (Const (Exact  a)) = typeArray (A.bounds a) RealT
typeRef (Const (Approx a)) = typeArray (A.bounds a) RealT
typeRef (Index a _) = let (ArrayT _ _ t) = typeRef a in t

typeArray :: ([Integer],[Integer]) -> Type -> Type
typeArray ([],[]) = id
typeArray (lo,hi) = ArrayT Nothing $ zip (f lo) (f hi)
  where f = map $ Const . fromInteger


------------------------------------------------------------------------------
-- DAG BUILDING                                                             --
------------------------------------------------------------------------------

-- http://okmij.org/ftp/tagless-final/sharing/sharing.pdf
hashcons :: Node -> State Block NodeRef
hashcons e = do
    DAG level inp vdag : parent <- get
    case Bimap.lookup e vdag of
        Just k -> internal level k
        Nothing -> do
            let k = Bimap.size vdag
            put $ DAG level inp (Bimap.insert e k vdag) : parent
            internal level k

-- perform an action in the enclosing block
liftBlock :: MonadState [a] m => State [a] b -> m b
liftBlock s = do
    dag:parent <- get
    let (ref, parent') = runState s parent
    put $ dag:parent'
    return ref

-- does a list of expressions depend on the inputs to this block?
varies :: DAG -> [NodeRef] -> Bool
varies (DAG level inp _) xs = level == 0 || any p xs
  where p (Var (Internal l _) _) = l == level
        p (Var s _) = s `elem` inp
        p (Const _) = False
        p (Index a is) = p a || any p is

-- collect External references
externRefs :: DAG -> [Id]
externRefs (DAG _ _ d) = go d
  where go defs = concatMap (f . snd) $ Bimap.toAscListR defs
        f (Apply _ args _) = mapMaybe extern args
        f (Array sh (DAG _ _ defs') r _) =
            mapMaybe extern [r] ++ mapMaybe (extern . fst) sh
                                ++ mapMaybe (extern . snd) sh ++ go defs'
        extern (Var (Internal _ _) _) = Nothing
        extern (Var i _) = Just i
        extern _ = Nothing
        -- TODO Index


------------------------------------------------------------------------------
-- FUNCTION APPLICATION                                                     --
------------------------------------------------------------------------------

-- simplify and float constants
simplify :: Node -> State Block NodeRef
-- TODO: eval const exprs
simplify e = do
    dag:_ <- get
    if varies dag $ fArgs e
      then hashcons e
      else liftBlock $ simplify e

apply :: forall a r.    (ExprType r) => String ->    [ Expr a ]    -> Expr r
apply f xs = expr $ do
    js <- mapM fromExpr xs
    simplify $ Apply f js t
  where (TypeIs t) = typeOf :: TypeOf r

apply1 :: forall a r.   (ExprType r) => String -> Expr a           -> Expr r
apply1 f x = apply f [x]

apply2 :: forall a b r. (ExprType r) => String -> Expr a -> Expr b -> Expr r
apply2 f x y = expr $ do
    i <- fromExpr x
    j <- fromExpr y
    simplify $ Apply f [i,j] t
  where (TypeIs t) = typeOf :: TypeOf r

applyClosed2 :: (ExprType a) => String -> Expr a -> Expr a -> Expr a
applyClosed2 f x y = expr $ do
    i <- fromExpr x
    j <- fromExpr y
    let s = typeRef i
        t = typeRef j
    if s /= t then error $ "type mismatch: "++ show i ++" :: "++ show s ++
                                      " /= "++ show j ++" :: "++ show t
              else simplify $ Apply f [i,j] t

apply3 :: forall a b c r. (ExprType r) => String -> Expr a -> Expr b
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
capture :: Num i => AA.AbstractArray i (Expr t) -> [Id]
capture ar = externRefs adag
  where dim = length $ AA.shape ar
        dummy = replicate dim 0
        (_,adag:_) = runExpr $ AA.apply ar dummy

makeArray :: ExprType i => AA.AbstractArray (Expr i) (Expr t)
                        -> [AA.Interval NodeRef] -> State Block NodeRef
makeArray ar sh = do
    block <- get
    let ids = [ Dummy (length block) i | i <- [1..length sh] ]
        e = ar ! [ expr . return $ Var i IntT | i <- ids ]
        (ret, dag:block') = runState (fromExpr e) $
            DAG (length block) ids Bimap.empty : block
    put block'
    hashcons $ Array sh dag ret (ArrayT Nothing sh $ typeExpr e)

-- constant floating
floatArray :: (Num i, ExprType i) => AA.AbstractArray (Expr i) (Expr t)
                                  -> State Block NodeRef
floatArray ar = do
    dag:_ <- get
    sh <- sequence . flip map (AA.shape ar) $ \interval -> do
        i <- fromExpr $ fst interval
        j <- fromExpr $ snd interval
        return (i,j)
    if varies dag (map fst sh) ||
       varies dag (map snd sh) ||
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
fromRational' = expr . return . Const . fromRational

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

instance (ExprType e) => AA.Indexable (Expr [e]) (Expr Integer) (Expr e) where
    a ! e = expr $ do
        f <- fromExpr a
        j <- fromExpr e
        return $ case f of
            (Index g js) -> Index g (j:js)
            _            -> Index f [j]
    bounds a = (expr $ return lo, expr $ return hi)
      where (ArrayT _ [(lo,hi)] _) = typeExpr a
instance (ExprType e) => AA.Vector (Expr [e]) (Expr Integer) (Expr e) where
    vector = asVector . array 1
instance (ExprType e) => AA.Matrix (Expr [[e]]) (Expr Integer) (Expr e) where
    matrix = asMatrix . array 2

asVector :: (ExprType e) => Expr [e] -> Expr [e]
asVector v = expr $ do
    i <- fromExpr v
    let (ArrayT _ [n] t) = typeRef i
    simplify $ Apply "asVector" [i] (ArrayT (Just "vector") [n] t)

asMatrix :: (ExprType e) => Expr [[e]] -> Expr [[e]]
asMatrix m = expr $ do
    i <- fromExpr m
    let (ArrayT _ [r,c] t) = typeRef i
    simplify $ Apply "asMatrix" [i] (ArrayT (Just "matrix") [r,c] t)

instance (ExprType e) => AA.LinearOperator (Expr [[e]])
                                           (Expr [e]) (Expr [e]) where
    m #> v = expr $ do
        i <- fromExpr $ asMatrix m
        j <- fromExpr $ asVector v
        let (ArrayT _ [r,_] t) = typeRef i
        simplify $ Apply "#>" [i,j] (ArrayT (Just "vector") [r] t)

instance (ExprType e) => AA.SquareMatrix (Expr [[e]]) where
    chol m = expr $ do
        i <- fromExpr $ asMatrix m
        simplify $ Apply "chol" [i] (typeRef i)

instance Boolean (Expr Bool) where
    true  = apply "true" []
    false = apply "false" []
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
    fromList = expr . return . Const . fromList . map toRational
    toList = error "not implemented"

class ExprTuple t where
    unify :: t -> t -> [(DExpr,DExpr)]

instance ExprTuple (Expr a) where
    unify a x = [(erase a, erase x)]
instance ExprTuple (Expr a, Expr b) where
    unify (a,b) (x,y) = [(erase a, erase x)
                        ,(erase b, erase y)]
instance ExprTuple (Expr a, Expr b, Expr c) where
    unify (a,b,c) (x,y,z) = [(erase a, erase x)
                            ,(erase b, erase y)
                            ,(erase c, erase z)]
instance ExprTuple (Expr a, Expr b, Expr c, Expr d) where
    unify (a,b,c,d) (x,y,z,w) = [(erase a, erase x)
                                ,(erase b, erase y)
                                ,(erase c, erase z)
                                ,(erase d, erase w)]
instance ExprTuple (Expr a, Expr b, Expr c, Expr d, Expr e) where
    unify (a,b,c,d,e) (x,y,z,w,v) = [(erase a, erase x)
                                    ,(erase b, erase y)
                                    ,(erase c, erase z)
                                    ,(erase d, erase w)
                                    ,(erase e, erase v)]
instance ExprTuple (Expr a, Expr b, Expr c, Expr d, Expr e, Expr f) where
    unify (a,b,c,d,e,f) (x,y,z,w,v,u) = [(erase a, erase x)
                                        ,(erase b, erase y)
                                        ,(erase c, erase z)
                                        ,(erase d, erase w)
                                        ,(erase e, erase v)
                                        ,(erase f, erase u)]

instance Show Node where
  show (Apply f js _) = show f ++ show js
  show (Array sh dag ret _) =
    "ARRAY "++ show (inputs dag) ++ show sh ++ show ret ++"\nWHERE {\n"++ show dag ++"}"
instance Show DAG where
  show dag = unlines $ map f (nodes dag)
    where f (i,n) = show (Internal (dagLevel dag) i) ++" = "++ show n
