{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables, TypeFamilies,
             TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
             FlexibleContexts, ConstraintKinds #-}
module Data.Expression where

import Prelude hiding (const,foldr)

import qualified Data.Array as A
import qualified Data.Array.Abstract as AA
import Data.Array.Abstract ((!))
import qualified Data.Bimap as Bimap
import Data.Boolean
import Data.Expression.Const
import Data.List hiding (foldr)
import qualified Data.List as List
import Data.Maybe
import Data.Ratio
import Control.Applicative ()
import Control.Monad.State
import GHC.Exts hiding (coerce)


------------------------------------------------------------------------------
-- ORPHANS                                                                  --
------------------------------------------------------------------------------

compose :: [a -> a] -> a -> a
compose = List.foldr (.) id

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
        deriving (Eq, Ord)

isInternal :: Id -> Bool
isInternal (Internal _ _) = True
isInternal _ = False

instance Show Id where
  show (Dummy l p)    = "i_"++ show l ++"_"++ show p
  show (Volatile l p) = "x_"++ show l ++"_"++ show p
  show (Internal l p) = "v_"++ show l ++"_"++ show p

data NodeRef = Var Id Type
             | Const ConstVal
             | Index NodeRef [NodeRef]
             deriving (Eq, Ord)

instance Show NodeRef where
  show (Var i t) = "("++ show i ++" :: "++ show t ++")"
  show (Const c) = show c
  show (Index f js) = intercalate "!" (show f : map show (reverse js))

data Node = Apply { fName :: String
                  , fArgs :: [NodeRef]
                  , typeNode :: Type
                  }
          | Array { aShape :: [AA.Interval NodeRef]
                  , aDefs  :: DAG
                  , aHead  :: NodeRef
                  , typeNode :: Type
                  }
          | FoldR { rDefs :: DAG
                  , rHead :: NodeRef
                  , rSeed :: NodeRef
                  , rList :: NodeRef
                  , typeNode :: Type
                  }
          deriving (Eq, Ord, Show)

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

data DExpr = DExpr { fromDExpr :: State Block NodeRef }
runDExpr :: DExpr -> (NodeRef, Block)
runDExpr e = runState (fromDExpr e) emptyBlock
instance Eq DExpr where e == f = runDExpr e == runDExpr f
instance Ord DExpr where
    e `compare` f = runDExpr e `compare` runDExpr f

newtype Expr t = Expr { erase :: DExpr }
expr :: State Block NodeRef -> Expr t
expr = Expr . DExpr
fromExpr :: Expr t -> State Block NodeRef
fromExpr = fromDExpr . erase
runExpr :: Expr t -> (NodeRef, Block)
runExpr  =  runDExpr . erase

type B = Expr Bool
type R = Expr Double
type Z = Expr Integer
type BVec = Expr [Bool]
type RVec = Expr [Double]
type ZVec = Expr [Integer]
type BMat = Expr [[Bool]]
type RMat = Expr [[Double]]
type ZMat = Expr [[Integer]]


------------------------------------------------------------------------------
-- TYPES                                                                    --
------------------------------------------------------------------------------

data Type
    = IntT
    | RealT
    | SubrangeT Type (Maybe NodeRef) (Maybe NodeRef)
    | ArrayT (Maybe String) [AA.Interval NodeRef] Type
    deriving (Eq, Ord)
boolT = SubrangeT IntT (Just $ Const 0) (Just $ Const 1)

instance Show Type where
  show IntT = "Z"
  show RealT = "R"
  show (SubrangeT t a b) = unwords ["Subrange", show t, show a, show b]
  show (ArrayT name sh t) = unwords ["Array", show name, show sh, show t]

newtype TypeOf t = TypeIs Type
class ScalarType t where
    typeOf :: TypeOf t
instance ScalarType Integer where
    typeOf = TypeIs IntT
instance ScalarType Bool where
    typeOf = TypeIs boolT
instance ScalarType Double where
    typeOf = TypeIs RealT

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
typeRef (Const (Exact a))
  | and [ denominator x == 1 | x <- A.elems a ] = typeArray (A.bounds a) IntT
  | otherwise = typeArray (A.bounds a) RealT
typeRef (Const (Approx a)) = typeArray (A.bounds a) RealT
typeRef (Index a _) = let (ArrayT _ _ t) = typeRef a in t

typeArray :: ([Integer],[Integer]) -> Type -> Type
typeArray ([],[]) = id
typeArray (lo,hi) = ArrayT Nothing $ zip (f lo) (f hi)
  where f = map $ Const . fromInteger

typeIndex :: Type -> Type
typeIndex (ArrayT _ [_] t) = t
typeIndex (ArrayT (Just "matrix") (_:sh) t) = ArrayT (Just "row_vector") sh t
typeIndex (ArrayT _ (_:sh) t) = ArrayT Nothing sh t
typeIndex t = error $ "cannot index objects of type "++ show t

coerce :: Type -> Type -> Type
coerce a b | a == b = a
coerce IntT RealT = RealT
coerce RealT IntT = RealT
coerce s t = error $ "cannot coerce "++ show s ++" with "++ show t

cast :: Expr a -> Expr b
cast = expr . fromExpr


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
        f FoldR{} = [] -- TODO
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
simplify (Apply "+" [Const a, Const b] _) = return . Const $ a + b
simplify (Apply "negate" [Const a] _) = return . Const $ negate a
simplify e = do
    dag:_ <- get
    if varies dag $ fArgs e
      then hashcons e
      else liftBlock $ simplify e

apply :: String -> Type -> [ Expr a ] -> Expr r
apply f t xs = expr $ do
    js <- mapM fromExpr xs
    simplify $ Apply f js t

apply1 :: String -> Type -> Expr a -> Expr r
apply1 f t x = apply f t [x]

apply2 :: String -> Type -> Expr a -> Expr b -> Expr r
apply2 f t x y = expr $ do
    i <- fromExpr x
    j <- fromExpr y
    simplify $ Apply f [i,j] t

applyClosed1 :: String -> Expr a -> Expr a
applyClosed1 f x = expr $ do
    i <- fromExpr x
    simplify $ Apply f [i] (typeRef i)

applyClosed2 :: String -> Expr a -> Expr a -> Expr a
applyClosed2 f x y = expr $ do
    i <- fromExpr x
    j <- fromExpr y
    let s = typeRef i
        t = typeRef j
    simplify $ Apply f [i,j] (coerce s t)


------------------------------------------------------------------------------
-- ARRAY COMPREHENSIONS                                                     --
------------------------------------------------------------------------------

type AAI r = AA.AbstractArray (Expr Integer) (Expr r)

-- External references captured by this closure
capture :: Num i => AA.AbstractArray i (Expr t) -> [Id]
capture ar = externRefs adag
  where (_,adag:_) = runExpr $ ar ! replicate (length $ AA.shape ar) 0

makeArray :: forall i t. (ScalarType t)
          => AA.AbstractArray (Expr i) (Expr t)
          -> [AA.Interval NodeRef] -> State Block NodeRef
makeArray ar sh = do
    block <- get
    let ids = [ Dummy (length block) i | i <- [1..length sh] ]
        e = ar ! [ expr . return $ Var i IntT | i <- ids ]
        (ret, dag:block') = runState (fromExpr e) $
            DAG (length block) ids Bimap.empty : block
        TypeIs t = typeOf :: TypeOf t
    put block'
    hashcons $ Array sh dag ret (ArrayT Nothing sh t)

-- constant floating
floatArray :: (Num i, ScalarType t)
           => AA.AbstractArray (Expr i) (Expr t) -> State Block NodeRef
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

array :: (Num i, ScalarType e)
      => Int -> AA.AbstractArray (Expr i) (Expr e) -> Expr t
array n a = if length sh == n
                then expr $ floatArray a
                else error "dimension mismatch"
  where sh = AA.shape a

index :: Expr a -> [Expr i] -> Expr r
index a es = expr $ do
    f <- fromExpr a
    js <- mapM fromExpr es
    return $ Index f js

foldr :: forall a b. (ScalarType b)
      => (Expr a -> Expr b -> Expr b) -> Expr b -> Expr [a] -> Expr b
foldr f r xs = expr $ do
    seed <- fromExpr r
    l <- fromExpr xs
    block <- get
    let d = length block
        s = typeIndex $ typeRef l
        TypeIs t = typeOf :: TypeOf b
        i = expr . return $ Var (Dummy d 1) s
        j = expr . return $ Var (Dummy d 2) t
        (ret, dag:block') = runState (fromExpr $ f i j) $
            DAG d [Dummy d 1, Dummy d 2] Bimap.empty : block
    put block'
    hashcons $ FoldR dag ret seed l t


------------------------------------------------------------------------------
-- INSTANCES                                                                --
------------------------------------------------------------------------------

fromRational' :: Rational -> Expr t
fromRational' = expr . return . Const . fromRational

instance (Num t) => Num (Expr t) where
    (+) = applyClosed2 "+"
    (-) = applyClosed2 "-"
    (*) = applyClosed2 "*"

    negate = applyClosed1 "negate"
    abs    = applyClosed1 "abs"
    signum = applyClosed1 "signum"

    fromInteger x = fromRational' (x % 1)

instance (Fractional t) => Fractional (Expr t) where
    fromRational = fromRational'
    (/) = applyClosed2 "/"

instance (Floating t) => Floating (Expr t) where
    pi = apply "pi" RealT []
    (**) = applyClosed2 "**"

    -- TODO: applying these functions to IntT should yield RealT
    exp   = applyClosed1 "exp"
    log   = applyClosed1 "log"
    sqrt  = applyClosed1 "sqrt"
    sin   = applyClosed1 "sin"
    cos   = applyClosed1 "cos"
    tan   = applyClosed1 "tan"
    asin  = applyClosed1 "asin"
    acos  = applyClosed1 "acos"
    atan  = applyClosed1 "atan"
    sinh  = applyClosed1 "sinh"
    cosh  = applyClosed1 "cosh"
    tanh  = applyClosed1 "tanh"
    asinh = applyClosed1 "asinh"
    acosh = applyClosed1 "acosh"
    atanh = applyClosed1 "atanh"

instance AA.Indexable (Expr [e]) (Expr Integer) (Expr e) where
    a ! e = expr $ do
        f <- fromExpr a
        j <- fromExpr e
        return $ case f of
            (Index g js) -> Index g (j:js)
            _            -> Index f [j]
instance (ScalarType e) => AA.Vector (Expr [e]) (Expr Integer) (Expr e) where
    vector = asVector . array 1
instance (ScalarType e) => AA.Matrix (Expr [[e]]) (Expr Integer) (Expr e) where
    matrix = asMatrix . array 2

asVector :: Expr [e] -> Expr [e]
asVector v = expr $ do
    i <- fromExpr v
    let (ArrayT _ [n] t) = typeRef i
    simplify $ Apply "asVector" [i] (ArrayT (Just "vector") [n] t)

asMatrix :: Expr [[e]] -> Expr [[e]]
asMatrix m = expr $ do
    i <- fromExpr m
    let (ArrayT _ [r,c] t) = typeRef i
    simplify $ Apply "asMatrix" [i] (ArrayT (Just "matrix") [r,c] t)

instance AA.InnerProduct (Expr [e]) (Expr e) where
    u <.> v = expr $ do
        i <- fromExpr $ asVector u
        j <- fromExpr $ asVector v
        let (ArrayT _ _ t) = typeRef i
        simplify $ Apply "<.>" [i,j] t

instance AA.LinearOperator (Expr [[e]]) (Expr [e]) (Expr [e]) where
    m #> v = expr $ do
        i <- fromExpr $ asMatrix m
        j <- fromExpr $ asVector v
        let (ArrayT _ [r,_] t) = typeRef i
        simplify $ Apply "#>" [i,j] (ArrayT (Just "vector") [r] t)

instance AA.SquareMatrix (Expr [[e]]) where
    chol m = expr $ do
        i <- fromExpr $ asMatrix m
        simplify $ Apply "chol" [i] (typeRef i)
    inv m = expr $ do
        i <- fromExpr $ asMatrix m
        simplify $ Apply "inv" [i] (typeRef i)

instance Boolean (Expr Bool) where
    true  = apply "true" boolT []
    false = apply "false" boolT []
    notB  = applyClosed1 "not"
    (&&*) = applyClosed2 "&&"
    (||*) = applyClosed2 "||"

type instance BooleanOf (Expr t) = Expr Bool

instance IfB (Expr t) where
  ifB c x y = expr $ do
    k <- fromExpr c
    i <- fromExpr x
    j <- fromExpr y
    let s = typeRef i
        t = typeRef j
    simplify $ Apply "ifThenElse" [k,i,j] (coerce s t)

instance EqB (Expr t) where
    (==*) = apply2 "==" boolT
    (/=*) = apply2 "/=" boolT

instance OrdB (Expr t) where
    (<*)  = apply2 "<"  boolT
    (<=*) = apply2 "<=" boolT
    (>=*) = apply2 ">=" boolT
    (>*)  = apply2 ">"  boolT

instance (Fractional e, Real e) => IsList (Expr [e]) where
    type Item (Expr [e]) = e
    fromList = expr . return . Const . fromList . map real
    toList e =
      case fst (runExpr e) of
        Const c -> map real $ toList c
        r -> error $ "cannot convert "++ show r ++" to list"

class ExprTuple t where
    fromExprTuple :: t -> [DExpr]
    fromConstVals :: [ConstVal] -> t

zipExprTuple :: (ExprTuple t) => t -> t -> [(DExpr,DExpr)]
zipExprTuple s t = fromExprTuple s `zip` fromExprTuple t

const :: ConstVal -> Expr t
const = expr . return . Const

instance ExprTuple (Expr a) where
    fromExprTuple (a) = [erase a]
    fromConstVals [a] = (const a)
    fromConstVals _ = undefined
instance ExprTuple (Expr a, Expr b) where
    fromExprTuple (a,b) = [erase a, erase b]
    fromConstVals [a,b] = (const a, const b)
    fromConstVals _ = undefined
instance ExprTuple (Expr a, Expr b, Expr c) where
    fromExprTuple (a,b,c) = [erase a, erase b, erase c]
    fromConstVals [a,b,c] = (const a, const b, const c)
    fromConstVals _ = undefined
instance ExprTuple (Expr a, Expr b, Expr c, Expr d) where
    fromExprTuple (a,b,c,d) = [erase a, erase b, erase c, erase d]
    fromConstVals [a,b,c,d] = (const a, const b, const c, const d)
    fromConstVals _ = undefined
instance ExprTuple (Expr a, Expr b, Expr c, Expr d, Expr e) where
    fromExprTuple (a,b,c,d,e) =
      [erase a, erase b, erase c, erase d, erase e]
    fromConstVals [a,b,c,d,e] =
      (const a, const b, const c, const d, const e)
    fromConstVals _ = undefined
instance ExprTuple (Expr a, Expr b, Expr c, Expr d, Expr e, Expr f) where
    fromExprTuple (a,b,c,d,e,f) =
      [erase a, erase b, erase c, erase d, erase e, erase f]
    fromConstVals [a,b,c,d,e,f] =
      (const a, const b, const c, const d, const e, const f)
    fromConstVals _ = undefined
instance ExprTuple (Expr a, Expr b, Expr c, Expr d, Expr e, Expr f, Expr g) where
    fromExprTuple (a,b,c,d,e,f,g) =
      [erase a, erase b, erase c, erase d, erase e, erase f, erase g]
    fromConstVals [a,b,c,d,e,f,g] =
      (const a, const b, const c, const d, const e, const f, const g)
    fromConstVals _ = undefined
instance ExprTuple (Expr a, Expr b, Expr c, Expr d, Expr e, Expr f, Expr g, Expr h) where
    fromExprTuple (a,b,c,d,e,f,g,h) =
      [erase a, erase b, erase c, erase d, erase e, erase f, erase g, erase h]
    fromConstVals [a,b,c,d,e,f,g,h] =
      (const a, const b, const c, const d, const e, const f, const g, const h)
    fromConstVals _ = undefined

instance Show DAG where
  show dag = unlines $ map f (nodes dag)
    where f (i,n) = show (Internal (dagLevel dag) i) ++" = "++ show n
